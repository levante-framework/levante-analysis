# Reorganizing the LEVANTE analysis book — proposal

*Drafted 2026-10-30 after a full read of the repo (all 00–10 chapters, `tasks/`,
`reports/`, `slides/`, `old/`, and every helper script). Status: **proposal for
discussion** — nothing in the book has been moved yet.*

The book has grown from ~10 sequential notebooks into ~90 files / 27k lines
covering three different kinds of work that are currently interleaved:
battery-wide monitoring, per-task psychometrics, and one-off investigations.
This document proposes (1) a front-end organization, (2) a shared backend,
(3) two conceptual frameworks for the questions we keep asking, and (4) a
testing strategy, then a phased migration plan. Section 6 lists the bugs the
survey turned up along the way.

---

## 0. What the survey found (the short version)

**The book's "living" core has quietly gone static.** Chapters 00–08 and most
task chapters were last touched in July/August and run on the 2026-08-31
per-site `"next"` drafts. Only the H&F v2 chapter uses `levante_data_latest`
v3_17, which has been current since October. Re-rendering would not help much,
because:

- **Every chapter hardcodes its numbers in prose** (00, 01, 02, 03, 04, 05, 07,
  10, index, README, slides). A re-render on new data produces text that
  contradicts its own tables. `index.qmd` claims the opposite.
- **`freeze: true` plus 25 `cache=TRUE` chunks** keyed on chunk code only means
  a new `scores_all_sites.rds` does not invalidate cached fits; a re-render
  silently mixes old and new results.
- **There are five data regimes**, not one:
  - `common.R` binds per-site `"next"` drafts. These are mutable; the cache filename is the only anchor.
  - About six scripts pull raw tables with their own pins, and the pins disagree. Bogotá raw is `v17_0` in Stories and `v18_4` in H&F v2.
  - `../levante-pilots/01_fetched_data/*.rds` (SDS, SRE, mrot) is outside the repo and unversioned.
  - The deprecated hackathon dataset (SRE DDM).
  - Hand-placed caches with no producer anywhere: `scoring_table_cache.rds`,
    `math_model_record_v1_14.rds`, `vocab_model_*_v1_14.rds`,
    `scores_latest_v3_17.rds`, `hf_v2/item_parameters_released.rds`,
    `latest_r2_trials_mg.rds`, `mg_model_record_current.rds`,
    `fcat_story_level__2026-09-24.rds`, `data/tom_raw/*`, `trog_item_images.csv`.
- **Item parameters don't match the scores.** `load_item_parameters()` defaults
  to scoring metadata v1_14. Scores are registry v2_3. Chapters 06, 07, 08 and 10
  (person-fit, difficulty detrending, IRF baselines) and the H&F, memory, TROG,
  vocab and mrot item sections all use v1_14 parameters.

**The same concept is implemented 2–6 different ways**, so numbers aren't
comparable across chapters:

| Concept | # implementations | How they differ |
|---|---|---|
| Test–retest pair | ≥ 9 | first-two vs consecutive; min gap none / 18 d / 14 d / 1 mo / 2 mo / 300 d; Western in/out; mode filter or not; ordered by age vs `time_started` |
| Split-half reliability | 6 | odd/even unsorted (row-order dependent), first/second half, sum vs IRT halves |
| Empirical / marginal reliability | 5 | different samples (adaptive-only vs all); mirt `marginal_rxx` misused for Rasch with estimated variance (the source of June's ToM ".92") |
| Grid-EAP scorer | 5 | memory (3 copies), Stories rel, Stories CAT sim, H&F nominal, H&F refit |
| Fixed-guess IRT fitting | 4 | mirt `guess=`, editing the values table, hand 1-D optimize, custom glmer link |
| Rescoring wrapper around `score_task_irt` | 3 | vocab, corrected_trajectories, hero |
| Per-language DIF | 3 | crosslang, TROG `fit_site`, Stories `run_invariance` |
| Model-comparison table | 6 | SDS `eval_table`, Stories `fit_stats`/`msum`/`fit_summary`, mrot `model_comparison`, H&F nominal `metrics` |
| "First run per child" | ≥ 8 | grouped by user vs user×task (→ a real bug, §6) |
| Age | 3 | `scores$age`; birth month/year + `time_started`; pilots `run_data$age`; and `age_c` centred at mean / 8 / 9 |
| Admin time | 3 | trial timestamp span; `time_finished − time_started`; consecutive-gap per story |
| Dataset/site/language lookups | ~15 | re-typed lists, including `sites4`, `long_sites`, `raw_specs`, `lang_of`, `pop_lab`; rural CO is mislabelled "Bogotá-rural" in 03/04/07 |

**Almost every rds a chapter writes is never read.** The only real data
dependency is `00 → data/scores_all_sites.rds → ~25 files`. The inter-chapter
coupling (04 cites 03's ΔCFI, 05 adopts 03's structure, 07 cites 06) exists in
prose only and does not update.

**The good news:** the hard parts already exist, scattered.

- `_stories_cat_sim.R` is a real design-simulation engine. It separates
  selection, generating and scoring parameter sets; uses common random numbers;
  has a timing model; and anchors against the observed design.
- `_sds_scoring_fits.R` has the cleanest mirt fitting and comparison machinery.
- The memory-length scripts have exact-replay and simulation engines plus the
  only per-trial time model.
- `sre.qmd` and `mrot_item_set_compare.qmd` each built the same
  "variants × criteria" scoreboard.
- The Stories build's reconciliation against releases is effectively a
  regression test of the production pipeline.

The proposal is mostly to **lift these into one place and make the rest of the
book use them.**

---

## 1. Front end: four kinds of chapter

The main organizing principle: **each chapter declares its data contract.**

- A *living* chapter runs on "the current release". It must compute every
  number it states, and it re-renders when the release changes.
- A *study* is pinned to a specific data version. It is frozen once concluded,
  and it carries a dated header saying what it found, what was decided, and
  whether that still holds.

### 1a. Monitor — living, battery-wide (re-render on each `levante_data_latest` release)

| New chapter | From | Notes |
|---|---|---|
| **Release & data QC** | 00 + 01 + `reports/adaptive_missingness` | inventory, coverage, retest-pair counts; plus the automated data-contract checks of §4b ("what changed since the last release") |
| **Reliability & stability** | `test_retest_figures` + reliability bits of 03/05/07 | one scoreboard row per task (§3a): empirical rel, split-half, retest by gap bin, age r, minutes, floor/ceiling |
| **Item health & DIF** | `crosslang_dif_batch` + TROG DIF + below-chance screens | all tasks × all languages, using the production model's parameters, not ad hoc Rasch fits |
| **Growth** | 02 | within-child vs cross-sectional, SE-aware (§3b) |
| **Structure** | 03 + 04 | 04 is blocked until MEFS scores have an in-repo source |
| **Battery efficiency** | 05 | becomes the battery level of the design framework (§3a) |
| **Network coverage** | 09 + 00's coverage section | planned (grant metadata) vs actual (inventory) |
| **Downward extension** | 10 | living per-cohort report while data arrive |

### 1b. Task reports — living, one parameterized template × 13 measures

A single `task_report.qmd` with `params: task_id`, rendered once per task.
Every section below already exists for some task; the template makes them exist
for all:

1. Deployment & forms: variants, task versions, adaptive/fixed, the **design
   registry** (§3a), timeline of changes.
2. Coverage & inventory: runs by site × age × wave; repeat children.
3. Item bank: Wright map (bank vs items seen vs population θ), uncalibrated
   items, exposure for CAT tasks.
4. Item health: below-chance by item × language (key-defect screen), rest-score
   monotonicity, flips vs last release.
5. Scores: floor/ceiling share by age, SE by θ, rescoring reproduces released
   scores (r = 1 check).
6. Reliability, stability, validity: the scoreboard row expanded.
7. Efficiency: information per minute by age band (§3a).
8. A short **task-specific signature** section (angle effect, grammatical
   category, span length, number-line routing, story/question type). This is
   where today's per-task deep dives shrink to.
9. Open issues and decisions, linking to the studies that produced them.

Today's `shape_rotation`, `sentence_understanding`, `memory`, `vocab`,
`roar_literacy`, `hearts_and_flowers` (v1) and the reusable §1 of
`math_item_calibration` dissolve into this template plus a signature section.

### 1c. Studies — static, dated, pinned

Each study is a folder `studies/YYYY-MM_slug/` with its qmd, its scripts, and a
`STUDY.yml` front matter. Fields:

- `question`
- `data` (exact pinned refs)
- `date_concluded`
- `verdict` (2–3 lines)
- `status`: one of `adopted` / `recommended` / `pending DCC` / `superseded by …`
- `upstream`: PRs/issues

Studies render from `_freeze` forever. They do **not** need to be ported to the
new backend: their code stays as it was, with pinned inputs. That is the point.

| Study | Current files |
|---|---|
| 2026-06 IRT column-order bug | `math_eap_vs_ml`, `corrected_trajectories`, vocab gold-validation, `hero_math_figure.R`, `reports/rlevante_handoff.md` |
| 2026-06 ToM reality-check forensics | `tom_reality_check_bug` |
| 2026-07 SRE scoring | `sre.qmd`, `_fit_sre_*`, `sherlock_sre_ddm/` |
| 2026-07 SDS scoring models | `same_different`, `_sds_scoring_fits.R` |
| 2026-07 ROAR-Word engagement rule | `roar_word` |
| 2026-07 Training vs development | `training_vs_development` |
| 2026-07 Adaptive-flag missingness | `reports/adaptive_missingness` |
| 2026-07/08 Within-child variability | 06, 07, 08 (one study, three chapters; 07's RT-IIV retest could later graduate to the monitor) |
| 2026-08 Memory length | `memory_length`, `_prep_memory_length*` |
| 2026-09 Stories consolidated (+ 2026-10 follow-ups) | `stories`, `_build_stories_data.R`, `_stories_*`, `build_tom_metadata.R` |
| 2026-09 TROG skin tone | `trog_skin_tone*` |
| 2026-10 H&F v2 | `hearts_and_flowers_v2`, `_hf_v2_*`, `_prep_hf_v2` |
| 2026-10 SDS timing | **scripts are not in the repo** — needs committing |
| (incomplete) mrot item sets | `tasks/mrot/` (inputs missing; already excluded from the book) |

### 1d. Archive

`old/*`, `math.qmd` and `math_item_calibration.qmd` (their conclusions were
retracted by the column-bug study), `mrot_model_explore.qmd`, the deprecated
hackathon pull scripts, and the June `slides/summary.qmd` deck. Not rendered.

### 1e. Supporting front matter

- **A findings & decisions register** (`findings.yml` → rendered table). One row
  per finding with id, date, data version, status, and the study/report that
  supports it. This replaces the hand-maintained "Headline findings" in README,
  index and slides, which currently disagree with each other (e.g. 14- vs 16-min
  screener, 55 vs 59 min).
- `handoffs/` for the DCC-facing `.md` write-ups now in `reports/`.

Proposed `_quarto.yml` parts: **Monitor · Tasks · Studies · Register**.

---

## 2. Backend: one in-repo R package

Make the repo root a small R package, working name **`levanteanalysis`**.
`common.R` becomes a three-line shim (`pkgload::load_all()` plus theme), so
existing chapters keep working during migration.

Boundary rule:

- Anything that changes **how production scores are made** (the SDS recode fix,
  the SRE rate score, ToM uid fixes, H&F v2 recode windows) is drafted here as a
  clearly-marked `repair_*()` with a test, then upstreamed to `levantemodels`.
  Once merged, the local copy becomes a one-line alias with a deprecation note.
- Everything **analysis-only** stays here.

Modules (`R/` files), with the main signatures:

```r
# ---- data/  : one way in -----------------------------------------------------
lv_source("latest", version = "v3_17")          # levante_data_latest, pinned
lv_source("site",   dataset = "pilot_mpieva_de_main", version = "v7_2")
lv_source("raw",    dataset = "pilot_mpieva_de_main", version = "v16_29")
lv_source("draft",  dataset = ..., snapshot = "2026-08-31")   # "next" only with a stamp
lv_scores(src); lv_trials(src, tasks = ); lv_runs(src); lv_raw(src, tables = , task = )
lv_model_record(task, registry = "v2_3"); lv_item_params(task, registry = "v2_3")
```

- **`datasets.yml` registry** replaces `levante_site_specs`, `site_pal`,
  `site_labels_named`, `dataset_labels_named`, the H&F/Stories `raw_specs`, and
  every local language map. Each entry carries: dataset name + code, raw
  counterpart + code, site, language(s), cohort type
  (longitudinal/cross-sectional/downex), label, colour.
- **Content-addressed cache with a manifest.** The key is the full qualified ref
  plus query. The manifest records row counts, a hash, and pull time. A
  `"next"` ref is refused without a snapshot stamp, and a refresh never silently
  overwrites (it versions the file).
- **Every returned tibble carries `attr(, "lv_provenance")`.** A `provenance()`
  chunk at the top of every chapter prints exactly what was loaded.
- **Item parameters and model records come from the same registry version that
  produced the scores.** This fixes the v1_14/v2_3 mismatch by construction.

```r
# ---- canonical tables -----------------------------------------------------------
lv_runs()   # one row per run: run_id, user_id, dataset, site, language, task_id,
            # form, variant, adaptive, task_version, time_started, minutes, age,
            # wave, gap_from_prev — ONE definition of each
lv_panel(scores)  # child × task × occasion θ with SE, age, gap, form/mode changes
```

```r
# ---- repairs/  : known data defects, each with a test and an upstream status -----
repair_hf_legacy_formats(); repair_tom_uids(); repair_sds_recode(); score_sre_rate()
```

```r
# ---- irt/  : psychometric primitives -------------------------------------------
irt_matrix(trials, items = NULL, occurrences = FALSE)  # wide; aligned to model items
fit_irt(mat, spec)          # spec: itemtype, guess rule, priors, groups, invariance,
                            # theta grid; cache keyed on hash(data, spec)
score_eap(mat, params, prior, grid)  # one grid-EAP engine (replaces 5)
irf(); item_info(); test_info()
```

`irt_matrix()` makes the positional-`fscores()` bug impossible: columns are
always reordered to the model's items, and a test proves it.

```r
# ---- reliability/ & longitudinal/ ------------------------------------------------
rel_empirical(); rel_marginal(params, mu, sigma2)   # correct under N(μ, σ²)
rel_split_half(trials, split = c("odd_even","halves"), score = c("sum","irt"))
spearman_brown(); alpha()
retest_pairs(runs, rule = c("first_two","consecutive"), min_gap, max_gap,
             same_form = NULL)                        # ONE definition, explicit args
retest_r(pairs, ci = c("fisher","boot"), disattenuate = TRUE)
growth_lmm(panel, ...); shrinkage_factor(panel)       # var(EAP)/σ²
```

```r
# ---- evaluate/  : the scoreboard (§3a) ----------------------------------------------
evaluate_scores(scores, criteria = lv_criteria(), within = "dataset",
                reference = NULL, boot = 500)
```

```r
# ---- design/  : the design evaluator (§3a) ----------------------------------------
timing_model(task, src)                 # per-item/unit time, per-child speed
replay(trials, rule)                    # exact counterfactual on real sequences
simulate_design(bank, population, design, timing, scoring, reps)
anchor_check(sim, observed)             # must reproduce the deployed design first
```

```r
# ---- structure/ ---------------------------------------------------------------
fit_cfa_set(); invariance_ladder(); local_sem(); model_table(); dif_screen()
```

```r
# ---- report/  : prose helpers so living chapters never hardcode numbers ----------
fmt_r(); fmt_ci(); fmt_n(); pick()      # (generalizing stories.qmd's fail-loud pick)
```

Plots: `cor_heatmap()`, `retest_scatter()`, `wright_map()`, `scoreboard_plot()`,
palettes from the registry.

**What not to build:** a framework for studies. Studies can use the package
opportunistically, but a study's job is to answer one question once.

---

## 3. Conceptual unification: two families of questions

### 3a. Measurement efficiency — "how much do we learn per minute?"

Most task studies are the same question with a different variant:

- memory staircase rules
- H&F v2 deadlines / feedback / stop rule
- SDS scoring floors and timing
- Stories block CAT
- SRE scoring
- mrot item granularity
- battery length (05)

The proposed objects:

- **Task design** = (item bank + parameters, administration rule [selection,
  blocks, staircase, deadlines, stop rule], timing model, scoring model). A
  **design registry** table records which design was live for which variants
  and dates. This matters because H&F form follows the *variant*, not the code
  version, and the SDS prompt change broke processing.
- **Variant** = a design with one thing changed. A variant is one of five
  kinds:
  - trial transform (counterfactual deadline)
  - rule replay (error budget, stop rule)
  - simulator rule (selection/stopping)
  - recode
  - scoring model
- **Two engines**, both already written once:
  1. **Replay**: exact counterfactuals on real trial sequences (memory budget,
     H&F deadlines, stop rule). Use it when the variant only truncates or
     re-codes what children actually did.
  2. **Simulation**: model-based, generalized from `_stories_cat_sim.R`. Use it
     when the variant changes *which* items a child would see. It must pass
     `anchor_check()`, reproducing the deployed design's observed
     reliability and minutes, before any variant is compared. Stories and
     memory both did this; it should be mandatory.
- **One scoreboard** (`evaluate_scores`). Every study so far used some subset
  of these criteria:

  | Tier | Criteria |
  |---|---|
  | precision | model reliability; empirical reliability; SE at target θ; information per minute (one estimator: ratio of means, reported with the per-run median) |
  | stability | split-half (IRT halves); retest by gap bin, disattenuated |
  | validity | age slope/r within dataset; partial r with same-construct tasks; agreement with reference scores |
  | cost | minutes q50/q90 overall and by ability quartile; early-stop / frustration rate for low-ability children |
  | targeting | floor/ceiling share by age; information-vs-population-density overlap |

  The scoreboard always computes within dataset and pools n-weighted, with
  paired bootstrap differences against the reference design. This is how H&F
  v2, SDS and SRE already did it, by hand, three times.
- **Hierarchy:** item → task design → battery. 05's factor-score determinacy
  per minute is the same evaluator one level up: a battery is a set of task
  designs, and the objective is construct recovery per minute. Monitoring is the
  same machinery run on the *deployed* design each release: "is each task still
  delivering the information/minute we expect, in each age band and site?"

This is also how we would systematically **assess old variants**. Every past
design in the registry gets an efficiency card. A new variant proposal (from
dev, sites, or us) gets evaluated with the same scoreboard against the current
design before a recommendation goes to the DCC.

### 3b. Cross-task & longitudinal structure — "what is the shape of development?"

The proposed object is the **panel**: child × task × occasion θ **with SE**,
plus age, gap, form/mode changes, site, language. Every structural question is
a model on the panel:

| Question | Today | Unified as |
|---|---|---|
| Structure (g, fluid/verbal, bifactor) | 03, 04 | `fit_cfa_set` on first occasions, SE-informed fixed error |
| Invariance (site, age, language) | 03 ladders ×2, Stories `run_invariance` | `invariance_ladder` (task-level) / `dif_screen` (item-level) |
| Differentiation | 04 local SEM | `local_sem` over age |
| Growth | 02, corrected trajectories | `growth_lmm`, with SE-aware within/between decomposition |
| Stability | test_retest, 07, Stories `stab_table` | `retest_pairs` + `retest_r` by gap bin |
| Variability | 06–08 | person-level indices on the panel (if revisited) |

Two methodological rules become defaults instead of things each chapter
remembers (or doesn't):

1. **Measurement error is always carried.** Retest and structure use
   SE-disattenuation. Growth corrects the EAP shrinkage factor var(EAP)/σ²,
   which the Stories sim found to be 0.41–0.78.
2. **Comparisons are within site/dataset, pooled after.** Site mean comparisons
   are never the target.

The panel also makes the adaptive/fixed and form boundaries explicit columns,
so "did this pair cross a mode boundary?" is a filter, not a re-derivation.

### 3c. A third, implicit family: data-integrity forensics

Each of these was found by a human noticing something odd, months later:

- the column-order bug
- the ToM uid mislabels
- the SDS prompt-text change that has dropped nearly all match blocks since September
- SRE trials-vs-seconds
- the TROG `answer` field
- ToM placeholder rows

They are better treated as **automated checks** (§4b) that run on every release.
Then the monitor's QC chapter catches them in the release where they appear.

---

## 4. Tests and error-finding

### 4a. Unit tests (testthat, synthetic data, runs in CI without Redivis)

Each test targets a bug class we have actually hit:

- `irt_matrix` + `score_eap`: shuffle the input columns and require identical
  scores, and r = 1.000 with `mirt::fscores` (the column-order bug).
- Reliability estimators: on simulated data with known true reliability. This
  also guards against the `marginal_rxx` misuse.
- `retest_pairs` / "first per child": multi-task input must keep one row per
  child × task (the SRE `first_run` bug, §6).
- Split-half: invariant to input row order (the 06/07 bug).
- `simulate_design`: a fixed-form design recovers the closed-form reliability;
  random selection with a known bank recovers its expected information.
- Every `repair_*()`: a minimal fixture reproducing the defect, plus its fix.
  For SDS, a "new way" prompt fixture.

A GitHub Action runs these on every push. R installs fine in a cloud container,
so this also works in Claude sessions.

### 4b. Data contracts (run against each release; feed the QC chapter)

- **Schema / uniqueness:**
  - one score row per run × task (catches the ToM placeholder rows)
  - no NA `adaptive`
  - every trial `item_uid` is present in `corpus_items`
- **Raw → processed coverage** by task × month × site: the share of raw test
  trials surviving processing. The SDS match-block drop (18% in Aug, 1% in
  Sep–Oct) would have been flagged immediately.
- **Rescoring reproduces the release:** for each task, re-score a sample of runs
  with the registry model and require r ≥ .999.
- **Release-to-release drift:** the score correlation per task for runs present
  in both releases. An alert below ~.98 would have flagged the ToM v2_3 re-scoring
  (r = .88).
- **Key-defect screen:** items below chance by language, plus monotonicity flips.
- **Prompt/content drift:** distinct prompt strings per task per month. This
  catches a silent text change like SDS "different" → "new".

### 4c. Render hygiene

- Living chapters: no hardcoded numbers in prose. Use inline `r` / `pick()`. A
  lint script flags digit-bearing prose outside inline code in `monitor/` and
  `tasks/`.
- No `cache=TRUE` in living chapters (it doesn't see data changes). Use package
  fit caching keyed on data hash instead.
- Studies: freeze forever. Living chapters re-execute on each release
  (`quarto render monitor/` after a data bump).
- Keep the adversarial-verification practice (independent reimplementation; it
  worked for SDS and Stories) as a checklist item for any study whose verdict
  goes to the DCC.

---

## 5. Migration plan (each phase is a reviewable PR; nothing is deleted)

| Phase | Content | Check |
|---|---|---|
| **0** | Tag the current book `snapshot-2026-10-30`. Commit the missing SDS timing scripts. On your Mac, run a one-off script that inventories `data/` (filenames, sizes, mtimes), recording the orphan caches before anything moves. | — |
| **1. Skeleton & data layer** | Package skeleton, `datasets.yml`, `lv_source`/`lv_scores`/`lv_trials`/`lv_runs`, manifest cache, provenance, CI with the first tests. `common.R` becomes a shim. | 00 re-renders on `levante_data_latest` v3_17; diff inventory vs the 2026-08 snapshot |
| **2. Front-end move** | `studies/`, `archive/`, `handoffs/` folders with `STUDY.yml` headers (paths only, no code changes); `_quarto.yml` parts; findings register seeded from README/LEVANTE.md | book builds from freeze; every study page shows its header |
| **3. Psychometric primitives + scoreboard** | `irt/`, `reliability/`, `longitudinal/`, `evaluate/` | **must reproduce** three existing results: SDS `eval_table`, H&F nominal metrics, SRE scoreboard (after the `first_run` fix) |
| **4. Monitor + task template** | release QC with data contracts; reliability & stability; item health & DIF; pilot `task_report.qmd` on Math and H&F, then the rest | contracts catch the SDS drop on v3_17 retroactively |
| **5. Design evaluator** | generalize the Stories CAT engine (de-hardcode the 18 stories, pluggable timing/population/stopping), port the memory replay and timing model, design registry | must reproduce the Stories `results_cat_sim` and memory C3 numbers |
| **6. Panel & structure** | `lv_panel`, CFA/invariance/local-SEM helpers; port 02/03/04/05 into the monitor | conclusions re-stated with computed numbers on v3_17 |

Phases 1–2 are mechanical and low-risk. Phase 3 is where most of the
consolidation payoff is. Phases 5–6 are where the scientific payoff is.

---

## 6. Problems found during the survey (for the DCC log)

**Verified by reading the code:**

1. **SRE scoreboard validity columns are computed on wrong subsamples.** At
   `tasks/sre.qmd:290`, `first_run()` groups by (dataset, user) on a table that
   is long by task, so each child keeps only *one* of PA/SWR/vocab/TROG (the
   first administered). The PA/SWR/Vocab/TROG validity numbers (e.g. "PA
   validity .57 vs .27", "TROG .50 vs .44", also in LEVANTE.md) need
   recomputing. Fix: `group_by(dataset, user_id, task)`.
2. **H&F v1 chapter's Rasch-vs-2PL test uses ~1/8 of the data.** At
   `tasks/hearts_and_flowers.qmd:90`, `distinct(run_id, item_uid)` keeps the first
   trial of each item type per run, and the fit has no guessing floor. The "2PL is the right
   model" conclusion is unsupported and contradicts the v2 chapter (production is
   Rasch + g = .5).

**Reported by the survey, plausible, not yet re-verified:**

3. Item parameters are v1_14 while scores are v2_3 (06/07/08/10 and several task
   chapters). 06's "scoring-matched IRF" no longer holds.
4. Trials snapshot (08-24) lags scores (08-31). A cache miss would pull newer
   `"next"` trials into a folder named for 08-24.
5. `roar_word.qmd` and `reports/adaptive_missingness.qmd` read the *cleaned*
   scores, so on re-render they erase their own subject.
6. The `version = "v1_2"` fallback in 00 is broken (it filters on `exclusion`,
   which v1_2 lacks).
7. The Stories "Reproducing" section omits six scripts and the run order. Two
   inputs have no producer (`fcat_story_level__2026-09-24.rds`, `data/tom_raw/*`).
   V0 Bogotá is defined five different ways across the Stories scripts.
8. `_prep_hf_v2.R` says processing drops timeouts; the v2 chapter says it
   checked and they are kept.
9. memory_length mixes deduplicated and non-deduplicated score tables across
   rounds, so the S-rounds and the C-round use different retest inputs.
10. SRE DDM output schemas don't match between the three producers and the
    chapter.
11. `math_eap_vs_ml` "Shuffled" row is unseeded. Several chapters label
    current scores "Pipeline (buggy)" or "corrected (v1.2)", and both are now false.

---

## 7. Decisions needed from you

1. **Package home.** In-repo package (proposed; fast iteration, upstream
   selectively), or put the psychometric primitives straight into
   `levantemodels`?
2. **Monitor data source.** Living chapters on pinned `levante_data_latest`
   only, with per-site drafts allowed only in studies? (Proposed: yes, now that
   v3_17 is current.)
3. **One book or two.** One book with parts (proposed), or a separate "LEVANTE
   monitor" site that re-renders on release while the studies book stays
   frozen?
4. **Publishing living reports.** Re-renders need Redivis auth. Worth checking
   whether a Redivis API token (`REDIVIS_API_TOKEN`) would allow headless
   scheduled re-renders. That would make the monitor genuinely automatic.
5. **Scope of the task template.** All 13 measures (incl. ROAR + MEFS), or the 9
   core tasks first?
6. **Where the design registry lives.** Here, or in `levante-data-meta` next to
   the variant metadata (it is really deployment metadata)?
