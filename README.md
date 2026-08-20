# levante-longitudinal

Exploratory longitudinal analyses of LEVANTE core-task data, built as a
sequence of reproducible Quarto notebooks. Data come from `levante_data_latest`
on Redivis (currently **v1.2**, the bug-fixed release) via the `rlevante`
package. Shared conventions, palettes, loaders, and cleaning helpers live in
`common.R`; every notebook starts with `source(here::here("common.R"))`.

See [`levante-meta/LEVANTE.md`](https://github.com/levante-framework/levante-data-meta/blob/main/LEVANTE.md) for cross-project LEVANTE context.

## Quarto book

The notebook stack is also assembled into a browsable **Quarto book** (chapter
sidebar + search). Config in `_quarto.yml`; landing page `index.qmd`; the two
warning banners (AI-generated / preliminary-analysis) live in `_disclaimer.qmd`
and are `{{< include >}}`d at the top of every chapter, so they render in the
book *and* in standalone chapter renders.

Render locally (needs Redivis access via `rlevante`):

```bash
quarto render                          # whole book → _book/
quarto render 07_rt_variability.qmd    # one chapter (into _book/, with nav)
```

`_book/`, `_freeze/`, and `.quarto/` are **gitignored** — rendered results and
the execution cache stay out of this (public) repo.

### Publishing is GATED

`quarto publish quarto-pub` pushes the rendered book to a **public, indexed
URL**. This is **unpublished, multi-site, human-subjects analysis**, so do not
publish without:

- consortium / collaborator sign-off on public exposure of the cross-site
  findings (MPI-Leipzig, Uniandes-Bogotá, Western-Canada);
- a check that the rendered figures — including the **de-identified
  individual-level trajectory plots in ch. 01** — are acceptable to expose.

Every chapter carries the AI-generated + preliminary-analysis banners. When
cleared, publish from an interactive shell (needs a one-time browser login):

```bash
quarto render && cd . && quarto publish quarto-pub --no-render
```

Commit the resulting `_publish.yml` so re-publishes reuse the same URL. Keep the
clearance note here current after each publish.

## Main notebook stack (run in order)

| Notebook | What it does |
|---|---|
| `00_load_data.qmd` | Pull + cache `levante_data_latest` v1.2, apply cleaning, inventory by site/task/age, **log of known data issues**. Writes `data/scores_all_sites.rds` (cleaned) and `scores_raw.rds`. |
| `01_data_integrity.qmd` | Per-(site, task) audit + verdict table (`data/task_site_verdict.rds`). Adaptive/mode coverage, RTM, extreme scores, score SE. |
| `02_growth.qmd` | Per-task longitudinal growth (LMMs) for Leipzig / Bogotá; within-child vs cross-sectional slopes. |
| `03_structure_invariance.qmd` | **Construct structure & measurement invariance.** Disattenuated (fixed-error) task correlations (single-indicator problem); structure comparison g/2f/3f/bifactor read propriety→BIC→fit (+ clean SEM path diagrams; ROAR-robustness 12-task check; age→latent regressions); per-site replication; site (DE+CA) and age-bin **measurement-invariance ladders**. (Previous latent-growth version preserved in `old/03_sem_growth_lgc.qmd`.) |
| `04_differentiation.qmd` | Construct structure with the **full 13-measure set** — ROAR (reading) and MEFS (EF) treated as real measures, not validators. ESEM first (~2 factors: fluid + verbal/literacy; MEFS→fluid, ROAR→verbal), then age-differentiation (local SEM) of the **2-factor** (fluid/verbal, 03's preferred structure; fluid–verbal r falls 0.93→0.83 with age, **replicating across all four sites**, Δ 0.06–0.16), plus a shared-speed/method check. |
| `05_battery_design.qmd` | **Battery-length optimization.** Combines the calibrated factor structure, per-task marginal reliabilities, and per-task durations (newest/adaptive versions) to find how well shorter task subsets recover factor scores (factor-score determinacy, Monte-Carlo validated). Enumerates the full minutes-vs-recovery frontier; defines Full / Minor / Minimal / broad-screen versions. |
| `06_within_child_variability.qmd` | **Within-child variability (exploratory).** Extracts three naive per-child indices — RT intra-individual variability (SD log-RT), person-misfit (IRT infit), 2-wave growth deviation — assesses their reliability, then asks whether they cohere across tasks/indices and change with age. §4 adds model-based upgrades (guessing-aware person-fit via recorded `chance`; random-slope growth; RT-IIV variance-components). |
| `07_rt_variability.qmd` | **RT-IIV deep dive.** Purifies the RT signal — the pipeline is chosen by a systematic 2⁴ sweep (§1.4: detrend by trial? all/correct? **detrend by item difficulty?** remove autocorrelation?) scored on split-half reliability + test–retest → `log RT ~ ns(trial) + difficulty×trial + site`, all trials, autocorrelation kept. Then cross-task structure (correlation + factor analysis), reliability vs trial count, **site & age differences**, and **longitudinal test–retest stability** (Leipzig + Bogotá). |
| `08_accuracy_variability.qmd` | **Accuracy variability (MSSD).** Parallel to 07 for the binary stream: raw / detrended / excess-over-independence MSSD, the accuracy-level confound, and reliability + longitudinal stability of each. Verdict: accuracy MSSD is reliable & stable *only* because it re-encodes accuracy; de-confounded, nothing survives. |
| `09_longitudinal_coverage.qmd` | **Network planning view (not analyzed data).** Curated metadata for all 23 funded grants (pilots, RfP1, RfP2) transcribed from the Q1 2026 Advisory Committee deck — PI, data-collection country, recruitment setting, school-based flag, target age range, and planned wave schedule (`n_waves`/`cadence`) — written to `09_site_coverage.csv`. Figures: an enrollment-age heatmap (site × age) and a stacked longitudinal-coverage plot where each recruited year-cohort is followed forward by its planned waves (dots = waves), with the marginal histogram as the column-sum. Coverage is densest at ages 6–11 (peak ~age 9) and thin at 2–5 and 13+ (the empirical case for RfP3's downward extension). |
| `10_downward_extension.qmd` | **Downward extension, first tranche (Boston Children's, ages 2–5).** Pulls the raw dataset `pilot_bostonchildrens_us_main_raw:7e6c` v4_26 (31 children, 228 runs, 9 tasks) and the newly-scored processed dataset `pilot_bostonchildrens_us_main:2fj2` v0_1. Hand-scores each task from trial-level correctness (H&F RT recode, vocab catch vs test, Corsi max span, Math by subtask) and describes accuracy / time-in-task / trials-completed by age; item-level vocab accuracies incl. 2s-vs-3+ and comparison to the 5–12 calibrated difficulties (ordering holds for 3+, ρ = −0.62; absent for 2s). Official θs track hand scores (r ≈ 0.8–1.0) but SEs are large and exclusions strip the youngest; **Pattern Matching unscorable (1/32 runs) — downex variant items don't map into the calibrated bank**. §7 task-selection verdict for 2-year-olds: **keep Vocab & Sentence Understanding (clear lift-off by 2.5–3; catch trials show the format works), drop Math & Memory (flat/floor)**; H&F borderline below ~3; Matrix undecidable until its items are calibrated. In the published book. |

`tasks/` holds per-task trial-level deep dives; `reports/` holds DCC-facing
write-ups; `slides/summary.qmd` is a revealjs deck summarizing findings across
the whole repo (render with `quarto render slides/summary.qmd`); `old/` holds
retired exploratory notebooks. Data and rendered HTML are git-ignored.

## Headline findings so far

- **A column-order IRT scoring bug** in `rlevante::score_irt` mis-scored the
  v1.0 release (worst for CAT / guessing-floor items). Found, validated, and
  fixed upstream; data re-released as v1.2. Write-up: `reports/rlevante_handoff.md`.
- With corrected scores, apparent longitudinal **declines disappear** — growth
  is uniformly positive (math steepest, ~0.6 θ/yr).
- The core battery is a **strong general factor** with only weakly-separable
  domains (inter-factor r ≈ 0.85–0.96; bifactor specifics degenerate). Among
  *proper* models the **2-factor (fluid/verbal) is BIC-preferred**, tying the
  3-factor on fit (`03`). It **replicates across Germany, Bogotá, Canada**, with
  **configural + approximate-metric** measurement invariance across site and age
  (loadings ~shared; scalar fails → absolute levels not comparable; ToM not
  separable at the task level even error-corrected). The age-differentiation
  (`04`) is therefore **structural**, not loading drift.
- **Stories/ToM** is a reliable but heterogeneous composite (not unidimensional);
  question type organizes it more than story construct; controls drive most
  cross-language non-invariance; partial scalar invariance achievable on targets.
- **The g-loading is substantive, not artifactual** (`04_differentiation.qmd`,
  now with ROAR + MEFS as first-class measures): it's within-site (not a site-mean
  effect) and ~orthogonal to response speed (not a method artifact). Empirically
  the 13-measure battery is ~**two** correlated factors (fluid/nonverbal incl.
  **MEFS**; verbal/literacy incl. **ROAR reading**; r≈0.67), not three — there is
  no separate EF factor, and reasoning & EF stay fused (r≈0.92, flat with age).
  The verbal/reading domains **differentiate with age** from the fluid core *and
  from each other* (every language/reading pair falls ~0.9→0.7 across ages 7–11;
  broad fluid–verbal 0.93→0.83). Reading relates to *g* about as much as to
  language → differential validity is weak at these ages.
- **Within-child variability indices don't cohere — yet** (`06_within_child_variability.qmd`):
  three naive consistency indices (RT intra-individual variability, IRT person-misfit
  infit, 2-wave growth deviation) show **near-zero cross-task generality** (r≈0.05)
  and are **mutually uncorrelated** (|r|<0.1) → no general "variability trait" with
  estimators. RT-IIV is cleanly measured (rel ≈0.45–0.85) and *not* an ability
  proxy but is task-specific and flat with age. **Person-misfit, scored against
  the model's actual IRF, carries almost no usable signal**: the scoring models
  already apply a *fixed guessing asymptote at each item's chance level*
  (confirmed in the model records — the curated `item_parameters` table just
  omits `g`), and once person-fit uses that same IRF it is well-calibrated and
  free of the extremity confound but has ≈0 reliability (the earlier "reliable,
  age-declining, ability-confounded" misfit was an artifact of a mis-specified
  plain-logistic IRF). 2-wave random-slope models can't identify individual
  growth (slope reliability ≈0 — naive 2-wave reliabilities were optimistic); a
  variance-components model puts the stable-person share of RT-IIV at just ~4%.
  Measuring variability as a *trait* needs more waves + within-task manipulations.
- **Purified RT-IIV shows a modest, speeded-led factor** (`07_rt_variability.qmd`):
  the purification pipeline is chosen by a systematic 2⁴ sweep (§1.4) — detrend by
  trial **and item difficulty**, all trials, autocorrelation kept (it is partly
  person-stable). Difficulty-detrending is the key win, mostly on the non-speeded
  tasks. This roughly triples cross-task coherence (naive ≈0.05 → ≈0.13 all tasks,
  ≈0.20 reliable subset): a **broad factor led by the speeded tasks** (ROAR-Word/
  Sentence, Hearts&Flowers) that the difficulty-purified non-speeded tasks now also
  load onto — no longer two clean strands, but not a strong general trait either.
  Reliability tracks trial count (≈0.5 at ≤25 trials → 0.94 at 120), though
  difficulty-detrending lifts several moderate-trial non-speeded tasks. **Site/age:**
  RT-IIV declines with age (speeded factor ≈−0.19 z/yr; untimed-cognitive only
  ≈−0.06) and is markedly higher in Bogotá (urban +0.29, rural +0.38 SD vs Leipzig,
  age-adjusted; Western ≈ Leipzig; site ~4.6% of total variance). **Longitudinal
  stability** (~1-yr waves): test–retest r ≈ 0.22 pooled (up to 0.47 for H&F) — well
  below within-session reliability, so RT-IIV is *partly* trait-like, most stable
  for the speeded tasks. Aggregating into the §3 two-factor scores does **not**
  improve stability (cognitive ≈0.25, speeded ≈0.20) — the durable RT-IIV signal is
  task-specific (esp. H&F), not shared-factor.
- **Accuracy variability is ability in disguise** (`08_accuracy_variability.qmd`):
  accuracy MSSD (trial-to-trial flip rate) is reliable (≈0.44) and longitudinally
  stable (≈0.38) — but *only* because it mechanically re-encodes accuracy level
  (confound ≈−0.7; flip rate = 2·p·(1−p)); detrending the trial trend doesn't touch
  it. Removing the accuracy dependency (excess-over-independence ≡ lag-1
  autocorrelation) collapses reliability (≈0.03) and stability (≈0.02) to zero,
  with no cross-task or age structure. Confirms 06's person-misfit result with a
  model-free measure, and contrasts 07 (RT keeps real signal beyond mean RT;
  accuracy keeps none beyond accuracy level).
- **The battery is highly compressible** (`05_battery_design.qmd`): because the
  constructs are so intercorrelated, factor scores can be recovered from far
  fewer tasks. Cutting administration time roughly in half (59 → ~31 min, 8
  tasks) still recovers every construct at ≈ 0.91–0.95 determinacy; dropping just
  ToM + Same&Different (→ ~44 min) costs almost nothing. **Reading is the binding
  constraint** (protect ROAR-Word when trimming); a ~16-min 4-task screener
  recovers the broad fluid/verbal split at ≈ 0.90.
- **Cross-language DIF** (`tasks/crosslang_dif_batch.qmd`, + ToM & TROG): the
  multigroup_site tasks are broadly invariant (Shape Rotation cleanest; Math &
  H&F lower but item-specific). One broken item found (TROG German
  `embedding_cat_cow_chase_black`, ~6.5 logits); top Math flags are
  multiplication/subtraction — likely curriculum-timing, not translation.
- **ToM reality-check DIF was (mostly) data defects**
  (`tasks/tom_reality_check_bug.qmd`): a shadow re-processing of raw CO+DE data
  proved answer-key inversions (4 below-chance cells, both sites),
  hostile-attribution content mislabeled as ToM (110 DE runs, Sept–Oct 2024),
  and trial-map shifts. Repairing only the provable defects removes ~70% of
  scalar non-invariance with controls included (χ² 702 → 212). The paper's 12
  hand-flagged items all map onto identified defects.

## Remaining known data issues (v1.2)

1. **Memory (`memory-game`) — DROP flag likely obsolete.** `tasks/memory.qmd`
   shows grid size *is* already a separate item dimension (2×2 vs 3×3 items are
   calibrated apart), difficulty is cleanly ordered by span length, and the
   structure replicates across sites (r ≈ 0.90). The earlier problem was the
   scoring bug, not the grid. Recommend revisiting the DROP (pending any
   official scoring update).
2. **Same & Different (`same-different-selection`) — CAVEAT.** Scoring update
   pending (release notes); also zero T2 at Bogotá.
3. **ROAR-Word non-engagement — handled in cleaning.** Rushers (median RT <
   500 ms) and near-chance non-readers floor at θ = −6; `00` filters
   accuracy < 0.4 OR median RT < 500 ms.
4. **`adaptive` flag missing on 235 runs — backfilled.** Early-beta
   task_versions; all confirmed non-adaptive (DCC); set to FALSE in cleaning.
   Still unfixed upstream.
5. **EAP shrinkage across mode boundaries** (minor): two-wave θ that cross a
   CAT/non-CAT boundary (notably Bogotá math) are mildly compressed by the
   EAP prior. Not a bug; see EAP-vs-WLE discussion.
6. **Open question, not a defect:** Pattern Matching & Sentence Understanding
   show T2 > T1 (release notes) — training vs. development, undetermined.

## Task examination status (stock-take)

Deep-dived = has a dedicated `tasks/` trial-level notebook. `kids_2wave` =
children with ≥ 2 administrations (longitudinal signal).

| Task | task_id | runs | kids 2-wave | status |
|---|---|---:|---:|---|
| Math | egma-math | 2967 | 470 | **deep-dived** (`tasks/math*.qmd`) |
| Stories / ToM | theory-of-mind | 1931 | 198 | **deep-dived** (`tasks/stories_tom*`, `tom_*`) |
| Vocabulary | vocab | 1481 | 239 | **deep-dived** (`tasks/vocab.qmd`) |
| ROAR-Word | swr | 1610 | 195 | **deep-dived** (`tasks/roar_word.qmd`) |
| Memory | memory-game | 1717 | 284 | **deep-dived** (`tasks/memory.qmd`) — grid OK, DROP likely obsolete |
| Hearts & Flowers | hearts-and-flowers | 1685 | 280 | **deep-dived** (`tasks/hearts_and_flowers.qmd`) — 2PL > Rasch, start trials low-info |
| Pattern Matching | matrix-reasoning | 1536 | 263 | **deep-dived** (`tasks/training_vs_development.qmd`) — no practice effect on corrected data |
| Sentence Understanding | trog | 1465 | 229 | **deep-dived** (`tasks/sentence_understanding.qmd` items/categories/DIF; `tasks/training_vs_development.qmd`) |
| Shape Rotation | mental-rotation | 1482 | 247 | **deep-dived** (`tasks/shape_rotation.qmd`) — textbook angle effect |
| ROAR-Sentence | sre | 1622 | 202 | **deep-dived** (`tasks/roar_literacy.qmd`) — speeded efficiency, age-valid |
| ROAR-Phoneme | pa | 1554 | 69 | **deep-dived** (`tasks/roar_literacy.qmd`) — fsm<lsm difficulty, age-valid |
| Same & Different | same-different-selection | 1066 | 201 | **pending new scoring models** (not yet implemented) |

**Training vs development (PM & SU):** the release-notes "T2 > T1" did **not**
replicate on corrected data — it was largely the scoring bug. The one clean
longitudinal cell (Leipzig Pattern Matching, mode-stable + equal length) shows
no practice gain. A fixed-form fixed-length retest would isolate it.

**Status:** 11 of 12 core/literacy tasks now have trial-level deep dives. Only
**Same & Different** remains — deferred until its new scoring models are
implemented in the pipeline.
