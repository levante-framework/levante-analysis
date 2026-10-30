# levante-analysis

(Formerly `levante-longitudinal`.)

Analyses of the LEVANTE core-task battery by the Data Coordinating Center,
built as one Quarto website with four sections:

| Section | Folder | What it holds |
|---|---|---|
| Monitor | `monitor/` | Battery-wide analyses of the current data: loading and integrity, coverage, growth, structure, battery length, test–retest, cross-language DIF, downward extension. |
| Task reports | `tasks/` | One page per task (a standard task-report template is planned). |
| Studies | `studies/YYYY-MM_slug/` | Dated investigations of one question each, with their scripts in the same folder. |
| Findings & decisions | `register/` | The register of conclusions and decisions, each linked to its evidence. |

`archive/` (retired pages and the June slide deck), `handoffs/` (DCC-facing
write-ups) and `planning/` are not rendered. House rules for pages are in
[`CHARTER.md`](CHARTER.md). Shared conventions, palettes and loaders live in
`common.R`; every page starts with `source(here::here("common.R"))`. Pages
are moving from the 2026-08 per-site snapshot to pinned releases of
`levante_data_latest` (`load_levante_latest()`).

See [`levante-meta/LEVANTE.md`](https://github.com/levante-framework/levante-data-meta/blob/main/LEVANTE.md) for cross-project LEVANTE context.

## Quarto website

Config in `_quarto.yml`; the landing page is `index.qmd`, and `styles.css`
styles its four section buttons. The two warning banners (AI-generated /
preliminary analysis) live in `_disclaimer.qmd` and are included at the top
of every page with `{{< include /_disclaimer.qmd >}}`.

Render locally (needs Redivis access and the local data caches):

```bash
quarto render                                              # whole site → _site/
quarto render studies/2026-07_sre-scoring/sre.qmd          # one page
```

`_site/`, `_freeze/`, and `.quarto/` are **gitignored**: rendered results and
the execution cache stay out of this (public) repo. Pages still render from
the local `_freeze/` while they are moved onto pinned releases.

### Publishing is GATED

`quarto publish quarto-pub` pushes the rendered book to a **public, indexed
URL**. This is **unpublished, multi-site, human-subjects analysis**, so do not
publish without:

- consortium / collaborator sign-off on public exposure of the cross-site
  findings (MPI-Leipzig, Uniandes-Bogotá, Western-Canada);
- a check that the rendered figures — including the **de-identified
  individual-level trajectory plots in ch. 01** — are acceptable to expose.

Every chapter carries the AI-generated + preliminary-analysis banners. When
cleared, publish from an interactive shell (needs a one-time browser login).
The site will publish to a new Quarto Pub URL, `levante-analysis`, retiring
`levante-longitudinal`; `_publish.yml` still points to the old book until
then.

```bash
quarto render && cd . && quarto publish quarto-pub --no-render
```

Commit the resulting `_publish.yml` so re-publishes reuse the same URL. Keep the
clearance note here current after each publish.

**Last published:** 2026-10-26 (commit 153b7e8), at Mike Frank's request — Hearts & Flowers
v2 chapter revised after Mika's review: all models use the 10 released items (heart and
flower, start/stay/switch, no longer pooled), and 317 early v1 runs in two old raw formats
are recovered; numbers updated throughout.
Earlier: 2026-10-25 (ab729b6), Hearts & Flowers v2 chapter §11: concurrent refit of both forms on one scale (ceiling falls, range doesn't
widen); §4 and the verdict now defer to it.
Earlier: 2026-10-22 (deb01b4), Hearts & Flowers v2 chapter: floor/ceiling-by-age figures (§4), RT and Wright-map figures (§2, §9), and §10
scoring alternatives (nominal timeout model, imputing error-out runs).
Earlier: 2026-10-18 (3cf9e3c), second pass on the
Stories question-type relabels (knowledge-access type; story 10 "Is Brian lying?" back to
reality check; "mad" questions kept as false belief / reality check).
Earlier: 2026-10-17 (7e4452f), Stories chapter
follow-ups to Rebecca Saxe's review: corrected question types (relabel table
`studies/2026-09_stories/stories_qtype_relabels.csv`) and story type beyond question type, why development
looks flat in Leipzig and Colombia (Bogotá V0), and Argentina vs Colombia.
Earlier: 2026-10-15 (1446e1a), §9 of the
Hearts & Flowers v2 chapter (timing, calibration, and the ceiling: counterfactual deadline
items on the released scale, construct check, stop-rule replay).
Earlier: 2026-10-15 (764c4cd), the Hearts & Flowers v2 chapter
(`studies/2026-10_hf-v2-redesign/hearts_and_flowers_v2.qmd`, on `levante_data_latest` v3_17 + raw per-site trials);
2026-09-28 (1bf6af0), the TROG skin-tone chapter; 2026-09-28 (b5309c1), the
Stories item-actions table; 2026-09-25 (98460f2), the consolidated Stories chapter.

Quarto Pub rejects any single file over about 1 MiB (`API Error: 413 - Payload Too
Large` during upload). `studies/2026-09_stories/stories.qmd` crossed that on 2026-09-28, so it now renders
with `highlight-style: none` (folded code without syntax spans; 1.09 MB → 0.82 MB).
Check `find _site -type f -size +1000k` before publishing.

## Monitor and within-child notebooks

| Notebook | What it does |
|---|---|
| `monitor/snapshot.qmd` | **What is in the pinned release**: children, runs, tasks and ages by dataset; repeat assessments; exclusions and cleaning (`clean_levante_latest()`); changes since the previous monitor release; data checks. Replaced 00 and 01 (now in `archive/`). |
| `monitor/02_growth.qmd` | Per-task longitudinal growth (LMMs) for Leipzig / Bogotá / rural Colombia (new wave 2: Math + ROAR, within-child slopes ≈ cross-sectional gradients); within-child vs cross-sectional slopes. |
| `monitor/03_structure_invariance.qmd` | **Construct structure & measurement invariance.** Disattenuated (fixed-error) task correlations (single-indicator problem); structure comparison g/2f/3f/bifactor read propriety→BIC→fit (+ clean SEM path diagrams; ROAR-robustness 12-task check; age→latent regressions); per-site replication; site (DE+CA) and age-bin **measurement-invariance ladders**. (Previous latent-growth version preserved in `archive/03_sem_growth_lgc.qmd`.) |
| `monitor/04_differentiation.qmd` | Construct structure with the **full 13-measure set** — ROAR (reading) and MEFS (EF) treated as real measures, not validators. ESEM first (~2 factors: fluid + verbal/literacy; MEFS→fluid, ROAR→verbal), then age-differentiation (local SEM) of the **2-factor** (fluid/verbal, 03's preferred structure; fluid–verbal r falls 0.96→0.87 with age on the 2026-08 sample, **replicating across all four sites**, Δ 0.06–0.15), plus a shared-speed/method check. |
| `monitor/05_battery_design.qmd` | **Battery-length optimization.** Combines the calibrated factor structure, per-task marginal reliabilities, and per-task durations (newest/adaptive versions) to find how well shorter task subsets recover factor scores (factor-score determinacy, Monte-Carlo validated). Enumerates the full minutes-vs-recovery frontier; defines Full / Minor / Minimal / broad-screen versions. |
| `studies/2026-08_within-child-variability/06_within_child_variability.qmd` | **Within-child variability (exploratory).** Extracts three naive per-child indices — RT intra-individual variability (SD log-RT), person-misfit (IRT infit), 2-wave growth deviation — assesses their reliability, then asks whether they cohere across tasks/indices and change with age. §4 adds model-based upgrades (guessing-aware person-fit via recorded `chance`; random-slope growth; RT-IIV variance-components). |
| `studies/2026-08_within-child-variability/07_rt_variability.qmd` | **RT-IIV deep dive.** Purifies the RT signal — the pipeline is chosen by a systematic 2⁴ sweep (§1.4: detrend by trial? all/correct? **detrend by item difficulty?** remove autocorrelation?) scored on split-half reliability + test–retest → `log RT ~ ns(trial) + difficulty×trial + site`, all trials, autocorrelation kept. Then cross-task structure (correlation + factor analysis), reliability vs trial count, **site & age differences**, and **longitudinal test–retest stability** (Leipzig + Bogotá). |
| `studies/2026-08_within-child-variability/08_accuracy_variability.qmd` | **Accuracy variability (MSSD).** Parallel to 07 for the binary stream: raw / detrended / excess-over-independence MSSD, the accuracy-level confound, and reliability + longitudinal stability of each. Verdict: accuracy MSSD is reliable & stable *only* because it re-encodes accuracy; de-confounded, nothing survives. |
| `monitor/network.qmd` | **The LEVANTE network**: the 23 funded studies (`monitor/network_studies.csv`, transcribed from the Q1 2026 Advisory Committee deck, with a column linking each study to its datasets) and their planned ages and waves, next to the ages, waves and children actually in the pinned release. |
| `monitor/10_downward_extension.qmd` | **Downward extension, first tranche (Boston Children's, ages 2–5).** Pulls the raw dataset `pilot_bostonchildrens_us_main_raw:7e6c` v4_26 (31 children, 228 runs, 9 tasks) and the newly-scored processed dataset `pilot_bostonchildrens_us_main:2fj2` v0_1. Hand-scores each task from trial-level correctness (H&F RT recode, vocab catch vs test, Corsi max span, Math by subtask) and describes accuracy / time-in-task / trials-completed by age; item-level vocab accuracies incl. 2s-vs-3+ and comparison to the 5–12 calibrated difficulties (ordering holds for 3+, ρ = −0.62; absent for 2s). Official θs track hand scores (r ≈ 0.8–1.0) but SEs are large and exclusions strip the youngest; **Pattern Matching unscorable (1/32 runs) — downex variant items don't map into the calibrated bank**. §7 task-selection verdict for 2-year-olds: **keep Vocab & Sentence Understanding (clear lift-off by 2.5–3; catch trials show the format works), drop Math & Memory (flat/floor)**; H&F borderline below ~3; Matrix undecidable until its items are calibrated. In the published book. |

`archive/slides/summary.qmd` is the June revealjs deck summarizing findings
across the repo (render with `quarto render archive/slides/summary.qmd`).
Data and rendered HTML are git-ignored. The headline findings below predate
the register (`register/`) and move there as each study is reviewed.

## Headline findings so far

- **A column-order IRT scoring bug** in `rlevante::score_irt` mis-scored the
  v1.0 release (worst for CAT / guessing-floor items). Found, validated, and
  fixed upstream; data re-released as v1.2. Write-up: `handoffs/rlevante_handoff.md`.
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
- **Stories/ToM** (`studies/2026-09_stories/stories.qmd`, corrected 2026-09 data): a single score
  is defensible, most clearly outside Leipzig; question type, not story
  construct, organizes item difficulty; keep the controls. Per-administration
  reliability is only moderate (.56–.82 per dataset on fixed forms, .30–.65 on
  the deployed fCAT; June's .92 was model-implied for a pool no child takes).
  Cross-language comparisons are partial-scalar at best (false-belief items are
  non-invariant; language is confounded with age, prior exposure and site), and
  June's "controls drive the non-invariance" is partly overturned (model- and
  design-dependent). 20 of the 28 story-level items behind the 12 generic
  exclusions should be un-excluded.
- **The g-loading is substantive, not artifactual** (`monitor/04_differentiation.qmd`,
  now with ROAR + MEFS as first-class measures): it's within-site (not a site-mean
  effect) and ~orthogonal to response speed (not a method artifact). Empirically
  the 13-measure battery is ~**two** correlated factors (fluid/nonverbal incl.
  **MEFS**; verbal/literacy incl. **ROAR reading**; r≈0.67), not three — there is
  no separate EF factor, and reasoning & EF stay fused (r≈0.92, flat with age).
  The verbal/reading domains **differentiate with age** from the fluid core *and
  from each other* (every language/reading pair falls ~0.9→0.7 across ages 7–11;
  broad fluid–verbal 0.96→0.87 on 2026-08 data). Reading relates to *g* about as much as to
  language → differential validity is weak at these ages.
- **Within-child variability indices don't cohere — yet** (`studies/2026-08_within-child-variability/06_within_child_variability.qmd`):
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
- **Purified RT-IIV shows a modest, speeded-led factor** (`studies/2026-08_within-child-variability/07_rt_variability.qmd`):
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
  stability** (~1-yr waves, 2026-08 data incl. rural wave 2): test–retest r ≈ 0.24
  pooled; best single task now ROAR-Word (0.55, n=389). **2026-08 reversal:** the
  speeded factor score (swr/sre/hf) reaches **≈ 0.63 (n=224), above any single
  task** — June's "aggregation doesn't help" verdict was a coverage artifact
  (only 57 kids had ≥2 speeded tasks repeated; rural's ROAR-heavy second wave
  fixes that). The cognitive factor still shows no aggregation gain (≈0.24).
- **Accuracy variability is ability in disguise** (`studies/2026-08_within-child-variability/08_accuracy_variability.qmd`):
  accuracy MSSD (trial-to-trial flip rate) is reliable (≈0.44) and longitudinally
  stable (≈0.38) — but *only* because it mechanically re-encodes accuracy level
  (confound ≈−0.7; flip rate = 2·p·(1−p)); detrending the trial trend doesn't touch
  it. Removing the accuracy dependency (excess-over-independence ≡ lag-1
  autocorrelation) collapses reliability (≈0.03) and stability (≈0.02) to zero,
  with no cross-task or age structure. Confirms 06's person-misfit result with a
  model-free measure, and contrasts 07 (RT keeps real signal beyond mean RT;
  accuracy keeps none beyond accuracy level).
- **The battery is highly compressible** (`monitor/05_battery_design.qmd`): because the
  constructs are so intercorrelated, factor scores can be recovered from far
  fewer tasks. Cutting administration time roughly in half (59 → ~31 min, 8
  tasks) still recovers every construct at ≈ 0.91–0.95 determinacy; dropping just
  ToM + Same&Different (→ ~44 min) costs almost nothing. **Reading is the binding
  constraint** (protect ROAR-Word when trimming); a ~16-min 4-task screener
  recovers the broad fluid/verbal split at ≈ 0.90.
- **Cross-language DIF** (`monitor/crosslang_dif_batch.qmd`, + ToM & TROG): the
  multigroup_site tasks are broadly invariant (Shape Rotation cleanest; Math &
  H&F lower but item-specific). One broken item found (TROG German
  `embedding_cat_cow_chase_black`, ~6.5 logits); top Math flags are
  multiplication/subtraction — likely curriculum-timing, not translation.
- **ToM reality-check DIF** (June forensics, `studies/2026-06_tom-reality-check/tom_reality_check_bug.qmd`):
  a shadow re-processing of raw CO+DE data found item-mapping defects, and
  June's repair cut scalar misfit by ~70% (χ² 702 → 212). The 2026-09 re-analysis
  (`studies/2026-09_stories/stories.qmd`) revises this: on corrected data the defective cells are
  not site-specific and not all key inversions; corrections plus exclusions
  remove about half of the misfit in June's design (χ² 797 → 407), mostly
  through the exclusions; and the paper's 12 hand-flagged uids are not a
  catalog of defects (20 of their 28 story-level items should be un-excluded).

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
| Math | egma-math | 2967 | 470 | **deep-dived** (`archive/math*.qmd`) |
| Stories / ToM | theory-of-mind | 1931 | 198 | **deep-dived** (`studies/2026-09_stories/stories.qmd`; forensic appendix `studies/2026-06_tom-reality-check/tom_reality_check_bug.qmd`) |
| Vocabulary | vocab | 1481 | 239 | **deep-dived** (`tasks/vocab.qmd`) |
| ROAR-Word | swr | 1610 | 195 | **deep-dived** (`studies/2026-07_roar-word-engagement/roar_word.qmd`) |
| Memory | memory-game | 1717 | 284 | **deep-dived** (`tasks/memory.qmd`) — grid OK, DROP likely obsolete |
| Hearts & Flowers | hearts-and-flowers | 1685 | 280 | **deep-dived** (`archive/hearts_and_flowers.qmd`) — 2PL > Rasch, start trials low-info |
| Pattern Matching | matrix-reasoning | 1536 | 263 | **deep-dived** (`studies/2026-07_training-vs-development/training_vs_development.qmd`) — no practice effect on corrected data |
| Sentence Understanding | trog | 1465 | 229 | **deep-dived** (`tasks/sentence_understanding.qmd` items/categories/DIF; `studies/2026-07_training-vs-development/training_vs_development.qmd`) |
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
