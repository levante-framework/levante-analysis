# Handoff: LEVANTE caregiver-survey psychometric analyses

**From:** the `levante-longitudinal` analysis sessions (cognitive-task scores +
data-integrity work).
**For:** a new session analyzing **LEVANTE survey data, focused on caregiver
report** (and, where useful, teacher / child self-report for cross-informant
checks).
**Date:** 2026-06-18.

This is a *strategy + style* handoff, not a results doc. It says how we work in
these repos, the two-pass strategy we use (per-instrument first, then
cross-construct), and — importantly — the **two distinct kinds of problems** to
hunt for: **data defects** and **psychometric measurement problems**. Read
`~/Projects/LEVANTE.md` first for the shared infrastructure/Redivis/`rlevante`
orientation; this doc assumes it.

---

## Style of work (how these repos are built)

Mirror the `levante-longitudinal` conventions exactly:

- **R + Quarto (`.qmd`) → HTML.** Every notebook starts with
  `source(here::here("common.R"))` for shared loaders, palettes, labels, theme,
  and helpers. Add survey-specific helpers there, not copied per notebook.
- **Sequential, numbered notebooks** (`00_…`, `01_…`, …), each building on the
  previous one's cached artifacts. **Cache to `data/*.rds`** (git-ignored) and
  `read_rds()` downstream rather than re-pulling from Redivis each time.
- **Incremental and surgical.** One question per notebook; don't refactor
  upstream notebooks when adding a new one. Per-instrument deep dives live in a
  `tasks/`-style subfolder (analogous to the cognitive `tasks/*.qmd`); DCC-facing
  writeups go in `reports/`.
- **Pin Redivis versions** with the qualified `name:hash:version` reference
  (e.g. `levante_data_latest:e9pf:v1_2`); record the version in `00`.
- **Git-ignore** rendered HTML, `data/`, `papers/`, `*_files/`. Committed
  deliverables (curated CSVs, reports) go in a tracked path.
- **Flag upstream problems.** Mike is the DCC head; a provable data/coding
  defect in the survey pipeline is in-scope to write up for the DCC (as we did
  with the `score_irt` column-order bug and the ToM answer-key defects).

**Suggested home:** a fresh numbered series, either a sibling repo
(`~/Projects/levante-surveys`) or a `surveys/` subtree here. A sibling repo is
cleaner since the construct set, scoring, and gotchas differ from the cognitive
tasks — but confirm with Mike.

---

## The data (how to pull it, what shape it's in)

Surveys come through `rlevante`, not the `scores`/`trials` tables. Entry point
is `get_surveys()` (wrapping `process_surveys()` + `link_surveys()`); the
internals are in `~/Projects/rlevante/R/process-surveys.R` — **read it**, it
encodes several decisions you must not silently inherit.

- `process_surveys(dataset_spec, survey_types = c("caregiver","student","teacher"))`
  joins `survey_responses` ⋈ `surveys`, attaches item metadata via
  `fetch_survey_items()`, and runs `code_survey_data()`.
- `link_surveys(surveys, participants)` resolves respondents to children:
  caregiver → child via `parent1_id`/`parent2_id` (**many-to-many**: a child can
  have two caregivers and a caregiver multiple children), teacher → children via
  `teacher_id`, student/child → self. Adds `site`, `dataset`, and child `age` at
  survey time.

**Resulting long-format columns to know:** `survey_type` (caregiver/teacher/
child), `survey_part` (caregiver splits into **`general`** = household-level,
across children, and **`specific`** = per-child), `construct`, `question_type`,
`variable` (item id) + `variable_order`, `value` (the analysis-ready numeric,
**already reverse-coded**), the raw `boolean_response`/`string_response`/
`numeric_response`, `is_complete`, `respondent_id`, `child_id`, `survey_id`,
`survey_timestamp`, `age`.

**Pipeline decisions that are bug surfaces — verify, don't trust:**

1. **Reverse-coding** (`code_survey_data` → `reverse_value`): items flagged
   `reverse_coded` are flipped against their declared `values` set. If the item
   metadata's `values`/`reverse_coded`/scale direction is wrong for any item,
   `value` is silently wrong. **`reverse_value()` returns `NA` for any response
   not in the declared `values`** — so an out-of-range or mis-typed level
   becomes missing with no warning. This is the survey analogue of the
   answer-key/column-order defects we found in the cognitive tasks: re-derive a
   handful of items from the raw `numeric_response` and confirm the recode and
   the scale direction by hand before trusting any composite.
2. **`is_complete` / partial surveys** and skip logic: decide and document a
   completion rule in `00`; don't let `remove_incomplete_surveys` happen
   invisibly.
3. **Household vs child-specific** (`survey_part`): household items must not be
   double-counted when a caregiver answered for multiple children; child-
   specific items are per (caregiver × child). Get the unit of analysis right
   before any aggregation.
4. **Multiple respondents per child** and multiple children per respondent —
   plan the join and watch row inflation (the many-to-many is real).

---

## General strategy: instruments first, then build up across constructs

Two passes, same as the cognitive-task work (`tasks/` deep dives → `03/04`
structure notebooks), and **both passes look for data defects *and* measurement
problems** — keep those two lenses separate in every notebook.

### Pass 1 — per-instrument (per-construct) deep dives

First **inventory** what exists: from `fetch_survey_items()` and the `construct`
/ `survey_part` / `question_type` columns, enumerate the actual caregiver
instruments, their items, scale types (Likert / binary / categorical / count /
free-numeric), and coverage by site, language, wave, and respondent. Do **not**
assume a construct list from memory — derive it from the release. (Expect
domains roughly like demographics/SES, home & literacy environment, parenting
practices, child socio-emotional/behavioral, health, language exposure, and
possibly caregiver-report EF — but confirm against the data.)

Then **one section/notebook per instrument**, each doing:

- *Data checks:* response distributions per item; floor/ceiling and zero-
  variance items; out-of-range / illegal values; missingness rate and pattern
  (item-level, by site/language/wave); skip-logic coherence; **careless
  responding** (straightlining / long-string, acquiescence, inconsistent
  duplicate or reverse pairs, implausible completion time); duplicate
  submissions; reverse-coding spot-checks (above).
- *Measurement checks:* item–total correlations; internal consistency
  (α and ordinal ω, with item-dropped deltas); **dimensionality** (parallel
  analysis / scree, then 1- vs multi-factor EFA on a **polychoric** matrix for
  ordinal items); whether the instrument's intended subscales hold; local
  dependence; basic item-response-theory / graded-response fit where the scale
  supports it.

Cache a tidy per-instrument item table and a per-child (per-construct) score
table for Pass 2.

### Pass 2 — cross-construct psychometric build-up

- **Construct scores + reliability** assembled from Pass 1; correlation
  structure across constructs (attenuation-corrected where useful).
- **Cross-construct dimensionality**: EFA then CFA (ordinal/WLSMV via `lavaan`)
  — how many higher-order factors, how clean the simple structure is. This is
  the survey analogue of `03_structure_invariance` / `04_differentiation`.
- **Measurement invariance** across **site / language / respondent type**:
  the configural → metric → scalar ladder (ΔCFI / ΔRMSEA criteria), and
  **item-level DIF** for flagged items. Expect to need *partial* invariance.
- **Cross-informant agreement** (caregiver vs teacher vs child) where the same
  construct is measured by multiple respondents.
- **External / convergent validity**: relate survey constructs to the
  **cognitive-task scores** (`levante_data_latest` `scores`) and to demographics
  — the eventual payoff and a strong check on whether the survey scores behave.

---

## Two problem lenses (keep them separate)

A finding is only interesting once you know which of these it is. We repeatedly
saw "measurement problems" in the cognitive work that were really **data
defects** (ToM cross-site DIF was ~70% manufactured by inverted answer keys and
trial-map errors). Same discipline here:

**Data defects** (fixable upstream; flag to DCC): wrong/!inverted reverse-coding
or scale direction; mis-declared `values` silently NA-ing responses; broken
translations of an item; mis-mapped respondent↔child links; duplicated or
partial submissions; skip-logic gaps; an item deployed with different options
across sites/waves. **Screen for these *before* interpreting any psychometric
result.** The cheap detectors: illegal-value scan, per-item response
distribution by site/language, reverse-pair consistency, and per-item
missingness by deployment epoch.

**Psychometric measurement problems** (properties of the instrument, not bugs):
low reliability; multidimensionality where unidimensionality was assumed; weak
or cross-loading items; floor/ceiling; local dependence; non-invariance / DIF
across groups; poor cross-informant convergence; weak external validity.

---

## Lessons carried over from the cognitive-task work

- **Separate provable defects from real measurement issues first** — repairing
  the defects often dissolves most apparent non-invariance.
- **An "illegal value" screen is your single best defect detector** — for
  cognitive tasks it was below-chance accuracy per item×site; for surveys it's
  out-of-range responses, impossible reverse-pair combinations, and items NA'd
  by the recode. Run it before DIF.
- **Site-to-site mean comparisons are discouraged** (sampling/recruitment/
  administration confounds). Within-site associations, cross-construct relations,
  and *invariance of structure* are the interpretable targets.
- **Use the invariance ladder**, report ΔCFI/ΔRMSEA, and accept partial scalar
  invariance rather than forcing full invariance.
- **Pin versions, cache artifacts, and keep notebooks incremental** so a re-run
  on the next data release is one parameter change, not a rewrite.
- **Ordinal data needs ordinal methods**: polychoric correlations, WLSMV
  estimation, ordinal ω — not Pearson/α defaults, which bias everything for
  Likert items.

---

## Confirm first (open questions for the new session)

1. Repo location: new `levante-surveys` repo vs `surveys/` subtree here.
2. Which **release/version** of survey data, and which sites/waves have caregiver
   coverage (the longitudinal cognitive sites are Leipzig, Bogotá ×2, Western —
   caregiver coverage may differ).
3. The authoritative **construct/instrument list** for the caregiver survey from
   `fetch_survey_items()` (derive, don't assume).
4. Whether the reverse-coding/`values` metadata has been validated upstream, or
   whether that validation is part of this work.

Suggested opening sequence: `00_load_inventory` (pull via `get_surveys()`, pin
version, inventory by construct/site/wave/respondent, log known issues) →
`01_data_integrity` (the defect screens above) → per-instrument deep dives →
`0X_structure` (cross-construct EFA/CFA) → `0Y_invariance` (site/language/
respondent) → validity against cognitive scores.
