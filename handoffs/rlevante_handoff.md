# Handoff report for the rlevante package/documentation work

**From:** the `levante-longitudinal` analysis session (exploratory longitudinal
analyses of `levante_data_latest`).
**Date:** 2026-06-03.
**Installed rlevante version used here:** 0.1.0. (Note: the source at
`~/Projects/rlevante` is *ahead* of this — it already exports `score_irt`,
`get_item_parameters`, `fetch_corpus_items`, etc. that 0.1.0 did not. Some
notes below may already be addressed in the source.)

This report has three parts:

1. **A confirmed, validated bug in `score_irt`** (most important — affects
   published scores).
2. **Documentation gaps / missing pieces** I hit while figuring out the
   item + model machinery.
3. **Scoring-pipeline context** that future analysts will need and that the
   docs should probably capture.

---

## 1. CONFIRMED BUG: `score_irt()` does not reorder columns before `mirt::fscores()`

### What's wrong

`mirt::fscores(mod, response.pattern = data)` matches the columns of
`response.pattern` to the model's items **by position, not by name.** In
`R/irt-score.R`, `score_irt()` builds `data_aligned` like this:

```r
overlap_items <- intersect(colnames(data_prepped), items(mod_rec))
data_aligned  <- data_prepped |> select(!!overlap_items)
missing_items <- setdiff(items(mod_rec), colnames(data_prepped))
data_aligned[, missing_items] <- NA
# data_aligned columns are now:
#   [ overlap_items in data_prepped's order ] ++ [ missing_items in setdiff order ]
# which is NOT items(mod_rec) order.
scores <- mirt::fscores(mod, method = "EAP", response.pattern = data_aligned)
```

`data_prepped` comes from `to_mirt_shape_grouped()`, whose column order is
whatever `tidyr::pivot_wider()` produced — i.e. first-appearance order of
`item_inst` in the trial data. That is essentially never equal to
`items(mod_rec)`. So `fscores()` scores each response vector against the
**wrong items** (a fixed permutation applied to every run).

Because it's a *fixed* permutation (not random per run), the resulting scores
are still positively correlated with the correct ones (r ≈ 0.75–0.95), which
is why the bug is easy to miss — scores look plausible and age-correlated, but
individual θ are wrong, sometimes by several logits.

### The fix (one line)

Right before the `fscores()` call, reorder to the model's item order:

```r
data_aligned <- data_aligned[, items(mod_rec)]
```

### Validation (this is airtight)

The `ModelRecord` stores the calibration-time EAP scores in `@scores`
(`run_id`, `ability`, `se`). Those are computed during fitting on the full
data matrix in correct column order, so they're ground truth. Rescoring the
**same calibration runs** (Bogotá math, n = 1080) and correlating with
`@scores$ability`:

| scoring path | cor with calibration gold-standard | max abs deviation |
|---|---:|---:|
| current `score_irt` (buggy) | **0.809** | 3.36 |
| with the reorder fix | **1.000** | 0.21 (mean abs dev 0.0004) |

The fix reproduces the model's own stored scores essentially exactly; the
current code does not.

A minimal repro that doesn't even need real data — same kid, same responses,
three column layouts, on the math multigroup model's CO group:

| column layout | EAP θ |
|---|---:|
| `items(mod_rec)` order | −1.30 |
| reversed | −3.54 |
| shuffled | −2.79 |

### Scope / blast radius

- Affects the **IRT path only**: tasks routed through `score_irt`
  (`matrix`, `mrot`, `math`, `hf`, `mg`, `sds`, `trog`, `vocab`, `tom`).
  `score_cat` (swr), `score_sre`, `score_pa` use different code paths and are
  not affected by *this* bug.
- It has **propagated into the published `levante_data_latest` scores.** The
  published `score` column for Bogotá math correlates 0.95 with the buggy
  `score_irt` output and only 0.75 with the corrected output — i.e. the
  release was scored with the buggy code.
- Practical consequence we hit: Bogotá math T1→T2 looked like severe
  regression-to-the-mean (Galton slope 0.19) — apparent decline for
  high-ability kids. After the fix, all four ability quartiles show positive
  growth (Galton slope 0.81). The "RTM" was mostly the bug.

### Recommended actions for the package

1. Add the reorder line to `score_irt`.
2. Add a regression test: score a small fixture, then score it again with
   `response.pattern` columns shuffled, and assert identical θ. (This would
   have caught it.)
3. Re-score and re-release the IRT-scored tasks in `levante_data_latest`.
4. Consider asserting `identical(colnames(data_aligned), items(mod_rec))`
   immediately before every `fscores()` call as a cheap permanent guard.

---

## 2. Documentation gaps / missing pieces

These are things I had to discover by reading source / experimenting, that
ideally the docs (vignette + function reference) would state directly.

### 2a. The scoring/registry workflow is undocumented end-to-end

There's no single place that explains how the pieces fit together to go from
*trials → score*. I had to reverse-engineer this chain:

```
fetch_scoring_table()   # which model spec applies to (task, dataset)
   └─ get_model_spec(task, dataset, scoring_table)        # internal, returns a 1-row spec list
fetch_registry_table()  # maps model filenames → Redivis file ids
   └─ get_model_record(spec, registry_table)              # downloads the .rds ModelRecord
         └─ model_spec_filename(spec)  /  mod_spec_str(spec)   # build the path/string
score_irt(trials, spec, mod_rec)                          # the actual scoring
   └─ recode_trials(trials)   # MUST be applied first (see 2c)
   └─ dedupe_items() -> to_mirt_shape_grouped()           # data shaping
   └─ mirt::multipleGroup(...) |> extract.group(...)      # rebuild model
   └─ mirt::fscores(method = "EAP", response.pattern = …) # score
```

A worked "rescore a task yourself" vignette section (download model → shape
data → score → compare to published) would be hugely valuable and would also
serve as a living test of the pipeline. There's a `rlevante_walkthrough.Rmd`
vignette; it should be extended to cover scoring, not just data download.

### 2b. `levante_data_latest` and `item_parameters` are not exposed via rlevante

- The **unified** dataset `levante_data_latest` (all sites bound together,
  with `site` and `dataset` columns) is the natural thing for an analyst to
  want, but `get_scores()` / `get_trials()` take a single `data_source`. I
  ended up calling `get_scores("levante_data_latest:e9pf:v1_0")` directly —
  which works, but isn't documented as a supported entry point. Worth either
  a convenience wrapper or at least a documented example.
- **Item parameters** (difficulty/discrimination per item) live in
  `levante_metadata_scoring`'s `item_parameters` table. In 0.1.0 there was no
  accessor, so I pulled it with the bare `redivis` package:
  ```r
  redivis$user("levante")$dataset("levante_metadata_scoring:e97h:v1_14")$
    table("item_parameters:4cvk")$to_tibble()
  ```
  The source now has `get_item_parameters()` — good. Please make sure its
  docs say which calibration each row corresponds to (the table has multiple
  `model_set` / `subset` / `itemtype` / `invariance` combinations per item;
  for cross-site work you want `model_set == "multigroup_site"`, scalar).

### 2c. `recode_trials()` must be applied before scoring — and this isn't obvious

`get_trials()` returns `correct` already, but `score_irt` is meant to run on
`recode_trials()`-processed data (slider thresholding, HF/SDS/ToM recodes,
specific item answer fixes, `chance` backfill). If you score raw
`get_trials()` output you get subtly different (wrong) numbers. The
relationship "`get_trials()` output is NOT yet scoring-ready; pipe through
`recode_trials()` first" should be stated explicitly in both function docs.

Relatedly: it would help to document *which* recodes `recode_trials()`
performs and why (e.g. the slider `chance = 1/slider_threshold/100` logic,
the `math_subtract_37_24` answer fix). These are buried.

### 2d. `difficulty` / `theta_estimate` columns in `get_trials()` are mostly NA

In the trials table, `difficulty` is populated for `swr` only and is NA for
every other task (including math), and `theta_estimate` is populated only for
CAT runs. This is a real gotcha — an analyst will reach for `difficulty` to
characterize item hardness and find it empty for 11 of 12 tasks. Either
populate these from the item-parameter calibration, or document clearly that
item difficulty must be joined from `item_parameters` and that
`theta_estimate` is a running-CAT estimate (present only for adaptive runs).

### 2e. Version-pinning syntax is under-documented

`get_scores(data_source, version)` is documented, but the practical detail
that you should pin a **qualified reference** like
`"levante_data_latest:e9pf:v1_0"` (rlevante even emits a warning telling you
to) isn't in the examples. A short note on the `name:hash:version` convention
and how to find the current hash/version would save people time.

### 2f. `ModelRecord` accessors

`ModelRecord` is documented as a class, but the accessor verbs an analyst
needs to actually use one are scattered: `items()`, `model_vals()`,
`model_class()`, plus the slots `@scores`, `@data`, `@groups`,
`@group_names`. A short "working with a ModelRecord" doc block listing the
accessors and what each returns (and crucially that `@scores` holds the
calibration-time EAP scores you can validate against) would help. The fact
that `@data` columns are guaranteed to be in `items(mod_rec)` order is exactly
the invariant the scoring code needs to respect.

### 2g. `notebook_dataset()` is opaque

It's exported and documented as a stub, but what it's *for* (resolving the
`_source_` table inside a Redivis notebook context) isn't clear from the docs.
Either flag it as notebook-only or explain the use case.

### 2h. The `item_parameters` table omits the guessing parameter (`g`) — easy to misread

The curated `item_parameters` table (`levante_metadata_scoring:…:item_parameters`)
exports **only `difficulty` and `discrimination`** per item, plus an `itemtype`
label of `rasch` / `2pl`. It does **not** include the guessing parameter. But
the *fitted* models carry a **fixed lower asymptote at each item's chance level**
(`g = chance`, `est = FALSE`): `score_irt()` reconstructs the model by feeding the
full `mirt::mod2values()` table — `g` rows included — into `mirt(pars = …)`, so
the released θ̂ genuinely account for guessing. Confirmed by reading the model
records directly (matrix `g = 0.25`, mental-rotation `0.5`, math per-item
`{0, 0.067, 0.25, 0.33, 0.5}`).

The trap: anyone who reconstructs the item response function from
`item_parameters` alone (as is natural for residual/person-fit work,
information curves, or independent re-scoring) will build a **plain logistic
with no lower asymptote**, which silently mismatches the scoring model — inflating
residuals at low ability and manufacturing spurious "reliable" person misfit.
(We hit exactly this in `studies/2026-08_within-child-variability/06_within_child_variability.qmd` before correcting it.)
Two cheap fixes: (a) add a `guessing` column to `item_parameters` (even if it is
just the fixed chance value), and/or (b) document that the authoritative,
complete parameter set lives in the model records' `model_vals()`
(`mod2values`), not in `item_parameters`. **No scoring bug — purely a
metadata-completeness / documentation issue.**

---

## 3. Scoring-pipeline context worth capturing

Things that are true about the data/scoring that aren't bugs but that every
analyst will need to know, and that the "Scoring and Psychometrics" docs page
should ideally state.

### 3a. EAP scoring uses a per-group prior

The multigroup IRT models carry a group-specific latent mean/variance. E.g.
the math model's `pilot_uniandes_co` group prior is **N(−0.88, 3.1)**. EAP
`fscores` shrinks toward that group mean, with strength inversely proportional
to test information. This has a concrete longitudinal consequence:

- Long non-adaptive forms (~90 items) → high information → little shrinkage.
- CAT-shortened sessions (~40 items) → less information → more shrinkage toward
  the group mean.

So when a child is measured non-adaptively at T1 and adaptively at T2, EAP
shrinkage *alone* induces apparent regression to the mean between waves even
with no true change. (In our Bogotá math case the dominant driver turned out
to be the column-order bug, but the EAP-shrinkage effect is real and will bite
any longitudinal analysis that crosses a mode boundary.) Worth a documented
caution: **EAP θ are not directly comparable across administrations with very
different test information.** ML/WLE scoring avoids the prior pull but is
unbounded (returns ±Inf for all-correct/all-incorrect; needs clipping).

### 3b. Adaptive vs non-adaptive scores are not interchangeable

Independent of the prior, CAT and fixed forms expose children to very
different item sets, and (for ceiling-prone non-adaptive forms) the
non-adaptive score can be biased upward because high-ability kids time out
before reaching hard items. Observed non-adaptive-minus-adaptive mean θ gaps
in this data: Leipzig math +2.4, Leipzig vocab +1.1. Cross-mode longitudinal
comparisons need explicit handling.

### 3c. `adaptive` flag missingness (separate short report)

`levante_data_latest` v1.0 has 196 scored runs with `adaptive = NA`,
concentrated in early-beta `task_version`s of Bogotá Memory and
Leipzig/Western Math. Trial-level `theta_estimate` is absent for all of them,
consistent with non-adaptive administration (confirmed with the DCC). See
`studies/2026-06_adaptive-missingness/adaptive_missingness.{qmd,html}` in this repo. Suggest a validator
rule: a row with `score_type == "ability"` and no `adaptive` value should fail
ingest.

### 3d. Math item bank is sharply unbalanced by sub-domain

Calibrated math difficulties (multigroup_site scalar Rasch): counting/identify
≈ −4, arithmetic in the middle, and the entire region above β = +1 is ~80%
number-line ("line") items plus fractions. So a CAT that estimates a child
above θ ≈ 1 mechanically routes them almost entirely to line items
(max-Fisher-information selection). Not a bug, but a measurement-design point
worth documenting because it strongly shapes what high-ability kids' adaptive
scores reflect.

### 3e. `score_type` taxonomy

The scores table mixes `score_type` values with different scales — `ability`
(IRT θ), `ability_cat` (CAT θ, swr), `guessing_adjusted_number_correct_scaled`
(sre, a z-scored raw measure), `prop_correct` (pa). "Extreme" thresholds and
any pooling logic must be scale-aware. Worth a small table in the docs mapping
each task → score_type → scale → interpretation.

---

## Pointers into this repo (if useful)

- `studies/2026-06_adaptive-missingness/adaptive_missingness.qmd` — the adaptive-NA writeup.
- `studies/2026-06_irt-column-order-bug/math_eap_vs_ml.qmd` — the bug demonstration, validation against
  calibration scores, and EAP/ML comparison.
- `archive/math_item_calibration.qmd` — item-bank composition and the
  CAT-routing analysis.
- `common.R::score_with_method()` — a corrected (column-reordered) reimpl of
  the `score_irt` path, parameterized by `fscores` method, that I used for the
  validation above. It's a reasonable reference for what the fixed
  `score_irt` should produce.

Happy to provide any of the underlying numbers or rerun anything if the
package session wants confirmation.
