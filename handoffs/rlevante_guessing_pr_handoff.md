# Handoff: item parameters omit the guessing parameter (`g`) — doc/metadata fix

**For:** an rlevante working session that can open a PR.
**Type:** documentation / metadata-completeness. **NOT a scoring bug** — the
scores are correct. (Don't confuse this with the separately-confirmed-and-fixed
`score_irt()` column-order bug; that one is real and already addressed. This is
only about what the *exported parameter table* exposes.)

## The gap, in one paragraph

The fitted IRT scoring models incorporate guessing as a **fixed lower asymptote
at each item's chance level** (`g = chance`, `est = FALSE`) inside the `mirt`
model. `score_irt()` reconstructs the model by passing the full
`mirt::mod2values()` table — `g` rows included — into `mirt(pars = …)`, so the
released θ̂ already account for guessing. **But the curated `item_parameters`
table** (in `levante_metadata_scoring`, the one most analysts reach for, and the
one `rlevante`-adjacent helpers pull) **exports only `difficulty` and
`discrimination`, and silently omits `g`.** Its `itemtype` label (`rasch`/`2pl`)
describes only the discrimination structure, not the presence of guessing. So
anyone who reconstructs the item response function from `item_parameters` alone
builds a plain logistic with **no lower asymptote**, which silently mismatches
the model the scores were fit under.

## Why it matters (concrete failure mode)

Reconstructing the IRF without `g` is a natural thing to do for: person-fit /
residual analyses, test-information / reliability curves, simulation, or
independent re-scoring. With the asymptote missing you get inflated residuals at
low ability (lucky guesses on hard items look "impossible") and downstream
artifacts — e.g. person-fit statistics that look reliable but are really model
mis-specification. We hit exactly this in a downstream notebook before catching
it; it cost real time and produced a wrong intermediate conclusion.

## Evidence (reproducible)

```r
library(rlevante)
ns <- getNamespace("rlevante")
get_model_spec   <- get("get_model_spec", ns)
get_model_record <- get("get_model_record", ns)

st <- fetch_scoring_table()      # model specs
rt <- fetch_registry_table()     # registry → model files

spec <- get_model_spec("matrix", "pilot_mpieva_de_main", st)
rec  <- get_model_record(spec, rt)

# guessing IS in the fitted model, fixed at the 4-AFC chance level:
model_vals(rec) |> dplyr::filter(name == "g") |> dplyr::count(value, est)
#>   value   est   n
#>   0.25  FALSE 459      # matrix = 0.25; mental-rotation = 0.5; math per-item {0,.067,.25,.33,.5}

# but the curated table has no guessing column at all:
# (item_parameters: task_id, item_task, item_uid, difficulty, discrimination,
#  n_responses, model_set, subset, itemtype, nfact, invariance)
```

The trial-level `chance` field equals these fixed `g` values, so `chance` is a
valid source for the asymptote too — but that isn't documented either.

## Proposed PR (pick what fits the repo; both are cheap)

1. **Expose `g` wherever item parameters are surfaced.** Locate where the
   `item_parameters` table is produced (it may live in the upstream
   scoring/calibration pipeline rather than `rlevante` proper — confirm). Add a
   `guessing` column carrying the fixed chance value per item. If `rlevante`
   itself has an accessor/helper that returns item parameters, make it return
   `g` (and `upper`, if any 4PL items ever appear).

2. **Document the authoritative source (do this regardless of #1).** Add a short
   note — in the `ModelRecord` docs and/or a scoring vignette — stating that the
   **complete, authoritative parameter set is `model_vals(record)`
   (`mirt::mod2values`)**, which includes `a`, `d`/`b`, **`g`**, and `u`; and
   that `item_parameters` is a convenience summary that omits `g`. Spell out that
   the deployed models use a **fixed** guessing asymptote at the item chance
   level (so reconstructing an IRF must use `P = g + (1 − g)·logistic`).

## Acceptance criteria

- A user can obtain each item's guessing value either from the parameter table
  (new column) or via a documented one-liner from the model record.
- Docs explicitly warn that `item_parameters` omits `g` and that IRF
  reconstruction must include the fixed asymptote.
- (If #1) a minimal test/example shows the table's `guessing` matches
  `model_vals()`'s `g` for a 4-AFC task (e.g. matrix = 0.25).

## Pointers

- Fuller context: `handoffs/rlevante_handoff.md` §2h (this repo) and §2f
  (`ModelRecord` accessors), §1 (the *separate* score_irt column-order bug).
- Relevant rlevante internals: `score_irt()`, `modelrecord()` /
  `model_vals()` (= `mirt::mod2values`), `get_model_record()`,
  `fetch_scoring_table()`, `fetch_registry_table()`.
