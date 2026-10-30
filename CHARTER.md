# Charter for the LEVANTE analysis book

This repo is a reference that researchers dip into to look something up,
not a document read front to back. Every chapter should make sense on its
own, say what it found in the first screen, and use the same words, colors
and conventions as every other chapter. These rules apply to new and revised
chapters. Older chapters are brought into line as they are revised.

## Data

- **Pinned data only.** Chapters load from a pinned `levante_data_latest`
  release with `load_levante_latest()` (`common.R`). Monitor and task pages
  use the default release (`levante_latest_version`); it moves on a schedule
  and only to a release that passes the snapshot checks. Studies pass their
  `version` explicitly, so moving the default never changes a study. A study
  that needs raw per-site data pins those references too. No unpinned
  `"current"` or `"next"` pulls.
- **A data line at the top.** Every chapter shows the exact reference it ran
  on (e.g. `levante_data_latest:e9pf:v3_17`), computed from the loaded data.
- **No hardcoded numbers in prose.** Numbers in text, captions and plot
  titles are computed inline (`` `r ...` ``), so the prose can't disagree
  with the tables after a re-render. Small helpers such as a fail-loud
  `pick()` keep this readable.
- **Within dataset by default.** Compare, correlate and z-score within
  dataset, then pool. Comparisons of site or dataset means need a stated
  reason.
- **Derived outputs come from committed scripts.** Expensive fits and
  simulations are produced by a script in the repo and stored in an
  access-controlled Redivis dataset, never in git. The repo is public, and
  the data are child-level and embargoed.

## Writing

- **Summary first.** Every chapter opens with a short summary block: the
  question, the data line, three to five findings with computed numbers, and
  the status or decision. A reader who stops there should have the answer.
- **Short.** Say it once, plainly. Put derivations, checks and side
  analyses below the main results or in collapsed sections.
- **No coined names.** Describe the thing ("runs shorter than 120 s"), don't
  name it ("the short-run tail", "the gate"). Use the terms in the glossary
  below with the same meaning everywhere.
- **No code names in reader-facing text.** Tables, axes and legends use human
  labels (`dataset_labels_named`, "Rate score"), not column names like
  `score_new` or `r_rate`.
- **Forward-looking.** Describe the current state of the analysis. Errors
  that were found and corrected go in a **Revision history** section at the
  bottom, dated. The one exception: if a correction reverses a conclusion
  someone may have acted on, add a single line at the top pointing to the
  revision history.
- **Uncertainty.** Any number a conclusion rests on carries its n and a CI
  or SE.
- **Bugs are reported, not documented.** Monitor pages describe the data as
  it is. When a check turns up a likely bug in the data or the pipeline,
  report it directly to Mike, Mika or whoever owns that code, and track the
  fix there. The monitor's checks may show the symptom (e.g. a count of
  unscored runs), but the chapters don't carry bug narratives.

## Figures and tables

- **Plots over tables**, usually. Keep a table when readers need the exact
  numbers (e.g. a scoreboard the DCC will act on), placed after the plot or
  collapsed.
- **One palette per thing, from `common.R` only.** Datasets use
  `scale_colour_dataset()`, sites `scale_colour_site()`, task categories
  `scale_colour_task()`. Don't define new palettes for these in a chapter.
  The dataset and site palettes are Paul Tol's schemes, to match
  levante-datapage, with a fixed color per dataset. (The datapage currently
  assigns its Tol colors by position, so the two don't match yet; the
  published papers keep their older solarized colors.)
- **Theme from `common.R`**, matching levante-pilots and the papers: Source
  Sans 3, no grid, plain axes.
- **Titles describe; captions conclude.** A plot title says what is shown. A
  claim about the data goes in the caption or text, with computed numbers.
- **Standard plots.** Prefer the shared plotting functions in `common.R`
  (e.g. `levante_age_score_plot()`) when one fits. New plot types used in
  more than one chapter belong there.

## Code and review

- Chapters `source(here::here("common.R"))`. Helper scripts live next to
  their chapter and start with `_`. Code is folded by default.
- Package changes (`levantemodels`, `levante-r`) go through PRs to the
  package maintainers. This repo pushes to `main`.
- A claim that goes to the DCC gets an independent check: a reimplementation
  or a second analysis that could have come out differently.
- Chapters keep the AI-generated disclaimer until a human has verified them.

## Glossary

| Term | Meaning |
|---|---|
| release | A pinned version of `levante_data_latest` (e.g. v3_17). |
| site | A partner institution (e.g. Universidad de los Andes). |
| dataset | A Redivis dataset of one site's data. A site can have several (Bogotá and rural Colombia). |
| child | A participant, identified by `user_id`. |
| run | One child doing one task once (`run_id`). |
| wave | A round of data collection at a dataset. A child's later runs of a task usually fall in later waves. |
| task | One of the LEVANTE measures (`task_id`, e.g. `hearts-and-flowers`). |
| variant | A task configuration (`variant_id`): language, item set, adaptive or fixed form. |
| task version | The version of the task code (`task_version`). |
| item | One stimulus or question (`item_uid`). |
| score | The production score in the `scores` table: an IRT ability estimate for most tasks, a rate for ROAR-Sentence. |
| exclusion | Production's run-level flag for runs it could not score (`exclusion`). |
| registry version | The version of the scoring-model registry that produced the scores (e.g. v2_3). |
