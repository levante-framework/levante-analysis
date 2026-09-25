# Notebook metadata schema (DRAFT)

Structured frontmatter for every analysis notebook (`0*_*.qmd`, `1*_*.qmd`,
`tasks/*.qmd`, `reports/*.qmd`), so that tools — a planned orientation MCP, a
CI check, the Quarto book — can answer "what has been done on task X / site Y,
on which data, and how settled is it?" without parsing prose.

All fields live under a single `levante:` key in the YAML header. Quarto
ignores unknown top-level keys, and namespacing keeps them from colliding with
Quarto options.

**Status: draft.** Field names and enums are open for discussion; nothing
reads this yet.

## Fields

| Field | Required | Type | Meaning |
|---|---|---|---|
| `summary` | yes | string (1–3 sentences) | The question the notebook asks, in plain language. Not the findings. |
| `status` | yes | enum: `draft`, `reviewed`, `superseded` | `draft` = AI-generated, not line-by-line verified by a human (the default; matches `_disclaimer.qmd`). `reviewed` = a named human has checked code and conclusions. `superseded` = kept for history; see `superseded_by`. |
| `reviewed_by` | if `reviewed` | list of names | Who reviewed it. |
| `superseded_by` | if `superseded` | path | Notebook that replaces it. |
| `task_ids` | yes | list of long `task_id`s | Tasks analyzed, using the long forms in `common.R::task_lookup` (e.g. `memory-game`, not `mg`). List every task explicitly — no shorthands — so a query for one task matches. Empty list for notebooks not about specific tasks (e.g. `09`). |
| `datasets` | yes | `all`, or list of `dataset` values | Datasets analyzed, as in the `dataset` column (e.g. `pilot_uniandes_co_bogota`). Uses `dataset` rather than `site` because notebooks filter at that level (`site` merges Bogotá and rural Colombia). `all` = every dataset in the loaded data, unfiltered. Empty list for notebooks that analyze no LEVANTE data. |
| `data` | yes | list of sources (below) | Every data input, at the version the recorded `findings` were computed on. This can differ from what the code loads today (e.g. findings rendered on v1_0 before the loaders moved to v1_2); re-rendering should update both. Empty list for notebooks that analyze no LEVANTE data. |
| `depends_on` | no | list of paths | Notebooks or scripts whose cached outputs this one reads (e.g. `00_load_data.qmd` for `data/scores_all_sites.rds`; `tasks/_sds_scoring_fits.R` for `data/sds_scoring/`). |
| `findings` | no | list of strings | Headline conclusions, one claim each, as currently rendered. Must be updated when the notebook is re-run on new data. |
| `last_run` | yes | date (`YYYY-MM-DD`), or `unknown` | When the rendered results and `findings` were last produced. Findings are only as current as this date. Use `unknown` when it can't be established; the `data` versions then carry the provenance. (Initial values were back-filled from each notebook's last commit date, since render dates weren't recorded; notebooks whose text shows the findings predate the repo's first commit are `unknown`.) |

Each `data` entry:

| Key | Required | Meaning |
|---|---|---|
| `source` | yes | A `common.R` loader (`load_levante_scores`, `load_levante_trials`, `load_item_parameters`, …), a Redivis reference `name:code`, or a file path (relative to this repo) for inputs that come from neither, e.g. `../levante-pilots/01_fetched_data/run_data.rds`. |
| `version` | yes, except file paths | For loaders: the snapshot they bind (`levante_sites_snapshot` / `levante_trials_snapshot`, e.g. `2026-08-31`), or the metadata version (e.g. `v1_14`). For Redivis refs: the pinned version (`v4_26`, `next`). `next` is a mutable draft — note it. |
| `table` | yes | `scores`, `trials`, `item_parameters`, `runs`, `surveys`, … |

## Example (`tasks/memory.qmd`)

```yaml
---
title: "Memory (Corsi) — item difficulty structure & the '2×2 vs 3×3' question"
levante:
  summary: >
    Was Memory's DROP flag (grid size 2×2 vs 3×3 not accounted for in scoring)
    a real problem, or an artifact of the v1.0 column-order scoring bug?
  status: draft
  task_ids: [memory-game]
  datasets: [pilot_mpieva_de_main, pilot_uniandes_co_bogota]
  data:
    - {source: load_item_parameters, version: v1_14, table: item_parameters}
    - {source: load_levante_trials, version: "2026-08-24", table: trials}
    - {source: load_levante_scores, version: "2026-08-24", table: scores}
  depends_on: [00_load_data.qmd]
  findings:
    - Difficulty is ordered by span length, backward > forward, and replicates across sites (r ≈ 0.90).
    - Grid is a separately calibrated item dimension; within-child θ is continuous across Leipzig's wave-1→2 grid switch.
    - Raw sumscores are not comparable across grids.
    - "Recommendation: lift the DROP flag."
  last_run: 2026-08-24
format:
  html:
    ...
---
```

## Open questions

- Should `findings` link to the rendered section (anchor) that supports each
  claim?
- Do `reports/*.md` (not Quarto) need the same fields, e.g. via a sidecar
  `.yml`?
- A CI check (validate required fields and enums; warn when `last_run` is older
  than the current data snapshot) is what keeps this from drifting — needs an
  owner.
