# common.R
#
# Shared LEVANTE conventions for this project:
#  - plotting theme & palettes (matched to ../levante-pilots/plot_settings.R)
#  - task / construct lookups
#  - site label lookups
#  - dataset loader for levante_data_latest with disk cache
#
# Each notebook should `source(here::here("common.R"))` near the top.

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(glue)
  library(fs)
  library(ggthemes)
  library(levantemodels)
})

# ---- Theme ------------------------------------------------------------------
# Matches levante-pilots/plot_settings.R. Source Sans 3 is loaded via
# sysfonts/showtext if available; otherwise we fall back to the system default
# so this still works in headless renders without those packages installed.

.levante_font <- "sans"
if (requireNamespace("sysfonts", quietly = TRUE) &&
    requireNamespace("showtext", quietly = TRUE)) {
  .levante_font <- "Source Sans 3"
  sysfonts::font_add_google(.levante_font)
  showtext::showtext_auto()
  showtext::showtext_opts(dpi = 300)
}

theme_set(theme_bw(base_size = 14, base_family = .levante_font))
theme_update(panel.grid = element_blank(),
             strip.background = element_blank(),
             legend.key = element_blank(),
             panel.border = element_blank(),
             axis.line = element_line(),
             strip.text = element_text(face = "bold"))

options("ggplot2.continuous.colour" = viridis::scale_colour_viridis)
options("ggplot2.continuous.fill"   = viridis::scale_fill_viridis)

# ---- Palettes ---------------------------------------------------------------

# Construct/task category palette (ptol)
task_categories_vec <- c("Executive function", "Math", "Reasoning",
                         "Spatial cognition", "Language", "Reading",
                         "Social cognition")
task_pal <- ptol_pal()(length(task_categories_vec)) |>
  rlang::set_names(task_categories_vec)

scale_colour_task <- function(...) scale_colour_manual(values = task_pal, ...)
scale_color_task  <- scale_colour_task
scale_fill_task   <- function(...) scale_fill_manual(values = task_pal, ...)

# Site palette (solarized for the original six; manual hexes for sites added
# in the 2026-08 data update) — mapped to internal site codes
site_pal <- c(
  solarized_pal()(6) |>
    rlang::set_names(c("pilot_uniandes_co", "pilot_mpieva_de",
                       "pilot_western_ca", "partner_mpib_de",
                       "partner_sparklab_us", "pilot_langcog_us")),
  rfp1_sheffield_gb        = "#7570b3",
  rfp1_mpib_intl           = "#e7298a",
  rfp1_utdt_intl           = "#66a61e",
  pilot_bostonchildrens_us = "#a6761d",
  pilot_capecoast_gh       = "#666666"
)

scale_colour_site <- function(...) {
  scale_colour_manual(values = site_pal, labels = site_labels_named, ...)
}
scale_color_site <- scale_colour_site
scale_fill_site  <- function(...) {
  scale_fill_manual(values = site_pal, labels = site_labels_named, ...)
}

# ---- Lookups ----------------------------------------------------------------

# Internal site → friendly label
site_labels_named <- c(
  pilot_uniandes_co        = "Colombia",
  pilot_mpieva_de          = "Germany (MPI EVA Leipzig)",
  pilot_western_ca         = "Canada (Western)",
  partner_mpib_de          = "Germany (MPIB Berlin)",
  partner_sparklab_us      = "US (Sparklab, downext)",
  pilot_langcog_us         = "US (LangCog, downext)",
  rfp1_sheffield_gb        = "UK (Sheffield)",
  rfp1_mpib_intl           = "Germany (MPIB, RfP1 YS)",
  rfp1_utdt_intl           = "Argentina (UTDT, RfP1 YS)",
  pilot_bostonchildrens_us = "US (Boston, downext)",
  pilot_capecoast_gh       = "Ghana (Cape Coast)"
)

# dataset → finer label (preserves bogota / rural / main distinctions)
dataset_labels_named <- c(
  pilot_uniandes_co_bogota      = "Colombia — Bogotá",
  pilot_uniandes_co_rural       = "Colombia — Caquetá/Boyacá",
  pilot_mpieva_de_main          = "Germany — Leipzig",
  pilot_western_ca_main         = "Canada — Ontario",
  partner_mpib_de_main          = "Germany — Berlin (MPIB)",
  partner_sparklab_us_downex    = "US — Sparklab (downext)",
  pilot_langcog_us_downex       = "US — LangCog (downext)",
  rfp1_sheffield_gb_main        = "UK — Sheffield",
  rfp1_mpib_intl_ys             = "Germany — MPIB (RfP1 YS)",
  rfp1_utdt_intl_ys             = "Argentina — UTDT (RfP1 YS)",
  pilot_bostonchildrens_us_main = "US — Boston Children's (downext)",
  pilot_capecoast_gh_main       = "Ghana — Cape Coast"
)

# task_id → short label + category + display label
task_lookup <- tribble(
  ~task_id,                    ~short,    ~task_category,        ~task_label,
  "hearts-and-flowers",        "hf",      "Executive function",  "Hearts and Flowers",
  "memory-game",               "mg",      "Executive function",  "Memory",
  "same-different-selection",  "sds",     "Executive function",  "Same and Different",
  "mefs",                      "mefs",    "Executive function",  "MEFS",
  "matrix-reasoning",          "matrix",  "Reasoning",           "Pattern Matching",
  "mental-rotation",           "mrot",    "Spatial cognition",   "Shape Rotation",
  "egma-math",                 "math",    "Math",                "Math",
  "vocab",                     "vocab",   "Language",            "Vocabulary",
  "trog",                      "trog",    "Language",            "Sentence Understanding",
  "theory-of-mind",            "tom",     "Social cognition",    "Stories (ToM)",
  "swr",                       "swr",     "Reading",             "ROAR-Word",
  "sre",                       "sre",     "Reading",             "ROAR-Sentence",
  "pa",                        "pa",      "Reading",             "ROAR-Phoneme"
)

# Convenience: the 9 LEVANTE core tasks (excluding ROAR + MEFS)
core_task_ids <- c(
  "hearts-and-flowers", "memory-game", "same-different-selection",
  "matrix-reasoning", "mental-rotation",
  "egma-math",
  "vocab", "trog",
  "theory-of-mind"
)

# ---- Data loading -----------------------------------------------------------
#
# DATA SOURCE SWITCH (2026-08 update): `levante_data_latest` is still at v1_2
# (June snapshot; 4 data-rich sites, 2 waves) — the months of new data live
# only in the per-site *processed* datasets. The loaders below therefore
# default to `version = "sites-2026-08"`, which binds the per-site datasets in
# `levante_site_specs` (pinned versions, pulled 2026-08-24). Pass
# version = "v1_2" to reproduce the old unified-dataset pull.

# Per-site processed datasets + pinned versions for the 2026-08 snapshot.
# Datasets with a processed release but zero scored rows as of the pull
# (capecoast, sheffield_intl_ys, utdt_ar_main) are listed in 00's prose but
# excluded here. `site` backfills the NA site column in the newest processing.
levante_site_specs <- tibble::tribble(
  ~name,                                ~version, ~site,
  "pilot_mpieva_de_main:8wjx",          "v3_2",  "pilot_mpieva_de",
  "pilot_uniandes_co_bogota:d0c5",      "v4_3",  "pilot_uniandes_co",
  "pilot_uniandes_co_rural:bxgv",       "v5_0",  "pilot_uniandes_co",
  "pilot_western_ca_main:bgcj",         "v3_3",  "pilot_western_ca",
  "partner_mpib_de_main:6bvk",          "v3_4",  "partner_mpib_de",
  "rfp1_mpib_intl_ys:cqyz",             "v1_0",  "rfp1_mpib_intl",
  "rfp1_sheffield_gb_main:3pzs",        "v1_0",  "rfp1_sheffield_gb",
  "rfp1_utdt_intl_ys:csvh",             "v1_1",  "rfp1_utdt_intl",
  "pilot_bostonchildrens_us_main:2fj2", "v1_0",  "pilot_bostonchildrens_us",
  "pilot_langcog_us_downex:d3f2",       "v3_5",  "pilot_langcog_us",
  "partner_sparklab_us_downex:4090",    "v3_5",  "partner_sparklab_us"
)
levante_sites_snapshot <- "2026-08-24"

#' Load scores bound across the per-site processed datasets (2026-08 snapshot).
#' Post-processing: backfill NA `site` from the spec; drop the ToM placeholder
#' rows (each theory-of-mind run carries an all-NA duplicate row in the new
#' processing — upstream quirk, flagged in 00); attach labels.
load_levante_scores_sites <- function(refresh = FALSE, cache_dir = here("data")) {
  dir_create(cache_dir)
  cache_path <- file.path(cache_dir, glue("scores_sites_{levante_sites_snapshot}.rds"))
  if (refresh || !file_exists(cache_path)) {
    pulls <- purrr::pmap(levante_site_specs, \(name, version, site)
      levante::get_scores(name, version = version))
    write_rds(bind_rows(pulls), cache_path)
  }
  read_rds(cache_path) |>
    left_join(levante_site_specs |> select(dataset_stub = name, site_spec = site) |>
                mutate(dataset_stub = sub(":.*", "", dataset_stub)),
              by = c("dataset" = "dataset_stub")) |>
    mutate(site = coalesce(site, site_spec)) |>
    select(-site_spec) |>
    group_by(run_id, task_id) |>
    filter(!(n() > 1 & is.na(score_type) & is.na(score))) |>
    ungroup() |>
    label_levante_scores()
}

#' Load trials for given tasks from the per-site processed datasets
#' (2026-08 snapshot). Per-dataset pulls are cached under
#' data/trials_sites_<snapshot>/<dataset>.rds; a per-task slice cache makes
#' repeated task-specific loads fast.
load_levante_trials_sites <- function(task_ids = NULL, refresh = FALSE,
                                      cache_dir = here("data")) {
  trials_dir <- file.path(cache_dir, glue("trials_sites_{levante_sites_snapshot}"))
  dir_create(trials_dir)
  key <- if (is.null(task_ids)) "all" else paste(sort(task_ids), collapse = "-")
  slice_path <- file.path(trials_dir, glue("bytask_{substr(digest::digest(key), 1, 8)}.rds"))
  if (!refresh && file_exists(slice_path)) return(read_rds(slice_path))

  per_ds <- purrr::pmap(levante_site_specs, \(name, version, site) {
    stub <- sub(":.*", "", name)
    path <- file.path(trials_dir, paste0(stub, ".rds"))
    if (refresh || !file_exists(path)) {
      write_rds(levante::get_trials(name, version = version), path)
    }
    out <- read_rds(path)
    if (!is.null(task_ids)) out <- out |> filter(task_id %in% task_ids)
    if (!"site" %in% names(out)) out <- out |> mutate(site = site)
    out |> mutate(site = coalesce(site, !!site))
  })
  # Normalize to the v1_2 unified trials schema: the newest per-site
  # processing adds columns (language, team, task_version, adaptive) that the
  # unified table never had; downstream notebooks join scores-side language
  # etc. onto trials, so carrying them here causes .x/.y suffix collisions.
  unified_cols <- c("redivis_source","site","dataset","task_id","user_id",
                    "run_id","trial_id","trial_number","item_uid","item_task",
                    "item_group","item","correct","original_correct","rt",
                    "rt_numeric","response","response_type","item_original",
                    "answer","distractors","chance","difficulty",
                    "theta_estimate","theta_se","timestamp")
  res <- bind_rows(per_ds) |>
    select(any_of(unified_cols)) |>
    label_levante_scores()
  write_rds(res, slice_path)
  res
}

#' Load the unified LEVANTE scores dataset, with on-disk cache.
#'
#' @param refresh re-download even if cached
#' @param version "sites-2026-08" (default) binds the per-site processed
#'        datasets (see `levante_site_specs`); "v1_2" reproduces the June
#'        unified `levante_data_latest` pull; "current" pulls the latest
#'        unified release.
load_levante_scores <- function(refresh = FALSE,
                                version = "sites-2026-08",
                                cache_dir = here("data")) {
  if (version == "sites-2026-08") {
    return(load_levante_scores_sites(refresh = refresh, cache_dir = cache_dir))
  }
  dir_create(cache_dir)
  cache_path <- file.path(cache_dir,
                          glue("levante_data_latest__{version}__scores.rds"))

  if (!refresh && file_exists(cache_path)) {
    return(read_rds(cache_path))
  }

  ref <- if (version == "current") "levante_data_latest"
         else glue("levante_data_latest:e9pf:{version}")

  out <- levante::get_scores(ref) |>
    label_levante_scores()

  write_rds(out, cache_path)
  out
}

#' Load trial-level data for one task (or all tasks). Default source is the
#' per-site processed datasets (2026-08 snapshot); pass version = "v1_2" for
#' the old unified pull. The unified trials table is ~280 MB so it is cached
#' once and sliced by task on demand.
#'
#' @param task_ids character vector of task_ids to keep, or NULL for all
#' @param refresh re-download even if cached
#' @param version "sites-2026-08" (default) or a levante_data_latest qualifier
load_levante_trials <- function(task_ids = NULL, refresh = FALSE,
                                version = "sites-2026-08",
                                cache_dir = here("data")) {
  if (version == "sites-2026-08") {
    return(load_levante_trials_sites(task_ids = task_ids, refresh = refresh,
                                     cache_dir = cache_dir))
  }
  dir_create(cache_dir)
  cache_path <- file.path(cache_dir,
                          glue("levante_data_latest__{version}__trials.rds"))

  if (!refresh && file_exists(cache_path)) {
    out <- read_rds(cache_path)
  } else {
    ref <- if (version == "current") "levante_data_latest"
           else glue("levante_data_latest:e9pf:{version}")
    out <- levante::get_trials(ref)
    write_rds(out, cache_path)
  }
  # Apply labels every time (label_levante_scores is idempotent enough)
  out <- label_levante_scores(out)
  if (!is.null(task_ids)) out <- out |> filter(task_id %in% task_ids)
  out
}

# ---- Rescoring with the production mirt model -------------------------------
#
# Thin wrapper around levantemodels::score_irt(): looks up the model spec for
# a task/dataset, filters + recodes the raw trials, and delegates scoring
# (including EAP/ML estimation) entirely to the package. Use this when you
# only have raw trials + a task/dataset pair and want to avoid repeating the
# spec-lookup/filter/recode boilerplate at each call site.
score_task_irt <- function(trials, task, dataset, mod_rec, method = "EAP",
                           scoring_table = NULL) {
  # Prefer the disk-cached scoring table so notebooks render without live
  # Redivis calls; fall back to fetching.
  if (is.null(scoring_table)) {
    st_cache <- here::here("data/scoring_table_cache.rds")
    scoring_table <- if (file.exists(st_cache)) readRDS(st_cache)
                     else levantemodels::fetch_scoring_table()
  }
  spec <- levantemodels::get_model_spec(task, dataset, scoring_table)
  trials_task <- trials |> filter(task_id == spec$task_id | item_task == spec$item_task)
  recoded     <- levantemodels::recode_trials(trials_task)
  levantemodels::score_irt(recoded, as.list(spec), mod_rec, method = method)
}

# ---- Reproducing the pre-fix score_irt() column-order bug -------------------
#
# levantemodels::score_irt() had a bug, fixed in PR #9
# (https://github.com/levante-framework/levantemodels/pull/9): mirt::fscores()
# matches response.pattern columns to the model's items by position, not by
# name, and score_irt() built data_aligned as [overlap items in data order]
# ++ [missing items] without reordering to items(mod_rec) before scoring. The
# installed package no longer has this bug, so to demonstrate its effect on
# scores we reproduce the old behavior here: this is score_irt()'s current
# logic with the column-reorder step removed. For historical comparison only
# -- not for general scoring use.
score_irt_buggy <- function(trial_data_task, mod_spec, mod_rec) {
  data_filtered <- trial_data_task |> rename(group = "dataset") |> levantemodels:::dedupe_items()
  data_wide     <- levantemodels:::to_mirt_shape_grouped(data_filtered)
  data_prepped  <- data_wide |> select(-"group")
  groups        <- data_wide |> pull("group")
  data_group    <- unique(groups)

  if (any(!(data_group %in% mod_rec@group_names))) {
    if (!is.na(mod_spec$invariance) && mod_spec$invariance %in% c("metric", "configural")) {
      return(NULL)
    } else if (!is.na(mod_spec$invariance) && mod_spec$invariance == "scalar") {
      data_group <- mod_rec@group_names[[1]]
    }
  }

  overlap_items <- intersect(colnames(data_prepped), levantemodels::items(mod_rec))
  data_aligned  <- data_prepped |> select(all_of(overlap_items))
  missing_items <- setdiff(levantemodels::items(mod_rec), colnames(data_prepped))
  data_aligned[, missing_items] <- NA
  # <- the bug: no reordering of data_aligned to items(mod_rec) here

  mod_vals <- levantemodels::model_vals(mod_rec)
  if (levantemodels::model_class(mod_rec) == "MultipleGroupClass") {
    mod_recon <- mirt::multipleGroup(data = mod_rec@data, group = mod_rec@groups,
                                      pars = mod_vals, TOL = NaN)
    mod <- mirt::extract.group(mod_recon, group = data_group)
  } else {
    mod <- mirt::mirt(data = mod_rec@data, pars = mod_vals, TOL = NaN)
  }

  fs <- mirt::fscores(mod, method = "EAP", response.pattern = data_aligned)
  tibble::tibble(
    run_id   = rownames(data_prepped),
    score    = as.numeric(fs[, "F1"]),
    score_se = as.numeric(fs[, "SE_F1"])
  )
}

# ---- Item parameters --------------------------------------------------------
#
# IRT item parameters live in the levante_metadata_scoring dataset on Redivis,
# not in levantemodels (as of v1.0). Pull them directly via the redivis R package.
# Each calibration model writes its own row; for cross-site analyses we want
# the multigroup_site / scalar Rasch fits.

#' Load item parameters from levante_metadata_scoring, with disk cache.
#'
#' @param version metadata-scoring version qualifier, default v1_14
#' @param refresh re-download even if cached
load_item_parameters <- function(version = "v1_14", refresh = FALSE,
                                 cache_dir = here("data")) {
  dir_create(cache_dir)
  cache_path <- file.path(cache_dir,
                          glue("item_parameters_{version}.rds"))
  if (!refresh && file_exists(cache_path)) return(read_rds(cache_path))

  if (!requireNamespace("redivis", quietly = TRUE)) {
    stop("Install the redivis R package to load item parameters.")
  }
  user    <- redivis::redivis$user("levante")
  dataset <- user$dataset(glue("levante_metadata_scoring:e97h:{version}"))
  table   <- dataset$table("item_parameters:4cvk")
  out     <- table$to_tibble()
  write_rds(out, cache_path)
  out
}

# ---- Cleaning helpers -------------------------------------------------------
#
# These encode the decisions documented in 01_data_integrity, reports/, and
# tasks/. They are applied once in 00_load_data; downstream notebooks read
# the cleaned tibble.

#' Backfill `adaptive` = FALSE for the 196 documented NA rows.
#' See `reports/adaptive_missingness.html` for the diagnosis: all NA rows are
#' in early beta task_versions of Bogotá Memory and Leipzig/Western Math, and
#' trial-level theta_estimate is entirely absent → non-adaptive administration.
backfill_adaptive_na <- function(scores) {
  scores |> mutate(adaptive = if_else(is.na(adaptive), FALSE, adaptive))
}

#' Return run_ids that should be dropped from ROAR-Word due to non-engagement.
#' Rule: trial-level accuracy < 0.4 OR median RT < 500 ms. See
#' `tasks/roar_word.html`.
roar_word_engagement_drops <- function(trials,
                                       min_accuracy = 0.4,
                                       min_rt_ms = 500) {
  trials |>
    filter(task_id == "swr") |>
    group_by(run_id) |>
    summarise(
      pct_correct = mean(correct, na.rm = TRUE),
      median_rt_ms = median(rt_numeric, na.rm = TRUE),
      n_trials = n(),
      .groups = "drop"
    ) |>
    filter(pct_correct < min_accuracy | median_rt_ms < min_rt_ms) |>
    mutate(reason = case_when(
      pct_correct < min_accuracy & median_rt_ms < min_rt_ms ~ "low_acc_and_fast",
      pct_correct < min_accuracy                             ~ "low_accuracy",
      median_rt_ms < min_rt_ms                                ~ "fast_rt"
    ))
}

#' One-shot clean: apply all documented fixes. Returns a list with the cleaned
#' scores tibble and a small report tibble describing what was changed.
clean_levante_scores <- function(scores, trials = NULL) {
  scores0 <- scores
  n_adapt_na <- sum(is.na(scores0$adaptive))
  scores1 <- backfill_adaptive_na(scores0)

  if (!is.null(trials)) {
    drops <- roar_word_engagement_drops(trials)
  } else {
    drops <- tibble(run_id = character(0), reason = character(0),
                    pct_correct = numeric(0), median_rt_ms = numeric(0),
                    n_trials = integer(0))
  }
  scores2 <- scores1 |> anti_join(drops, by = "run_id")

  report <- tibble(
    step = c("adaptive_na_backfilled", "roar_word_runs_dropped"),
    n    = c(n_adapt_na, nrow(drops))
  )
  list(scores = scores2, report = report, roar_drops = drops)
}

#' Add site / dataset / task labels to a scores tibble.
label_levante_scores <- function(df) {
  df |>
    mutate(
      site_label    = factor(site,    levels = names(site_labels_named),
                             labels  = site_labels_named),
      dataset_label = factor(dataset, levels = names(dataset_labels_named),
                             labels  = dataset_labels_named)
    ) |>
    left_join(task_lookup, by = "task_id") |>
    mutate(task_category = factor(task_category, levels = task_categories_vec))
}

# ---- Plot helpers -----------------------------------------------------------

#' Age × score plot, faceted by task, coloured by site.
levante_age_score_plot <- function(df, point_alpha = 0.2, smooth = TRUE) {
  p <- ggplot(df, aes(x = age, y = score, color = site)) +
    geom_point(alpha = point_alpha, size = 0.7)
  if (smooth) {
    p <- p + geom_smooth(method = "lm", se = FALSE, linewidth = 0.7)
  }
  p +
    facet_wrap(vars(task_category, task_label), scales = "free_y") +
    scale_colour_site() +
    labs(x = "Age (years)", y = "Score", color = NULL) +
    theme(legend.position = "bottom")
}

#' Spaghetti plot of repeat measurements for a single site.
levante_spaghetti <- function(df, color_var = "adaptive") {
  ggplot(df, aes(x = age, y = score, group = user_id,
                 colour = .data[[color_var]])) +
    geom_line(alpha = 0.4) +
    geom_point(alpha = 0.4, size = 0.6) +
    facet_wrap(vars(task_category, task_label), scales = "free_y") +
    labs(x = "Age (years)", y = "IRT ability")
}
