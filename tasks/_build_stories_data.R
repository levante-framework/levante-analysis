# Build the Stories (Theory of Mind) analysis dataset from RAW Redivis data.
#
# Output (gitignored), in data/stories_2026-09/:
#   stories_trials.rds  one row per included ToM trial (release-equivalent run and
#                       trial inclusion, plus the trials that levantemodels PR #27
#                       restores), with run attributes and item-policy FLAG columns
#   stories_runs.rds    one row per raw ToM run, with its production exclusion
#   README.md           provenance, columns, counts, reconciliation, judgement calls
#   raw/                pinned raw pulls (the reproducibility anchor)
#
# Pipeline:
#   1. raw ToM trials/runs/variants/users for the 10 raw datasets that have ToM,
#      pinned to the version tags below (re-pulled read-only only if raw/ lacks them);
#   2. levantemodels::process_trials() (all trials, run/trial filters off, as in
#      production) then recode_trials(), from the levantemodels source tree with the
#      ToM identity + chance corrections of PR #27 (loaded with pkgload::load_all());
#      only the Redivis fetchers are replaced, by the pinned local tables;
#   3. the run/trial inclusion rules that the latest processed releases apply
#      (levante-data-processing process_dataset.R, with the RT cut-offs inferred
#      from the releases: slow > 60 s, fast = "fast" message or < 300 ms);
#   4. the same pipeline with PR #27's fix_tom_item_uids() switched off, to label
#      each trial's PR #27 change and to reconcile against the latest releases.
#
# Run from a working directory OUTSIDE the repo (the repo's renv has an mgcv binary
# that breaks mirt, and running inside packages/levantemodels bootstraps renv):
#   cd <scratch dir> && Rscript <book>/tasks/_build_stories_data.R [<book>]
# Book root: first argument, else env STORIES_BOOK_ROOT, else the directory above this script's tasks/.
# Env (required): LEVANTEMODELS_PATH = levantemodels source tree on the PR #27 branch
#      (fix-tom-identity-corrections), e.g. a git worktree of that branch.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(tibble)
})
options(warn = 1, dplyr.summarise.inform = FALSE, width = 200)

args <- commandArgs(trailingOnly = TRUE)
script_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
default_root <- if (length(script_file) == 1) dirname(dirname(normalizePath(script_file))) else here::here()
book_root <- normalizePath(if (length(args) >= 1) args[1] else
  Sys.getenv("STORIES_BOOK_ROOT", default_root), mustWork = TRUE)
out_dir <- file.path(book_root, "data", "stories_2026-09")
raw_dir <- file.path(out_dir, "raw")
lm_path <- path.expand(Sys.getenv("LEVANTEMODELS_PATH"))
if (!nzchar(lm_path))
  stop("Set LEVANTEMODELS_PATH to a levantemodels source tree on the PR #27 branch ",
       "(fix-tom-identity-corrections), e.g. a git worktree of that branch.")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

# ---- 0. pinned sources ----------------------------------------------------------

# The 10 raw datasets with theory-of-mind runs. Not used: levante_data_pilots_raw,
# levante_data_example_raw and test_dashboard_canada_pilot_dev_raw (every ToM
# trial_id in them is also in a site dataset below); partner_mpib_de_main_raw,
# partner_sparklab_us_downex_raw, pilot_bostonchildrens_us_main_raw and the other raw
# datasets have no ToM runs.
raw_specs <- tribble(
  ~dataset,                   ~raw_ref,
  "pilot_langcog_us_downex",  "pilot_langcog_us_downex_raw:a6kb:v11_1",
  "pilot_mpieva_de_main",     "pilot_mpieva_de_main_raw:6c0n:v16_15",
  "pilot_uniandes_co_bogota", "pilot_uniandes_co_bogota_raw:3j4z:v17_0",
  "pilot_uniandes_co_rural",  "pilot_uniandes_co_rural_raw:66d2:v16_0",
  "pilot_western_ca_main",    "pilot_western_ca_main_raw:97mt:v16_12",
  "rfp1_mpib_de_main",        "rfp1_mpib_de_main_raw:0yye:v4_11",
  "rfp1_mpib_intl_ys",        "rfp1_mpib_intl_ys_raw:3r52:v6_0",
  "rfp1_sheffield_gb_main",   "rfp1_sheffield_gb_main_raw:5tmz:v3_25",
  "rfp1_utdt_ar_main",        "rfp1_utdt_ar_main_raw:3vkz:v4_13",
  "rfp1_utdt_intl_ys",        "rfp1_utdt_intl_ys_raw:8cjy:v4_9"
) |>
  mutate(stub = sub(":.*$", "", raw_ref),
         version = sub("^.*:", "", raw_ref),
         name_code = sub(":v[0-9_]+$", "", raw_ref),
         file = file.path(raw_dir, paste0(stub, "__", version, ".rds")),
         users_file = file.path(raw_dir, paste0(stub, "__", version, "__users_valid.rds")))
meta_ref <- "levante_metadata_items:czjv:v3_18"
meta_file <- file.path(raw_dir, "levante_metadata_items__v3_18.rds")
# latest processed releases (ToM trial ids) and their scores, pulled 2026-09-24; used to
# reconcile, and (scores) by the reliability script. Re-pulled read-only by the pinned
# processed versions below only if the cached files are missing. Sheffield's trial reference
# is its unreleased "next" draft of 2026-09-24, which cannot be pinned: a re-pull takes the
# draft of the day (or, once released, the version that replaced it).
release_trials_file <- file.path(raw_dir, "release_tom_trials__2026-09-24.rds")
release_scores_file <- file.path(raw_dir, "release_tom_scores__2026-09-24.rds")
release_specs <- tribble(
  ~spec,                           ~version, ~trials_from,
  "pilot_langcog_us_downex:d3f2",  "v5_0",   "current",
  "pilot_mpieva_de_main:8wjx",     "v7_2",   "current",
  "pilot_uniandes_co_bogota:d0c5", "v5_1",   "current",
  "pilot_uniandes_co_rural:bxgv",  "v5_1",   "current",
  "pilot_western_ca_main:bgcj",    "v5_1",   "current",
  "rfp1_mpib_de_main:1s67",        "v3_1",   "current",
  "rfp1_mpib_intl_ys:cqyz",        "v2_1",   "current",
  "rfp1_sheffield_gb_main:3pzs",   "v3_1",   "next",
  "rfp1_utdt_ar_main:2b4m",        "v4_0",   "current",
  "rfp1_utdt_intl_ys:csvh",        "v4_0",   "current")
# the released v2_3 ToM scoring model (only its group means/variances are used, by the
# reliability script); fetched with levantemodels' registry helpers if missing
v23_file <- file.path(out_dir, "v2_3_tom_2pl_f1_scalar_modelrecord.rds")
v23_registry_file <- "tom/multigroup_dataset/all_items/tom_2pl_f1_scalar.rds"

# read-only re-pull by pinned tag, used only when a cached file is missing
pull_raw <- function(name_code, version, file, users_file) {
  org <- redivis::redivis$organization("levante")
  ds <- org$dataset(name_code, version = version); ds$get()
  q_tbl <- function(sql) {
    q <- ds$query(sql)$get()
    if (q$properties$outputNumRows == 0) return(tibble())
    suppressWarnings(q$to_tibble())
  }
  tom_users <- "(SELECT DISTINCT user_id FROM runs WHERE task_id = 'theory-of-mind')"
  if (!file.exists(file)) {
    message("pulling ", name_code, ":", version)
    saveRDS(list(
      dataset_ref = name_code, version = version,
      qualified_reference = ds$qualified_reference, pulled_at = Sys.time(),
      trials = q_tbl("SELECT * FROM trials WHERE task_id = 'theory-of-mind'"),
      runs = q_tbl("SELECT * FROM runs WHERE task_id = 'theory-of-mind'"),
      variants = q_tbl(paste("SELECT * FROM variants WHERE variant_id IN",
                             "(SELECT DISTINCT variant_id FROM runs WHERE task_id = 'theory-of-mind')")),
      users = q_tbl(paste("SELECT users.user_id, users.user_type, users.created_at, sites.site_name",
                          "FROM users LEFT JOIN user_sites ON users.user_id = user_sites.user_id",
                          "LEFT JOIN sites ON user_sites.site_id = sites.site_id",
                          "WHERE users.user_id IN", tom_users))
    ), file)
  }
  if (!file.exists(users_file)) {
    message("pulling valid_user for ", name_code, ":", version)
    saveRDS(q_tbl(paste("SELECT user_id, valid_user, validation_msg_user FROM users",
                        "WHERE user_id IN", tom_users)), users_file)
  }
}
pwalk(raw_specs, \(name_code, version, file, users_file, ...) {
  if (!file.exists(file) || !file.exists(users_file)) pull_raw(name_code, version, file, users_file)
})
if (!file.exists(meta_file)) {
  message("pulling ", meta_ref)
  md <- redivis::redivis$organization("levante")$dataset("levante_metadata_items:czjv", version = "v3_18")
  md$get()
  meta <- list(version = "v3_18", qualified_reference = md$qualified_reference, pulled_at = Sys.time())
  for (t in c("item_mapping_trial:hjas", "item_mapping_fields:6v86", "item_mapping_id:tma7",
              "corpus_items:ezfc", "exclusions:0b5t")) {
    meta[[sub(":.*", "", t)]] <- suppressWarnings(md$table(t)$to_tibble())
  }
  saveRDS(meta, meta_file)
}

# read-only re-pull of the release snapshots (same structure as the 2026-09-24 files)
if (!file.exists(release_trials_file) || !file.exists(release_scores_file)) {
  org <- redivis::redivis$organization("levante")
  q_rel <- function(ds, sql) {
    q <- ds$query(sql)$get()
    if (q$properties$outputNumRows == 0) tibble() else suppressWarnings(q$to_tibble())
  }
  rel_pull <- pmap(release_specs, \(spec, version, trials_from) {
    v <- if (trials_from == "next") "next" else version
    message("pulling release ", spec, ":", v)
    ds <- org$dataset(spec, version = v); ds$get()
    ref <- sub("^levante\\.", "", ds$qualified_reference)
    tr <- q_rel(ds, paste("SELECT trial_id, run_id, item_uid, item_task, item_original, correct, timestamp",
                          "FROM trials WHERE task_id = 'theory-of-mind'"))
    ds_sc <- org$dataset(spec, version = version); ds_sc$get()
    sc <- q_rel(ds_sc, "SELECT * FROM scores WHERE task_id = 'theory-of-mind'")
    list(meta = tibble(spec = spec, which = trials_from, ref = ref, n_tom = nrow(tr)),
         trials = tr |> mutate(spec = spec, which = trials_from, processed_ref = ref),
         scores = sc |> mutate(ref = sub("^levante\\.", "", ds_sc$qualified_reference)))
  })
  if (!file.exists(release_trials_file))
    saveRDS(list(meta = map_dfr(rel_pull, "meta"), trials = map_dfr(rel_pull, "trials"),
                 pulled_at = Sys.time()), release_trials_file)
  if (!file.exists(release_scores_file)) saveRDS(map_dfr(rel_pull, "scores"), release_scores_file)
}

raw <- map(set_names(raw_specs$file, raw_specs$dataset), readRDS)
users_valid <- map(set_names(raw_specs$users_file, raw_specs$dataset), readRDS)
meta <- readRDS(meta_file)
stopifnot(all(map_chr(raw, \(x) sub("^levante\\.", "", x$qualified_reference)) == raw_specs$raw_ref))

# ---- 1. levantemodels (PR #27 tree) ------------------------------------------------

git <- \(...) suppressWarnings(system2("git", c("-C", shQuote(lm_path), ...), stdout = TRUE, stderr = TRUE))
lm_commit <- git("rev-parse", "HEAD")
lm_branch <- git("rev-parse", "--abbrev-ref", "HEAD")
lm_dirty <- length(git("status", "--porcelain")) > 0
message("levantemodels: ", lm_path, " @ ", lm_branch, " ", lm_commit, if (lm_dirty) " (DIRTY)")
pkgload::load_all(lm_path, quiet = TRUE)
stopifnot(exists("fix_tom_item_uids", envir = asNamespace("levantemodels")))

# read-only fetch of the released v2_3 ToM model record, if missing
if (!file.exists(v23_file)) {
  message("fetching ", v23_registry_file, " from scoring registry v2_3")
  saveRDS(get_registry_file(v23_registry_file, fetch_registry_dir("v2_3")), v23_file)
}

# process_trials() exactly as production calls it; the only replaced functions are the
# Redivis fetchers (pinned local copies). fix = FALSE also switches off PR #27's
# fix_tom_item_uids(), giving the identity the current releases use.
process_raw <- function(x, fix = TRUE) {
  # what levante:::get_datasets_data() returns: redivis_source first, schema row removed
  prelim <- x$trials |>
    mutate(redivis_source = sub("^levante\\.", "", x$qualified_reference), .before = everything()) |>
    filter(if_any(matches("_id"), \(v) v != "schema_row"))
  go <- \() process_trials(list(), remove_incomplete_runs = FALSE, remove_invalid_runs = FALSE,
                           remove_invalid_trials = FALSE, tasks = "theory-of-mind")
  fetchers <- list(process_trials_prelim = \(...) prelim,
                   fetch_item_mapping_trial = \(...) meta$item_mapping_trial,
                   fetch_item_mapping_fields = \(...) meta$item_mapping_fields,
                   fetch_item_mapping_id = \(...) meta$item_mapping_id,
                   fetch_corpus_items = \(...) meta$corpus_items)
  if (!fix) fetchers$fix_tom_item_uids <- \(trials) trials
  suppressMessages(do.call(testthat::with_mocked_bindings,
                           c(list(code = quote(go()), .package = "levantemodels"), fetchers)))
}

# ---- 2. runs + participants (process_runs() / process_participants() logic) --------

missing_language <- tribble(~variant_id, ~language_,
  "FPbJw79lcfHKJR3fjABb", "de-DE", "KNaxHVqdpe2CtS9NLoX8", "de-DE", "LTQ0EQ4pvI4FAkjY98Pq", "de-DE",
  "zlOE3yc4n4JimAhGtQ6r", "de-DE", "bZjwnLQ4awf0NRA1ZHi8", "de-DE",
  "1Y3K5lAs6yocDwkHH5aT", "es-CO", "OYKVpWxFYhA9Qh9w58Qy", "es-CO", "oD16mNDBnKwPnK7lvaCA", "es-CO",
  "DWTiUJXyw6XfKGiQlIMy", "es-CO",
  "3fFvykenyEYGlAsRfYiJ", "en-US", "5qBz8FFYIsuoYkuGXwVd", "en-US", "8NLzzprrkwJPeY18iRRH", "en-US",
  "r7o97xl8GcdtcCq651n4", "en-US", "rq7PRMzgtkw52HMfxvNW", "en-US")
missing_adaptive <- tribble(~variant_id, ~adaptive_,
  "OYKVpWxFYhA9Qh9w58Qy", FALSE, "zlOE3yc4n4JimAhGtQ6r", FALSE, "rq7PRMzgtkw52HMfxvNW", FALSE,
  "8NLzzprrkwJPeY18iRRH", FALSE, "bZjwnLQ4awf0NRA1ZHi8", TRUE, "DWTiUJXyw6XfKGiQlIMy", TRUE)
legacy_dataset_names <- c("CO-bogota-pilot" = "pilot_uniandes_co_bogota",
                          "CO-rural-pilot" = "pilot_uniandes_co_rural",
                          "CA-western-pilot" = "pilot_western_ca_main",
                          "DE-mpieva-pilot" = "pilot_mpieva_de_main",
                          "US-downward_extension-pilot" = "pilot_langcog_us_downex",
                          "partner-mpib-de" = "partner_mpib_de_main",
                          "partner-childexplore-intl" = "partner_childexplore_intl_ef",
                          "partner-sparklab-us" = "partner_sparklab_us_downex")
form_lookup <- c("theory-of-mind-item-bank"          = "item_bank",
                 "theory-of-mind-retest-A"           = "retest_A",
                 "theory-of-mind-retest-B"           = "retest_B",
                 "CO-theory-of-mind-retest-A"        = "co_retest_A",
                 "CO-theory-of-mind-retest-B"        = "co_retest_B",
                 "theory-of-mind-no-ha-item-bank"    = "no_ha",
                 "theory-of-mind-no-ha-retest-B"     = "no_ha_retest_B",
                 "theory-of-mind-item-bank-cat-test" = "fcat_random3")

build_runs <- function(x, uv, ds) {
  participants <- x$users |>
    filter(user_type %in% c("student", "guest")) |>
    mutate(p_dataset = if_else(site_name %in% names(legacy_dataset_names),
                               unname(legacy_dataset_names[site_name]),
                               str_replace_all(site_name, "-", "_")),
           p_dataset = if_else(is.na(p_dataset) & created_at < as.POSIXct("2024-06-30"),
                               "pilot_uniandes_co_bogota", p_dataset)) |>
    select(user_id, p_dataset)
  x$runs |>
    mutate(task_id = str_remove(task_id, "-es|-de$")) |>
    left_join(x$variants |> select(variant_id, variant_name, language, adaptive, corpus),
              by = "variant_id") |>
    left_join(missing_language, by = "variant_id") |>
    left_join(missing_adaptive, by = "variant_id") |>
    mutate(language = if_else(!is.na(language), language, language_),
           adaptive = if_else(!is.na(adaptive), adaptive, adaptive_)) |>
    select(-language_, -adaptive_) |>
    left_join(uv |> select(user_id, valid_user), by = "user_id") |>
    mutate(age = if_else(!valid_user, NA_real_, age)) |>
    left_join(participants, by = "user_id") |>
    mutate(raw_dataset = ds)
}

# ---- 3. production run/trial inclusion (process_dataset.R, inferred release RT rules) ----

apply_inclusion <- function(runs, processed) {
  trial_data <- processed |> filter(!is.na(item_task), item_task != "ha")
  runs_coded <- runs |>
    mutate(validation_msg_run = replace_na(validation_msg_run, ""),
           ex_no_data = !(run_id %in% trial_data$run_id),
           ex_straightlining = str_detect(validation_msg_run, "straightlining"),
           ex_incomplete = !completed,
           ex_task_bug = !is.na(task_version) & task_version == "1.0.0-beta.19") |>
    mutate(included_1 = !ex_no_data & !ex_task_bug & !ex_incomplete & !ex_straightlining)
  runs_coded_dup <- runs_coded |>
    filter(included_1) |>
    group_by(user_id, task_id, variant_id, administration_id) |>
    arrange(time_started) |>
    mutate(ex_duplicate = row_number() != 1) |>
    ungroup()
  runs_filtered <- runs_coded_dup |> filter(!ex_duplicate)
  trial_data_valid <- trial_data |>
    semi_join(runs_filtered, by = "run_id") |>
    mutate(vmsg = replace_na(validation_msg_trial, ""),
           slow_rt = rt_numeric > 60000,
           fast_rt = str_detect(vmsg, "fast") | (!is.na(rt_numeric) & rt_numeric < 300)) |>
    filter(is.na(rt_numeric) | !slow_rt, !fast_rt) |>
    select(-vmsg, -slow_rt, -fast_rt) |>
    mutate(ex_few_valid_trials = n() < 10, .by = run_id)
  trial_data_filtered <- trial_data_valid |> filter(!ex_few_valid_trials)
  ex_order <- c("no_data", "task_bug", "duplicate", "incomplete", "few_valid_trials", "straightlining")
  exclusion <- runs_coded |>
    select(run_id, starts_with("ex_")) |>
    left_join(runs_coded_dup |> select(run_id, ex_duplicate), by = "run_id") |>
    mutate(ex_few_valid_trials = !(run_id %in% unique(trial_data_filtered$run_id))) |>
    pivot_longer(starts_with("ex_"), names_to = "exclusion") |>
    filter(value) |>
    mutate(exclusion = factor(str_remove(exclusion, "ex_"), levels = ex_order)) |>
    group_by(run_id) |> arrange(exclusion) |> slice(1) |> ungroup() |>
    transmute(run_id, exclusion = as.character(exclusion))
  list(trials = trial_data_filtered,
       runs = runs |> select(run_id) |> left_join(exclusion, by = "run_id") |>
         mutate(included = run_id %in% trial_data_filtered$run_id))
}

# ---- 4. run everything -----------------------------------------------------------

res <- imap(raw, function(x, ds) {
  message("=== ", ds, " (", nrow(x$trials), " raw ToM trials, ", nrow(x$runs), " runs)")
  runs <- build_runs(x, users_valid[[ds]], ds)
  stopifnot(!anyDuplicated(runs$run_id), all(runs$task_id == "theory-of-mind"))
  # participant dataset (process_participants) must equal the raw dataset
  stopifnot(all(runs$p_dataset[runs$run_id %in% x$trials$run_id] == ds, na.rm = TRUE))
  post <- process_raw(x, fix = TRUE)
  pre <- process_raw(x, fix = FALSE)
  inc_post <- apply_inclusion(runs, post)
  inc_pre <- apply_inclusion(runs, pre)
  # recode_trials() on production's trial set (row-wise for ToM, so recoding before
  # or after the run/trial filters gives the same ToM rows)
  rec <- post |> filter(!is.na(item_task), item_task != "ha") |>
    mutate(dataset = ds) |> recode_trials()
  list(runs = runs, post = post, pre = pre, rec = rec, inc_post = inc_post, inc_pre = inc_pre,
       n_raw_trials = nrow(x$trials))
})

# ---- 5. assemble the run table ----------------------------------------------------

site_of <- \(d) str_extract(d, "^[A-z0-9]+_[A-z]+_[A-z]+(?=_)")   # process_participants() `team`
runs_all <- imap(res, function(r, ds) {
  n_raw <- raw[[ds]]$trials |> count(run_id, name = "n_trials_raw")
  r$runs |>
    left_join(r$inc_post$runs, by = "run_id") |>
    left_join(r$inc_pre$runs |> rename(exclusion_pre_pr27 = exclusion, included_pre_pr27 = included),
              by = "run_id") |>
    left_join(r$inc_post$trials |> count(run_id, name = "n_trials"), by = "run_id") |>
    left_join(n_raw, by = "run_id") |>
    mutate(dataset = ds, n_trials = coalesce(n_trials, 0L), n_trials_raw = coalesce(n_trials_raw, 0L))
}) |> bind_rows() |>
  mutate(site = site_of(dataset),
         form = unname(form_lookup[corpus]),
         adaptive_variant = adaptive,
         # no ToM variant has ever been adaptive: the only variants flagged adaptive run the
         # theory-of-mind-item-bank-cat-test corpus, which draws one random story per story group
         adaptive = adaptive_variant %in% TRUE & form != "fcat_random3")
stopifnot(!any(is.na(runs_all$form)), !any(runs_all$adaptive))
runs_out <- runs_all |>
  transmute(dataset, site, language, age, valid_user, form, adaptive, adaptive_variant, corpus,
            task_version, variant_id, variant_name, user_id, run_id, administration_id,
            time_started, time_finished, completed, stop_reason, n_trials_raw,
            included, exclusion, n_trials, included_pre_pr27, exclusion_pre_pr27) |>
  arrange(dataset, time_started)

# ---- 6. assemble the trial table ------------------------------------------------------

item_meta <- readRDS(file.path(book_root, "data", "tom_meta", "tom_item_metadata.rds"))
story_construct <- item_meta |>
  filter(!is_hostile_attribution) |>
  distinct(story = as.integer(tom_scenario), new_construct)
stopifnot(!anyDuplicated(story_construct$story), nrow(story_construct) == 18)

pre_uids <- imap(res, \(r, ds) r$pre |> filter(item_task %in% "tom") |>
                   transmute(trial_id, uid_pre = item_uid,
                             story_uid_pre = paste0("tom_story", str_extract(item_original, "^[0-9]+"),
                                                    "_", item_group, "_", item))) |> bind_rows()
generic <- imap(res, \(r, ds) r$post |> select(trial_id, generic_uid = item_uid)) |> bind_rows()
included_ids <- imap(res, \(r, ds) r$inc_post$trials |> select(trial_id)) |> bind_rows()
stopifnot(!anyDuplicated(included_ids$trial_id))

trials <- imap(res, \(r, ds) r$rec) |> bind_rows() |>
  filter(item_task == "tom") |>
  semi_join(included_ids, by = "trial_id") |>
  left_join(generic, by = "trial_id") |>
  left_join(pre_uids, by = "trial_id") |>
  left_join(runs_all |> select(run_id, site, language, age, form, adaptive, adaptive_variant, corpus,
                               task_version, variant_id, time_started, administration_id),
            by = "run_id") |>
  mutate(story = as.integer(str_extract(item_original, "^[0-9]+")),
         story_uid = as.character(item_uid),
         entry = item,
         old_group = item_group,
         question_type = str_remove(entry, "_[0-9]+$"),
         is_control = question_type == "reality_check",
         pr27_change = case_when(is.na(uid_pre) ~ "restored",
                                 story_uid_pre != story_uid ~ "relabelled",
                                 TRUE ~ "none"),
         # item-policy flags (rows are kept)
         # (the wording summarises the 2026-09 item screen, tasks/_stories_fits_items.R)
         policy_reason = case_when(
           old_group == "reference" & entry == "reference" ~
             paste("reference_reference (stories 5/11/17): below chance in every language except",
                   "German story 5 (at chance); negative item-rest r in every story"),
           story == 4 & entry %in% c("reality_check_2", "reality_check_3") & item_original == "4g" ~
             paste("story-4 reality checks 2/3 on image 4g (from 2024-10): rc_3 below chance in every",
                   "language, rc_2 in de-DE and en-US (weak in en-GB, at chance in es-CO); the 'yes' key",
                   "conflicts with the 4g image"),
           story == 2 & entry == "reality_check_1" & item_original == "2e" &
             timestamp < as.POSIXct("2024-07-01", tz = "UTC") ~
             paste("story-2 reality check 1, first Bogota content (2e, May-June 2024): below chance;",
                   "cause undetermined (inverted key, question identity or content)"),
           story == 10 & entry == "false_belief_1" & old_group == "deception" ~
             "story-10 deception false belief 1: no discrimination (item-rest r CI spans 0 in every language)",
           TRUE ~ NA_character_),
         policy_exclude = !is.na(policy_reason),
         spanish_fb_flag = old_group == "reality_known" & entry == "false_belief" &
           story %in% c(1, 7, 13) & str_starts(language, "es")) |>
  left_join(story_construct, by = "story")
stopifnot(!any(is.na(trials$story)), !any(is.na(trials$new_construct)),
          nrow(trials) == nrow(included_ids),
          all(trials$story_uid == paste0("tom_story", trials$story, "_", trials$old_group, "_", trials$entry)))

# metadata consistency: control status and old group agree with the book's item metadata
meta_chk <- trials |> distinct(story_uid, is_control, old_group) |>
  inner_join(item_meta |> select(story_uid = item_uid, m_control = is_control, m_group = old_group),
             by = "story_uid")
stopifnot(all(meta_chk$is_control == meta_chk$m_control), all(meta_chk$old_group == meta_chk$m_group))
uids_not_in_meta <- setdiff(unique(trials$story_uid), item_meta$item_uid)

trials_out <- trials |>
  transmute(dataset, site, language, age, form, adaptive, adaptive_variant, corpus, task_version,
            variant_id, time_started, user_id, run_id, administration_id,
            trial_id, trial_number, timestamp, item_original, story, story_uid, generic_uid, entry,
            old_group, question_type, is_control, new_construct, chance,
            correct = as.logical(correct), response, answer, distractors, rt_numeric,
            policy_exclude, policy_reason, spanish_fb_flag,
            pr27_change, story_uid_pre_pr27 = story_uid_pre) |>
  arrange(dataset, user_id, time_started, run_id, trial_number)

# ---- 7. reconciliation with the latest processed releases --------------------------------

recon <- NULL
if (file.exists(release_trials_file)) {
  rel_all <- readRDS(release_trials_file)$trials
  rel <- rel_all |>
    group_by(spec) |> filter(which == if ("next" %in% which) "next" else "current") |> ungroup() |>
    mutate(dataset = sub(":.*", "", spec))
  stopifnot(!anyDuplicated(rel$trial_id))
  rel_refs <- rel |> distinct(dataset, release_ref = processed_ref)
  pre_inc <- imap(res, \(r, ds) r$inc_pre$trials |> transmute(trial_id, dataset = ds)) |> bind_rows()
  post_inc <- trials_out |> select(trial_id, dataset, story_uid, correct, pr27_change, run_id)
  rel_runs <- unique(rel$run_id)
  pre_runs <- runs_all |> filter(included_pre_pr27) |> pull(run_id)
  post_runs <- runs_all |> filter(included) |> pull(run_id)
  both <- post_inc |> inner_join(rel |> select(trial_id, rel_uid = item_uid, rel_correct = correct), by = "trial_id")
  added <- post_inc |> anti_join(rel, by = "trial_id") |>
    mutate(kind = case_when(pr27_change == "restored" & run_id %in% rel_runs ~ "restored, run already released",
                            pr27_change == "restored" ~ "restored, run newly included",
                            !(run_id %in% rel_runs) ~ "mapped before, run newly included",
                            TRUE ~ "other"))
  dropped <- rel |> anti_join(post_inc, by = "trial_id") |>
    left_join(runs_all |> select(run_id, exclusion), by = "run_id")
  recon <- list(
    per_dataset = raw_specs |> select(dataset) |>
      left_join(rel_refs, by = "dataset") |>
      left_join(rel |> count(dataset, name = "release_trials"), by = "dataset") |>
      left_join(pre_inc |> count(dataset, name = "rebuilt_pre_pr27"), by = "dataset") |>
      left_join(pre_inc |> semi_join(rel, by = "trial_id") |> count(dataset, name = "pre_and_release"), by = "dataset") |>
      left_join(post_inc |> count(dataset, name = "stories_trials"), by = "dataset") |>
      left_join(added |> count(dataset, name = "added"), by = "dataset") |>
      left_join(dropped |> count(dataset, name = "dropped"), by = "dataset") |>
      left_join(both |> filter(story_uid != rel_uid) |> count(dataset, name = "relabelled"), by = "dataset") |>
      left_join(both |> filter(correct != rel_correct) |> count(dataset, name = "correct_differs"), by = "dataset") |>
      left_join(tibble(run_id = rel_runs) |> left_join(runs_all |> select(run_id, dataset), by = "run_id") |>
                  count(dataset, name = "release_runs"), by = "dataset") |>
      left_join(runs_all |> filter(included) |> count(dataset, name = "stories_runs"), by = "dataset") |>
      mutate(across(where(is.integer), \(v) coalesce(v, 0L))),
    added_by_kind = added |> count(dataset, kind),
    added_by_uid = added |> count(dataset, pr27_change, story_uid),
    dropped = dropped |> count(dataset, exclusion),
    pre_vs_release = c(pre_not_in_release = nrow(anti_join(pre_inc, rel, by = "trial_id")),
                       release_not_in_pre = nrow(anti_join(rel, pre_inc, by = "trial_id"))),
    runs = c(release_runs = length(rel_runs), pre_runs = length(pre_runs), post_runs = length(post_runs),
             pre_runs_not_release = length(setdiff(pre_runs, rel_runs)),
             release_runs_not_pre = length(setdiff(rel_runs, pre_runs)),
             post_runs_not_release = length(setdiff(post_runs, rel_runs)),
             release_runs_not_post = length(setdiff(rel_runs, post_runs))),
    run_flips = runs_all |>
      filter(coalesce(exclusion_pre_pr27, "included") != coalesce(exclusion, "included")) |>
      count(dataset, before = coalesce(exclusion_pre_pr27, "included"), after = coalesce(exclusion, "included")))
  trials_out <- trials_out |> mutate(in_release = trial_id %in% rel$trial_id, .after = pr27_change)
}
if (file.exists(release_scores_file) && !is.null(recon)) {
  sc <- readRDS(release_scores_file)
  cmp <- runs_all |> select(run_id, dataset, exclusion_pre_pr27) |>
    inner_join(sc |> select(run_id, release_exclusion = exclusion, release_source = redivis_source), by = "run_id")
  recon$scores_exclusion_agreement <- cmp |>
    mutate(agree = coalesce(exclusion_pre_pr27, "") == coalesce(release_exclusion, "")) |>
    count(dataset, release_source, agree)
  recon$scores_exclusion_disagree <- cmp |>
    filter(coalesce(exclusion_pre_pr27, "") != coalesce(release_exclusion, "")) |>
    count(dataset, rebuilt_pre_pr27 = exclusion_pre_pr27, release_exclusion)
}

# ---- 8. provenance + save ---------------------------------------------------------------

provenance <- list(
  built_at = Sys.time(),
  script = "tasks/_build_stories_data.R",
  raw_sources = raw_specs |> transmute(dataset, raw_ref,
                                       pulled_at = map(raw, "pulled_at") |> map_chr(format)),
  metadata = c(ref = meta_ref, pulled_at = format(meta$pulled_at)),
  levantemodels = c(path = lm_path, branch = lm_branch, commit = lm_commit, dirty = lm_dirty,
                    pr = "levante-framework/levantemodels#27 (fix-tom-identity-corrections)"),
  release_refs = if (!is.null(recon)) recon$per_dataset |> select(dataset, release_ref) else NULL,
  rules = c(run = "no_data, task_bug (1.0.0-beta.19), incomplete, straightlining, duplicate (first run per user x task x variant x administration)",
            trial = "drop item_task NA or 'ha'; drop rt_numeric > 60000; drop 'fast' message or rt_numeric < 300",
            run_after_trials = "drop runs left with < 10 trials")
)
attr(trials_out, "provenance") <- provenance
attr(runs_out, "provenance") <- provenance
saveRDS(trials_out, file.path(out_dir, "stories_trials.rds"))
saveRDS(runs_out, file.path(out_dir, "stories_runs.rds"))
saveRDS(recon, file.path(out_dir, "reconciliation.rds"))
message("saved ", nrow(trials_out), " trials, ", nrow(runs_out), " runs")

# ---- 9. README -------------------------------------------------------------------------

md <- \(df) paste(knitr::kable(df, format = "pipe"), collapse = "\n")
n_fmt <- \(v) format(v, big.mark = ",")
tot <- function(df) {
  s <- summarise(df, across(where(is.numeric), sum))
  s[[names(df)[1]]] <- "**total**"
  bind_rows(df, s)
}

counts_dfl <- trials_out |>
  group_by(dataset, form, language) |>
  summarise(runs = n_distinct(run_id), children = n_distinct(user_id), trials = n(),
            stories = paste(sort(unique(story)), collapse = ","),
            first = format(min(time_started), "%Y-%m-%d"), last = format(max(time_started), "%Y-%m-%d"),
            .groups = "drop")
runs_by_ds <- runs_out |>
  count(dataset, exclusion = coalesce(exclusion, "included")) |>
  pivot_wider(names_from = exclusion, values_from = n, values_fill = 0L) |>
  relocate(any_of(c("dataset", "included", "no_data", "task_bug", "duplicate", "incomplete",
                    "few_valid_trials", "straightlining"))) |>
  mutate(total = rowSums(across(where(is.numeric))))
policy_tab <- trials_out |> filter(policy_exclude) |>
  count(policy_reason, story_uid, dataset) |>
  group_by(policy_reason, story_uid) |>
  summarise(trials = sum(n), datasets = n_distinct(dataset), .groups = "drop")
policy_acc <- trials_out |> filter(policy_exclude) |>
  group_by(story_uid, item_original) |>
  summarise(trials = n(), accuracy = round(mean(correct), 3), chance = first(chance), .groups = "drop")
spanish_tab <- trials_out |>
  filter(old_group == "reality_known", entry == "false_belief") |>
  mutate(lang = if_else(str_starts(language, "es"), "es-*", language)) |>
  group_by(story, lang, spanish_fb_flag) |>
  summarise(trials = n(), accuracy = round(mean(correct), 3), .groups = "drop")
pr27_tab <- trials_out |> count(dataset, pr27_change) |>
  pivot_wider(names_from = pr27_change, values_from = n, values_fill = 0L)
western_empty <- runs_out |>
  filter(dataset == "pilot_western_ca_main", variant_id == "Qx8XB8ZAFwP0RhhiEKw5",
         time_started >= as.POSIXct("2026-04-25", tz = "UTC")) |>
  summarise(runs = n(), no_data = sum(exclusion %in% "no_data"), trials_raw = sum(n_trials_raw),
            first = format(min(time_started), "%Y-%m-%d"), last = format(max(time_started), "%Y-%m-%d"))
dropped_unmapped <- imap(res, \(r, ds) r$post |>
  filter(is.na(item_task)) |> count(dataset = ds, generic_uid = item_uid, name = "trials")) |>
  bind_rows() |> arrange(desc(trials))
no_uid <- imap(res, \(r, ds) raw[[ds]]$trials |> anti_join(r$post, by = "trial_id") |>
                 mutate(dataset = ds, empty = is.na(item) & is.na(answer) & is.na(response))) |> bind_rows()
n_no_item_info <- nrow(no_uid)
n_no_item_empty <- sum(no_uid$empty)
no_uid_tab <- no_uid |> count(dataset, empty, name = "trials")
n_invalid_users <- runs_out |> filter(valid_user %in% FALSE) |>
  summarise(users = n_distinct(user_id), runs = n(), datasets = paste(unique(dataset), collapse = ", "))
na_age_incl <- runs_out |> filter(included, is.na(age)) |> count(dataset)
na_tv <- trials_out |> filter(is.na(task_version)) |>
  summarise(trials = n(), runs = n_distinct(run_id), datasets = paste(unique(dataset), collapse = ", "),
            first = format(min(time_started), "%Y-%m-%d"), last = format(max(time_started), "%Y-%m-%d"))
opt_mismatch <- trials_out |>
  mutate(n_opt = 1 + str_count(distractors, "'[0-9]+':")) |>
  filter(abs(chance - 1 / n_opt) > 0.01) |>
  count(dataset, form, story_uid, item_original, n_opt, chance, name = "trials")
de_ha_answer <- trials_out |> filter(str_starts(answer, "hostile-attribution")) |>
  summarise(trials = n(), runs = n_distinct(run_id), datasets = paste(unique(dataset), collapse = ", "),
            first = format(min(time_started), "%Y-%m-%d"), last = format(max(time_started), "%Y-%m-%d"),
            task_versions = paste(sort(unique(task_version)), collapse = ", "))
# the Airtable exclusion list as synced to Redivis (generic uids, metadata v3_18) vs the flags here
excl_tab <- meta$exclusions |> distinct(generic_uid = item_uid) |>
  filter(str_starts(generic_uid, "tom_")) |>
  left_join(trials_out |> group_by(generic_uid) |>
              summarise(included_trials = n(), stories = paste(sort(unique(story)), collapse = ","),
                        policy_exclude = sum(policy_exclude), spanish_fb_flag = sum(spanish_fb_flag),
                        .groups = "drop"),
            by = "generic_uid") |>
  arrange(generic_uid)
fcat_runs <- runs_out |> filter(included, form == "fcat_random3") |> pull(run_id)
fcat_chk <- list(
  runs = length(fcat_runs),
  stories_per_run = trials_out |> filter(run_id %in% fcat_runs) |>
    summarise(n = n_distinct(story), .by = run_id) |> count(stories = n, name = "runs"),
  theta = imap(res, \(r, ds) r$post |> filter(run_id %in% fcat_runs) |> select(theta_estimate)) |>
    bind_rows() |> count(theta_estimate, name = "trials"))
chance_tab <- trials_out |> count(chance, question_type) |>
  pivot_wider(names_from = question_type, values_from = n, values_fill = 0L)

cols_trials <- tribble(
  ~column, ~description,
  "dataset", "raw site dataset (stub without `_raw`); equals the participant dataset that process_participants() derives",
  "site", "production `team` (`dataset` minus its last component), matching the site codes in common.R",
  "language", "variant language (process_runs() backfill applied)",
  "age", "runs.age in years; NA for invalid users, as process_runs() does",
  "form", "corpus as a form label (table below)",
  "adaptive", "TRUE only for a genuinely adaptive administration; FALSE for every ToM run",
  "adaptive_variant", "the variant's logged adaptive flag (TRUE for the fCAT corpus)",
  "corpus, task_version, variant_id, time_started, administration_id", "run attributes from raw runs/variants",
  "user_id, run_id, trial_id", "raw ids",
  "trial_number", "process_trials() order within run (by server timestamp)",
  "timestamp", "trial server timestamp",
  "item_original", "raw deployment item code, e.g. `4g`, `6d_new2` (story = its leading digits)",
  "story", "story 1-18",
  "story_uid", "scoring uid after recode_trials(): `tom_story{story}_{old_group}_{entry}`",
  "generic_uid", "story-less uid from add_item_ids() (+ PR #27 fix), as in corpus_items",
  "entry", "corpus entry (question within story), e.g. `false_belief_2`",
  "old_group", "story type: reality_known, moral_reasoning, interpretation, deception, reference, second_order",
  "question_type", "entry without its number: false_belief, reality_check, emotion_reasoning, reference",
  "is_control", "question_type == reality_check",
  "new_construct", "story-level construct from data/tom_meta/tom_item_metadata.rds",
  "chance", "guessing floor after PR #27 (one value per story question)",
  "correct", "raw correctness (recode_trials() changes nothing for ToM)",
  "response, answer, distractors", "raw response and English stimulus keys (see the DE beta.20/21 caveat)",
  "rt_numeric", "response time, ms",
  "policy_exclude, policy_reason", "item-policy flag and reason; rows are kept",
  "spanish_fb_flag", "reality_known false_belief (stories 1/7/13) in an es-* language; analyse with and without",
  "pr27_change", "none / relabelled / restored: what PR #27 did to this trial",
  "in_release", "trial_id is in the latest processed release (reconciliation snapshot)",
  "story_uid_pre_pr27", "story uid without PR #27 (NA if the trial was dropped without it)")
cols_runs <- tribble(
  ~column, ~description,
  "dataset ... variant_name", "as in the trial table, plus valid_user",
  "time_finished, completed, stop_reason", "raw run fields",
  "n_trials_raw", "raw ToM trials logged for the run",
  "included, exclusion", "production inclusion and first exclusion reason (hierarchy no_data > task_bug > duplicate > incomplete > few_valid_trials > straightlining), with PR #27",
  "n_trials", "trials of this run in stories_trials.rds",
  "included_pre_pr27, exclusion_pre_pr27", "the same without PR #27 (should reproduce the releases)")

readme <- c(
  "# Stories (Theory of Mind) analysis data, 2026-09",
  "",
  paste0("Built ", format(provenance$built_at, "%Y-%m-%d %H:%M %Z"), " by `tasks/_build_stories_data.R`. ",
         "Gitignored; rebuild with `Rscript tasks/_build_stories_data.R <book root>` from a working directory outside the repo."),
  "",
  "## Provenance",
  "",
  paste0("- **Raw data**: the ToM trials, runs, variants and users (+ site) of every raw LEVANTE dataset with ToM runs, ",
         "pinned to the tags below and cached in `raw/` (copied from the 2026-09-24 audit pulls). ",
         "The script re-pulls a dataset by its pinned tag (read-only) only if its cache file is missing. ",
         "`raw/*__users_valid.rds` holds `users.valid_user` for the ToM users, pulled separately at the same tags ",
         "because process_runs() needs it to null invalid users' ages."),
  paste0("- **Item identity metadata**: `", meta_ref, "` (item_mapping_trial/fields/id, corpus_items), cached in `raw/`."),
  paste0("- **levantemodels**: `", lm_path, "`, branch `", lm_branch, "`, commit `", lm_commit, "`",
         if (lm_dirty) " (**working tree dirty**)" else " (clean)",
         " = PR #27 (ToM item-identity fixes in three deployment epochs + per-story chance), on top of main bd24157."),
  "- **Book item metadata**: `data/tom_meta/tom_item_metadata.rds` (story-level `new_construct`).",
  "",
  md(provenance$raw_sources),
  "",
  "## Pipeline",
  "",
  "1. `process_trials()` with every run/trial filter off (as production calls it), on each raw dataset. The only functions replaced are the Redivis fetchers (`process_trials_prelim`, `fetch_item_mapping_*`, `fetch_corpus_items`), which return the pinned local tables.",
  "2. Production's trial set: drop trials whose uid is not in corpus_items (`item_task` NA) and hostile-attribution items (`item_task == \"ha\"`); then `recode_trials()`, which rewrites ToM uids to story level.",
  "3. Run exclusions from `levante-data-processing/scripts/processing/process_dataset.R` (f151f86): no_data, task_bug (task_version 1.0.0-beta.19), incomplete, straightlining, then duplicates (all but the first run per user x task x variant x administration). Trials: drop rt_numeric > 60 s and 'fast' messages or rt_numeric < 300 ms; then drop runs left with < 10 trials. The RT cut-offs are the ones the latest releases were built with (inferred and verified exactly by the audit; the repo script says 30 s).",
  "4. Steps 1-3 again with PR #27's `fix_tom_item_uids()` switched off. That rebuild should reproduce the latest releases; the difference between the two is what PR #27 changes.",
  "",
  "## Counts",
  "",
  paste0("`stories_trials.rds`: ", n_fmt(nrow(trials_out)), " trials, ", n_fmt(n_distinct(trials_out$run_id)),
         " runs, ", n_fmt(n_distinct(trials_out$user_id)), " children, ", n_distinct(trials_out$story_uid),
         " story uids. `stories_runs.rds`: ", n_fmt(nrow(runs_out)), " raw ToM runs."),
  "",
  "### Runs by dataset and production exclusion (with PR #27)",
  "",
  md(tot(runs_by_ds)),
  "",
  "### Included trials by dataset x form x language",
  "",
  md(counts_dfl),
  "",
  "### PR #27 changes among included trials",
  "",
  md(tot(pr27_tab)),
  ""
)
if (!is.null(recon)) {
  pv <- recon$pre_vs_release; rr <- recon$runs
  readme <- c(readme,
    "## Reconciliation with the latest processed releases",
    "",
    paste0("Release snapshot: ToM trials of each processed dataset's latest version, pulled 2026-09-24 (`raw/release_tom_trials__2026-09-24.rds`; ",
           "the Sheffield reference is its unreleased `next` draft of that day, which matches raw v3_25)."),
    "",
    paste0("- **Without PR #27** the rebuild reproduces the releases: ", n_fmt(sum(recon$per_dataset$rebuilt_pre_pr27)),
           " rebuilt trials vs ", n_fmt(sum(recon$per_dataset$release_trials)), " released; ",
           pv[["pre_not_in_release"]], " rebuilt trials are not in a release and ", pv[["release_not_in_pre"]],
           " released trials are not rebuilt. Runs: ", n_fmt(rr[["pre_runs"]]), " rebuilt vs ", n_fmt(rr[["release_runs"]]),
           " released (", rr[["pre_runs_not_release"]], " / ", rr[["release_runs_not_pre"]], " mismatches)."),
    paste0("- **With PR #27** (`stories_trials.rds`): ", n_fmt(sum(recon$per_dataset$stories_trials)), " trials = ",
           n_fmt(sum(recon$per_dataset$release_trials)), " released + ", n_fmt(sum(recon$per_dataset$added)), " added - ",
           n_fmt(sum(recon$per_dataset$dropped)), " dropped; ", n_fmt(sum(recon$per_dataset$relabelled)),
           " released trials carry a different story uid; `correct` differs for ", sum(recon$per_dataset$correct_differs),
           ". Runs: ", n_fmt(rr[["post_runs"]]), " included (", rr[["post_runs_not_release"]], " not in a release, ",
           rr[["release_runs_not_post"]], " released runs no longer included)."),
    "",
    md(tot(recon$per_dataset |> select(-release_ref))),
    "",
    "Release references:",
    "",
    md(recon$per_dataset |> select(dataset, release_ref)),
    "",
    "Added trials by kind:",
    "",
    md(recon$added_by_kind),
    "",
    if (nrow(recon$dropped)) c("Released trials no longer included, by their run's exclusion with PR #27:", "", md(recon$dropped), "") else character(),
    if (nrow(recon$run_flips)) c("Runs whose production exclusion changes with PR #27:", "", md(recon$run_flips), "") else character(),
    if (!is.null(recon$scores_exclusion_agreement)) c(
      "Run exclusions (rebuild without PR #27) vs the `exclusion` column of the released scores tables (`raw/release_tom_scores__2026-09-24.rds`; its Sheffield and LangCog scores come from one raw version earlier than the pinned raw data):",
      "", md(recon$scores_exclusion_agreement), "",
      if (nrow(recon$scores_exclusion_disagree)) c(md(recon$scores_exclusion_disagree), "") else character()) else character()
  )
}
readme <- c(readme,
  "## Item-policy flags (rows kept)",
  "",
  "`policy_exclude` marks items the analyses exclude by default; `spanish_fb_flag` marks items to analyse with and without. Neither drops rows.",
  "",
  md(policy_tab),
  "",
  "Accuracy of the flagged items (all included trials):",
  "",
  md(policy_acc),
  "",
  "Generic ToM uids on the exclusion list (`levante_metadata_items` v3_18 `exclusions`, synced from Airtable) and how many of their included trials carry each flag:",
  "",
  md(excl_tab),
  "",
  "reality_known false belief (stories 1, 7, 13) by language:",
  "",
  md(spanish_tab),
  "",
  "## Forms",
  "",
  md(tibble(corpus = names(form_lookup), form = unname(form_lookup)) |>
       left_join(runs_out |> filter(included) |> count(corpus, name = "included_runs"), by = "corpus")),
  "",
  "## Columns",
  "",
  "`stories_trials.rds`:",
  "",
  md(cols_trials),
  "",
  "`stories_runs.rds`:",
  "",
  md(cols_runs),
  "",
  "Both objects carry `attr(, \"provenance\")` (sources, commit, rules). `reconciliation.rds` holds the reconciliation tables.",
  "",
  "Chance by question type (trials):",
  "",
  md(chance_tab),
  "",
  "## Judgement calls",
  "",
  "1. **Datasets.** Ten raw datasets. `levante_data_pilots_raw` v5_1, `levante_data_example_raw` v5_2 and `test_dashboard_canada_pilot_dev_raw` v1_1 also hold ToM trials (41,330, 70 and 111), but every one of their ToM trial_ids is also in a site dataset (checked on the 2026-09-24 audit pulls), so they are not bound. `rfp1_mpib_de_main` is included although `common.R::levante_site_specs` does not list it; its `site` code `rfp1_mpib_de` has no entry in common.R's `site_pal`. `partner_mpib_de_main`, `partner_sparklab_us_downex` and `pilot_bostonchildrens_us_main` have no ToM runs.",
  "2. **Processing code.** levantemodels from the PR #27 tree, not the installed package or main. PR #27 is unmerged; the releases do not yet contain its changes, and this dataset anticipates them (restored trials kept).",
  "3. **Inclusion rules** follow what the latest releases do (60 s / 300 ms RT cut-offs), not the repo script's 30 s rule. The duplicate and few-valid-trials rules are re-applied after PR #27, so a run can change status when restored trials give it data.",
  "4. **Item exclusions are not applied by the pipeline** (production never reads the Airtable/Redivis exclusion table, and its generic uids match nothing after recode_tom()). The analyses use the story-level `policy_exclude` flag instead: reference_reference in stories 5/11/17 everywhere; story-4 reality checks 2/3 only on image 4g; story-2 reality check 1 only on the first Bogota content (2e, before 2024-07-01; the cause, an inverted key, a swapped question identity or content, is undetermined, so the trials are excluded rather than corrected); story-10 deception false belief 1, which does not discriminate in any language (the other two stories under the same generic uid, 4 and 16, do). The other generic uids on the exclusion list are not flagged, because the 2026-09 item screen found them fine at story level (table below). These flags are analysis choices defined in this script; the intended home for item exclusions is the Airtable exclusion table (so they are documented outside code), which would need story- and epoch-scoped rows to express them.",
  "5. **Spanish false belief.** reality_known false-belief items (stories 1/7/13) score below chance in es-* samples, but the cause (translation, audio, age or sample) is unresolved. `spanish_fb_flag` marks a sensitivity set that the analyses run with and without; it is not a proposed exclusion (stories 1 and 7 still discriminate in es-CO; see the chapter).",
  paste0("6. **adaptive.** No ToM administration has been adaptive. The `theory-of-mind-item-bank-cat-test` corpus (\"fCAT\", variants flagged adaptive) shows one random story from each of 3 story groups, with no item parameters and no theta updates (core-tasks code, per the 2026-09 audit); it is `form = fcat_random3`, `adaptive = FALSE`. The logged flag is kept as `adaptive_variant`. In the ",
         fcat_chk$runs, " included fcat_random3 runs, stories per run: ",
         paste0(fcat_chk$stories_per_run$stories, " (", fcat_chk$stories_per_run$runs, " runs)", collapse = ", "),
         "; logged theta_estimate over all their processed trials (before RT filtering): ",
         paste0(coalesce(as.character(fcat_chk$theta$theta_estimate), "NA"), " (", n_fmt(fcat_chk$theta$trials), " trials)", collapse = ", "), "."),
  "7. **Forms** are named after the corpus. `item_bank` spans several content versions over 2024-2026 (first Bogota content May-June 2024, V1 Aug-Oct 2024, V2 from Oct 2024, and the item_uid-column epoch May 2025 - Apr 2026); `item_original` and `time_started` distinguish them.",
  "8. **question_type / is_control** come from the corpus entry (entry without its number), so every story uid gets one; `new_construct` is joined by story from the book's item metadata (one construct per story). Control status and story type agree with that metadata for every story uid it contains.",
  paste0("9. **Age** is runs.age, nulled for users with `valid_user == FALSE`, as process_runs() does (",
         n_invalid_users$users, " users, ", n_invalid_users$runs, " runs, in ", n_invalid_users$datasets, "). ",
         "Included runs without age: ", paste0(na_age_incl$dataset, " ", na_age_incl$n, collapse = ", "),
         ". `task_version` and `administration_id` are NA for ", n_fmt(na_tv$trials), " included trials (", na_tv$runs,
         " runs, ", na_tv$datasets, ", ", na_tv$first, " to ", na_tv$last, ")."),
  paste0("10. **Chance** is levantemodels' value after PR #27: corpus_items per generic uid, with story 6 fb_3, story 12 fb_2 and story 18 fb_2 set to .5 (yes/no in those stories). ",
         "It equals 1 / (1 + number of logged distractors) in every included trial except ",
         if (nrow(opt_mismatch)) paste0(opt_mismatch$trials, " ", opt_mismatch$dataset, " ", opt_mismatch$form, " trials of ",
                                        opt_mismatch$story_uid, " (", opt_mismatch$item_original, ", ", opt_mismatch$n_opt,
                                        " options logged, chance ", opt_mismatch$chance, ")", collapse = "; ") else "none",
         "."),
  paste0("11. **Hostile-attribution answer keys on ToM trials.** In ", n_fmt(de_ha_answer$trials), " included trials (",
         de_ha_answer$runs, " runs, ", de_ha_answer$datasets, ", ", de_ha_answer$first, " to ", de_ha_answer$last,
         ", task_version ", de_ha_answer$task_versions, ") the logged `answer`/`distractors` are hostile-attribution strings. ",
         "The 2026-09 audit found `correct` valid there (it matches the V1 corpus key in every trial), so the trials are kept; do not use `answer` for those runs."),
  "12. **Known unrecoverable losses** (not in this table): Bogota May-June 2024 trials with no item, answer or response logged; the V0/V1 story-1 emotion question (`tom_reality_known_emotion_reasoning`, not in corpus_items); the beta.19 DE runs (task_bug); and the Western item-bank variant's empty runs (below).",
  paste0("13. **Western empty runs.** ", western_empty$runs, " Western runs on the item-bank variant Qx8XB8ZAFwP0RhhiEKw5 from ",
         western_empty$first, " to ", western_empty$last, " logged ", western_empty$trials_raw, " trials (", western_empty$no_data,
         " are excluded as no_data). Per the core-tasks team, the variant still loaded theory-of-mind-item-bank, whose hostile-attribution prompts are missing from the English item-bank translation, so the task aborted at its startup corpus check before the first trial (incident report 2026-07-12; Sentry DASHBOARD-19A/1A0; core-tasks#437). The children completed the other tasks; no ToM data exist for these runs."),
  "",
  "Trials still dropped because their uid is not in corpus_items (after PR #27):",
  "",
  md(dropped_unmapped),
  "",
  paste0("A further ", n_fmt(n_no_item_info), " raw ToM trials get no item uid in process_trials(); ",
         n_fmt(n_no_item_empty), " of them have no item, answer or response logged (`empty`):"),
  "",
  md(no_uid_tab),
  if (length(uids_not_in_meta)) paste0("\nStory uids absent from the book item metadata: ", paste(uids_not_in_meta, collapse = ", "), ".") else character()
)
writeLines(readme, file.path(out_dir, "README.md"))
message("wrote README.md")
