# Same & Different block-timing data build (2026-10-29), for the question of
# how to allocate the task's max time across its blocks (core-tasks #519,
# per-block time limits, first shipped in v1.3.18 on 2026-10-07).
# Pulls SDS runs (+ variant params) and ALL trials (practice and
# instructions included, with server timestamps) from every raw site
# dataset at the pinned versions in data/sds_raw_refs_2026-10-29.txt.
# Writes data/sds_timing/{raw_trials,runs}.rds.

suppressPackageStartupMessages({library(dplyr); library(purrr)})

out_dir <- here::here("data/sds_timing")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
org <- redivis::redivis$organization("levante")
raw_refs <- readLines(here::here("data/sds_raw_refs_2026-10-29.txt"))

pull_site <- function(ref) {
  d <- org$dataset(ref)
  tr <- d$query(paste(
    "SELECT trial_id, run_id, user_id, trial_index, assessment_stage, is_practice_trial,",
    "corpus_trial_type, item, item_id, item_uid, answer, response, correct, rt_numeric,",
    "server_timestamp, time_elapsed, theta_estimate, theta_se, valid_trial, validation_msg_trial",
    "FROM trials WHERE task_id = 'same-different-selection'"))$to_tibble()
  rn <- d$query(paste(
    "SELECT r.run_id, r.user_id, r.variant_id, r.task_version, r.time_started, r.time_finished,",
    "r.age, r.completed, r.valid_run, r.stop_reason, r.num_attempted,",
    "v.variant_name, v.created_at AS variant_created, v.max_time, v.adaptive, v.cat",
    "FROM runs r LEFT JOIN variants v ON r.variant_id = v.variant_id",
    "WHERE r.task_id = 'same-different-selection'"))$to_tibble()
  dataset <- sub("_raw:.*", "", ref)
  list(trials = tr |> mutate(dataset = dataset, across(everything(), as.character)),
       runs = rn |> mutate(dataset = dataset, raw_ref = ref, across(everything(), as.character)))
}

pulled <- map(raw_refs, \(r) {
  message("pulling ", r)
  tryCatch(pull_site(r), error = \(e) { message("  skipped: ", conditionMessage(e)); NULL })
}) |> compact()
trials <- map_dfr(pulled, "trials")
runs <- map_dfr(pulled, "runs")

saveRDS(trials, file.path(out_dir, "raw_trials.rds"))
saveRDS(runs, file.path(out_dir, "runs.rds"))
message("saved ", nrow(trials), " raw trials, ", nrow(runs), " runs")
