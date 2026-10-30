# Hearts & Flowers v1 vs v2 ("taskVersion 2", core-tasks PR #442, 2026-04-21)
# data build for studies/2026-10_hf-v2-redesign/hearts_and_flowers_v2.qmd.
#
# v2 adds hard response deadlines (pure 3000 ms; mixed 3000/2000/1500 ms in
# three 16-trial segments), coin/fail feedback audio on every test trial, and
# records no-response timeouts (response = null, task-scored incorrect).
# Processing drops timeouts (validation_msg_trial = "rt_missing"), so they are
# only visible in the raw per-site trials tables, pulled here.
#
# Writes data/hf_v2/{raw_trials,runs}.rds.

suppressPackageStartupMessages({library(dplyr); library(purrr); library(tidyr)})

out_dir <- here::here("data/hf_v2")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
org <- redivis::redivis$organization("levante")

raw_refs <- c(
  "pilot_mpieva_de_main_raw:6c0n:v16_29", "pilot_uniandes_co_bogota_raw:3j4z:v18_4",
  "pilot_uniandes_co_rural_raw:66d2:v17_1", "pilot_western_ca_main_raw:97mt:v16_21",
  "partner_mpib_de_main_raw:7n7w:v6_30", "rfp1_mpib_de_main_raw:0yye:v4_25",
  "rfp1_sheffield_gb_main_raw:5tmz:v3_45", "rfp1_utdt_ar_main_raw:3vkz:v4_23",
  "rfp1_utdt_intl_ys_raw:8cjy:v4_16", "pilot_bostonchildrens_us_main_raw:7e6c:v5_34",
  "pilot_capecoast_gh_main_raw:cnr4:v2_4", "pilot_leuphana_de_main_raw:1j0w:v1_3",
  "pilot_langcog_us_downex_raw:a6kb:v11_3")

pull_site <- function(ref) {
  d <- org$dataset(ref)
  tr <- d$query(paste(
    "SELECT run_id, user_id, trial_index, assessment_stage, is_practice_trial,",
    "corpus_trial_type, item, answer, response, correct, rt, rt_numeric,",
    "server_timestamp, input_type, valid_trial, validation_msg_trial",
    "FROM trials WHERE task_id = 'hearts-and-flowers'"))$to_tibble()
  rn <- d$query(paste(
    "SELECT r.run_id, r.user_id, r.variant_id, r.task_version, r.time_started,",
    "r.age, r.completed, r.valid_run, r.stop_reason, v.variant_name, v.created_at AS variant_created",
    "FROM runs r LEFT JOIN variants v ON r.variant_id = v.variant_id",
    "WHERE r.task_id = 'hearts-and-flowers'"))$to_tibble()
  dataset <- sub("_raw:.*", "", ref)
  list(trials = tr |> mutate(dataset = dataset, across(c(rt, response), as.character)),
       runs = rn |> mutate(dataset = dataset, raw_ref = ref,
                           across(c(variant_created, time_started), as.character)))
}

pulled <- map(raw_refs, \(r) { message("pulling ", r); pull_site(r) })
trials <- map_dfr(pulled, "trials")
runs <- map_dfr(pulled, "runs")

saveRDS(trials, file.path(out_dir, "raw_trials.rds"))
saveRDS(runs, file.path(out_dir, "runs.rds"))
message("saved ", nrow(trials), " raw trials, ", nrow(runs), " runs")
