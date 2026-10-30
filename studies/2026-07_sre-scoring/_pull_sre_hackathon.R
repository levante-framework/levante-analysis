# DEPRECATED: studies/2026-07_sre-scoring/sre.qmd and the _fit_sre_*.R scripts now read SRE trials
# from the model calibration dataset
# (../levante-pilots/01_fetched_data/task_data_nested.rds), not from this
# hackathon pull, and PA/SWR/Vocab/TROG validity criteria from
# ../levante-pilots/02_scoring_outputs/scores/registry_scores.rds. Kept around
# in case the hackathon dataset is needed again.
#
# Pull SRE / PA / SWR data from the unified hackathon pilots dataset
# (levante_data_pilots_hackathon:fpx0:v1_1) using the new `levante` package,
# and cache locally for studies/2026-07_sre-scoring/sre.qmd. Run once; triggers Redivis OAuth.

library(levante)
library(dplyr)
library(readr)

data_dir <- here::here("data")
src <- "levante_data_pilots_hackathon:fpx0"
ver <- "v1_1"

trials_file <- file.path(data_dir, "hackathon_v1_1_trials_roar.rds")
scores_file <- file.path(data_dir, "hackathon_v1_1_scores.rds")
participants_file <- file.path(data_dir, "hackathon_v1_1_participants.rds")

if (!file.exists(trials_file)) {
  trials <- get_trials(src, version = ver)
  trials |>
    filter(task_id %in% c("sre", "pa", "swr")) |>
    write_rds(trials_file, compress = "gz")
  rm(trials)
}

if (!file.exists(scores_file)) {
  get_scores(src, version = ver) |>
    write_rds(scores_file, compress = "gz")
}

if (!file.exists(participants_file)) {
  get_participants(src, version = ver) |>
    write_rds(participants_file, compress = "gz")
}

message("done: ", paste(basename(list.files(data_dir, pattern = "hackathon")), collapse = ", "))
