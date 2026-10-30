# DEPRECATED: studies/2026-07_sds-scoring/same_different.qmd and studies/2026-07_sds-scoring/_sds_scoring_fits.R now read
# SDS trials from the model calibration dataset
# (../levante-pilots/01_fetched_data/task_data_nested.rds), not from this
# hackathon pull. Kept around in case the hackathon dataset is needed again.
#
# Pull Same & Different Selection (SDS) trials from the unified hackathon
# pilots dataset (levante_data_pilots_hackathon:fpx0:v1_1) using the new
# `levante` package, and cache locally for studies/2026-07_sds-scoring/same_different.qmd.
# Run once; triggers Redivis OAuth. Scores come from the shared
# data/hackathon_v1_1_scores.rds (pulled by studies/2026-07_sre-scoring/_pull_sre_hackathon.R).

library(levante)
library(dplyr)
library(readr)

data_dir <- here::here("data")
src <- "levante_data_pilots_hackathon:fpx0"
ver <- "v1_1"

trials_file <- file.path(data_dir, "hackathon_v1_1_trials_sds.rds")
scores_file <- file.path(data_dir, "hackathon_v1_1_scores.rds")

if (!file.exists(trials_file)) {
  trials <- get_trials(src, version = ver)
  trials |>
    filter(item_task == "sds") |>
    write_rds(trials_file, compress = "gz")
  rm(trials)
}

if (!file.exists(scores_file)) {
  get_scores(src, version = ver) |>
    write_rds(scores_file, compress = "gz")
}

message("done: ", paste(basename(list.files(data_dir, pattern = "hackathon")), collapse = ", "))
