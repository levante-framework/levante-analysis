# Shared derivations for the H&F v1-vs-v2 chapter (sourced by
# tasks/hearts_and_flowers_v2.qmd). Inputs from tasks/_prep_hf_v2.R.

suppressPackageStartupMessages({library(dplyr); library(tidyr)})

hf_dir <- here::here("data/hf_v2")
# trial_index arrives as character — sorting it as text scrambles trial order
hf_raw <- readRDS(file.path(hf_dir, "raw_trials.rds")) |>
  mutate(trial_index = as.integer(trial_index))
hf_runs_raw <- readRDS(file.path(hf_dir, "runs.rds"))

# variant generation = intended form. v2 variants were created from
# 2026-04-23 on (core-tasks #442 shipped 2026-04-21 as 1.1.2).
hf_runs <- hf_runs_raw |>
  distinct(run_id, .keep_all = TRUE) |>
  mutate(form = if_else(variant_created >= "2026-04-01", "v2", "v1"),
         age = as.numeric(age),
         time_started = as.POSIXct(time_started, tz = "UTC"))

block_of <- c("hearts" = "hearts", "flowers" = "flowers",
              "hearts and flowers" = "mixed")

# test trials only; segment = 16-trial chunk of the mixed block in order
hf_test <- hf_raw |>
  filter(!is_practice_trial, assessment_stage == "test_response") |>
  inner_join(hf_runs |> select(run_id, form, age), by = "run_id") |>
  mutate(block = unname(block_of[corpus_trial_type]),
         rt_numeric = as.numeric(rt_numeric),
         correct = as.logical(correct),
         timeout = validation_msg_trial %in% "rt_missing") |>
  group_by(run_id, block) |>
  arrange(trial_index, .by_group = TRUE) |>
  mutate(pos = row_number(),
         segment = if_else(block == "mixed", as.integer(ceiling(pos / 16)), NA_integer_)) |>
  ungroup() |>
  mutate(
    # response window (ms) by design; v1 had none
    window = case_when(form == "v1" ~ Inf,
                       block != "mixed" ~ 3000,
                       segment == 1 ~ 3000, segment == 2 ~ 2000, TRUE ~ 1500),
    # pipeline (as released): timeouts are kept and incorrect; recode_hf()
    # also marks rt < 200 or rt > 2000 incorrect
    sc_pipeline = if_else(timeout, FALSE, correct & rt_numeric >= 200 & rt_numeric <= 2000),
    # task: what the child was told by the feedback sound — correct side
    # within the design window (no 2000 ms recode cutoff)
    sc_task = if_else(timeout, FALSE, correct %in% TRUE))

# run completeness on the intended form
hf_run_struct <- hf_test |>
  group_by(run_id, form) |>
  summarise(n_hearts = sum(block == "hearts"), n_flowers = sum(block == "flowers"),
            n_mixed = sum(block == "mixed"), .groups = "drop") |>
  mutate(layout = case_when(
    form == "v1" & n_mixed == 32 ~ "v1 complete (32 mixed)",
    form == "v2" & n_mixed == 48 ~ "v2 complete (48 mixed)",
    form == "v2" & n_mixed == 16 ~ "v2 heavyInstructions (16 mixed)",
    TRUE ~ "incomplete"))
