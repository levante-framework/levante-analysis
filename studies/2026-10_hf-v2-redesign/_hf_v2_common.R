# Shared derivations for the H&F v1-vs-v2 chapter (sourced by
# studies/2026-10_hf-v2-redesign/hearts_and_flowers_v2.qmd). Inputs from studies/2026-10_hf-v2-redesign/_prep_hf_v2.R.

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
uid_block <- c(hearts = "hearts", flowers = "flowers", mixed = "heartsflowers")

# Two early v1 logging formats need the block recovered another way:
#  - Bogotá, May-June 2024: corpus_trial_type empty, block in assessment_stage
#    ("hearts stimulus"), and trial_index missing (order by timestamp)
#  - Leipzig, Sept 2024: corpus_trial_type "1500" for every block; blocks are
#    the 1st, 2nd and later runs of test trials between practice trials
hf_all <- hf_raw |>
  inner_join(hf_runs |> select(run_id, form, age), by = "run_id") |>
  arrange(run_id, server_timestamp, trial_index) |>
  group_by(run_id) |>
  mutate(is_test = !is_practice_trial &
           (assessment_stage == "test_response" | grepl(" stimulus$", assessment_stage)),
         test_run = cumsum(is_test & !lag(is_test, default = FALSE))) |>
  ungroup() |>
  mutate(block = case_when(
    corpus_trial_type %in% names(block_of) ~ unname(block_of[corpus_trial_type]),
    is.na(corpus_trial_type) ~ unname(block_of[sub(" (stimulus|practice)$", "", assessment_stage)]),
    corpus_trial_type == "1500" ~ c("hearts", "flowers", rep("mixed", 10))[pmax(test_run, 1)],
    TRUE ~ NA_character_))

# test trials only, in order; segment = 16-trial chunk of the mixed block.
# item_uid follows recode_hf(): within each block the first trial is
# "start", a repeat of the previous stimulus is "stay", a change is "switch"
hf_test <- hf_all |>
  filter(is_test, !is.na(block)) |>
  mutate(rt_numeric = as.numeric(rt_numeric),
         correct = as.logical(correct),
         timeout = validation_msg_trial %in% "rt_missing") |>
  group_by(run_id, block) |>
  mutate(pos = row_number(),
         segment = if_else(block == "mixed", as.integer(ceiling(pos / 16)), NA_integer_),
         type = case_when(pos == 1 ~ "start",
                          item == lag(item) ~ "stay",
                          TRUE ~ "switch")) |>
  ungroup() |>
  mutate(
    item_uid = paste0("hf_", uid_block[block], "_", item, "_", type),
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
