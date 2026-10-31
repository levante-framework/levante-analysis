# Data and summaries for the standard task report (tasks/_task_report.qmd).
# Sourced by the template after common.R; everything is computed for one
# task, `task_id`, on the pinned release.

scores_all <- load_levante_latest("scores")
data_ref <- unique(scores_all$redivis_source)
scores_clean <- load_levante_clean()

task <- task_lookup |> filter(task_id == .env$task_id)
task_label <- task$task_label

runs <- scores_clean |> filter(task_id == .env$task_id) |> add_waves()
trials <- load_levante_latest_trials(task_id) |> filter(run_id %in% runs$run_id)
items <- load_levante_latest("items") |>
  filter(task_id == .env$task_id) |>
  distinct(item_uid, .keep_all = TRUE)

# datasets with enough runs to summarize on their own
min_runs <- 50
report_ds <- runs |> count(dataset, sort = TRUE) |> filter(n >= min_runs) |> pull(dataset)
by_ds <- \(df) df |>
  filter(dataset %in% report_ds) |>
  mutate(dataset_label = factor(dataset_labels_named[dataset],
                                rev(dataset_labels_named[report_ds])))

# each child's first run, and each child's first run in each wave
first_runs <- runs |>
  group_by(dataset, user_id) |>
  slice_min(time_started, n = 1, with_ties = FALSE) |>
  ungroup()
wave_runs <- runs |>
  group_by(dataset, user_id, wave) |>
  slice_min(time_started, n = 1, with_ties = FALSE) |>
  ungroup()

# forms: version (major.minor) and adaptive vs fixed
runs <- runs |>
  mutate(version = coalesce(sub("^(\\d+\\.\\d+).*", "\\1", task_version), "unknown"),
         form = paste(version, if_else(adaptive, "adaptive", "fixed")),
         minutes = as.numeric(difftime(time_finished, time_started, units = "mins")))

# reliability, retest and age correlation per dataset
reliability <- first_runs |>
  filter(!is.na(score_se)) |>
  group_by(dataset) |>
  filter(n() >= min_runs) |>
  summarise(n = n(), reliability = var(score) / (var(score) + mean(score_se^2)),
            .groups = "drop")
retest_pairs <- wave_runs |>
  filter(wave <= 2) |>
  group_by(dataset, user_id) |>
  filter(n() == 2) |>
  arrange(wave, .by_group = TRUE) |>
  summarise(score1 = score[1], score2 = score[2],
            gap_months = as.numeric(difftime(time_started[2], time_started[1],
                                             units = "days")) / 30.4,
            version_change = sub("^(\\d+\\.\\d+).*", "\\1", task_version[1]) !=
              sub("^(\\d+\\.\\d+).*", "\\1", task_version[2]),
            .groups = "drop")
retest <- retest_pairs |>
  group_by(dataset) |>
  filter(n() >= 20) |>
  reframe(cor_ci(score1, score2), gap_months = median(gap_months),
          version_change = mean(version_change, na.rm = TRUE))
retest_big <- retest |> filter(n >= 50)
age_cor <- first_runs |>
  filter(!is.na(age)) |>
  group_by(dataset) |>
  filter(n() >= min_runs) |>
  reframe(cor_ci(score, age))

# correlations with the other tasks: first runs, scores standardized within
# dataset and task, pooled across datasets
other_tasks <- scores_clean |>
  filter(task_id != .env$task_id, task_id %in% c(core_task_ids, "swr", "sre", "pa")) |>
  group_by(dataset, user_id, task_id) |>
  slice_min(time_started, n = 1, with_ties = FALSE) |>
  group_by(dataset, task_id) |>
  mutate(z_other = as.numeric(scale(score))) |>
  ungroup() |>
  select(dataset, user_id, other_task = task_label, z_other)
task_cor <- first_runs |>
  group_by(dataset) |>
  mutate(z = as.numeric(scale(score))) |>
  ungroup() |>
  select(dataset, user_id, z) |>
  inner_join(other_tasks, by = c("dataset", "user_id")) |>
  group_by(other_task) |>
  filter(n() >= 100) |>
  reframe(cor_ci(z, z_other)) |>
  arrange(desc(r))

# score range ends: share of runs within 5% of the dataset's score range
# from its lowest or highest score
ends <- first_runs |>
  group_by(dataset) |>
  mutate(position = (score - min(score)) / (max(score) - min(score))) |>
  summarise(n = n(), low = mean(position < .05), high = mean(position > .95),
            .groups = "drop")

# items: bank coverage, exposure and accuracy by language
item_use <- trials |>
  mutate(lang = substr(language, 1, 2)) |>
  group_by(item_uid) |>
  summarise(responses = n(), .groups = "drop")
item_lang <- trials |>
  mutate(lang = substr(language, 1, 2)) |>
  filter(!is.na(correct), chance > 0) |>
  group_by(item_uid, lang) |>
  summarise(n = n(), accuracy = mean(correct), chance = mean(chance), .groups = "drop") |>
  filter(n >= 30) |>
  mutate(upper = (accuracy + 1.96^2 / (2 * n) +
                    1.96 * sqrt(accuracy * (1 - accuracy) / n + 1.96^2 / (4 * n^2))) /
           (1 + 1.96^2 / n),
         below_chance = upper < chance)

# summary numbers
n_runs <- nrow(runs)
n_children <- n_distinct(runs$user_id)
share_adaptive <- mean(runs$adaptive, na.rm = TRUE)
minutes_median <- median(runs$minutes, na.rm = TRUE)

# a run's age, for trial-level summaries
age_at_run <- \(ids) runs$age[match(ids, runs$run_id)]
