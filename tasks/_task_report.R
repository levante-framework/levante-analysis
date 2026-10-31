# Data and summaries for the standard task report (tasks/_task_report.qmd).
# Sourced by the template after common.R; everything is computed for one
# task, `task_id`, on the pinned release.

scores_all <- load_levante_latest("scores")
data_ref <- unique(scores_all$redivis_source)
scores_clean <- load_levante_clean()

task <- task_lookup |> filter(task_id == .env$task_id)
task_label <- task$task_label

runs <- scores_clean |> filter(task_id == .env$task_id) |> add_waves()
trials <- load_levante_latest_trials(task_id) |>
  filter(run_id %in% runs$run_id) |>
  mutate(lang = substr(language, 1, 2))
# the items table has one row per item per dataset; a task calibrated by
# language has one set of parameters per language (`subset`, e.g. "es-CO")
items <- load_levante_latest("items") |>
  filter(task_id == .env$task_id) |>
  distinct(item_uid, subset, .keep_all = TRUE)
has_items <- nrow(items) > 0
by_language <- has_items && all(items$model_set == "by_language")
items <- items |> mutate(lang = if (by_language) substr(subset, 1, 2) else NA_character_)
item_langs <- sort(unique(items$lang))
has_se <- any(!is.na(runs$score_se))
# accuracy against chance is meaningless for some tasks (e.g. memory spans)
check_chance <- params$check_chance %||% TRUE
# a response is to a calibrated item if the item is in the bank (for its
# language, when the task is calibrated by language)
trials <- trials |>
  mutate(calibrated = if (by_language) paste(item_uid, lang) %in% paste(items$item_uid, items$lang)
                      else item_uid %in% items$item_uid)

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
  group_by(item_uid, lang = if (by_language) lang else NA_character_) |>
  summarise(responses = n(), calibrated = any(calibrated), .groups = "drop")
item_lang <- trials |>
  filter(check_chance, !is.na(correct), chance > 0) |>
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
share_calibrated <- mean(trials$calibrated)
# items whose calibration rests on few responses
min_calib <- 100
thin_items <- items |> filter(n_responses < min_calib)
item_key <- \(df) if (by_language) paste(df$item_uid, df$lang) else df$item_uid
share_thin <- mean(item_key(trials) %in% item_key(thin_items))
lang_names <- c(de = "German", en = "English", es = "Spanish")
score_type_label <- c(ability = "IRT ability estimate",
                      guessing_adjusted_rate_scaled = "guessing-adjusted rate of correct responses, scaled")[
  names(sort(table(runs$score_type), decreasing = TRUE))[1]]
minutes_median <- median(runs$minutes, na.rm = TRUE)

# a range of values for prose: "a–b", or one value when they agree
rng <- \(x, digits = 2) {
  fmt <- if (digits == 0) \(v) as.character(round(v)) else f2
  if (fmt(min(x)) == fmt(max(x))) fmt(min(x)) else paste0(fmt(min(x)), "–", fmt(max(x)))
}

# an open-issues bullet for items answered below chance, or nothing
below_chance_issue <- function() {
  k <- sum(item_lang$below_chance)
  if (k == 0) return("")
  paste0("- ", k, if (k == 1) " item-language combination is" else " item-language combinations are",
         " answered below chance (@fig-item-health); their answer keys, pictures",
         " and translations need checking.")
}

# a run's age, for trial-level summaries
age_at_run <- \(ids) runs$age[match(ids, runs$run_id)]
