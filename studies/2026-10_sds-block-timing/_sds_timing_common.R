# Shared derivations for the SDS block-timing analysis (inputs from
# studies/2026-10_sds-block-timing/_prep_sds_timing.R). One row per run with the time budget split into
# segments, using the task's own clock (`time_elapsed`, ms since the task
# started; preload is excluded from the timer since v1.3.18).
suppressPackageStartupMessages({library(dplyr); library(tidyr)})

sds_dir <- here::here("data/sds_timing")
vnum <- \(v) vapply(strsplit(sub("-.*", "", v), "[.]"),
                    \(x) if (length(x) == 3) sum(as.numeric(x) * c(1e6, 1e3, 1)) else NA_real_, 0)

sds_runs_raw <- readRDS(file.path(sds_dir, "runs.rds")) |>
  distinct(run_id, .keep_all = TRUE) |>
  mutate(age = as.numeric(age), max_time = as.numeric(max_time),
         block_timer = vnum(task_version) >= vnum("1.3.18"))

sds_trials <- readRDS(file.path(sds_dir, "raw_trials.rds")) |>
  semi_join(sds_runs_raw |> filter(max_time == 6), by = "run_id") |>
  mutate(ti = as.integer(trial_index), el = as.numeric(time_elapsed) / 1000,
         rt = as.numeric(rt_numeric) / 1000, correct = as.logical(correct),
         practice = is_practice_trial %in% "TRUE" | assessment_stage == "practice_response",
         segment = case_when(
           corpus_trial_type == "instructions" ~ "instructions",
           practice ~ "practice",
           corpus_trial_type == "test-dimensions" ~ "dimensions",
           startsWith(corpus_trial_type, "something-same") ~ "something_same",
           grepl("match", corpus_trial_type) ~ "match",
           TRUE ~ "other")) |>
  arrange(run_id, ti)

# segment boundaries per run (seconds on the task clock); a trial's start =
# its time_elapsed minus its response time
seg <- sds_trials |>
  filter(!is.na(el)) |>
  group_by(run_id) |>
  summarise(
    n_dims = sum(segment == "dimensions"), n_ss = sum(segment == "something_same"),
    n_match_resp = sum(segment == "match"), n_practice = sum(segment == "practice"),
    ss_practice = sum(segment == "practice" & startsWith(corpus_trial_type, "something-same")),
    dims_start = suppressWarnings(min(el[segment == "dimensions"] - rt[segment == "dimensions"], na.rm = TRUE)),
    dims_end = suppressWarnings(max(el[segment == "dimensions"])),
    ss_start = suppressWarnings(min(el[segment == "something_same"] - rt[segment == "something_same"], na.rm = TRUE)),
    ss_end = suppressWarnings(max(el[segment == "something_same"])),
    match_start = suppressWarnings(min(el[segment == "match"] - rt[segment == "match"], na.rm = TRUE)),
    match_end = suppressWarnings(max(el[segment == "match"])),
    task_end = max(el), .groups = "drop") |>
  mutate(across(where(is.numeric), \(x) if_else(is.finite(x), x, NA_real_)))

sds_budget <- sds_runs_raw |>
  filter(max_time == 6) |>
  select(run_id, dataset, user_id, age, task_version, block_timer, stop_reason, completed, variant_name) |>
  inner_join(seg, by = "run_id") |>
  mutate(young = age < 5,
         intro = dims_start,
         dims = dims_end - dims_start,
         ss_block = if_else(n_ss > 0, ss_end - ss_start, NA_real_),
         # time between the end of the last pre-match segment and the first match trial:
         # match instructions + practice (and, for older children, the something-same
         # practice that always runs)
         pre_match = match_start - coalesce(ss_end, dims_end),
         match = match_end - match_start,
         reached_match = !is.na(match_start))

# ---- information per trial --------------------------------------------------
# The released processed trials drop nearly all SDS match responses from
# August 2026 on (levantemodels::recode_sds() finds trial boundaries via the
# word "different" in the second-choice prompt, which changed to "...in a new
# way..."), so recent runs can't use processed item ids or released theta.
# Instead each raw test response gets a key (block x choice), and the
# pre-August runs (processed intact) give the distribution of scored items each
# key maps to, including "dropped by the recode" (no information). Theta is
# re-estimated from raw correctness with those mixtures (EAP, prior = released
# theta by age band in pre-August runs); expected information per response is
# the mixture average at that theta.
proc <- readRDS(file.path(sds_dir, "processed_latest.rds"))
ip <- readRDS(file.path(sds_dir, "item_parameters_released.rds")) |>
  filter(model_registry_version == max(model_registry_version)) |>
  group_by(item_uid) |>
  summarise(b = mean(difficulty), a = mean(discrimination), g = first(guessing), .groups = "drop")
p_item <- \(th, a, b, g) g + (1 - g) * plogis(a * (th - b))
info_item <- \(th, a, b, g) {
  p <- p_item(th, a, b, g)
  a^2 * ((p - g) / (1 - g))^2 * (1 - p) / p
}
choice_of <- c(first_response = 1, second_response = 2, third_response = 3, fourth_response = 4)
sds_trials <- sds_trials |>
  mutate(key = case_when(
    segment == "dimensions" ~ "dimensions",
    segment == "something_same" ~ corpus_trial_type,
    segment == "match" ~ paste0(substr(corpus_trial_type, 1, 1), "match_choice",
                                choice_of[assessment_stage]),
    TRUE ~ NA_character_))
pre_aug <- sds_runs_raw |> filter(substr(time_started, 1, 7) <= "2026-07") |> pull(run_id)
mix <- sds_trials |>
  filter(!practice, !is.na(key), run_id %in% pre_aug, run_id %in% proc$trials$run_id) |>
  left_join(proc$trials |> select(trial_id, scored = item_uid), by = "trial_id") |>
  mutate(scored = if_else(scored %in% ip$item_uid, scored, NA_character_)) |>
  count(key, scored) |>
  group_by(key) |> mutate(w = n / sum(n)) |> ungroup() |>
  left_join(ip, by = c("scored" = "item_uid"))
mix_scored <- mix |> filter(!is.na(scored)) |> group_by(key) |> mutate(w_s = w / sum(w)) |> ungroup()

theta_grid <- seq(-7, 4, by = 0.05)
key_p <- mix_scored |> group_by(key) |>
  summarise(P = list(sapply(theta_grid, \(t) sum(w_s * p_item(t, a, b, g)))), .groups = "drop")
key_i <- mix |> mutate(i = if_else(is.na(scored), 0, 1)) |> group_by(key) |>
  summarise(I = list(sapply(theta_grid, \(t) sum(if_else(is.na(scored), 0, w * info_item(t, a, b, g))))),
            .groups = "drop")

released_theta <- proc$scores |>
  filter(!is.na(score)) |> distinct(run_id, .keep_all = TRUE) |>
  select(run_id, theta_released = score, se_released = score_se)
age_band <- \(a) cut(a, c(0, 5, 6, 8, 10, 20), right = FALSE)
prior_by_age <- sds_runs_raw |>
  filter(run_id %in% pre_aug) |>
  inner_join(released_theta, by = "run_id") |>
  group_by(band = age_band(age)) |>
  summarise(mu = mean(theta_released), sd = sd(theta_released), .groups = "drop") |>
  filter(!is.na(band))

resp <- sds_trials |>
  filter(!practice, key %in% key_p$key, !is.na(correct)) |>
  left_join(sds_runs_raw |> select(run_id, age), by = "run_id")
keys <- key_p$key
Pmat <- do.call(rbind, key_p$P); rownames(Pmat) <- keys
theta <- resp |>
  mutate(band = age_band(age)) |>
  left_join(prior_by_age, by = "band") |>
  mutate(mu = coalesce(mu, mean(prior_by_age$mu)), sd = coalesce(sd, mean(prior_by_age$sd))) |>
  group_by(run_id) |>
  summarise(theta = {
    lp <- dnorm(theta_grid, first(mu), first(sd), log = TRUE)
    for (k in seq_along(key)) {
      pk <- Pmat[key[k], ]; lp <- lp + if (correct[k]) log(pk) else log1p(-pk)
    }
    w <- exp(lp - max(lp)); sum(w * theta_grid) / sum(w)
  }, n_resp = n(), .groups = "drop") |>
  left_join(released_theta, by = "run_id")

Imat <- do.call(rbind, key_i$I); rownames(Imat) <- key_i$key
info_at <- \(key, th) {
  out <- numeric(length(key)); ok <- key %in% rownames(Imat) & !is.na(th)
  gi <- pmin(pmax(round((th[ok] - min(theta_grid)) / 0.05) + 1, 1), length(theta_grid))
  out[ok] <- Imat[cbind(match(key[ok], rownames(Imat)), gi)]
  out
}
sds_trials <- sds_trials |>
  left_join(theta |> select(run_id, theta), by = "run_id") |>
  mutate(info = if_else(practice, 0, info_at(key, theta)))

# ---- event list per run: the run's own timeline, in task order --------------
# component durations (s) come from consecutive saved rows on the task clock;
# each test trial's duration = time since the previous saved row (includes
# fixation/feedback); practice components are summed by kind.
sds_events <- sds_trials |>
  filter(!is.na(el)) |>
  group_by(run_id) |>
  arrange(ti, .by_group = TRUE) |>
  mutate(prev_el = lag(el), first_start = first(el - coalesce(rt, 0)),
         dur = if_else(is.na(prev_el), el - first_start, el - prev_el),
         kind = case_when(
           segment == "dimensions" ~ "dims",
           segment == "practice" & grepl("match", corpus_trial_type) ~ "pm",
           segment == "practice" & startsWith(corpus_trial_type, "something-same") ~ "pss",
           segment == "something_same" ~ "ss",
           segment == "instructions" ~ "ig",
           segment == "match" ~ "match",
           TRUE ~ "other")) |>
  ungroup() |>
  filter(kind != "other", is.finite(dur), dur >= 0, dur < 90)
