# Scoring v2 with timeouts modelled, and imputing error-out runs (sourced by
# studies/2026-10_hf-v2-redesign/hearts_and_flowers_v2.qmd, section 10). All models are fit to v2 runs
# only (heavyInstructions excluded), single group. Items are the released
# items (block x stimulus x start/stay/switch), split by response window in
# the mixed block; parameters are constrained equal across the occurrences
# of an item, so the only thing that differs between models is how a trial
# outcome is coded:
#   timeout_wrong   2PL, timeout = incorrect (like the released scoring)
#   timeout_missing 2PL, timeout = missing
#   nominal         nominal model, categories wrong / timeout / correct, for
#                   mixed items with enough timeouts; other items (pure
#                   blocks, where timeouts are ~0.2%, and the mixed start
#                   trials) stay dichotomous with timeouts coded wrong
# A trial is "correct" if the side is right and RT >= 200 ms (no 2000 ms
# cutoff: v2's windows already bound RT).
# Writes data/hf_v2/nominal_results.rds.

source(here::here("studies/2026-10_hf-v2-redesign/_hf_v2_common.R"))
set.seed(20261022)

dl <- readRDS(here::here("data/hf_v2/deadline_items.rds"))
v2_runs <- hf_run_struct |> filter(form == "v2", !grepl("heavy", layout))

trials <- dl$typed |>
  semi_join(v2_runs, by = "run_id") |>
  arrange(run_id, trial_index) |>
  mutate(group = if_else(block == "mixed", paste(item_class, window, sep = "_"), item_class),
         outcome = case_when(timeout & block == "mixed" ~ "timeout",
                             correct %in% TRUE & !timeout & rt_numeric >= 200 ~ "correct",
                             TRUE ~ "wrong")) |>
  group_by(run_id) |>
  mutate(half = row_number() %% 2) |>
  group_by(run_id, group) |>
  mutate(k = row_number()) |>
  ungroup() |>
  mutate(item = sprintf("%s_%02d", group, k))

# keep occurrence columns with enough data. A mixed item is modelled as
# nominal if it has enough timeouts; its columns then need every category
# (mirt silently drops categories an item never shows)
item_ok <- trials |>
  group_by(item, group) |>
  summarise(n = n(), n_timeout = sum(outcome == "timeout"),
            n_wrong = sum(outcome == "wrong"), .groups = "drop") |>
  filter(n >= 150, n_wrong >= 5) |>
  group_by(group) |>
  mutate(nominal = grepl("^heartsflowers", group) & sum(n_timeout) >= 30) |>
  ungroup() |>
  filter(!nominal | n_timeout >= 3)
trials <- trials |> semi_join(item_ok, by = "item")
items <- sort(item_ok$item)
groups <- item_ok$group[match(items, item_ok$item)]
nominal_items <- items[item_ok$nominal[match(items, item_ok$item)]]

code_outcome <- list(
  timeout_wrong   = c(wrong = 0, timeout = 0, correct = 1),
  timeout_missing = c(wrong = 0, timeout = NA, correct = 1),
  nominal         = c(wrong = 0, timeout = 1, correct = 2))

wide <- function(tr, coding) {
  m <- tr |>
    mutate(y = unname(coding[outcome])) |>
    select(run_id, item, y) |>
    pivot_wider(names_from = item, values_from = y)
  out <- matrix(NA_real_, nrow(m), length(items), dimnames = list(m$run_id, items))
  have <- intersect(items, names(m))
  out[, have] <- as.matrix(m[, have])
  out
}

constrain_within_groups <- function(sv, par_names) {
  cons <- list()
  for (g in unique(groups)) {
    its <- items[groups == g]
    if (length(its) < 2) next
    for (p in par_names) {
      nums <- sv$parnum[sv$item %in% its & sv$name == p & sv$est]
      if (length(nums) > 1) cons[[length(cons) + 1]] <- nums
    }
  }
  cons
}

# in the nominal model, non-nominal items stay dichotomous: timeout (1)
# becomes wrong (0), correct (2) becomes 1
to_binary <- function(m) {
  bin <- !colnames(m) %in% nominal_items
  x <- m[, bin]
  x[which(x == 1)] <- 0
  x[which(x == 2)] <- 1
  m[, bin] <- x
  m
}

fit_model <- function(model) {
  y <- wide(trials, code_outcome[[model]])
  if (model == "nominal") {
    type <- if_else(items %in% nominal_items, "nominal", "2PL")
    y <- to_binary(y)
  } else {
    type <- rep("2PL", length(items))
  }
  sv <- mirt::mirt(y, 1, itemtype = type, pars = "values", verbose = FALSE)
  cons <- constrain_within_groups(sv, c("a1", "d", "ak1", "d1", "d2"))
  fit <- mirt::mirt(y, 1, itemtype = type, constrain = cons, verbose = FALSE,
                    technical = list(NCYCLES = 3000))
  list(fit = fit, y = y)
}

score_eap <- function(fit, y) {
  fs <- mirt::fscores(fit, method = "EAP", response.pattern = y,
                      full.scores.SE = TRUE)
  tibble(run_id = rownames(y), theta = fs[, "F1"], se = fs[, "SE_F1"])
}

models <- names(code_outcome)
fits <- purrr::map(rlang::set_names(models), fit_model)

# full-data scores, and odd/even-trial half scores with full-data parameters
half_matrix <- function(model, h) {
  m <- wide(trials |> mutate(outcome = if_else(half == h, outcome, NA_character_)),
            code_outcome[[model]])
  if (model == "nominal") to_binary(m) else m
}
scores <- purrr::imap_dfr(fits, \(f, model) {
  full <- score_eap(f$fit, f$y)
  h0 <- score_eap(f$fit, half_matrix(model, 0)) |> select(run_id, theta_h0 = theta)
  h1 <- score_eap(f$fit, half_matrix(model, 1)) |> select(run_id, theta_h1 = theta)
  full |> left_join(h0, by = "run_id") |> left_join(h1, by = "run_id") |>
    mutate(model = model)
})

# --- imputation: unreached trials of error-out runs set to timeout ----------
# planned v2 sequence is 12 hearts, 16 flowers, 3 x 16 mixed. An unreached
# trial is the block's start trial if the block was never reached; otherwise
# a pure-block stay trial, or for the mixed block a stimulus x stay/switch
# combination drawn at its observed frequency
error_out <- hf_runs |> filter(form == "v2", stop_reason %in% "errorOut") |>
  semi_join(v2_runs, by = "run_id") |> pull(run_id)
mixed_mix <- trials |>
  filter(block == "mixed", type != "start") |>
  count(item_class) |>
  mutate(p = n / sum(n))
plan <- tibble(block = c(rep("hearts", 12), rep("flowers", 16), rep("mixed", 48)),
               pos = c(1:12, 1:16, 1:48)) |>
  mutate(window = case_when(block != "mixed" ~ 3000, pos <= 16 ~ 3000,
                            pos <= 32 ~ 2000, TRUE ~ 1500))
reached <- dl$typed |> filter(run_id %in% error_out) |> count(run_id, block, name = "n_done")
imputed <- tidyr::crossing(run_id = error_out, plan) |>
  left_join(reached, by = c("run_id", "block")) |>
  filter(pos > coalesce(n_done, 0L)) |>
  mutate(item_class = case_when(
           pos == 1 & block == "hearts" ~ "hearts_heart_start",
           pos == 1 & block == "flowers" ~ "flowers_flower_start",
           pos == 1 ~ sample(c("heartsflowers_heart_start", "heartsflowers_flower_start"),
                             n(), replace = TRUE),
           block == "hearts" ~ "hearts_heart_stay",
           block == "flowers" ~ "flowers_flower_stay",
           TRUE ~ sample(mixed_mix$item_class, n(), replace = TRUE, prob = mixed_mix$p)),
         group = if_else(block == "mixed", paste(item_class, window, sep = "_"), item_class),
         outcome = if_else(block == "mixed", "timeout", "wrong"))
trials_imp <- bind_rows(trials |> select(run_id, group, outcome, half),
                        imputed |> select(run_id, group, outcome) |>
                          mutate(half = NA_integer_)) |>
  group_by(run_id) |>
  mutate(half = if_else(is.na(half), row_number() %% 2, half)) |>
  group_by(run_id, group) |>
  mutate(k = row_number()) |>
  ungroup() |>
  mutate(item = sprintf("%s_%02d", group, k)) |>
  filter(item %in% items)

imputed_scores <- purrr::imap_dfr(fits, \(f, model) {
  coding <- code_outcome[[model]]
  mk <- function(tr) {
    m <- wide(tr, coding)
    if (model == "nominal") to_binary(m) else m
  }
  full <- score_eap(f$fit, mk(trials_imp))
  h0 <- score_eap(f$fit, mk(trials_imp |> mutate(outcome = if_else(half == 0, outcome, NA_character_)))) |>
    select(run_id, theta_h0 = theta)
  h1 <- score_eap(f$fit, mk(trials_imp |> mutate(outcome = if_else(half == 1, outcome, NA_character_)))) |>
    select(run_id, theta_h1 = theta)
  full |> left_join(h0, by = "run_id") |> left_join(h1, by = "run_id") |>
    mutate(model = paste0(model, " + imputed"))
})

# near-miss check of the imputation assumption: children who reached 6 errors
# in a 10-trial window (one short of the stop rule) but went on
near_miss <- dl$typed |>
  semi_join(v2_runs |> filter(grepl("complete \\(", layout)), by = "run_id") |>
  arrange(run_id, trial_index) |>
  group_by(run_id) |>
  mutate(err = !(correct %in% TRUE) | timeout,
         err10 = as.numeric(stats::filter(err, rep(1, 10), sides = 1)),
         first_hit = suppressWarnings(min(which(err10 == 6))),
         after = row_number() > first_hit) |>
  filter(is.finite(first_hit), after) |>
  summarise(next_trials = n(), acc_after = mean(correct %in% TRUE & !timeout),
            timeout_after = mean(timeout), .groups = "drop")

params <- purrr::map(fits, \(f) mirt::coef(f$fit, simplify = TRUE, IRTpars = FALSE)$items)

saveRDS(list(scores = scores, imputed_scores = imputed_scores,
             items = item_ok, params = params, near_miss = near_miss,
             n_error_out = length(error_out),
             n_imputed_trials = nrow(imputed)),
        here::here("data/hf_v2/nominal_results.rds"))
