# Stories (Theory of Mind): analysis IRT model, reliability, stability, age validity,
# and comparison with the released v2_3 scores.
#
# Inputs (gitignored, data/stories_2026-09/; built by tasks/_build_stories_data.R):
#   stories_trials.rds, stories_runs.rds       corrected analysis data (levantemodels PR #27)
#   raw/release_tom_scores__2026-09-24.rds     released ToM scores (registry v2_3), pulled 2026-09-24
#   raw/levante_metadata_items__v3_18.rds      corpus_items (chance per generic uid, before PR #27)
#   v2_3_tom_2pl_f1_scalar_modelrecord.rds     v2_3 ToM ModelRecord, registry file
#                                              tom/multigroup_dataset/all_items/tom_2pl_f1_scalar.rds
#                                              (optional; only its group means/variances are used)
# Outputs (gitignored):
#   data/stories_2026-09/results_reliability.rds      every table / plot-ready frame the chapter reads
#   data/stories_2026-09/stories_scores_analysis.rds  one row per included run: EAP + SE per model, etc.
#
# Analysis model: one multigroup IRT model with group = dataset (group by group, as proposed in PR #26),
# scalar invariance (shared item parameters; free group means and variances; pilot_mpieva_de_main is
# the N(0, 1) reference, as in v2_3), guessing fixed at each item's chance, the production priors
# (d ~ N(0, 3), a1 ~ N(1, 0.3)), policy-excluded items removed. Rasch vs 2PL chosen by BIC.
# Differences from the production fit (levantemodels::fit_task_models_multigroup):
#   * concurrent calibration of all items in one model (production fits the items shared by every
#     group first and then fixes them; with 8 groups on different forms almost no item is shared by all);
#   * a quadrature grid of theta in [-12, 8] (201 points) for calibration and scoring. mirt's default
#     [-6, 6] (61 points), which production uses, truncates the low tail of the lowest-scoring groups:
#     their means and variances are biased toward the reference. A fit on [-16, 8] checks that [-12, 8]
#     is wide enough (results$grid_convergence); a default-grid fit is kept as a sensitivity check.
# Datasets with < 25 included runs get no group of their own and are scored under the reference
# group's prior (the levantemodels PR #26 rule).
# Decomposition fits (same model, chosen itemtype, same grid):
#   release-equivalent : the trials in the releases, pre-PR #27 uids and chance, all items
#   identity fixes only: corrected data (PR #27 uids, chance, restored trials), all items kept
#   analysis           : corrected data, policy-excluded items removed
# Sensitivity checks (2026-09 revision): the guessing rule of the dimensionality analysis (floor 0
# for items at/below chance in a group); a nonparametric (empirical-histogram) latent density per
# group with items fixed; and a block-CAT simulation within children (fixed-form runs rescored on
# fCAT-style random and adaptive story draws).
#
# Run from a working directory OUTSIDE the repo (the repo's renv has an mgcv binary that breaks mirt):
#   cd <scratch dir> && Rscript <book>/tasks/_stories_fits_reliability.R [<book root>]
# Book root: first argument, else env STORIES_BOOK_ROOT, else the directory above this script's
# tasks/. Env LEVANTEMODELS_PATH (required): see below. About 12 minutes.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(tibble)
  library(mirt)
})
options(warn = 1, dplyr.summarise.inform = FALSE, width = 200)
set.seed(20260924)

# levantemodels (dedupe_items(), to_mirt_shape*(), generate_model_str_numeric(),
# remove_no_var_items(), ModelRecord) from the same source tree the data build used (the
# PR #27 branch; env LEVANTEMODELS_PATH), loaded with pkgload and recorded by commit. These
# helpers are identical to the installed levantemodels 0.1.0 (checked 2026-09-24).
lm_path <- path.expand(Sys.getenv("LEVANTEMODELS_PATH"))
if (!nzchar(lm_path))
  stop("Set LEVANTEMODELS_PATH to a levantemodels source tree on the PR #27 branch ",
       "(fix-tom-identity-corrections), e.g. a git worktree of that branch.")
lm_git <- \(...) suppressWarnings(system2("git", c("-C", shQuote(lm_path), ...), stdout = TRUE, stderr = TRUE))
lm_commit <- lm_git("rev-parse", "HEAD")
lm_dirty <- length(lm_git("status", "--porcelain")) > 0
suppressMessages(pkgload::load_all(lm_path, quiet = TRUE))
message("levantemodels: ", lm_path, " @ ", lm_commit, if (lm_dirty) " (DIRTY)")

args <- commandArgs(trailingOnly = TRUE)
script_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
default_root <- if (length(script_file) == 1) dirname(dirname(normalizePath(script_file))) else here::here()
book_root <- normalizePath(
  if (length(args) >= 1) args[1] else
    Sys.getenv("STORIES_BOOK_ROOT", default_root),
  mustWork = TRUE)
data_dir <- file.path(book_root, "data", "stories_2026-09")
f_trials   <- file.path(data_dir, "stories_trials.rds")
f_runs     <- file.path(data_dir, "stories_runs.rds")
f_released <- file.path(data_dir, "raw", "release_tom_scores__2026-09-24.rds")
f_meta     <- file.path(data_dir, "raw", "levante_metadata_items__v3_18.rds")
f_v23      <- file.path(data_dir, "v2_3_tom_2pl_f1_scalar_modelrecord.rds")
f_out      <- file.path(data_dir, "results_reliability.rds")
f_scores   <- file.path(data_dir, "stories_scores_analysis.rds")

ref_group <- "pilot_mpieva_de_main"
min_group_runs <- 25                                          # levantemodels PR #26
priors <- list(d = c("norm", 0, 3), a1 = c("norm", 1, 0.3))   # levante-pilots 01_fit_irt_modular.qmd
scalar <- c("free_means", "free_var", "slopes", "intercepts")  # levantemodels::invariances$scalar
grid_wide <- list(theta_lim = c(-12, 8), quadpts = 201)
grid_default <- list(theta_lim = c(-6, 6), quadpts = 61)       # mirt's default for one factor
grid_check <- list(theta_lim = c(-16, 8), quadpts = 241)       # convergence check for grid_wide
gap_breaks <- c(0, 60, 180, 300, Inf)                         # days; as in the 2026-09 audit
n_boot <- 2000

# ---- data ----------------------------------------------------------------------------------------

tr <- readRDS(f_trials)
runs <- readRDS(f_runs)
released <- readRDS(f_released) |>
  filter(is.na(exclusion), !is.na(score)) |>
  select(run_id, score_released = score, se_released = score_se)
chance_pre <- readRDS(f_meta)$corpus_items |>
  filter(item_task == "tom") |>
  distinct(generic_uid_pre = item_uid, chance_pre = chance)
stopifnot(!anyDuplicated(chance_pre$generic_uid_pre))

run_info <- runs |>
  filter(included) |>
  transmute(run_id, dataset, site, language, form, user_id, age, time_started,
            run_minutes = as.numeric(difftime(time_finished, time_started, units = "mins")))

# analysis data: corrected, policy-excluded items removed
d_corr <- tr |>
  filter(!policy_exclude) |>
  transmute(run_id, group = dataset, item_uid = story_uid, correct, chance)
# corrected, all items
d_corr_all <- tr |>
  transmute(run_id, group = dataset, item_uid = story_uid, correct, chance)
# release-equivalent: the trials in the releases, with their pre-PR #27 uid and chance, all items
d_rel <- tr |>
  filter(in_release) |>
  mutate(generic_uid_pre = str_replace(story_uid_pre_pr27, "_story[0-9]+_", "_")) |>
  left_join(chance_pre, by = "generic_uid_pre") |>
  transmute(run_id, group = dataset, item_uid = story_uid_pre_pr27, correct, chance = chance_pre)
stopifnot(!anyNA(d_rel$item_uid), !anyNA(d_rel$chance))

runs_per_dataset <- d_corr |> distinct(run_id, group) |> count(group, name = "n_runs")
cal_groups <- runs_per_dataset |> filter(n_runs >= min_group_runs) |> pull(group)
small_groups <- setdiff(runs_per_dataset$group, cal_groups)
stopifnot(ref_group %in% cal_groups)
message("calibration groups: ", paste(cal_groups, collapse = ", "))
message("scored under the reference prior (< ", min_group_runs, " runs): ", paste(small_groups, collapse = ", "))

# ---- model helpers -------------------------------------------------------------------------------

# wide 0/1 matrix, rows named by run_id, columns = item instances (production naming, "<uid>-<k>")
wide_mat <- function(d) {
  w <- d |> dedupe_items() |> to_mirt_shape()
  m <- as.matrix(w)
  rownames(m) <- rownames(w)
  m
}
align_cols <- function(m, cols) {
  miss <- setdiff(cols, colnames(m))
  m <- cbind(m[, intersect(colnames(m), cols), drop = FALSE],
             matrix(NA_real_, nrow(m), length(miss), dimnames = list(NULL, miss)))
  m[, cols, drop = FALSE]
}

fit_mg <- function(d, itemtype, label, grid = grid_wide) {
  d <- d |> filter(group %in% cal_groups) |> dedupe_items() |> remove_no_var_items()
  w <- to_mirt_shape_grouped(d)
  run_id <- rownames(w)
  grp <- factor(w$group, levels = c(ref_group, sort(setdiff(unique(w$group), ref_group))))
  mat <- as.matrix(w[, setdiff(names(w), "group")])
  rownames(mat) <- run_id
  guess <- d |> distinct(item_inst = as.character(item_inst), chance)
  stopifnot(!anyDuplicated(guess$item_inst))
  guess <- setNames(guess$chance, guess$item_inst)[colnames(mat)]
  ms <- generate_model_str_numeric(d, mat, itemtype, 1, priors)
  message("fitting ", label, " (", itemtype, "): ", nrow(mat), " runs x ", ncol(mat), " items, grid [",
          paste(grid$theta_lim, collapse = ", "), "]")
  t0 <- Sys.time()
  mod <- multipleGroup(mat, mirt.model(ms), group = grp, itemtype = itemtype, guess = unname(guess),
                       invariance = scalar, quadpts = grid$quadpts, verbose = FALSE,
                       technical = list(NCYCLES = 5000, theta_lim = grid$theta_lim))
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  list(mod = mod, label = label, itemtype = itemtype, grid = grid, run_id = run_id,
       group = as.character(grp), mat = mat, secs = secs)
}

fit_stats <- function(f) tibble(
  model = f$label, itemtype = f$itemtype, grid = paste0("[", paste(f$grid$theta_lim, collapse = ", "), "]"),
  runs = nrow(f$mat), items = ncol(f$mat), groups = n_distinct(f$group),
  n_par = extract.mirt(f$mod, "nest"), logLik = extract.mirt(f$mod, "logLik"),
  logPrior = extract.mirt(f$mod, "logPrior"), AIC = extract.mirt(f$mod, "AIC"),
  BIC = extract.mirt(f$mod, "BIC"), converged = extract.mirt(f$mod, "converged"),
  iterations = extract.mirt(f$mod, "iterations"), seconds = f$secs)

# EAP + SE (posterior SD) under each run's own calibration group
score_own <- function(f) {
  fs <- fscores(f$mod, method = "EAP", full.scores.SE = TRUE,
                theta_lim = f$grid$theta_lim, quadpts = f$grid$quadpts)
  tibble(run_id = f$run_id, prior_group = f$group, eap = fs[, 1], se = fs[, 2])
}
# EAP + SE of a response matrix under one group's prior (columns aligned BY NAME to the model's order;
# fscores(response.pattern =) matches by position)
score_under <- function(f, m, group) {
  m <- align_cols(m, colnames(f$mat))
  fs <- fscores(extract.group(f$mod, group = group), method = "EAP", response.pattern = m,
                append_response.pattern = FALSE, theta_lim = f$grid$theta_lim, quadpts = f$grid$quadpts)
  tibble(run_id = rownames(m), prior_group = group, eap = fs[, "F1"], se = fs[, "SE_F1"])
}
# all runs: calibration groups under their own prior, small datasets under the reference prior
score_all <- function(f, d) {
  small <- d |> filter(group %in% small_groups)
  n_drop <- sum(!as.character(dedupe_items(small)$item_inst) %in% colnames(f$mat))
  if (n_drop > 0) message(f$label, ": ", n_drop, " small-dataset responses on items not in the model are dropped")
  bind_rows(score_own(f), if (nrow(small) > 0) score_under(f, wide_mat(small), ref_group))
}
group_pars <- function(f) {
  imap_dfr(coef(f$mod, simplify = TRUE), \(x, g) tibble(group = g, mean = x$means[1], var = x$cov[1, 1]))
}

# ---- fits ----------------------------------------------------------------------------------------

fit_c_rasch <- fit_mg(d_corr, "Rasch", "analysis")
fit_c_2pl   <- fit_mg(d_corr, "2PL", "analysis")
bic_c <- bind_rows(fit_stats(fit_c_rasch), fit_stats(fit_c_2pl))
chosen <- bic_c$itemtype[which.min(bic_c$BIC)]
fit_c   <- if (chosen == "2PL") fit_c_2pl else fit_c_rasch
fit_alt <- if (chosen == "2PL") fit_c_rasch else fit_c_2pl
message("BIC prefers ", chosen, " (dBIC = ", round(diff(range(bic_c$BIC)), 1), ")")
fit_all <- fit_mg(d_corr_all, chosen, "identity fixes only (all items)")
fit_b   <- fit_mg(d_rel, chosen, "release-equivalent")
fit_def <- fit_mg(d_corr, chosen, "analysis, default grid", grid = grid_default)
fit_chk <- fit_mg(d_corr, chosen, "analysis, wider grid (check)", grid = grid_check)
# guessing-rule sensitivity (the dimensionality analysis's rule): floor 0 for items at or
# below chance in any calibration group with >= 50 responses, chance otherwise
g0_items <- d_corr |>
  filter(group %in% cal_groups) |>
  group_by(group, item_uid) |>
  summarise(n = n(), acc = mean(correct), chance = first(chance), .groups = "drop") |>
  filter(n >= 50, acc <= chance) |>
  distinct(item_uid) |> pull(item_uid) |> sort()
d_g0 <- d_corr |> mutate(chance = if_else(item_uid %in% g0_items, 0, chance))
fit_g0 <- fit_mg(d_g0, chosen, "analysis, floor 0 for items at/below chance in a group")
fits <- list(fit_c_rasch, fit_c_2pl, fit_all, fit_b, fit_def, fit_chk, fit_g0)
itemtype_bic <- map_dfr(fits, fit_stats) |>
  group_by(model, grid) |>
  mutate(dBIC = BIC - min(BIC), dAIC = AIC - min(AIC)) |>
  ungroup()

# ---- scores --------------------------------------------------------------------------------------

all_mat_c <- wide_mat(d_corr)
sc <- list(
  c   = score_all(fit_c, d_corr),
  alt = score_all(fit_alt, d_corr),
  all = score_all(fit_all, d_corr_all),
  b   = score_all(fit_b, d_rel),
  def = score_all(fit_def, d_corr),
  g0  = score_all(fit_g0, d_g0),
  ref = score_under(fit_c, all_mat_c, ref_group))  # every run under the reference prior

# Spanish false-belief sensitivity: es-* runs rescored without reality_known false_belief (1/7/13)
fb_uids <- tr |> filter(spanish_fb_flag) |> distinct(story_uid) |> pull(story_uid)
es_runs <- run_info |> filter(str_starts(language, "es")) |> pull(run_id)
sc_nofb <- sc$c |>
  filter(run_id %in% es_runs) |>
  group_by(prior_group) |>
  group_modify(\(x, key) {
    m <- all_mat_c[x$run_id, , drop = FALSE]
    m[, colnames(m) %in% paste0(fb_uids, "-1")] <- NA
    score_under(fit_c, m, key$prior_group) |> select(-prior_group)
  }) |>
  ungroup() |>
  select(run_id, eap_nofb = eap, se_nofb = se)

run_items <- d_corr |>
  group_by(run_id) |>
  summarise(n_items = n(), n_correct = sum(correct), prop_correct = mean(correct),
            guess_expected = sum(chance))

pick <- \(x, suffix) x |> select(run_id, eap, se) |> rename_with(\(v) paste0(v, suffix), c(eap, se))
scores <- run_info |>
  inner_join(run_items, by = "run_id") |>
  left_join(sc$c, by = "run_id") |>
  left_join(pick(sc$alt, "_alt"), by = "run_id") |>
  left_join(pick(sc$all, "_allitems"), by = "run_id") |>
  left_join(pick(sc$b, "_uncorrected"), by = "run_id") |>
  left_join(pick(sc$def, "_defaultgrid"), by = "run_id") |>
  left_join(pick(sc$g0, "_g0"), by = "run_id") |>
  left_join(pick(sc$ref, "_refprior"), by = "run_id") |>
  left_join(released, by = "run_id") |>
  left_join(sc_nofb, by = "run_id") |>
  mutate(form_type = if_else(form == "fcat_random3", "fCAT (3 random stories)", "fixed form"),
         calibration_group = dataset %in% cal_groups,
         at_or_below_guessing = n_correct <= guess_expected,
         near_ceiling = prop_correct >= .9)
stopifnot(nrow(scores) == sum(runs$included), !anyNA(scores$eap), !anyDuplicated(scores$run_id))
attr(scores, "provenance") <- list(
  script = "tasks/_stories_fits_reliability.R", built_at = Sys.time(),
  model = paste0("multigroup (group = dataset) scalar ", chosen,
                 ", guessing = chance, policy_exclude removed, grid [-12, 8]"),
  columns = c(eap = "analysis model EAP (own dataset prior; datasets < 25 runs: reference prior)",
              eap_alt = "analysis model with the other itemtype",
              eap_allitems = "corrected data, all items kept (identity fixes only)",
              eap_uncorrected = "same model refit on release-equivalent data (pre-PR #27, all items)",
              eap_defaultgrid = "analysis model fit and scored on mirt's default grid [-6, 6]",
              eap_g0 = "analysis model with floor 0 for items at/below chance in a calibration group",
              eap_refprior = "analysis model, every run under the reference (DE) prior",
              score_released = "released v2_3 score",
              eap_nofb = "es-* runs without reality_known false_belief (stories 1/7/13)"))
saveRDS(scores, f_scores)

# ---- helpers for the tables ----------------------------------------------------------------------

safe_cor <- \(x, y, min_n = 10) {
  ok <- !is.na(x) & !is.na(y)
  if (sum(ok) >= min_n) cor(x[ok], y[ok]) else NA_real_
}
fisher_ci <- \(r, n, side) ifelse(is.na(r) | n < 10, NA_real_, tanh(atanh(r) + side * qnorm(.975) / sqrt(pmax(n, 4) - 3)))
emp_rel <- \(eap, se) {
  ok <- !is.na(eap) & !is.na(se)
  var(eap[ok]) / (var(eap[ok]) + mean(se[ok]^2))
}

# ---- group and item parameters -------------------------------------------------------------------

gp <- runs_per_dataset |>
  rename(dataset = group) |>
  left_join(group_pars(fit_c) |> rename(mean_analysis = mean, var_analysis = var), by = c("dataset" = "group")) |>
  left_join(group_pars(fit_alt) |> rename(mean_alt_itemtype = mean, var_alt_itemtype = var), by = c("dataset" = "group")) |>
  left_join(group_pars(fit_all) |> rename(mean_allitems = mean, var_allitems = var), by = c("dataset" = "group")) |>
  left_join(group_pars(fit_b) |> rename(mean_uncorrected = mean, var_uncorrected = var), by = c("dataset" = "group")) |>
  left_join(group_pars(fit_def) |> rename(mean_defaultgrid = mean, var_defaultgrid = var), by = c("dataset" = "group")) |>
  left_join(group_pars(fit_g0) |> rename(mean_g0 = mean, var_g0 = var), by = c("dataset" = "group"))
if (file.exists(f_v23)) {
  v23 <- readRDS(f_v23)@model_vals |>
    filter(item == "GROUP") |>
    select(dataset = group, name, value) |>
    pivot_wider(names_from = name, values_from = value) |>
    select(dataset, mean_v2_3 = MEAN_1, var_v2_3 = COV_11)
  gp <- gp |> left_join(v23, by = "dataset")
}
sigma2 <- setNames(gp$var_analysis, gp$dataset)
sigma2[is.na(sigma2)] <- sigma2[[ref_group]]   # small datasets are scored under the reference prior

cf_ref <- coef(fit_c$mod, simplify = TRUE)[[ref_group]]$items
item_meta <- tr |> distinct(story_uid, story, old_group, question_type, is_control, new_construct)
item_pars <- tibble(item = rownames(cf_ref), story_uid = str_remove(rownames(cf_ref), "-[0-9]+$"),
                    a = cf_ref[, "a1"], d = cf_ref[, "d"], g = cf_ref[, "g"]) |>
  mutate(b = -d / a) |>
  left_join(item_meta, by = "story_uid") |>
  left_join(d_corr |>
              group_by(story_uid = item_uid) |>
              summarise(n_responses = n(), n_datasets = n_distinct(group), p_correct = mean(correct)),
            by = "story_uid")
item_slopes_by_type <- item_pars |>
  group_by(question_type, is_control) |>
  summarise(n_items = n(), median_a = median(a), min_a = min(a), max_a = max(a),
            median_b = median(b), .groups = "drop")

# ---- (a) reliability ----------------------------------------------------------------------------

rel_summ <- function(x) x |>
  summarise(n_runs = n(), median_items = median(n_items), median_minutes = median(run_minutes),
            mean_prop_correct = mean(prop_correct), prop_at_or_below_guessing = mean(at_or_below_guessing),
            prop_near_ceiling = mean(near_ceiling), mean_eap = mean(eap), sd_eap = sd(eap),
            rms_se = sqrt(mean(se^2)), rel_empirical = emp_rel(eap, se),
            rel_group = 1 - mean(se^2) / sigma2[[first(dataset)]],
            info_per_minute = median((1 / se^2 - 1 / sigma2[[first(dataset)]]) / run_minutes),
            rms_se_released = sqrt(mean(se_released^2, na.rm = TRUE)),
            rel_empirical_released = emp_rel(score_released, se_released),
            rel_empirical_defaultgrid = emp_rel(eap_defaultgrid, se_defaultgrid),
            calibration_group = first(calibration_group), .groups = "drop")
rel_by_form <- bind_rows(
  scores |> group_by(dataset, form, form_type) |> rel_summ(),
  scores |> group_by(dataset, form_type) |> rel_summ() |>
    mutate(form = if_else(form_type == "fixed form", "all fixed forms", "fCAT")) |>
    group_by(dataset) |> filter(n() == 2) |> ungroup(),
  scores |> group_by(dataset) |> rel_summ() |> mutate(form = "all forms", form_type = "all"))

fcat_vs_fixed <- rel_by_form |>
  filter(calibration_group, form %in% c("all fixed forms", "fCAT")) |>
  left_join(scores |> group_by(dataset, form_type) |> summarise(median_age = median(age, na.rm = TRUE)),
            by = c("dataset", "form_type")) |>
  group_by(dataset) |>
  mutate(se_ratio_fcat_to_fixed = rms_se[form == "fCAT"] / rms_se[form == "all fixed forms"],
         info_per_minute_ratio = info_per_minute[form == "fCAT"] / info_per_minute[form == "all fixed forms"]) |>
  ungroup() |>
  select(dataset, form_type, n_runs, median_age, median_items, median_minutes, mean_prop_correct,
         prop_near_ceiling, mean_eap, rms_se, rel_empirical, rel_group, info_per_minute,
         se_ratio_fcat_to_fixed, info_per_minute_ratio)

se_by_theta <- scores |>
  filter(calibration_group) |>
  mutate(theta_bin = cut(eap, seq(-12, 8, by = 0.5))) |>
  group_by(dataset, form_type, theta_bin) |>
  summarise(n_runs = n(), theta_mid = mean(eap), mean_se = mean(se), .groups = "drop")

# ---- (b) alternate-form stability ---------------------------------------------------------------

versions <- tribble(
  ~version,                         ~s,                 ~e,
  "analysis",                       "eap",              "se",
  "identity fixes only",            "eap_allitems",     "se_allitems",
  "release-equivalent refit",       "eap_uncorrected",  "se_uncorrected",
  "released v2_3",                  "score_released",   "se_released")

# consecutive included runs of the same child
pairs <- scores |>
  arrange(dataset, user_id, time_started) |>
  group_by(dataset, user_id) |>
  mutate(k = row_number(), run_id_1 = lag(run_id), form_1 = lag(form), age_1 = lag(age),
         t_1 = lag(time_started)) |>
  ungroup() |>
  filter(k > 1) |>
  transmute(dataset, user_id, k, run_id_1, run_id_2 = run_id, form_1, form_2 = form, age_1, age_2 = age,
            gap_days = as.numeric(difftime(time_started, t_1, units = "days")),
            gap_bin = cut(gap_days, gap_breaks, right = FALSE, dig.lab = 4))
sv <- scores |> select(run_id, all_of(c(versions$s, versions$e)))
pairs <- pairs |>
  left_join(sv |> rename_with(\(v) paste0(v, "_1"), -run_id), by = c("run_id_1" = "run_id")) |>
  left_join(sv |> rename_with(\(v) paste0(v, "_2"), -run_id), by = c("run_id_2" = "run_id")) |>
  mutate(all_versions = if_all(all_of(c(paste0(versions$s, "_1"), paste0(versions$s, "_2"))), \(v) !is.na(v)))

stab_table <- function(p) {
  p <- p |> filter(all_versions)
  n <- nrow(p)
  r_of <- \(i) setNames(map_dbl(versions$s, \(s) cor(p[[paste0(s, "_1")]][i], p[[paste0(s, "_2")]][i])),
                        versions$version)
  est <- r_of(seq_len(n))
  bs <- replicate(n_boot, r_of(sample(n, replace = TRUE)))
  bs_diff <- sweep(bs, 2, bs["released v2_3", ])
  q <- \(m, pr) apply(m, 1, quantile, pr)
  pmap_dfr(versions, \(version, s, e) {
    s1 <- p[[paste0(s, "_1")]]; s2 <- p[[paste0(s, "_2")]]
    ok <- !is.na(p$age_1)
    tibble(version = version, n_pairs = n, n_children = n_distinct(p$user_id),
           median_gap_days = median(p$gap_days), r = est[[version]],
           r_lo = q(bs, .025)[[version]], r_hi = q(bs, .975)[[version]],
           r_partial_age = cor(resid(lm(s1[ok] ~ p$age_1[ok])), resid(lm(s2[ok] ~ p$age_1[ok]))),
           r_disattenuated = est[[version]] / sqrt(emp_rel(s1, p[[paste0(e, "_1")]]) *
                                                     emp_rel(s2, p[[paste0(e, "_2")]])),
           diff_vs_released = est[[version]] - est[["released v2_3"]],
           diff_lo = q(bs_diff, .025)[[version]], diff_hi = q(bs_diff, .975)[[version]])
  })
}
stability_bins <- pairs |>
  filter(all_versions) |>
  group_by(dataset, gap_bin) |>
  filter(n() >= 10) |>
  group_modify(\(p, key) stab_table(p)) |>
  ungroup()
stability_first2 <- pairs |>
  filter(all_versions, k == 2) |>
  group_by(dataset) |>
  filter(n() >= 10) |>
  group_modify(\(p, key) stab_table(p)) |>
  ungroup()
stability_forms <- pairs |>
  filter(all_versions) |>
  group_by(dataset, gap_bin, form_1, form_2) |>
  summarise(n_pairs = n(), median_gap_days = median(gap_days),
            r = safe_cor(eap_1, eap_2), r_released = safe_cor(score_released_1, score_released_2),
            .groups = "drop")
pairs_plot <- pairs |>
  select(dataset, user_id, k, gap_days, gap_bin, form_1, form_2, age_1, age_2,
         eap_1, eap_2, score_released_1, score_released_2, all_versions)

# ---- (c) age validity ---------------------------------------------------------------------------

age_summ <- function(x) x |>
  summarise(n_runs = n(), n_with_age = sum(!is.na(age)),
            age_mean = mean(age, na.rm = TRUE), age_sd = sd(age, na.rm = TRUE),
            age_min = min(age, na.rm = TRUE), age_max = max(age, na.rm = TRUE),
            r_age = safe_cor(eap, age), r_age_released = safe_cor(score_released, age),
            r_age_uncorrected = safe_cor(eap_uncorrected, age),
            r_age_allitems = safe_cor(eap_allitems, age),
            r_age_prop_correct = safe_cor(prop_correct, age),
            slope_per_year = if (sum(!is.na(age)) >= 10) coef(lm(eap ~ age))[["age"]] else NA_real_,
            .groups = "drop") |>
  mutate(r_age_lo = fisher_ci(r_age, n_with_age, -1), r_age_hi = fisher_ci(r_age, n_with_age, 1),
         .after = r_age)
age_validity <- bind_rows(
  scores |> group_by(dataset, form, form_type) |> age_summ(),
  scores |> group_by(dataset, form_type) |> age_summ() |>
    mutate(form = if_else(form_type == "fixed form", "all fixed forms", "fCAT")) |>
    group_by(dataset) |> filter(n() == 2) |> ungroup(),
  scores |> group_by(dataset) |> age_summ() |> mutate(form = "all forms", form_type = "all"))

# ---- (d) comparison with the released v2_3 scores ------------------------------------------------

cmp_summ <- function(x) x |>
  summarise(n_runs = n(),
            mean_released = mean(score_released), mean_analysis = mean(eap),
            sd_released = sd(score_released), sd_analysis = sd(eap),
            r_analysis_released = safe_cor(eap, score_released),
            shift_analysis_minus_released = mean(eap - score_released),
            # the steps from released to analysis:
            shift_refit_minus_released = mean(eap_uncorrected - score_released),   # recalibration, groups, grid
            shift_idfix_minus_refit = mean(eap_allitems - eap_uncorrected),          # PR #27 identity/chance/restored
            shift_analysis_minus_idfix = mean(eap - eap_allitems),                   # policy exclusions
            r_refit_released = safe_cor(eap_uncorrected, score_released),
            r_idfix_refit = safe_cor(eap_allitems, eap_uncorrected),
            r_analysis_idfix = safe_cor(eap, eap_allitems),
            # prior and grid, holding the analysis model's items fixed:
            shift_own_minus_ref_prior = mean(eap - eap_refprior),
            shift_wide_minus_default_grid = mean(eap - eap_defaultgrid),
            r_analysis_alt_itemtype = safe_cor(eap, eap_alt),
            .groups = "drop")
cmp_base <- scores |> filter(!is.na(score_released), !is.na(eap_uncorrected))
released_comparison <- bind_rows(
  cmp_base |> group_by(dataset) |> cmp_summ(),
  cmp_base |> cmp_summ() |> mutate(dataset = "all (pooled)"))
released_comparison_form <- cmp_base |> group_by(dataset, form) |> cmp_summ()
# which datasets had their own group (prior) in v2_3; the others were scored under the first
# group's prior (2026-09 audit, finding E1)
released_models <- readRDS(f_released) |>
  filter(is.na(exclusion), !is.na(score)) |>
  count(dataset, registry_version, scoring_model, name = "n_runs") |>
  mutate(own_group_in_v2_3 = if (exists("v23")) dataset %in% v23$dataset else NA)

# does the analysis grid [-12, 8] integrate the groups fully? compare with [-16, 8]
pars_of <- \(f) mod2values(f$mod) |> filter(est) |> select(group, item, name, value)
grid_convergence <- pars_of(fit_c) |>
  inner_join(pars_of(fit_chk), by = c("group", "item", "name"), suffix = c("_wide", "_check")) |>
  mutate(kind = if_else(item == "GROUP", "group mean / variance", "item parameter")) |>
  group_by(kind) |>
  summarise(n_par = n(), max_abs_diff = max(abs(value_wide - value_check)), .groups = "drop") |>
  bind_rows(tibble(kind = "EAP (all calibration runs)", n_par = nrow(fit_c$mat),
                   max_abs_diff = max(abs(score_own(fit_c)$eap - score_own(fit_chk)$eap))))

grid_sensitivity <- scores |>
  filter(calibration_group) |>
  group_by(dataset) |>
  summarise(n_runs = n(), r_wide_default = cor(eap, eap_defaultgrid),
            mean_shift_wide_minus_default = mean(eap - eap_defaultgrid),
            max_abs_shift = max(abs(eap - eap_defaultgrid)),
            n_shift_gt_0.25 = sum(abs(eap - eap_defaultgrid) > .25),
            min_eap_wide = min(eap), min_eap_default = min(eap_defaultgrid),
            prop_at_or_below_guessing = mean(at_or_below_guessing),
            rel_empirical_wide = emp_rel(eap, se), rel_empirical_default = emp_rel(eap_defaultgrid, se_defaultgrid),
            r_age_wide = safe_cor(eap, age), r_age_default = safe_cor(eap_defaultgrid, age), .groups = "drop") |>
  left_join(gp |> select(dataset, mean_analysis, var_analysis, mean_defaultgrid, var_defaultgrid), by = "dataset")

# guessing-rule sensitivity: the same model with floor 0 for the items at/below chance in a group
g0_sensitivity <- scores |>
  filter(calibration_group) |>
  group_by(dataset) |>
  summarise(n_runs = n(), r_analysis_g0 = cor(eap, eap_g0), mean_shift_g0_minus_analysis = mean(eap_g0 - eap),
            rel_empirical_analysis = emp_rel(eap, se), rel_empirical_g0 = emp_rel(eap_g0, se_g0),
            r_age_analysis = safe_cor(eap, age), r_age_g0 = safe_cor(eap_g0, age), .groups = "drop") |>
  left_join(gp |> select(dataset, mean_analysis, var_analysis, mean_g0, var_g0), by = "dataset")

# ---- trace lines on the analysis grid (for the density check and the block-CAT simulation) -------

theta_q <- seq(grid_wide$theta_lim[1], grid_wide$theta_lim[2], length.out = grid_wide$quadpts)
mod_ref <- extract.group(fit_c$mod, group = ref_group)       # scalar model: items shared by all groups
P_q <- sapply(seq_len(ncol(fit_c$mat)), \(j) probtrace(extract.item(mod_ref, j), matrix(theta_q))[, 2])
colnames(P_q) <- colnames(fit_c$mat)
item_info_q <- sapply(seq_len(ncol(fit_c$mat)), \(j) iteminfo(extract.item(mod_ref, j), matrix(theta_q)))
colnames(item_info_q) <- colnames(fit_c$mat)
# log-likelihood of each run's responses (rows of m, columns named like the model) on the grid
loglik_q <- function(m) {
  m <- align_cols(m, colnames(fit_c$mat))
  x1 <- (m == 1) & !is.na(m); x0 <- (m == 0) & !is.na(m)
  x1 %*% t(log(P_q)) + x0 %*% t(log(1 - P_q))
}
post_summ <- function(ll, w) {      # EAP and posterior SD under grid weights w
  p <- exp(ll - apply(ll, 1, max)) * rep(w, each = nrow(ll))
  p <- p / rowSums(p)
  eap <- drop(p %*% theta_q)
  list(eap = eap, se = sqrt(pmax(drop(p %*% theta_q^2) - eap^2, 0)), post = p)
}
prior_w <- \(g) { w <- dnorm(theta_q, gp$mean_analysis[gp$dataset == g], sqrt(gp$var_analysis[gp$dataset == g])); w / sum(w) }

# ---- latent-density check: a nonparametric (empirical-histogram) group density -------------------
# Item parameters fixed at the analysis calibration; each calibration group's latent density is
# estimated on the grid by EM over the grid weights (Woods' empirical histogram / NPMLE, started
# from the group's normal density). mirt's multipleGroup() does not estimate these densities
# with free means and variances (Davidian: not supported; empiricalhist: did not converge in a
# test), hence the fixed-item version. If the lower tail of a group were identified by the data,
# the histogram would recover a mean and variance close to the normal model's.
m_cal <- fit_c$mat
ll_cal <- loglik_q(m_cal)
density_check <- map_dfr(cal_groups, \(g) {
  rows <- fit_c$group == g
  ll <- ll_cal[rows, , drop = FALSE]
  w0 <- prior_w(g); w <- w0
  L <- exp(ll - apply(ll, 1, max))
  for (it in seq_len(20000)) {
    p <- L * rep(w, each = nrow(L)); p <- p / rowSums(p)
    w_new <- colMeans(p)
    change <- max(abs(w_new - w)); w <- w_new
    if (change < 1e-9) break
  }
  nrm <- post_summ(ll, w0); eh <- post_summ(ll, w)
  ids <- fit_c$run_id[rows]
  ages <- run_info$age[match(ids, run_info$run_id)]
  mu_eh <- sum(w * theta_q); var_eh <- sum(w * theta_q^2) - mu_eh^2
  tibble(dataset = g, n_runs = sum(rows), em_iterations = it, em_last_change = change,
         mean_normal = sum(w0 * theta_q), var_normal = sum(w0 * theta_q^2) - sum(w0 * theta_q)^2,
         mean_eh = mu_eh, var_eh = var_eh,
         mass_below_m4_normal = sum(w0[theta_q < -4]), mass_below_m4_eh = sum(w[theta_q < -4]),
         post_mass_below_m4 = mean(rowSums(nrm$post[, theta_q < -4, drop = FALSE])),
         max_abs_diff_vs_mirt_eap = max(abs(nrm$eap - scores$eap[match(ids, scores$run_id)])),
         r_eap_normal_eh = cor(nrm$eap, eh$eap), mean_eap_shift_eh = mean(eh$eap - nrm$eap),
         rel_empirical_normal = emp_rel(nrm$eap, nrm$se), rel_empirical_eh = emp_rel(eh$eap, eh$se),
         r_age_normal = safe_cor(nrm$eap, ages), r_age_eh = safe_cor(eh$eap, ages))
})

# ---- block-CAT simulation within children (fixed-form runs) -----------------------------------------
# The deployed fCAT shows one random story from each of three story blocks, always in the same
# block order. Here every fixed-form run of a calibration group is rescored, with the
# analysis model and its own group prior, on (i) the whole form, (ii) a random fCAT-style draw
# (one of the form's stories per block; the expectation over all draws), and (iii) an adaptive
# draw (in block order, the story whose items the child answered carry the most Fisher
# information at the current EAP; EAP updated after each story). Minutes are the child's own
# time on the chosen stories (sum of the gaps between consecutive trial timestamps; the first
# gap runs from time_started). Same children, same responses: only the story selection differs.
fcat_pos <- tr |>
  filter(form == "fcat_random3") |>
  arrange(run_id, trial_number) |>
  group_by(run_id) |> mutate(pos = match(story, unique(story))) |> ungroup() |>
  distinct(story, block = pos)
stopifnot(!anyDuplicated(fcat_pos$story), nrow(fcat_pos) == 18)
trial_minutes <- tr |>
  left_join(run_info |> select(run_id, run_start = time_started), by = "run_id") |>
  arrange(run_id, trial_number) |>
  group_by(run_id) |>
  mutate(gap = as.numeric(difftime(timestamp, lag(timestamp, default = first(run_start)), units = "mins"))) |>
  group_by(run_id, story) |>
  summarise(minutes = sum(gap), .groups = "drop")
fixed_runs <- scores |>
  filter(calibration_group, form_type == "fixed form") |>
  select(run_id, dataset, form, age, eap, se)
m_fixed <- align_cols(all_mat_c[fixed_runs$run_id, , drop = FALSE], colnames(fit_c$mat))
tm_split <- split(trial_minutes, trial_minutes$run_id)
ll_items <- function(i, cols) {     # one run's log-likelihood on the grid from a subset of its items
  x <- m_fixed[i, cols]; ok <- !is.na(x)
  if (!any(ok)) return(rep(0, length(theta_q)))
  drop(log(P_q[, cols[ok], drop = FALSE]) %*% x[ok] + log(1 - P_q[, cols[ok], drop = FALSE]) %*% (1 - x[ok]))
}
eap_w <- function(ll, w) {
  p <- exp(ll - max(ll)) * w; p <- p / sum(p)
  e <- sum(p * theta_q); c(eap = e, psd2 = sum(p * theta_q^2) - e^2)
}
item_story <- as.integer(str_match(colnames(m_fixed), "^tom_story(\\d+)_")[, 2])
cat_sim <- map_dfr(seq_len(nrow(fixed_runs)), \(i) {
  r <- fixed_runs[i, ]; w <- prior_w(r$dataset)
  answered <- colnames(m_fixed)[!is.na(m_fixed[i, ])]
  st <- tibble(story = sort(unique(item_story[colnames(m_fixed) %in% answered]))) |>
    left_join(fcat_pos, by = "story") |>
    left_join(tm_split[[r$run_id]] |> select(story, minutes), by = "story")
  if (n_distinct(st$block) < 3) return(NULL)
  cols_of <- \(s) answered[item_story[match(answered, colnames(m_fixed))] %in% s]
  ll_story <- map(set_names(st$story), \(s) ll_items(i, cols_of(s)))
  full <- eap_w(Reduce(`+`, ll_story), w)
  # random draw: every combination of one story per block, equally likely
  combos <- expand.grid(split(st$story, st$block))
  rnd <- map_dfr(seq_len(nrow(combos)), \(k) {
    s <- unlist(combos[k, ]); e <- eap_w(Reduce(`+`, ll_story[as.character(s)]), w)
    tibble(psd2 = e[["psd2"]], minutes = sum(st$minutes[st$story %in% s]))
  })
  # adaptive draw, in block order
  ll_cur <- rep(0, length(theta_q)); chosen <- integer(0); th <- eap_w(ll_cur, w)[["eap"]]
  for (b in sort(unique(st$block))) {
    cand <- st$story[st$block == b]
    q <- which.min(abs(theta_q - th))
    info <- map_dbl(cand, \(s) sum(item_info_q[q, cols_of(s)]))
    s <- cand[which.max(info)]; chosen <- c(chosen, s)
    ll_cur <- ll_cur + ll_story[[as.character(s)]]
    th <- eap_w(ll_cur, w)[["eap"]]
  }
  ad <- eap_w(ll_cur, w)
  tibble(run_id = r$run_id, dataset = r$dataset, form = r$form, sigma2 = sum(w * theta_q^2) - sum(w * theta_q)^2,
         n_stories = nrow(st), n_draws = nrow(combos),
         psd2_full = full[["psd2"]], minutes_full = sum(st$minutes),
         psd2_random = mean(rnd$psd2), minutes_random = mean(rnd$minutes),
         psd2_adaptive = ad[["psd2"]], minutes_adaptive = sum(st$minutes[st$story %in% chosen]),
         eap_full_check = full[["eap"]] - r$eap)
})
cat_sim_summary <- cat_sim |>
  group_by(dataset, form) |>
  summarise(n_runs = n(), median_stories = median(n_stories),
            across(c(minutes_full, minutes_random, minutes_adaptive), median, .names = "median_{.col}"),
            rel_full = 1 - mean(psd2_full) / first(sigma2),
            rel_random = 1 - mean(psd2_random) / first(sigma2),
            rel_adaptive = 1 - mean(psd2_adaptive) / first(sigma2),
            info_per_min_full = median((1 / psd2_full - 1 / sigma2) / minutes_full),
            info_per_min_random = median((1 / psd2_random - 1 / sigma2) / minutes_random),
            info_per_min_adaptive = median((1 / psd2_adaptive - 1 / sigma2) / minutes_adaptive),
            adaptive_minus_random_psd2 = mean(psd2_adaptive - psd2_random),
            prop_adaptive_better = mean(psd2_adaptive < psd2_random),
            max_abs_eap_check = max(abs(eap_full_check)), .groups = "drop") |>
  filter(n_runs >= 25)

# ---- Spanish false-belief sensitivity (es-* runs) -------------------------------------------------

fb_answered <- tr |> filter(spanish_fb_flag) |> count(run_id, name = "n_fb")
sens_spanish_fb <- scores |>
  filter(!is.na(eap_nofb)) |>
  left_join(fb_answered, by = "run_id") |>
  group_by(dataset, language, form) |>
  summarise(n_runs = n(), n_runs_with_fb_item = sum(!is.na(n_fb)),
            r_with_without = safe_cor(eap, eap_nofb), mean_diff = mean(eap_nofb - eap),
            rel_with = emp_rel(eap, se), rel_without = emp_rel(eap_nofb, se_nofb),
            r_age_with = safe_cor(eap, age), r_age_without = safe_cor(eap_nofb, age), .groups = "drop")

# ---- quantities comparable to the June chapters, recomputed ---------------------------------------

de <- scores |> filter(dataset == ref_group)
first2 <- stability_first2 |> filter(dataset == ref_group)
june_equivalents <- tibble(
  quantity = c("DE full-bank marginal reliability (every calibrated item, DE prior)",
               "DE empirical reliability, item_bank runs",
               "DE empirical reliability, all runs",
               "DE r(age, proportion correct), item_bank runs",
               "DE r(age, analysis EAP), item_bank runs",
               "DE r(age, analysis EAP), all runs",
               "DE r(first two runs), analysis",
               "DE r(first two runs), released v2_3"),
  value = c(as.numeric(marginal_rxx(extract.group(fit_c$mod, group = ref_group))),
            with(filter(de, form == "item_bank"), emp_rel(eap, se)),
            emp_rel(de$eap, de$se),
            with(filter(de, form == "item_bank"), safe_cor(prop_correct, age)),
            with(filter(de, form == "item_bank"), safe_cor(eap, age)),
            safe_cor(de$eap, de$age),
            first2$r[first2$version == "analysis"],
            first2$r[first2$version == "released v2_3"]))

scores_plot <- scores |>
  select(run_id, dataset, site, language, form, form_type, age, n_items, prop_correct, run_minutes,
         eap, se, eap_defaultgrid, eap_uncorrected, score_released, se_released, calibration_group,
         at_or_below_guessing, near_ceiling)

# ---- save ----------------------------------------------------------------------------------------

inputs <- c(f_trials, f_runs, f_released, f_meta, f_v23)
results <- list(
  meta = list(
    built_at = Sys.time(), script = "tasks/_stories_fits_reliability.R", book_root = book_root,
    inputs = tibble(file = inputs, md5 = unname(tools::md5sum(inputs))),
    data_provenance = attr(tr, "provenance"),
    mirt_version = as.character(packageVersion("mirt")),
    levantemodels_version = as.character(packageVersion("levantemodels")),
    levantemodels = c(path = lm_path, commit = lm_commit, dirty = lm_dirty,
                      loaded_with = "pkgload::load_all"),
    session_info = sessionInfo(),
    g0_items = g0_items,
    R_version = R.version.string,
    chosen_itemtype = chosen, ref_group = ref_group, min_group_runs = min_group_runs,
    calibration_groups = cal_groups, small_groups = small_groups, priors = priors,
    invariance = scalar, grid = grid_wide, grid_default = grid_default, gap_breaks_days = gap_breaks,
    n_boot = n_boot, n_items_analysis = ncol(fit_c$mat), n_items_allitems = ncol(fit_all$mat),
    n_items_uncorrected = ncol(fit_b$mat), policy_excluded_trials = sum(tr$policy_exclude),
    spanish_fb_uids = fb_uids),
  itemtype_bic = itemtype_bic,
  group_pars = gp,
  item_pars = item_pars,
  item_slopes_by_type = item_slopes_by_type,
  rel_by_form = rel_by_form,
  fcat_vs_fixed = fcat_vs_fixed,
  se_by_theta = se_by_theta,
  stability_bins = stability_bins,
  stability_first2 = stability_first2,
  stability_forms = stability_forms,
  age_validity = age_validity,
  released_comparison = released_comparison,
  released_comparison_form = released_comparison_form,
  released_models = released_models,
  grid_sensitivity = grid_sensitivity,
  grid_convergence = grid_convergence,
  g0_sensitivity = g0_sensitivity,
  density_check = density_check,
  fcat_blocks = fcat_pos,
  cat_sim = cat_sim,
  cat_sim_summary = cat_sim_summary,
  sens_spanish_fb = sens_spanish_fb,
  june_equivalents = june_equivalents,
  scores_plot = scores_plot,
  pairs_plot = pairs_plot)
saveRDS(results, f_out)

# ---- console summary -----------------------------------------------------------------------------

show <- function(x, title) { cat("\n==", title, "==\n"); print(as.data.frame(x), digits = 3, row.names = FALSE) }
show(itemtype_bic, "fit statistics")
show(gp, "group means / variances")
show(item_slopes_by_type, "item slopes by question type")
show(rel_by_form, "(a) reliability by dataset x form")
show(fcat_vs_fixed, "(a) fCAT vs fixed")
show(stability_bins, "(b) stability by interval")
show(stability_first2, "(b) first two runs")
show(stability_forms |> filter(dataset == ref_group), "(b) DE stability by form pair")
show(age_validity, "(c) age validity")
show(released_comparison, "(d) vs released")
show(released_comparison_form, "(d) vs released by form")
show(released_models, "released scoring models")
show(grid_sensitivity, "quadrature grid sensitivity")
show(grid_convergence, "grid convergence: [-12, 8] vs [-16, 8]")
show(g0_sensitivity, "guessing-rule sensitivity")
show(density_check, "latent-density check (empirical histogram, items fixed)")
show(cat_sim_summary, "block-CAT simulation within children")
show(sens_spanish_fb, "Spanish false-belief sensitivity")
show(june_equivalents, "June-comparable quantities")
show(item_pars |> arrange(a) |> head(8), "lowest-slope items")
message("saved ", f_out, " and ", f_scores)
