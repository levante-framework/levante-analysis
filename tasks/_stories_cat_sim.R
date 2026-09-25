# Stories (Theory of Mind): design simulation for a story-level block CAT.
#
# Model-based simulation. Responses are generated from the analysis 2PL with guessing fixed at chance
# (item parameters and group means/variances from results_reliability.rds), stories are selected by a
# rule, and children are scored by EAP under their dataset's group prior on the analysis grid
# (theta in [-12, 8], 201 points). Reliability = 1 - mean posterior variance / group variance unless a
# section says otherwise ("realized" = 1 - mean squared error / group variance, used where the scoring
# model is misspecified on purpose). Time = sum of simulated story durations (each story's empirical
# spread in the deployed fCAT, with a per-child speed effect) plus one task introduction.
#
# Questions (sections below):
#   Q1 design grid: 3 vs 4 stories x grouping (current 3-block, canvas 4-block, a searched 4-block, no
#      blocks with at most one story per story type) x selection rule (random, max Fisher information at
#      the current EAP, max information per expected minute, top-2 randomesque) x population
#   Q2 discrimination-only selection vs Fisher information at the current EAP
#   Q3 starting value (0, group mean, age-predicted; 0 with the reference prior while testing; the
#      pooled 8-group prior while testing, as for a site without its own calibration)
#   Q4 two waves 6 or 12 months apart, and up to 6 waves: repeated stories, history-aware selection
#   Q5 calibration error: select and score online with perturbed parameters (draws from the analysis
#      model's sampling covariance, refitted here with SE = TRUE), generate with the point estimates;
#      then rescore with the point estimates
#   Q6 cross-language non-invariance: generate es-AR / es-CO (and DE) responses from group-specific
#      intercepts (results_invariance.rds), select with pooled parameters, score pooled vs group-specific
#   Q7 local dependence: generate from marginal-matched story-testlet models, select/score unidimensionally
#
# Inputs (gitignored, data/stories_2026-09/):
#   results_reliability.rds       item_pars, group_pars, age slopes (via stories_scores_analysis.rds),
#                                 Leipzig stability (disattenuated r)
#   results_invariance.rds        per-group Rasch+g difficulties, analyses A (stories 1-6) and B (13-18)
#   results_dimensionality.rds    story-bifactor slopes (Q7)
#   stories_trials.rds, stories_runs.rds, stories_scores_analysis.rds
#   raw/fcat_story_level__2026-09-24.rds   per-run x story durations of the 960 deployed fCAT runs
#                                 (built 2026-09-24 from the raw datasets' trials, time_elapsed; one row
#                                 per run x story; source of fcat_story_durations.csv). Override with env
#                                 STORIES_FCAT_STORY_LEVEL.
# Output (gitignored): data/stories_2026-09/results_cat_sim.rds
#
# Q5 refits the analysis model with standard errors, so it needs the levantemodels helpers used by
# _stories_fits_reliability.R: env LEVANTEMODELS_PATH (a PR #27 checkout), loaded with pkgload.
#
# Run from a working directory OUTSIDE the repo (the repo's renv has an mgcv binary that breaks mirt):
#   cd <scratch dir> && LEVANTEMODELS_PATH=<levantemodels checkout> Rscript <book>/tasks/_stories_cat_sim.R [<book root>]
# Book root: first argument, else env STORIES_BOOK_ROOT, else the directory above this script's tasks/.
# Env STORIES_SIM_SCALE (default 1) multiplies every simulee count (use e.g. 0.05 for a smoke test).

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(tibble)
})
options(warn = 1, dplyr.summarise.inform = FALSE, width = 200)
t_start <- Sys.time()

args <- commandArgs(trailingOnly = TRUE)
script_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
default_root <- if (length(script_file) == 1) dirname(dirname(normalizePath(script_file))) else getwd()
book_root <- normalizePath(
  if (length(args) >= 1) args[1] else Sys.getenv("STORIES_BOOK_ROOT", default_root),
  mustWork = TRUE)
data_dir <- file.path(book_root, "data", "stories_2026-09")
f_rel    <- file.path(data_dir, "results_reliability.rds")
f_inv    <- file.path(data_dir, "results_invariance.rds")
f_dim    <- file.path(data_dir, "results_dimensionality.rds")
f_trials <- file.path(data_dir, "stories_trials.rds")
f_runs   <- file.path(data_dir, "stories_runs.rds")
f_scores <- file.path(data_dir, "stories_scores_analysis.rds")
f_fcat   <- Sys.getenv("STORIES_FCAT_STORY_LEVEL", file.path(data_dir, "raw", "fcat_story_level__2026-09-24.rds"))
f_out    <- file.path(data_dir, "results_cat_sim.rds")
stopifnot(file.exists(f_rel), file.exists(f_inv), file.exists(f_dim), file.exists(f_trials),
          file.exists(f_runs), file.exists(f_scores), file.exists(f_fcat))

sim_scale <- as.numeric(Sys.getenv("STORIES_SIM_SCALE", "1"))
nn <- \(n) max(50L, as.integer(round(n * sim_scale)))
N_MAIN   <- nn(2000)   # Q1/Q2 per population x design x rule
N_SEARCH <- nn(1000)   # 4-block grouping search
N_Q3     <- nn(2000)
N_Q4     <- nn(2000)
N_Q5     <- nn(500)    # per perturbed bank
B_Q5     <- 40L        # perturbed banks
N_Q6     <- nn(4000)
N_Q7     <- nn(2000)
seed0 <- 20260925
set.seed(seed0)
log_notes <- character(0)
note <- function(...) { msg <- paste0(...); log_notes <<- c(log_notes, msg); message("NOTE: ", msg) }
tick <- function(...) message(format(Sys.time(), "%H:%M:%S"), "  ", ...)

# ---- inputs ------------------------------------------------------------------------------------------

rel <- readRDS(f_rel)
inv <- readRDS(f_inv)
dimr <- readRDS(f_dim)
tr <- readRDS(f_trials)
runs <- readRDS(f_runs)
scores <- readRDS(f_scores)
fcat <- readRDS(f_fcat)

theta_q <- seq(rel$meta$grid$theta_lim[1], rel$meta$grid$theta_lim[2], length.out = rel$meta$grid$quadpts)
Q <- length(theta_q)

type_abbr <- c(reality_known = "RK", moral_reasoning = "MR", interpretation = "IN",
               deception = "DC", reference = "RF", second_order = "SO")

# The CAT bank: the analysis model's 85 items minus story 4's reality checks 2/3. Their parameters
# come from pre-4g content (2 datasets); on the deployed 4g image their key is contradicted (policy
# exclusion, below chance), so a CAT deployed today cannot use them. Their time is still counted
# (the questions are still asked).
drop_items <- c("tom_story4_deception_reality_check_2-1", "tom_story4_deception_reality_check_3-1")
bank <- rel$item_pars |>
  filter(!item %in% drop_items) |>
  arrange(story, item) |>
  mutate(type = type_abbr[old_group], j = row_number())
stopifnot(nrow(bank) == 83, all(1:18 %in% bank$story))
note("CAT bank = the analysis model's 85 items minus story-4 reality checks 2/3 (broken on the deployed ",
     "4g image; parameters only from pre-4g content): 83 items. Durations still include them.")
story_tab <- bank |>
  group_by(story) |>
  summarise(type = first(type), old_group = first(old_group), n_items = n(),
            sum_a2 = sum(a^2), mean_a = mean(a), max_a = max(a), .groups = "drop")
story_type <- setNames(story_tab$type, story_tab$story)
same_type <- outer(story_type, story_type, "==")          # 18 x 18
items_of <- split(bank$j, bank$story)                      # list by story (names "1".."18")
S_inc <- matrix(0, nrow(bank), 18); S_inc[cbind(bank$j, bank$story)] <- 1

# ---- populations ---------------------------------------------------------------------------------------

pop_ids <- rel$meta$calibration_groups
pop_lab <- c(pilot_mpieva_de_main = "DE (Leipzig)", pilot_uniandes_co_bogota = "Bogota",
             pilot_uniandes_co_rural = "CO rural", pilot_western_ca_main = "Western",
             rfp1_mpib_intl_ys = "MPIB-intl", rfp1_sheffield_gb_main = "Sheffield",
             rfp1_utdt_ar_main = "UTDT-AR", rfp1_utdt_intl_ys = "UTDT-intl")
stopifnot(setequal(names(pop_lab), pop_ids))
gp <- rel$group_pars |> filter(dataset %in% pop_ids) |> select(dataset, mu = mean_analysis, s2 = var_analysis)

# Age structure. Within dataset, EAP regressed on age gives slope_eap. EAP under the group prior is
# E[theta | x], so cov(EAP, age) = cov(theta, age) * var(EAP) / var(theta): the latent slope is
# slope_eap / (var(EAP) / sigma^2). Children are generated as theta = mu + beta_lat (age - mean age) + e,
# e ~ N(0, sigma^2 - beta_lat^2 var(age)), with ages resampled from the dataset's analysed runs.
age_tab <- scores |>
  filter(dataset %in% pop_ids, !is.na(age)) |>
  group_by(dataset) |>
  summarise(n_age = n(), age_mean = mean(age), age_sd = sd(age),
            slope_eap = unname(coef(lm(eap ~ age))[2]), var_eap = var(eap), .groups = "drop")
av <- rel$age_validity |> filter(form == "all forms") |> select(dataset, slope_per_year)
pops <- gp |>
  left_join(age_tab, by = "dataset") |>
  left_join(av, by = "dataset") |>
  mutate(label = pop_lab[dataset],
         attenuation = var_eap / s2,
         beta_lat = slope_eap / attenuation,
         r_age_latent = beta_lat * age_sd / sqrt(s2),
         resid_var = s2 - beta_lat^2 * age_sd^2)
stopifnot(max(abs(pops$slope_eap - pops$slope_per_year)) < 1e-8, all(pops$r_age_latent < 0.95))
ages_of <- split(scores$age[scores$dataset %in% pop_ids & !is.na(scores$age)],
                 scores$dataset[scores$dataset %in% pop_ids & !is.na(scores$age)])

# Individual variation in growth (Q4): Leipzig alternate-form stability disattenuated for measurement
# error; theta_2 = theta_1 + growth + u, u ~ N(0, tau^2), tau^2 = sigma^2 (1 / r^2 - 1).
sb <- rel$stability_bins |> filter(dataset == "pilot_mpieva_de_main", version == "analysis")
r_true <- c(`0.5` = sb$r_disattenuated[sb$gap_bin == "[60,180)"],
            `1` = sb$r_disattenuated[sb$gap_bin == "[300,Inf)"])
stability_used <- sb |> select(gap_bin, n_pairs, median_gap_days, r, r_disattenuated) |>
  mutate(used_for_gap_years = case_when(gap_bin == "[60,180)" ~ 0.5, gap_bin == "[300,Inf)" ~ 1))
note("Growth variation: true-score correlation between waves = Leipzig disattenuated alternate-form ",
     "stability, ", round(r_true[["0.5"]], 3), " (60-180 days, median ", round(sb$median_gap_days[sb$gap_bin == "[60,180)"]),
     " d) for 6-month gaps and ", round(r_true[["1"]], 3), " (>= 300 days, median ",
     round(sb$median_gap_days[sb$gap_bin == "[300,Inf)"]), " d) for 12-month gaps; applied to every population.")

# ---- story durations -------------------------------------------------------------------------------------

fs <- fcat |> filter(complete_story, dur > 0, dur < 600)
dur_tab <- fs |>
  group_by(story) |>
  summarise(n_runs = n(), med = median(dur), q25 = quantile(dur, .25), q75 = quantile(dur, .75), .groups = "drop")
# per-child speed: variance components of log duration (story means removed), complete 3-story runs
vc <- fs |>
  mutate(res = log(dur) - ave(log(dur), story)) |>
  group_by(run_id) |> filter(n() == 3) |>
  summarise(m = mean(res), v = var(res), .groups = "drop")
sig2_within <- mean(vc$v)
tau2_child <- var(vc$m) - sig2_within / 3
rho_speed <- tau2_child / (tau2_child + sig2_within)
# task introduction: in the fCAT the block-1 story (1, 3, 7, 9, 13, 15) always comes first and its
# duration includes the introduction. Stories 3, 9, 15 also run in 3rd position on fixed forms; their
# fCAT-minus-fixed-form difference, minus the same difference for stories that are never first,
# estimates the introduction.
run_start <- runs |> filter(included) |> select(run_id, run_start = time_started)
ff <- tr |>
  filter(form %in% c("item_bank", "retest_A", "retest_B", "co_retest_A", "co_retest_B")) |>
  inner_join(run_start, by = "run_id") |>
  arrange(run_id, trial_number) |>
  group_by(run_id) |>
  mutate(gap = as.numeric(difftime(timestamp, lag(timestamp, default = first(run_start)), units = "secs")),
         pos = match(story, unique(story))) |>
  group_by(run_id, story) |>
  summarise(secs = sum(gap), pos = first(pos), .groups = "drop") |>
  filter(secs > 0, secs < 600)
ff_med <- ff |> group_by(story) |> summarise(n_fixed = n(), med_fixed = median(secs), pos_fixed = median(pos), .groups = "drop")
intro_tab <- dur_tab |>
  select(story, med_fcat = med) |>
  left_join(ff_med, by = "story") |>
  mutate(diff = med_fcat - med_fixed,
         role = case_when(story %in% c(3, 9, 15) ~ "fCAT first, fixed form not first",
                          story %in% c(1, 7, 13) ~ "first in both (unused)",
                          TRUE ~ "never first"))
intro_secs <- median(intro_tab$diff[intro_tab$role == "fCAT first, fixed form not first"]) -
  median(intro_tab$diff[intro_tab$role == "never first"])
block1_fcat <- c(1, 3, 7, 9, 13, 15)
dur_tab <- dur_tab |>
  mutate(med_net = if_else(story %in% block1_fcat, med - intro_secs, med),
         sd_log = if_else(story %in% block1_fcat,
                          (log(q75 - intro_secs) - log(q25 - intro_secs)) / (2 * qnorm(.75)),
                          (log(q75) - log(q25)) / (2 * qnorm(.75)))) |>
  left_join(story_tab, by = "story")
stopifnot(nrow(dur_tab) == 18, all(dur_tab$med_net > 20))
med_net_min <- setNames(dur_tab$med_net / 60, dur_tab$story)
note("Introduction = ", round(intro_secs, 1), " s (subtracted from block-1 stories' fCAT durations, added once per ",
     "administration). Per-child speed: share of log-duration variance between children = ", round(rho_speed, 2), ".")
# deployed 3-story fCAT totals (for the validation check)
fcat_totals <- fs |> group_by(run_id) |> filter(n() == 3) |> summarise(secs = sum(dur), .groups = "drop")

# ---- parameter sets and engine ---------------------------------------------------------------------------

make_ps <- function(a, d, g) {
  Z <- outer(theta_q, a) + rep(d, each = Q)
  G <- rep(g, each = Q)
  list(a = a, d = d, g = g,
       logP = log(G + (1 - G) * plogis(Z)),
       log1mP = log(1 - G) + plogis(Z, lower.tail = FALSE, log.p = TRUE),
       sum_a2 = drop(a^2 %*% S_inc), mean_a = drop(a %*% S_inc) / drop(colSums(S_inc)))
}
ps_true <- make_ps(bank$a, bank$d, bank$g)

# N x 18 story information at thetas th (2PL with fixed guessing)
story_info <- function(th, ps) {
  n <- length(th)
  Z <- outer(th, ps$a) + rep(ps$d, each = n)
  G <- rep(ps$g, each = n)
  P <- G + (1 - G) * plogis(Z)
  I <- rep(ps$a^2, each = n) * ((P - G) / (1 - G))^2 * (1 - P) / P
  I %*% S_inc
}
lprior <- \(mu, s2) dnorm(theta_q, mu, sqrt(s2), log = TRUE)
post_eap <- function(ll, lp) {
  x <- ll + rep(lp, each = nrow(ll))
  x <- exp(x - matrixStats::rowMaxs(x))
  s <- rowSums(x)
  e <- drop(x %*% theta_q) / s
  list(eap = e, psd2 = pmax(drop(x %*% theta_q^2) / s - e^2, 0))
}

# common random numbers for one population: children, response uniforms, durations, testlet effects
make_pop <- function(pid, N, seed) {
  set.seed(seed)
  p <- pops[pops$dataset == pid, ]
  ag <- ages_of[[pid]]
  age <- ag[sample.int(length(ag), N, replace = TRUE)]
  theta <- p$mu + p$beta_lat * (age - p$age_mean) + rnorm(N, 0, sqrt(p$resid_var))
  crn_draw(list(pid = pid, mu = p$mu, s2 = p$s2, lp = lprior(p$mu, p$s2), age = age, theta = theta,
                start_age = p$mu + p$beta_lat * (age - p$age_mean)))
}
crn_draw <- function(pp) {
  N <- length(pp$theta)
  pp$U <- matrix(runif(N * nrow(bank)), N)
  z <- sqrt(rho_speed) * rnorm(N) + sqrt(1 - rho_speed) * matrix(rnorm(N * 18), N)
  pp$dur <- exp(rep(log(dur_tab$med_net), each = N) + rep(dur_tab$sd_log, each = N) * z)
  pp$gamma <- matrix(rnorm(N * 18), N)
  pp
}

# generating model: 2PL + guessing, optional story testlet slopes lambda (logit per SD of gamma)
gen_uni <- function(ps) list(a = ps$a, d = ps$d, g = ps$g, lambda = NULL)
gen_resp <- function(gen, pp, idx, js, s) {
  z <- outer(pp$theta[idx], gen$a[js]) + rep(gen$d[js], each = length(idx))
  if (!is.null(gen$lambda)) z <- z + outer(pp$gamma[idx, s], gen$lambda[js])
  P <- rep(gen$g[js], each = length(idx)) + rep(1 - gen$g[js], each = length(idx)) * plogis(z)
  (pp$U[idx, js, drop = FALSE] < P) * 1
}

# one administration per child. steps = list of candidate story sets (one per story administered);
# at most one story per story type within an administration (this is what the block designs imply;
# it constrains only the no-block designs and the 4th story of "current 3-block + 1").
# rule: random | maxinfo | infomin | rq2 | disc_a2 | disc_mean | oracle (max info at the true theta)
# sel: parameter set for selection and the running EAP (prior lp_run); score: named list of parameter
# sets for the final EAP (prior lp_score); seen: N x 18 stories already administered (history-aware).
simulate <- function(pp, steps, rule, sel = ps_true, gen = gen_uni(ps_true), score = list(true = ps_true),
                     start = NULL, lp_run = pp$lp, lp_score = pp$lp, seen = NULL, theta = pp$theta) {
  pp$theta <- theta
  N <- length(theta)
  if (is.null(start)) start <- rep(pp$mu, N)
  ll_run <- matrix(0, N, Q)
  ll_sc <- lapply(score, \(x) matrix(0, N, Q))
  type_used <- matrix(FALSE, N, 18)
  chosen <- matrix(NA_integer_, N, length(steps))
  forced <- integer(N)
  secs <- rep(intro_secs, N)
  first_info <- NULL
  for (k in seq_along(steps)) {
    avail <- matrix(FALSE, N, 18); avail[, steps[[k]]] <- TRUE
    avail <- avail & !type_used
    if (!is.null(seen)) {
      a2 <- avail & !seen
      none <- rowSums(a2) == 0
      a2[none, ] <- avail[none, ]
      forced <- forced + none
      avail <- a2
    }
    th <- if (k == 1) start else post_eap(ll_run, lp_run)$eap
    crit <- switch(rule,
      random    = matrix(runif(N * 18), N),
      maxinfo   = story_info(th, sel),
      rq2       = story_info(th, sel),
      infomin   = story_info(th, sel) / rep(med_net_min, each = N),
      disc_a2   = matrix(sel$sum_a2, N, 18, byrow = TRUE),
      disc_mean = matrix(sel$mean_a, N, 18, byrow = TRUE),
      oracle    = story_info(theta, sel),
      stop("unknown rule ", rule))
    crit[!avail] <- -Inf
    ch <- max.col(crit, ties.method = "first")
    if (rule == "rq2") {
      c2 <- crit; c2[cbind(seq_len(N), ch)] <- -Inf
      ch2 <- max.col(c2, ties.method = "first")
      use2 <- is.finite(c2[cbind(seq_len(N), ch2)]) & runif(N) < 0.5
      ch[use2] <- ch2[use2]
    }
    stopifnot(all(avail[cbind(seq_len(N), ch)]))
    chosen[, k] <- ch
    for (s in unique(ch)) {
      idx <- which(ch == s); js <- items_of[[as.character(s)]]
      X <- gen_resp(gen, pp, idx, js, s)
      ll_run[idx, ] <- ll_run[idx, ] + X %*% t(sel$logP[, js, drop = FALSE]) + (1 - X) %*% t(sel$log1mP[, js, drop = FALSE])
      for (m in names(score))
        ll_sc[[m]][idx, ] <- ll_sc[[m]][idx, ] + X %*% t(score[[m]]$logP[, js, drop = FALSE]) +
          (1 - X) %*% t(score[[m]]$log1mP[, js, drop = FALSE])
    }
    type_used <- type_used | same_type[ch, , drop = FALSE]
    secs <- secs + pp$dur[cbind(seq_len(N), ch)]
  }
  run <- post_eap(ll_run, lp_run)
  out <- tibble(theta = theta, eap_run = run$eap, psd2_run = run$psd2, secs = secs, forced = forced)
  for (m in names(score)) {
    e <- post_eap(ll_sc[[m]], lp_score)
    out[[paste0("eap_", m)]] <- e$eap; out[[paste0("psd2_", m)]] <- e$psd2
  }
  list(df = out, chosen = chosen)
}

summ <- function(res, s2, m = "true") {
  d <- res$df; e <- d[[paste0("eap_", m)]]; v <- d[[paste0("psd2_", m)]]
  ch <- res$chosen
  expo <- tabulate(ch, 18) / nrow(ch)
  tibble(n_sim = nrow(d),
         rel = 1 - mean(v) / s2,
         rel_mcse = sd(v) / sqrt(nrow(d)) / s2,
         rmse = sqrt(mean((e - d$theta)^2)),
         rel_realized = 1 - mean((e - d$theta)^2) / s2,
         r2_realized = cor(e, d$theta)^2,
         bias = mean(e - d$theta),
         mean_psd = mean(sqrt(v)),
         median_min = median(d$secs) / 60, q90_min = unname(quantile(d$secs, .9)) / 60,
         mean_min = mean(d$secs) / 60,
         info_per_min = mean(1 / v - 1 / s2) / mean(d$secs / 60),
         max_exposure = max(expo), n_stories_lt5pct = sum(expo < .05),
         top_story = which.max(expo),
         mean_forced = mean(d$forced))
}
exposure_long <- function(res) tibble(story = 1:18, exposure = tabulate(res$chosen, 18) / nrow(res$chosen))
csem <- function(res, m = "true", breaks = seq(-10, 4, 1)) {
  d <- res$df
  tibble(theta = d$theta, err2 = (d[[paste0("eap_", m)]] - d$theta)^2, psd2 = d[[paste0("psd2_", m)]]) |>
    mutate(theta_bin = cut(theta, breaks)) |>
    filter(!is.na(theta_bin)) |>
    group_by(theta_bin) |>
    summarise(n = n(), theta_mid = mean(theta), mean_psd = sqrt(mean(psd2)), rmse = sqrt(mean(err2)), .groups = "drop")
}

# ---- designs -----------------------------------------------------------------------------------------------

blocks_cur <- list(c(1, 3, 7, 9, 13, 15), c(2, 6, 8, 12, 14, 18), c(4, 5, 10, 11, 16, 17))
stories_of <- \(types) sort(story_tab$story[story_tab$type %in% types])
blocks_canvas <- list(stories_of("RK"), stories_of("MR"), stories_of(c("IN", "DC")), stories_of(c("RF", "SO")))
# the fCAT's blocks, checked against the deployed data (story -> position)
stopifnot(all(map_lgl(1:3, \(b) setequal(rel$fcat_blocks$story[rel$fcat_blocks$block == b], blocks_cur[[b]]))))

# ---- validation: simulated deployed fCAT vs the data ------------------------------------------------------

tick("validation")
val <- map_dfr(pop_ids, \(pid) {
  pp <- make_pop(pid, N_MAIN, seed0 + 1)
  res <- simulate(pp, blocks_cur, "random")
  s <- summ(res, pp$s2)
  obs <- rel$rel_by_form |> filter(dataset == pid, form == "fcat_random3")
  tibble(dataset = pid, label = pop_lab[pid], sim_rel_fcat3_random = s$rel, sim_mean_psd = s$mean_psd,
         sim_median_min = s$median_min, sim_q90_min = s$q90_min,
         obs_rel_group_fcat = if (nrow(obs)) obs$rel_group else NA_real_,
         obs_rms_se_fcat = if (nrow(obs)) obs$rms_se else NA_real_,
         obs_n_fcat = if (nrow(obs)) obs$n_runs else NA_integer_,
         check_rel_model_vs_realized = s$rel - s$rel_realized)
})
val_time <- tibble(obs_median_min = median(fcat_totals$secs) / 60, obs_q90_min = unname(quantile(fcat_totals$secs, .9)) / 60,
                   obs_n_runs = nrow(fcat_totals))
print(val); print(val_time)

# ---- story information table (Q2 background) ---------------------------------------------------------------

th_show <- c(-6, -5, -4, -3, -2, -1, 0, 1, 2)
I_show <- story_info(th_show, ps_true)
story_info_tab <- story_tab |>
  left_join(dur_tab |> select(story, med_secs = med, med_net_secs = med_net), by = "story") |>
  bind_cols(as_tibble(t(I_show), .name_repair = \(x) paste0("I_at_", th_show))) |>
  mutate(theta_peak = theta_q[apply(story_info(theta_q, ps_true), 2, which.max)])
exp_info <- map_dfr(pop_ids, \(pid) {
  p <- pops[pops$dataset == pid, ]
  w <- dnorm(theta_q, p$mu, sqrt(p$s2)); w <- w / sum(w)
  Iq <- story_info(theta_q, ps_true)
  tibble(dataset = pid, label = p$label, story = 1:18, exp_info = drop(w %*% Iq),
         exp_info_per_min = drop(w %*% Iq) / med_net_min)
})
disc_vs_info_cor <- tibble(theta = th_show,
  spearman_sum_a2 = map_dbl(seq_along(th_show), \(k) cor(story_tab$sum_a2, I_show[k, ], method = "spearman")),
  spearman_mean_a = map_dbl(seq_along(th_show), \(k) cor(story_tab$mean_a, I_show[k, ], method = "spearman")),
  best_story_info = map_int(seq_along(th_show), \(k) which.max(I_show[k, ])),
  best_story_sum_a2 = which.max(story_tab$sum_a2),
  info_ratio_top_a2_to_best = map_dbl(seq_along(th_show), \(k) I_show[k, which.max(story_tab$sum_a2)] / max(I_show[k, ])))

# ---- 4-block grouping search --------------------------------------------------------------------------------
# Content-coherent 4-block groupings = partitions of the six story types into two pairs and two
# single types (45 partitions; the canvas proposal is one). Screen: expected oracle information
# (the best story of each block at the child's true theta), then simulate the top candidates under
# every block order with max-information selection.

tick("grouping search")
types6 <- c("RK", "MR", "IN", "DC", "RF", "SO")
parts <- list()
for (p1 in combn(types6, 2, simplify = FALSE)) {
  rest <- setdiff(types6, p1)
  for (p2 in combn(rest, 2, simplify = FALSE)) {
    if (paste(p2, collapse = "") < paste(p1, collapse = "")) next
    singles <- setdiff(rest, p2)
    parts[[length(parts) + 1]] <- list(p1, p2, singles[1], singles[2])
  }
}
stopifnot(length(parts) == 45)
part_label <- \(pt) paste(map_chr(pt, \(b) paste(b, collapse = "+")), collapse = " | ")
Iq_true <- story_info(theta_q, ps_true)                                      # Q x 18
screen <- map_dfr(seq_along(parts), \(k) {
  blocks <- map(parts[[k]], stories_of)
  map_dfr(pop_ids, \(pid) {
    p <- pops[pops$dataset == pid, ]
    w <- dnorm(theta_q, p$mu, sqrt(p$s2)); w <- w / sum(w)
    best <- map(blocks, \(b) b[apply(Iq_true[, b, drop = FALSE], 1, which.max)])
    Itot <- Reduce(`+`, map(blocks, \(b) apply(Iq_true[, b, drop = FALSE], 1, max)))
    secs <- intro_secs + Reduce(`+`, map(best, \(s) dur_tab$med_net[s]))
    tibble(part = k, grouping = part_label(parts[[k]]), dataset = pid,
           rel_oracle_approx = 1 - sum(w / (1 / p$s2 + Itot)) / p$s2, secs_oracle = sum(w * secs))
  })
}) |>
  group_by(part, grouping) |>
  summarise(rel_oracle_approx = mean(rel_oracle_approx), min_oracle = mean(secs_oracle) / 60, .groups = "drop") |>
  arrange(desc(rel_oracle_approx))
canvas_part <- which(map_chr(parts, part_label) == "IN+DC | RF+SO | RK | MR")
stopifnot(length(canvas_part) == 1)
cand_parts <- unique(c(head(screen$part, 6), canvas_part))
perms4 <- as.matrix(expand.grid(1:4, 1:4, 1:4, 1:4)); perms4 <- perms4[apply(perms4, 1, \(r) n_distinct(r) == 4), ]
search_pops <- map(pop_ids, \(pid) make_pop(pid, N_SEARCH, seed0 + 2))
names(search_pops) <- pop_ids
search_sim <- map_dfr(cand_parts, \(k) map_dfr(seq_len(nrow(perms4)), \(o) {
  blocks <- map(parts[[k]], stories_of)[perms4[o, ]]
  lab <- paste(map_chr(parts[[k]][perms4[o, ]], \(b) paste(b, collapse = "+")), collapse = " | ")
  map_dfr(pop_ids, \(pid) {
    pp <- search_pops[[pid]]
    s <- summ(simulate(pp, blocks, "maxinfo"), pp$s2)
    tibble(part = k, order = lab, dataset = pid, rel = s$rel, median_min = s$median_min)
  })
}))
search_rank <- search_sim |>
  group_by(part, order) |>
  summarise(mean_rel = mean(rel), min_rel = min(rel), mean_median_min = mean(median_min), .groups = "drop") |>
  arrange(desc(mean_rel))
print(head(search_rank, 15))
# The top candidates differ by less than simulation noise, so the choice is pre-specified as: among
# orderings within 0.005 of the best mean reliability, the one with the best worst-case population
# (maximin), then the best mean.
search_tie_band <- 0.005
best_row <- search_rank |>
  filter(mean_rel >= max(mean_rel) - search_tie_band) |>
  arrange(desc(min_rel), desc(mean_rel)) |>
  slice(1)
best_blocks_types <- str_split(str_split_1(best_row$order, " \\| "), "\\+")
blocks_best <- map(best_blocks_types, stories_of)
note("Searched 4-block grouping (max-info selection; within 0.005 of the best mean model reliability over the 8 calibration groups, the best worst-case group): ",
     best_row$order)
# block balance: expected information of each block's best story and median minutes of its stories
block_balance <- function(blocks, name) map_dfr(seq_along(blocks), \(b) {
  s <- blocks[[b]]
  tibble(design = name, block = b, stories = paste(s, collapse = ","),
         types = paste(unique(story_type[s]), collapse = "+"),
         n_stories = length(s),
         exp_info_best = mean(map_dbl(pop_ids, \(pid) {
           p <- pops[pops$dataset == pid, ]; w <- dnorm(theta_q, p$mu, sqrt(p$s2)); w <- w / sum(w)
           sum(w * apply(Iq_true[, s, drop = FALSE], 1, max)) })),
         exp_info_mean = mean(exp_info$exp_info[exp_info$story %in% s]),
         median_secs_mean = mean(dur_tab$med_net[s]),
         median_secs_range = paste(round(range(dur_tab$med_net[s])), collapse = "-"))
})

designs <- list(
  cur3     = list(label = "current 3-block", n = 3, steps = blocks_cur),
  cur3p1   = list(label = "current 3-block + 1 free story", n = 4, steps = c(blocks_cur, list(1:18))),
  canvas3  = list(label = "canvas 4-block, RK block dropped", n = 3, steps = blocks_canvas[2:4]),
  canvas4  = list(label = "canvas 4-block", n = 4, steps = blocks_canvas),
  best3    = list(label = "searched 4-block, first 3 blocks", n = 3, steps = blocks_best[1:3]),
  best4    = list(label = "searched 4-block", n = 4, steps = blocks_best),
  free3    = list(label = "no blocks, <= 1 story per type", n = 3, steps = rep(list(1:18), 3)),
  free4    = list(label = "no blocks, <= 1 story per type", n = 4, steps = rep(list(1:18), 4)),
  fixed_1to6   = list(label = "reference: fixed form, stories 1-6", n = 6, steps = as.list(1:6)),
  fixed_13to18 = list(label = "reference: fixed form, stories 13-18", n = 6, steps = as.list(13:18)))
balance <- bind_rows(block_balance(blocks_cur, "cur3"), block_balance(blocks_canvas, "canvas4"),
                     block_balance(blocks_best, "best4"))
design_tab <- imap_dfr(designs, \(d, nm) tibble(design = nm, label = d$label, n_stories = d$n,
  steps = paste(map_chr(d$steps, \(s) if (length(s) == 18) "any" else paste(s, collapse = ",")), collapse = " | ")))

# ---- Q1 + Q2: design grid -----------------------------------------------------------------------------------

tick("Q1/Q2 grid")
rules_main <- c("random", "maxinfo", "infomin", "rq2", "disc_a2", "disc_mean", "oracle")
main_pops <- map(pop_ids, \(pid) make_pop(pid, N_MAIN, seed0 + 3)); names(main_pops) <- pop_ids
q1_rows <- list(); q1_csem <- list(); q1_expo <- list()
for (pid in pop_ids) {
  pp <- main_pops[[pid]]
  for (dn in names(designs)) {
    rl <- if (str_starts(dn, "fixed")) "random" else rules_main
    for (r in rl) {
      res <- simulate(pp, designs[[dn]]$steps, r)
      key <- tibble(dataset = pid, label = pop_lab[pid], design = dn, n_stories = designs[[dn]]$n,
                    rule = if (str_starts(dn, "fixed")) "fixed" else r)
      q1_rows[[length(q1_rows) + 1]] <- bind_cols(key, summ(res, pp$s2))
      q1_csem[[length(q1_csem) + 1]] <- bind_cols(key, csem(res))
      q1_expo[[length(q1_expo) + 1]] <- bind_cols(key, exposure_long(res))
    }
  }
  tick("  ", pid)
}
q1 <- bind_rows(q1_rows); q1_csem <- bind_rows(q1_csem); q1_expo <- bind_rows(q1_expo)

# Q2: at each block, how often a discrimination-only choice equals the max-information choice at the
# true theta, and how much information it gives up (children drawn from each population)
q2_agree <- map_dfr(pop_ids, \(pid) {
  pp <- main_pops[[pid]]
  I_true <- story_info(pp$theta, ps_true)
  map_dfr(c("cur3", "canvas4", "best4"), \(dn) map_dfr(seq_along(designs[[dn]]$steps), \(b) {
    s <- designs[[dn]]$steps[[b]]
    Ib <- I_true[, s, drop = FALSE]
    best <- apply(Ib, 1, which.max)
    ka2 <- which.max(story_tab$sum_a2[s]); kma <- which.max(story_tab$mean_a[s])
    tibble(dataset = pid, label = pop_lab[pid], design = dn, block = b,
           stories = paste(s, collapse = ","),
           story_top_sum_a2 = s[ka2], story_top_mean_a = s[kma],
           modal_best_story = s[as.integer(names(which.max(table(best))))],
           agree_sum_a2 = mean(best == ka2), agree_mean_a = mean(best == kma),
           info_ratio_sum_a2 = mean(Ib[, ka2] / apply(Ib, 1, max)),
           info_ratio_mean_a = mean(Ib[, kma] / apply(Ib, 1, max)),
           info_ratio_random = mean(rowMeans(Ib) / apply(Ib, 1, max)))
  }))
})

# ---- Q3: starting value -------------------------------------------------------------------------------------

tick("Q3 start values")
q3_rows <- list(); q3_first <- list()
q3_designs <- c("cur3", "canvas4", "best4", "free4")
# a site without its own calibrated group distribution: the run-weighted mixture of the 8 groups
w_pool <- rel$group_pars$n_runs[match(pops$dataset, rel$group_pars$dataset)]
mu_pool <- sum(w_pool * pops$mu) / sum(w_pool)
s2_pool <- sum(w_pool * (pops$s2 + pops$mu^2)) / sum(w_pool) - mu_pool^2
for (pid in pop_ids) {
  pp <- make_pop(pid, N_Q3, seed0 + 4)
  I_true <- story_info(pp$theta, ps_true)
  starts <- list(
    zero       = list(start = rep(0, length(pp$theta)), lp_run = pp$lp),
    group_mean = list(start = rep(pp$mu, length(pp$theta)), lp_run = pp$lp),
    age        = list(start = pp$start_age, lp_run = pp$lp),
    zero_refprior = list(start = rep(0, length(pp$theta)), lp_run = lprior(0, 1)),
    pooled_prior = list(start = rep(mu_pool, length(pp$theta)), lp_run = lprior(mu_pool, s2_pool)))
  for (dn in q3_designs) for (r in c("maxinfo", "infomin", "rq2")) for (sv in names(starts)) {
    res <- simulate(pp, designs[[dn]]$steps, r, start = starts[[sv]]$start, lp_run = starts[[sv]]$lp_run)
    s1 <- designs[[dn]]$steps[[1]]
    oracle1 <- s1[apply(I_true[, s1, drop = FALSE], 1, which.max)]
    ch1 <- res$chosen[, 1]
    key <- tibble(dataset = pid, label = pop_lab[pid], design = dn, rule = r, start = sv)
    q3_rows[[length(q3_rows) + 1]] <- bind_cols(key, summ(res, pp$s2),
      tibble(first_matches_oracle = mean(ch1 == oracle1),
             first_info_ratio = mean(I_true[cbind(seq_along(ch1), ch1)] / apply(I_true[, s1, drop = FALSE], 1, max)),
             first_n_distinct = n_distinct(ch1), first_modal = as.integer(names(which.max(table(ch1)))),
             rel_running = 1 - mean(res$df$psd2_run) / pp$s2))
    q3_first[[length(q3_first) + 1]] <- bind_cols(key, tibble(story = 1:18, share_first = tabulate(ch1, 18) / length(ch1)))
  }
}
q3 <- bind_rows(q3_rows); q3_first <- bind_rows(q3_first) |> filter(share_first > 0)

# ---- Q4: waves --------------------------------------------------------------------------------------------------

tick("Q4 waves")
q4_designs <- c("cur3", "canvas4", "best4", "free3", "free4")
growth_of <- function(pid, which) {
  p <- pops[pops$dataset == pid, ]
  if (which == "eap_slope") p$slope_eap else p$beta_lat
}
q4_two <- list(); q4_multi <- list()
for (pid in pop_ids) {
  kp <- match(pid, pop_ids)
  p <- pops[pops$dataset == pid, ]
  pp1 <- make_pop(pid, N_Q4, seed0 + 5)
  for (dn in q4_designs) for (r in c("random", "maxinfo", "infomin", "rq2")) {
    # separate seeds for the wave-1 stream and each wave-2 stream (re-using one seed inside the loop
    # makes the random draws of the two waves overlap)
    set.seed(seed0 + 5000 + 10 * kp)
    w1 <- simulate(pp1, designs[[dn]]$steps, r)
    seen1 <- matrix(FALSE, N_Q4, 18); seen1[cbind(rep(seq_len(N_Q4), ncol(w1$chosen)), c(w1$chosen))] <- TRUE
    for (gap in c(0.5, 1)) for (gr in c("eap_slope", "latent_slope")) {
      code <- 2 * (gap == 1) + (gr == "latent_slope")
      set.seed(seed0 + 6000 + 10 * kp + code)
      tau2 <- p$s2 * (1 / r_true[[as.character(gap)]]^2 - 1)
      theta2 <- pp1$theta + growth_of(pid, gr) * gap + rnorm(N_Q4, 0, sqrt(tau2))
      pp2 <- crn_draw(modifyList(pp1, list(theta = theta2)))
      for (ha in c(FALSE, TRUE)) {
        set.seed(seed0 + 7000 + 10 * kp + code)
        w2 <- simulate(pp2, designs[[dn]]$steps, r, seen = if (ha) seen1 else NULL)
        rep_share <- rowMeans(matrix(seen1[cbind(rep(seq_len(N_Q4), ncol(w2$chosen)), c(w2$chosen))], N_Q4))
        s2w <- summ(w2, pp2$s2)
        q4_two[[length(q4_two) + 1]] <- tibble(dataset = pid, label = pop_lab[pid], design = dn, rule = r,
          gap_years = gap, growth = gr, history_aware = ha, growth_mean = growth_of(pid, gr) * gap, tau = sqrt(tau2),
          rel_wave1 = summ(w1, pp1$s2)$rel, rel_wave2 = s2w$rel, rmse_wave2 = s2w$rmse,
          median_min_wave2 = s2w$median_min,
          share_repeated = mean(rep_share), share_any_repeat = mean(rep_share > 0),
          share_forced = mean(w2$df$forced > 0))
      }
    }
  }
  # up to 6 waves, 6 months apart, EAP-slope growth
  for (dn in q4_designs) for (r in c("random", "maxinfo", "rq2")) for (ha in c(FALSE, TRUE)) {
    set.seed(seed0 + 7)
    pp <- pp1; seen <- matrix(FALSE, N_Q4, 18); tau2 <- p$s2 * (1 / r_true[["0.5"]]^2 - 1)
    for (wv in 1:6) {
      if (wv > 1) pp <- crn_draw(modifyList(pp, list(theta = pp$theta + p$slope_eap * 0.5 + rnorm(N_Q4, 0, sqrt(tau2)))))
      w <- simulate(pp, designs[[dn]]$steps, r, seen = if (ha) seen else NULL)
      rep_any <- rowSums(matrix(seen[cbind(rep(seq_len(N_Q4), ncol(w$chosen)), c(w$chosen))], N_Q4)) > 0
      seen[cbind(rep(seq_len(N_Q4), ncol(w$chosen)), c(w$chosen))] <- TRUE
      s <- summ(w, pp$s2)
      q4_multi[[length(q4_multi) + 1]] <- tibble(dataset = pid, label = pop_lab[pid], design = dn, rule = r,
        history_aware = ha, wave = wv, rel = s$rel, rmse = s$rmse, share_with_repeat = mean(rep_any),
        share_forced = mean(w$df$forced > 0), mean_distinct_seen = mean(rowSums(seen)))
    }
  }
  tick("  ", pid)
}
q4_two <- bind_rows(q4_two); q4_multi <- bind_rows(q4_multi)
# combinatorial bound: waves without any repeat (blocks: smallest block; no blocks: each wave takes n
# stories of n different types, 3 stories per type)
waves_bound <- tibble(design = q4_designs) |>
  mutate(bound = map_int(design, \(dn) {
    st <- designs[[dn]]$steps
    if (all(lengths(st) == 18)) as.integer(floor(18 / length(st))) else min(lengths(st))
  }))

# ---- Q5: calibration error ---------------------------------------------------------------------------------------

tick("Q5 refit with SE")
lm_path <- path.expand(Sys.getenv("LEVANTEMODELS_PATH"))
if (!nzchar(lm_path)) stop("Q5 needs LEVANTEMODELS_PATH (a levantemodels checkout on the PR #27 branch)")
suppressPackageStartupMessages(library(mirt))
suppressMessages(pkgload::load_all(lm_path, quiet = TRUE))
lm_commit <- suppressWarnings(system2("git", c("-C", shQuote(lm_path), "rev-parse", "HEAD"), stdout = TRUE, stderr = TRUE))
ref_group <- rel$meta$ref_group
d_corr <- tr |> filter(!policy_exclude) |> transmute(run_id, group = dataset, item_uid = story_uid, correct, chance)
dd <- d_corr |> filter(group %in% pop_ids) |> dedupe_items() |> remove_no_var_items()
wmat <- to_mirt_shape_grouped(dd)
grp <- factor(wmat$group, levels = c(ref_group, sort(setdiff(unique(wmat$group), ref_group))))
mat <- as.matrix(wmat[, setdiff(names(wmat), "group")])
guess <- dd |> distinct(item_inst = as.character(item_inst), chance)
guess <- setNames(guess$chance, guess$item_inst)[colnames(mat)]
pri <- map(rel$meta$priors, as.character)
ms <- generate_model_str_numeric(dd, mat, rel$meta$chosen_itemtype, 1, pri)
t_fit <- Sys.time()
mod_se <- multipleGroup(mat, mirt.model(ms), group = grp, itemtype = rel$meta$chosen_itemtype, guess = unname(guess),
                        invariance = rel$meta$invariance, quadpts = rel$meta$grid$quadpts, verbose = FALSE,
                        technical = list(NCYCLES = 5000, theta_lim = rel$meta$grid$theta_lim),
                        SE = TRUE, SE.type = "Oakes")
refit_secs <- as.numeric(difftime(Sys.time(), t_fit, units = "secs"))
cf <- coef(mod_se, simplify = TRUE)[[ref_group]]$items
refit_check <- c(max_abs_a = max(abs(cf[bank$item, "a1"] - bank$a)), max_abs_d = max(abs(cf[bank$item, "d"] - bank$d)),
                 converged = extract.mirt(mod_se, "converged"), secondordertest = extract.mirt(mod_se, "secondordertest"))
stopifnot(refit_check[["max_abs_a"]] < 1e-4, refit_check[["max_abs_d"]] < 1e-4)
V <- vcov(mod_se)
mv <- mod2values(mod_se) |> filter(group == ref_group)
vc_par <- as.integer(str_match(colnames(V), "^[^.]+\\.(\\d+)")[, 2])
vc_name <- str_match(colnames(V), "^([^.]+)\\.")[, 2]
vc_item <- mv$item[match(vc_par, mv$parnum)]
idx_a <- match(bank$item, vc_item[vc_name == "a1"]); idx_d <- match(bank$item, vc_item[vc_name == "d"])
ia <- which(vc_name == "a1")[idx_a]; id <- which(vc_name == "d")[idx_d]
stopifnot(!anyNA(ia), !anyNA(id))
Vb <- V[c(ia, id), c(ia, id)]
se_tab <- bank |> transmute(item, story, a, d, n_responses, se_a = sqrt(diag(Vb))[seq_len(nrow(bank))],
                            se_d = sqrt(diag(Vb))[nrow(bank) + seq_len(nrow(bank))])
R_chol <- chol((Vb + t(Vb)) / 2)
tick("Q5 simulation")
q5_rows <- list(); q5_expo <- list()
q5_pops <- pop_ids
q5_cell <- 0L
for (b in seq_len(B_Q5)) {
  set.seed(seed0 + 100 + b)
  z <- drop(rnorm(ncol(Vb)) %*% R_chol)
  for (infl in c(1, 2)) {
    a_est <- pmax(bank$a + infl * z[seq_len(nrow(bank))], 0.02)
    d_est <- bank$d + infl * z[nrow(bank) + seq_len(nrow(bank))]
    ps_est <- make_ps(a_est, d_est, bank$g)
    for (pid in q5_pops) {
      pp <- make_pop(pid, N_Q5, seed0 + 200 + b)
      dns <- if (infl == 1) c("cur3", "best4", "free4") else "best4"
      for (dn in dns) for (r in c("random", "maxinfo", "infomin", "rq2")) {
        q5_cell <- q5_cell + 1L
        set.seed(seed0 + 100000 + q5_cell)
        est <- simulate(pp, designs[[dn]]$steps, r, sel = ps_est, score = list(est = ps_est, true = ps_true))
        set.seed(seed0 + 100000 + q5_cell)     # same random draws as the online run (paired)
        orc <- simulate(pp, designs[[dn]]$steps, r, sel = ps_true, score = list(true = ps_true))
        de <- est$df; do <- orc$df
        q5_rows[[length(q5_rows) + 1]] <- tibble(bank = b, se_inflation = infl, dataset = pid, label = pop_lab[pid],
          design = dn, rule = r, s2 = pp$s2,
          online_predicted = 1 - mean(de$psd2_est) / pp$s2,
          online_realized = 1 - mean((de$eap_est - de$theta)^2) / pp$s2,
          online_bias = mean(de$eap_est - de$theta),
          rescored_model = 1 - mean(de$psd2_true) / pp$s2,
          rescored_realized = 1 - mean((de$eap_true - de$theta)^2) / pp$s2,
          oracle_model = 1 - mean(do$psd2_true) / pp$s2,
          oracle_realized = 1 - mean((do$eap_true - do$theta)^2) / pp$s2,
          same_stories = mean(rowSums(est$chosen == orc$chosen) == ncol(est$chosen)),
          max_exposure = max(tabulate(est$chosen, 18)) / N_Q5)
        if (infl == 1) q5_expo[[length(q5_expo) + 1]] <- tibble(bank = b, dataset = pid, design = dn, rule = r,
          story = 1:18, exposure_est = tabulate(est$chosen, 18) / N_Q5, exposure_true = tabulate(orc$chosen, 18) / N_Q5)
      }
    }
  }
  if (b %% 10 == 0) tick("  bank ", b)
}
q5_raw <- bind_rows(q5_rows)
q5 <- q5_raw |>
  group_by(se_inflation, dataset, label, design, rule) |>
  summarise(n_banks = n(),
            across(c(online_predicted, online_realized, online_bias, rescored_model, rescored_realized,
                     oracle_model, oracle_realized, same_stories, max_exposure), mean),
            capitalization = mean(online_predicted - online_realized),
            capitalization_sd_banks = sd(online_predicted - online_realized),
            selection_loss = mean(oracle_realized - rescored_realized),
            selection_loss_sd_banks = sd(oracle_realized - rescored_realized),
            rescoring_gain = mean(rescored_realized - online_realized), .groups = "drop")
q5_expo <- bind_rows(q5_expo) |>
  group_by(dataset, design, rule, story) |>
  summarise(exposure_est = mean(exposure_est), exposure_true = mean(exposure_true),
            exposure_est_sd_banks = sd(exposure_est), .groups = "drop")

# ---- Q6: cross-language non-invariance --------------------------------------------------------------------------
# Per-group item difficulties from the partial-scalar Rasch+g invariance fits (analysis A: stories
# 1-6, de-DE / en-US / es-CO; analysis B: stories 13-18, de-DE / es-AR); anchors are equal by
# construction. A difficulty difference is a logit shift at fixed theta, applied to the 2PL
# intercept. The pooled analysis estimate is taken to be the response-weighted average of the groups'
# versions (weights = each language's responses to the item in the pooled calibration data), so
# group g's intercept is d_pooled - (delta_g - weighted mean delta). Boundary estimates (|delta| > 6,
# the es-AR false-belief items of stories 13 and 14, at chance in es-AR) are capped at 6 logits;
# variant "boundary_pooled" gives those two items no shift at all (the truth lies between).
# Items without group estimates keep the pooled intercept. Groups: es-AR (UTDT-AR population), es-CO
# (Bogota) and de-DE (Leipzig). For stories 13-18 about 89% of the pooled calibration responses are
# es-AR, so there the pooled estimates are close to es-AR's and DE is the group furthest from them.

tick("Q6 DIF")
dif_raw <- inv$dif |>
  filter(analysis %in% c("A", "B")) |>
  transmute(analysis, story_uid = item, boundary, class,
            `de-DE` = 0, `en-US` = b_en_US - b_de_DE, `es-CO` = b_es_CO - b_de_DE, `es-AR` = b_es_AR - b_de_DE) |>
  pivot_longer(c(`de-DE`, `en-US`, `es-CO`, `es-AR`), names_to = "language", values_to = "delta") |>
  filter(!is.na(delta)) |>
  mutate(capped = abs(delta) > 6, delta = pmax(pmin(delta, 6), -6))
cal_counts <- tr |>
  filter(!policy_exclude, dataset %in% pop_ids) |>
  count(story_uid, language, name = "n_cal")
dif_w <- dif_raw |>
  left_join(cal_counts, by = c("story_uid", "language")) |>
  mutate(n_cal = coalesce(n_cal, 0L)) |>
  group_by(analysis, story_uid) |>
  mutate(w = n_cal / sum(n_cal), delta_pooled = sum(w * delta), delta_rel = delta - delta_pooled) |>
  ungroup()
dif_items <- dif_w |>
  mutate(item = paste0(story_uid, "-1")) |>
  filter(item %in% bank$item)
make_group_ps <- function(lang, variant = "cap6") {
  sh <- dif_items |> filter(language == lang)
  # sensitivity: the two boundary items (es-AR at chance) carry no shift for any group
  if (variant == "boundary_pooled") sh$delta_rel[sh$boundary] <- 0
  d_g <- bank$d - coalesce(sh$delta_rel[match(bank$item, sh$item)], 0)
  make_ps(bank$a, d_g, bank$g)
}
q6_groups <- tibble(dataset = c("rfp1_utdt_ar_main", "pilot_uniandes_co_bogota", "pilot_mpieva_de_main"),
                    language = c("es-AR", "es-CO", "de-DE"))
# check of the generators: observed accuracy in each dataset vs the accuracy each parameter version
# implies for that dataset's latent distribution (items with a shift of at least .25 logits). Rough:
# the children who answered an item are not a random sample of the dataset.
q6_check <- map_dfr(seq_len(nrow(q6_groups)), \(k) {
  pid <- q6_groups$dataset[k]; lang <- q6_groups$language[k]
  p <- pops[pops$dataset == pid, ]
  w <- dnorm(theta_q, p$mu, sqrt(p$s2)); w <- w / sum(w)
  its <- dif_items |> filter(language == lang, abs(delta_rel) >= .25) |> pull(item)
  js <- match(its, bank$item)
  expp <- \(ps) drop(w %*% exp(ps$logP[, js, drop = FALSE]))
  obs <- tr |> filter(dataset == pid, !policy_exclude, paste0(story_uid, "-1") %in% its) |>
    group_by(item = paste0(story_uid, "-1")) |> summarise(n_obs = n(), p_obs = mean(correct), .groups = "drop")
  tibble(dataset = pid, label = pop_lab[pid], language = lang, item = its,
         p_pooled = expp(ps_true), p_cap6 = expp(make_group_ps(lang, "cap6")),
         p_boundary_pooled = expp(make_group_ps(lang, "boundary_pooled"))) |>
    left_join(obs, by = "item")
})
q6_rows <- list(); q6_cbias <- list()
q6_cell <- 0L
for (k in seq_len(nrow(q6_groups))) for (vr in c("cap6", "boundary_pooled")) {
  pid <- q6_groups$dataset[k]; lang <- q6_groups$language[k]
  ps_g <- make_group_ps(lang, vr)
  pp <- make_pop(pid, N_Q6, seed0 + 8)
  for (dn in c("cur3", "canvas4", "best4", "free4")) for (r in c("random", "maxinfo", "infomin", "rq2")) {
    q6_cell <- q6_cell + 1L
    set.seed(seed0 + 200000 + q6_cell)
    on <- simulate(pp, designs[[dn]]$steps, r, sel = ps_true, gen = gen_uni(ps_g),
                   score = list(pooled = ps_true, group = ps_g))
    set.seed(seed0 + 200000 + q6_cell)     # paired random draws
    orc <- simulate(pp, designs[[dn]]$steps, r, sel = ps_g, gen = gen_uni(ps_g), score = list(group = ps_g))
    d1 <- on$df; d2 <- orc$df
    q6_rows[[length(q6_rows) + 1]] <- tibble(dataset = pid, label = pop_lab[pid], language = lang, variant = vr,
      design = dn, rule = r,
      bias_pooled = mean(d1$eap_pooled - d1$theta), bias_rescored = mean(d1$eap_group - d1$theta),
      bias_oracle = mean(d2$eap_group - d2$theta),
      rmse_pooled = sqrt(mean((d1$eap_pooled - d1$theta)^2)), rmse_rescored = sqrt(mean((d1$eap_group - d1$theta)^2)),
      rmse_oracle = sqrt(mean((d2$eap_group - d2$theta)^2)),
      realized_pooled = 1 - mean((d1$eap_pooled - d1$theta)^2) / pp$s2,
      realized_rescored = 1 - mean((d1$eap_group - d1$theta)^2) / pp$s2,
      realized_oracle = 1 - mean((d2$eap_group - d2$theta)^2) / pp$s2,
      predicted_pooled = 1 - mean(d1$psd2_pooled) / pp$s2,
      r_pooled_group = cor(d1$eap_pooled, d1$eap_group),
      share_dif_stories = mean(on$chosen %in% c(1:6, 13:18)),
      same_stories = mean(rowSums(on$chosen == orc$chosen) == ncol(on$chosen)))
    q6_cbias[[length(q6_cbias) + 1]] <- tibble(theta = d1$theta, e_p = d1$eap_pooled, e_g = d1$eap_group) |>
      mutate(theta_bin = cut(theta, seq(-10, 4, 1))) |> filter(!is.na(theta_bin)) |>
      group_by(theta_bin) |>
      summarise(n = n(), bias_pooled = mean(e_p - theta), bias_rescored = mean(e_g - theta), .groups = "drop") |>
      mutate(dataset = pid, label = pop_lab[pid], variant = vr, design = dn, rule = r, .before = 1)
  }
}
q6 <- bind_rows(q6_rows); q6_cbias <- bind_rows(q6_cbias)
q6_shift <- dif_items |>
  select(analysis, item, language, class, boundary, capped, n_cal, w, delta, delta_pooled, delta_rel)

# ---- Q7: local dependence -----------------------------------------------------------------------------------------
# Generating models: story testlet effects gamma_s ~ N(0, 1) with item slopes lambda_i, and general
# slopes/intercepts inflated so that each item's marginal response curve (over gamma) matches the
# unidimensional curve: a* = a k_i, d* = d k_i, k_i = sqrt(1 + 0.345 lambda_i^2) (probit approximation
# of the logistic-normal integral; accuracy checked below). Selection and scoring use the
# unidimensional parameters. Scenarios:
#   prop_s: proportional testlet (Bradlow-type), lambda_i = a*_i s, s = testlet SD on the theta scale
#   bifactor: the story-bifactor specific slopes (dimensionality analysis, fixed-floor 2PL, slope prior
#     N(0, 2^2)), averaged over samples (stories 1-6: Leipzig, Western, Bogota; 13-18: es-AR); stories
#     7-12 borrow the mean of the same question in the parallel stories (s - 6, s + 6); items without an
#     estimate get 0. These are noisy MAP estimates with mixed signs; treat as one scenario, not truth.

tick("Q7 local dependence")
bf <- dimr$irt_slopes |>
  filter(family == "guess", sample %in% c("de_ib", "wca_ib", "bog_ib", "ar_nhb")) |>
  group_by(item) |> summarise(lambda = mean(a_specific), ratio = mean(a_specific / a_general), .groups = "drop")
bank_q <- bank |> mutate(story_uid_ = str_remove(item, "-1$"),
                         q_key = str_remove(story_uid_, "^tom_story\\d+_"))
bf_key <- bf |> mutate(story = as.integer(str_match(item, "story(\\d+)_")[, 2]), q_key = str_remove(item, "^tom_story\\d+_"))
lambda_bf <- map_dbl(seq_len(nrow(bank_q)), \(j) {
  s <- bank_q$story[j]; key <- bank_q$q_key[j]
  own <- bf_key$lambda[bf_key$story == s & bf_key$q_key == key]
  if (length(own) == 1) return(own)
  if (s %in% 7:12) {
    par <- bf_key$lambda[bf_key$story %in% c(s - 6, s + 6) & bf_key$q_key == key]
    if (length(par)) return(mean(par))
  }
  0
})
note("Q7 bifactor scenario: ", sum(lambda_bf == 0), " of 83 items have no specific-slope estimate (set to 0).")
median_abs_ratio <- median(abs(bf$ratio))
make_testlet <- function(lambda) {
  k <- sqrt(1 + 0.345 * lambda^2)
  list(a = bank$a * k, d = bank$d * k, g = bank$g, lambda = lambda)
}
testlet_prop <- function(s) {
  # lambda = a* s and a* = a k, k = sqrt(1 + 0.345 lambda^2)  =>  k = 1 / sqrt(1 - 0.345 a^2 s^2)
  k <- 1 / sqrt(1 - 0.345 * bank$a^2 * s^2)
  list(a = bank$a * k, d = bank$d * k, g = bank$g, lambda = bank$a * k * s)
}
ld_scen <- list(prop_0.3 = testlet_prop(0.3), prop_0.5 = testlet_prop(0.5), prop_0.7 = testlet_prop(0.7),
                bifactor = make_testlet(lambda_bf))
gh_nodes <- local({   # Gauss-Hermite (probabilists') nodes via the Golub-Welsch eigenproblem, 41 points
  n <- 41; b <- sqrt(seq_len(n - 1)); Jm <- matrix(0, n, n); Jm[cbind(1:(n - 1), 2:n)] <- b; Jm[cbind(2:n, 1:(n - 1))] <- b
  e <- eigen(Jm, symmetric = TRUE); list(x = e$values, w = e$vectors[1, ]^2)
})
th_chk <- seq(-8, 4, 0.25)
marg_check <- imap_dfr(ld_scen, \(gs, nm) {
  P_uni <- rep(bank$g, each = length(th_chk)) + rep(1 - bank$g, each = length(th_chk)) *
    plogis(outer(th_chk, bank$a) + rep(bank$d, each = length(th_chk)))
  P_mar <- Reduce(`+`, map(seq_along(gh_nodes$x), \(q) gh_nodes$w[q] * (rep(gs$g, each = length(th_chk)) +
    rep(1 - gs$g, each = length(th_chk)) * plogis(outer(th_chk, gs$a) + rep(gs$d + gs$lambda * gh_nodes$x[q], each = length(th_chk))))))
  tibble(scenario = nm, max_abs_diff = max(abs(P_mar - P_uni)), mean_abs_diff = mean(abs(P_mar - P_uni)),
         median_abs_lambda = median(abs(gs$lambda)), max_abs_lambda = max(abs(gs$lambda)))
})
print(marg_check)
q7_rows <- list()
q7_designs <- c("cur3", "cur3p1", "canvas3", "canvas4", "best3", "best4", "free3", "free4", "fixed_1to6", "fixed_13to18")
for (pid in pop_ids) {
  pp <- make_pop(pid, N_Q7, seed0 + 9)
  for (sc in c("independent", names(ld_scen))) {
    gen <- if (sc == "independent") gen_uni(ps_true) else ld_scen[[sc]]
    for (dn in q7_designs) {
      rl <- if (str_starts(dn, "fixed")) "random" else c("random", "maxinfo", "infomin", "rq2")
      for (r in rl) {
        res <- simulate(pp, designs[[dn]]$steps, r, gen = gen)
        d <- res$df
        q7_rows[[length(q7_rows) + 1]] <- tibble(dataset = pid, label = pop_lab[pid], scenario = sc, design = dn,
          rule = if (str_starts(dn, "fixed")) "fixed" else r,
          predicted = 1 - mean(d$psd2_true) / pp$s2,
          realized = 1 - mean((d$eap_true - d$theta)^2) / pp$s2,
          r2 = cor(d$eap_true, d$theta)^2,
          mse_over_psd2 = mean((d$eap_true - d$theta)^2) / mean(d$psd2_true),
          median_min = median(d$secs) / 60)
      }
    }
  }
  tick("  ", pid)
}
q7 <- bind_rows(q7_rows)
q7_rank <- q7 |>
  filter(!str_starts(design, "fixed")) |>
  group_by(dataset, label, scenario) |>
  summarise(spearman_pred_real = cor(predicted, realized, method = "spearman"),
            top_by_predicted = paste(design, rule)[which.max(predicted)],
            top_by_realized = paste(design, rule)[which.max(realized)], .groups = "drop")

# ---- save ------------------------------------------------------------------------------------------------------------

res_out <- list(
  meta = list(built_at = Sys.time(), script = "tasks/_stories_cat_sim.R", book_root = book_root,
              inputs = tibble(file = c(f_rel, f_inv, f_dim, f_trials, f_runs, f_scores, f_fcat),
                              md5 = unname(tools::md5sum(c(f_rel, f_inv, f_dim, f_trials, f_runs, f_scores, f_fcat)))),
              seed = seed0, sim_scale = sim_scale,
              n = c(main = N_MAIN, search = N_SEARCH, q3 = N_Q3, q4 = N_Q4, q5_per_bank = N_Q5, q5_banks = B_Q5,
                    q6 = N_Q6, q7 = N_Q7),
              theta_grid = range(theta_q), quadpts = Q, bank_items = bank$item, dropped_items = drop_items,
              levantemodels = c(path = lm_path, commit = lm_commit), mirt_version = as.character(packageVersion("mirt")),
              refit_secs = refit_secs, notes = log_notes,
              runtime_min = as.numeric(difftime(Sys.time(), t_start, units = "mins")),
              session = sessionInfo()$R.version$version.string),
  pops = pops, stability_used = stability_used, durations = dur_tab, intro = intro_tab, intro_secs = intro_secs,
  speed = c(tau2_child = tau2_child, sig2_within = sig2_within, rho = rho_speed),
  validation = list(fcat3_random = val, time = val_time),
  story_info = story_info_tab, exp_info = exp_info, disc_vs_info_cor = disc_vs_info_cor,
  search = list(screen = screen, sim = search_sim, rank = search_rank, chosen = best_row$order, tie_band = search_tie_band),
  designs = design_tab, block_balance = balance,
  q1 = q1, q1_csem = q1_csem, q1_exposure = q1_expo,
  q2_agree = q2_agree,
  q3 = q3, q3_first = q3_first,
  q4_two = q4_two, q4_multi = q4_multi, q4_waves_bound = waves_bound,
  q5 = q5, q5_raw = q5_raw, q5_exposure = q5_expo, q5_param_se = se_tab, q5_refit_check = refit_check,
  q6 = q6, q6_cond_bias = q6_cbias, q6_shift = q6_shift, q6_check = q6_check,
  q7 = q7, q7_rank = q7_rank, q7_marginal_check = marg_check, q7_lambda_bifactor = tibble(item = bank$item, lambda = lambda_bf),
  q7_median_abs_ratio_bifactor = median_abs_ratio)
saveRDS(res_out, f_out)
tick("saved ", f_out, " (", round(res_out$meta$runtime_min, 1), " min)")
