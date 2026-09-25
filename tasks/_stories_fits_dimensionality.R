# _stories_fits_dimensionality.R
#
# Dimensionality and local dependence of the Stories (Theory of Mind) task on
# the corrected 2026-09 analysis data (data/stories_2026-09/, built by
# tasks/_build_stories_data.R with levantemodels PR #27).
#
# Re-tests the June 2026 conclusions of old/tom_dimensionality.qmd and
# old/stories_tom.qmd section 5 (retired June chapters) on complete fixed-form blocks:
#   de_ib        Leipzig, item bank (stories 1-6), first run per child
#   de_ib_v2     same, V2 content only (runs from 2024-10-24; sensitivity)
#   de_ib_junelike  Leipzig item bank as the June chapters saw it: all runs,
#                released (pre-PR #27) uids, policy-flagged items kept
#   wca_ib       Western, item bank (stories 1-6), first run per child
#   bog_ib[_nofb]  Bogota, item bank (first Bogota content, stories 1-6),
#                with / without the Spanish reality_known false-belief item
#   ar_nhb[_nofb]  UTDT es-AR, no-HA retest-B form (stories 13-18; both
#                UTDT datasets), with / without the Spanish false-belief item
# Every sample drops policy-flagged items (policy_exclude) except
# de_ib_junelike, then applies the June item filters (coverage > .4,
# .08 < p < .97).
#
# Per sample: tetrachoric eigenvalues, ev1/ev2, parallel analysis (June
# method and a resampling version); within- vs between-group mean inter-item r
# for story / question type / old group / new construct (+ a story x question
# type decomposition and a person bootstrap); 2-factor oblimin EFA; 2PL
# unidimensional vs story-testlet bifactor (and question-type bifactor):
# AIC/BIC, specific/general slope ratio, ECV, omega-h, EAP SE inflation, Q3;
# sub-scales (targets-only, false-belief-only) vs all items, with
# random-subset benchmarks; and a check of the June pooled-form reliability.
#
# Run from a working directory OUTSIDE the book repo (the repo renv has a
# broken mgcv binary that breaks mirt):
#   cd <scratch dir> && Rscript <book root>/tasks/_stories_fits_dimensionality.R [book root]
# The book root defaults to $STORIES_BOOK_ROOT, then the directory above this
# script's tasks/ folder.
# STORIES_QUICK=1 runs a fast smoke test (few bootstrap draws, low EM cap),
# saved to the working directory, never over the real results.
#
# Output: data/stories_2026-09/results_dimensionality.rds (a named list; see
# the `res` block at the end). The chapter only reads that file.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
  library(stringr)
  library(tibble)
  library(psych)
  library(mirt)
})

args <- commandArgs(trailingOnly = TRUE)
script_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
default_root <- if (length(script_file) == 1) dirname(dirname(normalizePath(script_file))) else here::here()
book_root <- if (length(args) >= 1) args[1] else
  Sys.getenv("STORIES_BOOK_ROOT", default_root)
book_root <- normalizePath(book_root, mustWork = TRUE)
if (startsWith(normalizePath(getwd()), book_root))
  stop("Run this script from a working directory outside the book repo ",
       "(its renv breaks mirt); pass the book root as an argument.")

quick   <- Sys.getenv("STORIES_QUICK", "0") == "1"
n_boot  <- if (quick) 5 else 200    # person bootstrap draws (tetrachoric stats)
n_rand  <- if (quick) 20 else 500   # random item subsets (sub-scale benchmark)
n_pa    <- if (quick) 3 else 20     # resampling parallel-analysis draws
ncycles <- if (quick) 500 else 5000 # EM cycle cap for mirt fits

data_dir <- file.path(book_root, "data", "stories_2026-09")
out_file <- if (quick) file.path(getwd(), "results_dimensionality_QUICK.rds") else
  file.path(data_dir, "results_dimensionality.rds")
set.seed(20260924)
# psych::fa.parallel() resamples inside parallel::mclapply(), whose RNG streams
# are not reproducible from set.seed(); one core keeps every draw in this
# process (checked: identical parallel-analysis values across repeat runs).
options(mc.cores = 1)
t_start <- Sys.time()
msg <- function(...) message(format(Sys.time(), "%H:%M:%S "), ...)

trials <- read_rds(file.path(data_dir, "stories_trials.rds"))
data_provenance <- attr(trials, "provenance")

# ---- Samples -----------------------------------------------------------------

first_run_per_child <- function(d) {
  keep <- d |>
    distinct(user_id, run_id, time_started) |>
    group_by(user_id) |>
    slice_min(time_started, n = 1, with_ties = FALSE) |>
    pull(run_id)
  filter(d, run_id %in% keep)
}

v2_start <- as.POSIXct("2024-10-24", tz = "UTC")  # image 4g / V2 content

sample_specs <- list(
  de_ib = list(
    label = "Leipzig, item bank", language = "de-DE", stories = "1-6",
    role = "primary",
    rows = \(t) filter(t, dataset == "pilot_mpieva_de_main", form == "item_bank"),
    first_run = TRUE, drop_policy = TRUE, drop_spanish_fb = FALSE,
    uid = "story_uid"),
  de_ib_v2 = list(
    label = "Leipzig, item bank, V2 content only", language = "de-DE",
    stories = "1-6", role = "sensitivity",
    rows = \(t) filter(t, dataset == "pilot_mpieva_de_main", form == "item_bank",
                       time_started >= v2_start),
    first_run = TRUE, drop_policy = TRUE, drop_spanish_fb = FALSE,
    uid = "story_uid"),
  de_ib_junelike = list(
    label = "Leipzig, item bank, June-style (released uids, all runs, no policy exclusions)",
    language = "de-DE", stories = "1-6", role = "june_replication",
    rows = \(t) filter(t, dataset == "pilot_mpieva_de_main", form == "item_bank"),
    first_run = FALSE, drop_policy = FALSE, drop_spanish_fb = FALSE,
    uid = "story_uid_pre_pr27"),
  wca_ib = list(
    label = "Western, item bank", language = "en-US", stories = "1-6",
    role = "primary",
    rows = \(t) filter(t, dataset == "pilot_western_ca_main", form == "item_bank"),
    first_run = TRUE, drop_policy = TRUE, drop_spanish_fb = FALSE,
    uid = "story_uid"),
  bog_ib = list(
    label = "Bogota, item bank (with Spanish FB item)", language = "es-CO",
    stories = "1-6", role = "primary",
    rows = \(t) filter(t, dataset == "pilot_uniandes_co_bogota", form == "item_bank"),
    first_run = TRUE, drop_policy = TRUE, drop_spanish_fb = FALSE,
    uid = "story_uid"),
  bog_ib_nofb = list(
    label = "Bogota, item bank (without Spanish FB item)", language = "es-CO",
    stories = "1-6", role = "spanish_fb_variant",
    rows = \(t) filter(t, dataset == "pilot_uniandes_co_bogota", form == "item_bank"),
    first_run = TRUE, drop_policy = TRUE, drop_spanish_fb = TRUE,
    uid = "story_uid"),
  ar_nhb = list(
    label = "UTDT es-AR, no-HA retest-B (with Spanish FB item)", language = "es-AR",
    stories = "13-18", role = "primary",
    rows = \(t) filter(t, form == "no_ha_retest_B"),
    first_run = TRUE, drop_policy = TRUE, drop_spanish_fb = FALSE,
    uid = "story_uid"),
  ar_nhb_nofb = list(
    label = "UTDT es-AR, no-HA retest-B (without Spanish FB item)", language = "es-AR",
    stories = "13-18", role = "spanish_fb_variant",
    rows = \(t) filter(t, form == "no_ha_retest_B"),
    first_run = TRUE, drop_policy = TRUE, drop_spanish_fb = TRUE,
    uid = "story_uid")
)

story_groups <- "reality_known|moral_reasoning|interpretation|deception|reference|second_order"
construct_by_story <- trials |> distinct(story, new_construct)

# Item descriptors are parsed from the uid itself so that the released
# (pre-PR #27) uids of de_ib_junelike get descriptors that match their label.
describe_uids <- function(uids) {
  tibble(item = uids) |>
    mutate(story = as.integer(str_extract(item, "(?<=^tom_story)[0-9]+")),
           old_group = str_extract(item, story_groups),
           entry = str_remove(item, paste0("^tom_story[0-9]+_(", story_groups, ")_")),
           question_type = str_remove(entry, "_[0-9]+$"),
           is_control = question_type == "reality_check") |>
    left_join(construct_by_story, by = "story")
}

build_sample <- function(spec) {
  d0 <- spec$rows(trials)
  if (spec$first_run) d0 <- first_run_per_child(d0)
  d0 <- d0 |> mutate(item = .data[[spec$uid]])
  n_runs0 <- n_distinct(d0$run_id)
  removed <- bind_rows(
    d0 |> filter(is.na(item)) |> mutate(reason = "no uid under this identity"),
    if (spec$drop_policy) d0 |> filter(!is.na(item), policy_exclude) |>
      mutate(reason = "policy_exclude"),
    if (spec$drop_spanish_fb) d0 |> filter(!is.na(item), spanish_fb_flag,
                                           !(spec$drop_policy & policy_exclude)) |>
      mutate(reason = "spanish_fb_flag")
  )
  d <- d0 |> filter(!is.na(item))
  if (spec$drop_policy) d <- filter(d, !policy_exclude)
  if (spec$drop_spanish_fb) d <- filter(d, !spanish_fb_flag)
  # first occurrence if a uid repeats within a run (only possible under the
  # released identities, as in the June chapters)
  d <- d |> arrange(run_id, trial_number) |> distinct(run_id, item, .keep_all = TRUE)
  wide <- d |>
    transmute(run_id, item, correct = as.integer(correct)) |>
    pivot_wider(names_from = item, values_from = correct)
  m_all <- as.matrix(select(wide, -run_id)); rownames(m_all) <- wide$run_id
  cov_all <- colMeans(!is.na(m_all)); p_all <- colMeans(m_all, na.rm = TRUE)
  keep <- cov_all > 0.4 & p_all > 0.08 & p_all < 0.97
  m <- m_all[, keep, drop = FALSE]
  m <- m[rowSums(!is.na(m)) > 0, , drop = FALSE]
  m <- m[, order(colnames(m)), drop = FALSE]
  m <- m[, order(as.integer(str_extract(colnames(m), "(?<=story)[0-9]+"))), drop = FALSE]
  modal <- function(x) names(sort(table(x), decreasing = TRUE))[1]
  chance <- d |> group_by(item) |>
    summarise(chance = mean(chance), answer_modal = modal(answer),
              item_original_modal = modal(item_original), .groups = "drop")
  items <- describe_uids(colnames(m_all)) |>
    left_join(chance, by = "item") |>
    mutate(n = colSums(!is.na(m_all))[item], coverage = cov_all[item],
           p = p_all[item],
           kept = item %in% colnames(m),
           drop_reason = case_when(kept ~ NA_character_,
                                   coverage <= 0.4 ~ "coverage <= .4",
                                   p >= 0.97 ~ "p >= .97",
                                   p <= 0.08 ~ "p <= .08"))
  removed_items <- removed |>
    group_by(item, reason) |>
    summarise(trials = n(), runs = n_distinct(run_id), p = mean(correct),
              .groups = "drop")
  ages <- d |> distinct(run_id, age)
  list(spec = spec, m = m, items = items, removed = removed_items,
       age = ages$age[match(rownames(m), ages$run_id)],
       n_runs_input = n_runs0,
       n_children = n_distinct(d$user_id[d$run_id %in% rownames(m)]),
       first = min(d$time_started), last = max(d$time_started),
       datasets = paste(sort(unique(d$dataset)), collapse = ", "))
}

msg("building samples")
samples <- map(sample_specs, build_sample)

# ---- Helpers -----------------------------------------------------------------

tet <- function(m) suppressWarnings(suppressMessages(
  psych::tetrachoric(m, smooth = TRUE)$rho))

pair_table <- function(rho, info) {
  idx <- which(upper.tri(rho), arr.ind = TRUE)
  inf <- info |> select(item, story, question_type, old_group, new_construct)
  tibble(i = colnames(rho)[idx[, 1]], j = colnames(rho)[idx[, 2]],
         r = rho[idx]) |>
    left_join(inf |> rename_with(~ paste0(.x, "_i")), by = c(i = "item_i")) |>
    left_join(inf |> rename_with(~ paste0(.x, "_j")), by = c(j = "item_j")) |>
    mutate(same_story = story_i == story_j,
           same_qtype = question_type_i == question_type_j,
           same_old_group = old_group_i == old_group_j,
           same_construct = new_construct_i == new_construct_j,
           cell = case_when(
             same_story & same_qtype ~ "same story, same question type",
             same_story ~ "same story, different question type",
             same_qtype ~ "different story, same question type",
             TRUE ~ "different story, different question type"))
}

grouping_contrast <- function(pairs, value = "r") {
  ds <- pairs[!pairs$same_story, ]   # question type among different-story pairs only
  bind_rows(
    map_dfr(c(story = "same_story", `question type` = "same_qtype",
              `old group` = "same_old_group", `new construct` = "same_construct"),
            function(col) {
              same <- pairs[[col]]; v <- pairs[[value]]
              tibble(n_within = sum(same), n_between = sum(!same),
                     mean_within = mean(v[same]), mean_between = mean(v[!same]))
            }, .id = "grouping"),
    tibble(grouping = "question type (different stories)", n_within = sum(ds$same_qtype),
           n_between = sum(!ds$same_qtype), mean_within = mean(ds[[value]][ds$same_qtype]),
           mean_between = mean(ds[[value]][!ds$same_qtype]))) |>
    mutate(within_minus_between = mean_within - mean_between)
}

cell_means <- function(pairs, value = "r") {
  bind_rows(
    pairs |> group_by(cell) |>
      summarise(n_pairs = n(), mean = mean(.data[[value]]), .groups = "drop"),
    pairs |> filter(!same_story) |>
      mutate(cell = if_else(same_construct,
                            "different story, same new construct",
                            "different story, different new construct")) |>
      group_by(cell) |>
      summarise(n_pairs = n(), mean = mean(.data[[value]]), .groups = "drop"))
}

pair_lm <- function(pairs) {
  f <- lm(r ~ same_story + same_qtype, data = pairs)
  co <- coef(f)
  c(intercept = unname(co[1]), same_story = unname(co["same_storyTRUE"]),
    same_qtype = unname(co["same_qtypeTRUE"]))
}

eig_stats <- function(rho) {
  ev <- eigen(rho, symmetric = TRUE, only.values = TRUE)$values
  c(ev1 = ev[1], ev2 = ev[2], ev1_ev2 = ev[1] / ev[2], ev1_prop = ev[1] / length(ev))
}

# coefficient alpha from the pairwise covariance matrix (handles planned or
# sporadic missingness); Spearman-Brown projection to k_target items
alpha_pw <- function(m) {
  C <- cov(m, use = "pairwise.complete.obs"); k <- ncol(m)
  k / (k - 1) * (1 - sum(diag(C)) / sum(C))
}
sb_project <- function(a, k, k_target) {
  r1 <- a / (k - (k - 1) * a)
  k_target * r1 / (1 + (k_target - 1) * r1)
}

# ---- 1-3. Eigenvalues, parallel analysis, grouping structure, EFA -------------

classical <- imap(samples, function(s, nm) {
  msg("classical: ", nm, " (", nrow(s$m), " x ", ncol(s$m), ")")
  m <- s$m; info <- s$items |> filter(kept)
  info <- info[match(colnames(m), info$item), ]
  rho <- tet(m)
  ev <- eigen(rho, symmetric = TRUE, only.values = TRUE)$values
  # June method: fa.parallel on the tetrachoric matrix vs normal random data
  pa_june <- suppressWarnings(suppressMessages(
    fa.parallel(rho, n.obs = nrow(m), fa = "fa", plot = FALSE)))
  # resampling version: column-wise resampled binary data, tetrachoric
  pa_rs <- suppressWarnings(suppressMessages(
    fa.parallel(m, cor = "tet", fa = "both", sim = FALSE, n.iter = n_pa,
                plot = FALSE)))
  pairs <- pair_table(rho, info)
  f2 <- suppressWarnings(suppressMessages(
    fa(rho, nfactors = 2, n.obs = nrow(m), rotate = "oblimin", fm = "minres")))
  L <- unclass(f2$loadings)
  flip <- sign(colSums(L)); flip[flip == 0] <- 1
  L <- sweep(L, 2, flip, `*`)
  phi <- f2$Phi[1, 2] * flip[1] * flip[2]
  colnames(L) <- c("F1", "F2")
  # person bootstrap of the tetrachoric-based statistics
  boot <- map_dfr(seq_len(n_boot), function(b) {
    idx <- sample(nrow(m), replace = TRUE)
    rb <- tryCatch(tet(m[idx, , drop = FALSE]), error = function(e) NULL)
    if (is.null(rb) || any(!is.finite(rb))) return(NULL)
    pb <- pair_table(rb, info)
    gc <- grouping_contrast(pb)
    cm <- cell_means(pb)
    tibble(b = b, stat = c("ev1_ev2", paste0("diff: ", gc$grouping),
                           paste0("cell: ", cm$cell),
                           paste0("lm: ", names(pair_lm(pb)))),
           value = c(eig_stats(rb)["ev1_ev2"], gc$within_minus_between,
                     cm$mean, pair_lm(pb)))
  })
  list(rho = rho, ev = ev, pa_june = pa_june, pa_rs = pa_rs, pairs = pairs,
       f2 = f2, L = L, phi = phi, info = info, boot = boot)
})

# ---- 4. IRT: unidimensional vs story-testlet (and question-type) bifactor -----
#
# 2PL with a fixed guessing floor g = the item's chance level (the production
# convention), except g = 0 for items answered below chance in the sample
# (a floor above the observed rate cannot fit). Every slope gets a weak
# normal(0, 2^2) prior (MAP-EM): without it, near-ceiling items with a fixed
# floor drift to divergent slopes and EM never converges; a lognormal prior is
# not usable because it forbids the negative slopes of below-chance items and
# stops EM after 2 cycles. The same prior is used in every model, so the
# comparisons are like for like; AIC/BIC are computed from the log-likelihood
# at the MAP estimates. A no-guessing 2PL is fitted as a robustness check.

slope_prior <- function(sv) {
  is_a <- grepl("^a[0-9]+$", sv$name) & sv$est
  sv$prior.type[is_a] <- "norm"; sv$prior_1[is_a] <- 0; sv$prior_2[is_a] <- 2
  sv
}

fit_2pl <- function(m, guess, model = NULL) {
  tech <- list(NCYCLES = ncycles)
  if (is.null(model)) {
    sv <- mirt(m, 1, itemtype = "2PL", guess = guess, pars = "values")
    mirt(m, 1, itemtype = "2PL", guess = guess, pars = slope_prior(sv),
         verbose = FALSE, technical = tech)
  } else {
    sv <- bfactor(m, model, itemtype = "2PL", guess = guess, pars = "values")
    bfactor(m, model, itemtype = "2PL", guess = guess, pars = slope_prior(sv),
            verbose = FALSE, technical = tech)
  }
}

group_vector <- function(x) {
  tab <- table(x)
  x[!(x %in% names(tab)[tab >= 2])] <- NA
  as.integer(factor(x))
}

# standardized loadings (mirt's D = 1.702 normal-ogive metric)
std_loadings <- function(a) {
  a <- a / 1.702
  a / sqrt(1 + rowSums(a^2))
}

q3_resid <- function(mod, m, theta) {
  P <- sapply(seq_len(ncol(m)), function(j)
    probtrace(extract.item(mod, j), matrix(theta))[, 2])
  R <- m - P
  colnames(R) <- colnames(m)
  R
}
# Grouping contrasts on Q3 (residual correlations after the unidimensional 2PL + g,
# i.e. with difficulty and guessing modelled), with a person bootstrap that resamples
# rows of the residual matrix (item parameters and theta held fixed). Uses its own
# seed so the main random stream (and the other bootstrap results) is unchanged.
q3_boot <- function(R, info, n) withr::with_seed(20260925, map_dfr(seq_len(n), function(b) {
  idx <- sample(nrow(R), replace = TRUE)
  qb <- suppressWarnings(cor(R[idx, , drop = FALSE], use = "pairwise.complete.obs"))
  if (any(!is.finite(qb[upper.tri(qb)]))) return(NULL)
  gc <- grouping_contrast(pair_table(qb, info))
  cm <- cell_means(pair_table(qb, info))
  tibble(b = b, stat = c(paste0("diff: ", gc$grouping), paste0("cell: ", cm$cell)),
         value = c(gc$within_minus_between, cm$mean))
}))

fit_summary <- function(mod) {
  tibble(logLik = extract.mirt(mod, "logLik"), AIC = extract.mirt(mod, "AIC"),
         BIC = extract.mirt(mod, "BIC"), n_par = extract.mirt(mod, "nest"),
         converged = extract.mirt(mod, "converged"),
         iterations = extract.mirt(mod, "iterations"))
}

irt <- imap(samples, function(s, nm) {
  m <- s$m; info <- classical[[nm]]$info
  p <- colMeans(m, na.rm = TRUE)
  below <- p <= info$chance
  out <- list()
  for (fam in c("guess", "noguess")) {
    msg("irt: ", nm, " / ", fam)
    g <- if (fam == "guess") ifelse(below, 0, info$chance) else rep(0, ncol(m))
    st <- group_vector(info$story)
    qt <- group_vector(info$question_type)
    uni <- fit_2pl(m, g)
    bf  <- fit_2pl(m, g, st)
    bq  <- fit_2pl(m, g, qt)
    fu <- fscores(uni, method = "EAP", full.scores.SE = TRUE)
    fb <- fscores(bf, method = "EAP", full.scores.SE = TRUE, QMC = TRUE)
    cu <- coef(uni, simplify = TRUE)$items
    cb <- coef(bf, simplify = TRUE)$items
    a_cols <- grep("^a[0-9]+$", colnames(cb), value = TRUE)
    a_s <- if (length(a_cols) > 1)
      apply(cb[, a_cols[-1], drop = FALSE], 1, \(r) r[which.max(abs(r))]) else
        rep(0, nrow(cb))
    lam <- std_loadings(cb[, a_cols, drop = FALSE])
    lam_g <- lam[, 1]
    lam_s <- if (ncol(lam) > 1) rowSums(lam[, -1, drop = FALSE]) else rep(0, nrow(lam))
    h2 <- rowSums(lam^2)
    spec_sums <- tapply(lam_s, st, sum)
    omega_t <- (sum(lam_g)^2 + sum(spec_sums^2, na.rm = TRUE)) /
      (sum(lam_g)^2 + sum(spec_sums^2, na.rm = TRUE) + sum(1 - h2))
    omega_h <- sum(lam_g)^2 /
      (sum(lam_g)^2 + sum(spec_sums^2, na.rm = TRUE) + sum(1 - h2))
    n_s <- table(st); k <- ncol(m)
    puc <- 1 - sum(choose(n_s, 2)) / choose(k, 2)
    has_s <- !is.na(st)
    R_uni <- q3_resid(uni, m, fu[, "F1"])
    q3 <- suppressWarnings(cor(R_uni, use = "pairwise.complete.obs"))
    q3_pairs <- pair_table(q3, info) |> rename(q3 = r)
    q3b <- if (fam == "guess") q3_boot(R_uni, info, n_boot) else NULL
    info_uni <- mean(1 / fu[, "SE_F1"]^2 - 1)
    info_g   <- mean(1 / fb[, "SE_G"]^2 - 1)
    ok_age <- !is.na(s$age)
    out[[fam]] <- list(
      fits = bind_rows(fit_summary(uni) |> mutate(model = "unidimensional"),
                       fit_summary(bf) |> mutate(model = "story bifactor"),
                       fit_summary(bq) |> mutate(model = "question-type bifactor")),
      ld = tibble(
        n_items = k, n_persons = nrow(m),
        n_story_factors = length(n_s), items_with_story_factor = sum(has_s),
        dAIC_story = extract.mirt(bf, "AIC") - extract.mirt(uni, "AIC"),
        dBIC_story = extract.mirt(bf, "BIC") - extract.mirt(uni, "BIC"),
        dAIC_qtype = extract.mirt(bq, "AIC") - extract.mirt(uni, "AIC"),
        dBIC_qtype = extract.mirt(bq, "BIC") - extract.mirt(uni, "BIC"),
        median_as_ag = median(abs(a_s[has_s]) / abs(cb[has_s, "a1"])),
        median_as_ag_3plus = {
          big <- has_s & st %in% as.integer(names(n_s)[n_s >= 3])
          median(abs(a_s[big]) / abs(cb[big, "a1"]))
        },
        ECV = sum(lam_g^2) / sum(lam_g^2 + lam_s^2),
        PUC = puc, omega_total = omega_t, omega_h = omega_h,
        mean_se_uni = mean(fu[, "SE_F1"]), mean_se_general = mean(fb[, "SE_G"]),
        se_ratio = mean(fb[, "SE_G"]) / mean(fu[, "SE_F1"]),
        info_overstatement = info_uni / info_g - 1,
        rxx_uni = unname(empirical_rxx(fu)),
        rxx_general = unname(empirical_rxx(fb[, c("G", "SE_G")])),
        cor_theta_uni_general = cor(fu[, "F1"], fb[, "G"]),
        age_r_uni = if (sum(ok_age) > 10) cor(fu[ok_age, "F1"], s$age[ok_age]) else NA,
        age_r_general = if (sum(ok_age) > 10) cor(fb[ok_age, "G"], s$age[ok_age]) else NA,
        q3_within_story = mean(q3_pairs$q3[q3_pairs$same_story]),
        q3_between_story = mean(q3_pairs$q3[!q3_pairs$same_story]),
        n_within_q3_gt_.2 = sum(q3_pairs$q3[q3_pairs$same_story] > 0.2),
        n_within_pairs = sum(q3_pairs$same_story),
        items_g0_below_chance = if (fam == "guess") paste(info$item[below], collapse = ", ") else NA_character_),
      slopes = tibble(item = colnames(m), story = info$story,
                      question_type = info$question_type, p = p, g = g,
                      a_uni = cu[, "a1"], d_uni = cu[, "d"],
                      a_general = cb[, "a1"], a_specific = a_s,
                      lambda_general = lam_g, lambda_specific = lam_s,
                      has_story_factor = has_s),
      q3_pairs = q3_pairs,
      q3_boot = q3b,
      scores = tibble(run_id = rownames(m), age = s$age,
                      theta_uni = fu[, "F1"], se_uni = fu[, "SE_F1"],
                      theta_general = fb[, "G"], se_general = fb[, "SE_G"]))
  }
  out
})

# ---- 5. Sub-scales ------------------------------------------------------------

subscales <- imap(samples, function(s, nm) {
  msg("subscales: ", nm)
  m <- s$m; info <- classical[[nm]]$info; age <- s$age
  sets <- list(`all items` = info$item,
               `targets only` = info$item[!info$is_control],
               `false-belief only` = info$item[info$question_type == "false_belief"],
               `controls only` = info$item[info$is_control])
  k_all <- ncol(m)
  score_stats <- function(items) {
    mm <- m[, items, drop = FALSE]
    pc <- rowMeans(mm, na.rm = TRUE)
    ok <- !is.na(age) & is.finite(pc)
    c(alpha = alpha_pw(mm), age_r = cor(pc[ok], age[ok]))
  }
  tab <- imap_dfr(sets, function(items, lab) {
    if (length(items) < 3) return(tibble(scale = lab, n_items = length(items)))
    mm <- m[, items, drop = FALSE]
    mm <- mm[rowSums(!is.na(mm)) > 0, , drop = FALSE]
    rho <- tet(mm); e <- eigen(rho, symmetric = TRUE, only.values = TRUE)$values
    fit <- mirt(mm, 1, itemtype = "Rasch", verbose = FALSE,
                technical = list(NCYCLES = 3000))
    fit2 <- fit_2pl(mm, rep(0, ncol(mm)))
    st <- score_stats(items)
    pc <- rowMeans(m[, items, drop = FALSE], na.rm = TRUE)
    ok <- !is.na(age) & is.finite(pc)
    ct <- cor.test(pc[ok], age[ok])
    rand <- map_dfr(seq_len(n_rand), \(i) {
      x <- score_stats(sample(colnames(m), length(items)))
      tibble(alpha = x["alpha"], age_r = x["age_r"])
    })
    tibble(scale = lab, n_items = length(items), ev1_ev2 = e[1] / e[2],
           rasch_marginal_rxx = as.numeric(marginal_rxx(fit)),
           twopl_marginal_rxx = as.numeric(marginal_rxx(fit2)),
           alpha = st["alpha"],
           alpha_sb_to_all = sb_project(st["alpha"], length(items), k_all),
           age_r = st["age_r"], age_r_lo = ct$conf.int[1], age_r_hi = ct$conf.int[2],
           n_age = sum(ok),
           rand_alpha_mean = mean(rand$alpha), rand_age_r_mean = mean(rand$age_r),
           rand_age_r_q025 = quantile(rand$age_r, .025),
           rand_age_r_q975 = quantile(rand$age_r, .975),
           pct_rand_age_r_below = mean(rand$age_r < st["age_r"]),
           pct_rand_alpha_below = mean(rand$alpha < st["alpha"]))
  })
  # a random subset of all items is the full scale: no benchmark
  tab |> mutate(across(starts_with(c("rand_", "pct_rand_")),
                       ~ if_else(scale == "all items", NA_real_, .x)))
})

# ---- 6. June pooled-form reliability check -------------------------------------
# old/stories_tom.qmd (C) reported Rasch marginal reliability .915 for DE on a
# 58-item matrix pooling every DE form (released uids, all runs, items with
# > 15% coverage). mirt::marginal_rxx() integrates the information of ALL
# items in the model, i.e. of a test nobody took. Recompute that number the
# June way on the forms that existed in June, next to the empirical
# reliability of the EAP scores (the items each child actually answered).

msg("june pooled-form check")
june_pool <- trials |>
  filter(dataset == "pilot_mpieva_de_main",
         form %in% c("item_bank", "retest_A", "retest_B"),
         !is.na(story_uid_pre_pr27)) |>
  arrange(run_id, trial_number) |>
  distinct(run_id, story_uid_pre_pr27, .keep_all = TRUE) |>
  transmute(run_id, item = story_uid_pre_pr27, correct = as.integer(correct)) |>
  pivot_wider(names_from = item, values_from = correct)
jp <- as.matrix(select(june_pool, -run_id))
jp <- jp[, colMeans(!is.na(jp)) > 0.15, drop = FALSE]
jp <- jp[, apply(jp, 2, \(x) length(unique(x[!is.na(x)])) > 1), drop = FALSE]
jp <- jp[rowSums(!is.na(jp)) > 0, , drop = FALSE]
jp_fit <- mirt(jp, 1, itemtype = "Rasch", verbose = FALSE,
               technical = list(NCYCLES = 3000))
jp_sc <- fscores(jp_fit, method = "EAP", full.scores.SE = TRUE)
june_pool_check <- tibble(
  runs = nrow(jp), items_in_model = ncol(jp),
  items_per_run_median = median(rowSums(!is.na(jp))),
  rasch_marginal_rxx_june_method = as.numeric(marginal_rxx(jp_fit)),
  rasch_empirical_rxx_eap = unname(empirical_rxx(jp_sc)),
  june_reported = 0.915)

# ---- Assemble -----------------------------------------------------------------

msg("assembling")
sample_table <- imap_dfr(samples, function(s, nm) {
  tibble(sample = nm, label = s$spec$label, role = s$spec$role,
         language = s$spec$language, stories = s$spec$stories,
         datasets = s$datasets, identity = s$spec$uid,
         first_run_per_child = s$spec$first_run,
         policy_items_dropped = s$spec$drop_policy,
         spanish_fb_dropped = s$spec$drop_spanish_fb,
         runs = nrow(s$m), children = s$n_children, items = ncol(s$m),
         items_per_run_median = median(rowSums(!is.na(s$m))),
         cells_missing = mean(is.na(s$m)),
         n_age = sum(!is.na(s$age)),
         age_min = suppressWarnings(min(s$age, na.rm = TRUE)),
         age_median = median(s$age, na.rm = TRUE),
         age_max = suppressWarnings(max(s$age, na.rm = TRUE)),
         first_run = as.Date(s$first), last_run = as.Date(s$last))
})

item_info <- imap_dfr(samples, \(s, nm) s$items |> mutate(sample = nm, .before = 1))
removed_items <- imap_dfr(samples, \(s, nm) s$removed |> mutate(sample = nm, .before = 1))

# Person-bootstrap summaries. Tetrachoric-based statistics are biased under
# resampling (duplicated rows add noise; e.g. ev2 is inflated, so ev1/ev2 is
# biased down), so intervals are estimate +/- 1.96 bootstrap SD, and the
# bootstrap bias (boot mean - estimate) is kept for inspection.
boot_ci <- function(bt, stat, est) {
  est <- unname(est)
  v <- bt$value[bt$stat == stat]
  sd_ <- sd(v, na.rm = TRUE)
  c(lo = est - 1.96 * sd_, hi = est + 1.96 * sd_, se = sd_,
    bias = mean(v, na.rm = TRUE) - est, n = sum(!is.na(v)))
}

eigen_table <- imap_dfr(classical, function(cl, nm) {
  es <- eig_stats(cl$rho); ci <- boot_ci(cl$boot, "ev1_ev2", es[["ev1_ev2"]])
  tibble(sample = nm, n_items = ncol(cl$rho), n_persons = nrow(samples[[nm]]$m),
         ev1 = es["ev1"], ev2 = es["ev2"], ev3 = cl$ev[3],
         ev1_ev2 = es["ev1_ev2"], ev1_ev2_se = ci["se"], ev1_ev2_boot_bias = ci["bias"],
         ev1_prop = es["ev1_prop"], n_ev_gt1 = sum(cl$ev > 1),
         pa_june_nfact = cl$pa_june$nfact,
         pa_resample_nfact = cl$pa_rs$nfact, pa_resample_ncomp = cl$pa_rs$ncomp,
         mean_offdiag_r = mean(cl$rho[upper.tri(cl$rho)]))
})

scree <- imap_dfr(classical, function(cl, nm) {
  k <- length(cl$ev)
  tibble(sample = nm, component = seq_len(k), eigenvalue = cl$ev,
         pa_resampled_pc = cl$pa_rs$pc.simr[seq_len(k)],
         fa_eigenvalue = cl$pa_rs$fa.values[seq_len(k)],
         pa_resampled_fa = cl$pa_rs$fa.simr[seq_len(k)])
})

grouping_contrast_tbl <- imap_dfr(classical, function(cl, nm) {
  grouping_contrast(cl$pairs) |>
    mutate(sample = nm, .before = 1) |>
    rowwise() |>
    mutate(ci = list(boot_ci(cl$boot, paste0("diff: ", grouping), within_minus_between)),
           diff_lo = ci["lo"], diff_hi = ci["hi"], diff_boot_bias = ci["bias"]) |>
    ungroup() |> select(-ci)
})

grouping_cells <- imap_dfr(classical, function(cl, nm) {
  q3p <- irt[[nm]]$guess$q3_pairs
  cell_means(cl$pairs) |>
    rename(mean_r = mean) |>
    left_join(cell_means(q3p, "q3") |> select(cell, mean_q3 = mean), by = "cell") |>
    mutate(sample = nm, .before = 1) |>
    rowwise() |>
    mutate(ci = list(boot_ci(cl$boot, paste0("cell: ", cell), mean_r)),
           r_lo = ci["lo"], r_hi = ci["hi"], r_boot_bias = ci["bias"]) |>
    ungroup() |> select(-ci)
})

# the same contrasts on Q3 residual correlations (2PL + g, unidimensional)
q3_grouping_contrast <- imap_dfr(irt, function(x, nm) {
  g <- x$guess
  grouping_contrast(g$q3_pairs, "q3") |>
    mutate(sample = nm, .before = 1) |>
    rowwise() |>
    mutate(ci = list(boot_ci(g$q3_boot, paste0("diff: ", grouping), within_minus_between)),
           diff_lo = ci["lo"], diff_hi = ci["hi"], diff_boot_bias = ci["bias"],
           n_boot_ok = ci["n"]) |>
    ungroup() |> select(-ci)
})
q3_grouping_cells <- imap_dfr(irt, function(x, nm) {
  g <- x$guess
  cell_means(g$q3_pairs, "q3") |>
    rename(mean_q3 = mean) |>
    mutate(sample = nm, .before = 1) |>
    rowwise() |>
    mutate(ci = list(boot_ci(g$q3_boot, paste0("cell: ", cell), mean_q3)),
           q3_lo = ci["lo"], q3_hi = ci["hi"]) |>
    ungroup() |> select(-ci)
})

grouping_lm <- imap_dfr(classical, function(cl, nm) {
  co <- pair_lm(cl$pairs)
  tibble(sample = nm, term = names(co), estimate = co) |>
    rowwise() |>
    mutate(ci = list(boot_ci(cl$boot, paste0("lm: ", term), estimate)),
           lo = ci["lo"], hi = ci["hi"], boot_bias = ci["bias"]) |>
    ungroup() |> select(-ci)
})

pairs_all <- imap_dfr(classical, function(cl, nm) {
  cl$pairs |>
    left_join(irt[[nm]]$guess$q3_pairs |> select(i, j, q3), by = c("i", "j")) |>
    mutate(sample = nm, .before = 1)
})

efa2_summary <- imap_dfr(classical, function(cl, nm) {
  tibble(sample = nm, factor_r = cl$phi,
         prop_var = sum(cl$f2$Vaccounted["Proportion Var", ]),
         n_items = ncol(cl$rho))
})
efa2_loadings <- imap_dfr(classical, function(cl, nm) {
  as_tibble(cl$L, rownames = "item") |>
    left_join(cl$info |> select(item, story, question_type, old_group, is_control),
              by = "item") |>
    mutate(sample = nm, .before = 1)
})
efa2_by_qtype <- efa2_loadings |>
  group_by(sample, question_type) |>
  summarise(n = n(), F1 = mean(F1), F2 = mean(F2), .groups = "drop")
efa2_by_story <- efa2_loadings |>
  group_by(sample, story) |>
  summarise(n = n(), F1 = mean(F1), F2 = mean(F2), .groups = "drop")

irt_fits <- imap_dfr(irt, \(x, nm) imap_dfr(x, \(y, fam)
  y$fits |> mutate(sample = nm, family = fam, .before = 1)))
irt_ld <- imap_dfr(irt, \(x, nm) imap_dfr(x, \(y, fam)
  y$ld |> mutate(sample = nm, family = fam, .before = 1)))
irt_slopes <- imap_dfr(irt, \(x, nm) imap_dfr(x, \(y, fam)
  y$slopes |> mutate(sample = nm, family = fam, .before = 1)))
irt_scores <- imap_dfr(irt, \(x, nm) x$guess$scores |> mutate(sample = nm, .before = 1))

ld_by_story <- pairs_all |>
  filter(same_story) |>
  group_by(sample, story = story_i) |>
  summarise(n_pairs = n(), mean_r = mean(r), mean_q3 = mean(q3),
            max_q3 = max(q3), max_q3_pair = paste(i, j, sep = " & ")[which.max(q3)],
            .groups = "drop") |>
  left_join(irt_slopes |> filter(family == "guess", has_story_factor) |>
              group_by(sample, story) |>
              summarise(n_items = n(),
                        median_abs_a_general = median(abs(a_general)),
                        median_abs_a_specific = median(abs(a_specific)),
                        .groups = "drop"),
            by = c("sample", "story"))

# item pairs with residual dependence Q3 > .2 (2PL+g unidimensional model)
ld_pairs_top <- pairs_all |>
  filter(q3 > 0.2) |>
  left_join(item_info |> select(sample, item, answer_i = answer_modal,
                                code_i = item_original_modal, p_i = p),
            by = c("sample", i = "item")) |>
  left_join(item_info |> select(sample, item, answer_j = answer_modal,
                                code_j = item_original_modal, p_j = p),
            by = c("sample", j = "item")) |>
  select(sample, i, j, same_story, same_qtype, r, q3, code_i, answer_i, p_i,
         code_j, answer_j, p_j) |>
  arrange(sample, desc(q3))

subscale_tbl <- imap_dfr(subscales, \(x, nm) x |> mutate(sample = nm, .before = 1))

# ---- June reference numbers ---------------------------------------------------
# Transcribed from the frozen renders of the June chapters
# (_freeze/tasks/tom_dimensionality, _freeze/tasks/stories_tom) and from the
# 2026-09-24 audit (D_blockcat finding F11, v2_3 calibration data).

june_reference <- tribble(
  ~analysis, ~metric, ~june_value, ~source,
  "eigen", "n_items (DE, coverage > .4, .08 < p < .97)", 26, "tom_dimensionality",
  "eigen", "ev1", 5.56, "tom_dimensionality",
  "eigen", "ev2", 2.92, "tom_dimensionality",
  "eigen", "ev1_ev2", 1.90, "tom_dimensionality",
  "eigen", "parallel analysis nfact (June method)", 11, "tom_dimensionality",
  "eigen", "ev1 (DE, 'well-covered', 58-item pool)", 6.52, "stories_tom s5",
  "eigen", "ev2 (DE, 'well-covered', 58-item pool)", 3.33, "stories_tom s5",
  "grouping", "question type: within", 0.206, "tom_dimensionality",
  "grouping", "question type: between", 0.105, "tom_dimensionality",
  "grouping", "question type: within - between", 0.101, "tom_dimensionality",
  "grouping", "new construct: within", 0.172, "tom_dimensionality",
  "grouping", "new construct: between", 0.127, "tom_dimensionality",
  "grouping", "new construct: within - between", 0.045, "tom_dimensionality",
  "grouping", "old group: within", 0.171, "tom_dimensionality",
  "grouping", "old group: between", 0.132, "tom_dimensionality",
  "grouping", "old group: within - between", 0.039, "tom_dimensionality",
  "grouping", "story: within", 0.171, "tom_dimensionality",
  "grouping", "story: between", 0.132, "tom_dimensionality",
  "grouping", "story: within - between", 0.039, "tom_dimensionality",
  "efa2", "factor r", 0.19, "tom_dimensionality",
  "efa2", "proportion variance", 0.28, "tom_dimensionality",
  "efa2", "emotion_reasoning (n=5) F1/F2", NA, "tom_dimensionality: 0.35 / 0.15",
  "efa2", "false_belief (n=13) F1/F2", NA, "tom_dimensionality: 0.32 / 0.30",
  "efa2", "reality_check (n=7) F1/F2", NA, "tom_dimensionality: 0.21 / 0.16",
  "efa2", "reference (n=1) F1/F2", NA, "tom_dimensionality: -0.11 / -0.32",
  "subscale", "all items: n / ev1_ev2 / Rasch rxx / age r", NA, "tom_dimensionality: 26 / 1.901 / 0.842 / 0.402",
  "subscale", "targets only: n / ev1_ev2 / Rasch rxx / age r", NA, "tom_dimensionality: 19 / 2.248 / 0.773 / 0.386",
  "subscale", "false-belief only: n / ev1_ev2 / Rasch rxx / age r", NA, "tom_dimensionality: 13 / 1.977 / 0.605 / 0.401",
  "subscale", "all items (58, all DE forms): Rasch rxx / age r", NA, "stories_tom (C): 0.915 / 0.406",
  "subscale", "targets only (42, all DE forms): Rasch rxx / age r", NA, "stories_tom (C): 0.885 / 0.416",
  "local_dependence", "Q3 within-story mean", 0.013, "Sept audit F11 (v2_3 calib., stories 1-6, 774 runs, 28 items)",
  "local_dependence", "Q3 between-story mean", -0.023, "Sept audit F11",
  "local_dependence", "AIC uni", 16832, "Sept audit F11",
  "local_dependence", "AIC story bifactor", 16671, "Sept audit F11",
  "local_dependence", "BIC uni", 17092, "Sept audit F11",
  "local_dependence", "BIC story bifactor", 17061, "Sept audit F11",
  "local_dependence", "median |a_specific| / a_general", 0.43, "Sept audit F11",
  "local_dependence", "mean EAP SE uni", 0.417, "Sept audit F11",
  "local_dependence", "mean EAP SE general (bifactor)", 0.440, "Sept audit F11",
  "local_dependence", "information overstatement (uni vs bifactor)", 0.11, "Sept audit F11 (approx.)"
)

# ---- Headline comparison (June vs now) -----------------------------------------

hl <- function(sample_) {
  e <- eigen_table |> filter(sample == sample_)
  g <- grouping_contrast_tbl |> filter(sample == sample_)
  f <- efa2_summary |> filter(sample == sample_)
  l <- irt_ld |> filter(sample == sample_, family == "guess")
  sc <- subscale_tbl |> filter(sample == sample_)
  gv <- \(x) g$within_minus_between[g$grouping == x]
  tibble(
    metric = c("n_items", "ev1_ev2", "parallel analysis nfact (June method)",
               "parallel analysis nfact (resampled tetrachoric)",
               "parallel analysis ncomp (resampled tetrachoric)",
               "question type: within - between", "story: within - between",
               "old group: within - between", "new construct: within - between",
               "2-factor EFA factor r", "2-factor EFA proportion variance",
               "story bifactor dAIC (bifactor - uni)",
               "story bifactor dBIC (bifactor - uni)",
               "question-type bifactor dBIC (bifactor - uni)",
               "median |a_specific| / a_general",
               "median |a_specific| / a_general (stories with >= 3 items)",
               "ECV (general)",
               "information overstatement (uni vs bifactor)",
               "2PL+g EAP empirical reliability (uni)",
               "Rasch rxx: all items", "Rasch rxx: false-belief only",
               "age r: all items", "age r: false-belief only"),
    value = c(e$n_items, e$ev1_ev2, e$pa_june_nfact, e$pa_resample_nfact,
              e$pa_resample_ncomp,
              gv("question type"), gv("story"), gv("old group"), gv("new construct"),
              f$factor_r, f$prop_var, l$dAIC_story, l$dBIC_story, l$dBIC_qtype,
              l$median_as_ag, l$median_as_ag_3plus, l$ECV,
              l$info_overstatement, l$rxx_uni,
              sc$rasch_marginal_rxx[sc$scale == "all items"],
              sc$rasch_marginal_rxx[sc$scale == "false-belief only"],
              sc$age_r[sc$scale == "all items"],
              sc$age_r[sc$scale == "false-belief only"]))
}
june_col <- c(26, 1.90, 11, NA, NA, 0.101, 0.039, 0.039, 0.045, 0.19, 0.28,
              16671 - 16832, 17061 - 17092, NA, 0.43, NA, NA, 0.11, NA,
              0.842, 0.605, 0.402, 0.401)
june_src <- c(rep("June DE (tom_dimensionality)", 3), NA, NA,
              rep("June DE (tom_dimensionality)", 6),
              rep("Sept audit F11 (v2_3 calibration data)", 2), NA,
              "Sept audit F11 (v2_3 calibration data)", NA, NA,
              "Sept audit F11 (v2_3 calibration data)", NA,
              rep("June DE (tom_dimensionality)", 4))
headline <- map(names(samples), hl) |>
  set_names(names(samples)) |>
  imap(\(x, nm) rename(x, !!nm := value)) |>
  reduce(left_join, by = "metric") |>
  mutate(june = june_col, june_source = june_src, .after = metric)

res <- list(
  meta = list(
    script = "tasks/_stories_fits_dimensionality.R",
    run_at = t_start, finished_at = Sys.time(), quick = quick,
    seed = 20260924, n_boot = n_boot, n_rand = n_rand, n_pa = n_pa,
    em_ncycles = ncycles,
    r_version = R.version.string,
    packages = c(mirt = as.character(packageVersion("mirt")),
                 psych = as.character(packageVersion("psych"))),
    session_info = sessionInfo(),
    q3_bootstrap = paste("person bootstrap of the residual matrix (items and theta fixed),",
                         "n_boot draws, seed 20260925 (separate from the main stream)"),
    data_file = file.path("data", "stories_2026-09", "stories_trials.rds"),
    data_provenance = data_provenance,
    item_filters = "policy_exclude rows dropped (except de_ib_junelike); coverage > .4; .08 < p < .97 (June filters)",
    irt_spec = paste("2PL; guess = item chance (0 if observed p <= chance);",
                     "slopes normal(0, sd 2) prior (MAP-EM); bifactor via mirt::bfactor",
                     "(story testlets or question types with >= 2 items);",
                     "EAP scores, QMC for the bifactor; noguess = same without floor")),
  samples = sample_table,
  item_info = item_info,
  removed_items = removed_items,
  eigen = eigen_table,
  scree = scree,
  grouping_contrast = grouping_contrast_tbl,
  grouping_cells = grouping_cells,
  q3_grouping_contrast = q3_grouping_contrast,
  q3_grouping_cells = q3_grouping_cells,
  grouping_lm = grouping_lm,
  pairs = pairs_all,
  efa2_summary = efa2_summary,
  efa2_loadings = efa2_loadings,
  efa2_by_qtype = efa2_by_qtype,
  efa2_by_story = efa2_by_story,
  irt_fits = irt_fits,
  irt_ld = irt_ld,
  irt_slopes = irt_slopes,
  irt_scores = irt_scores,
  ld_by_story = ld_by_story,
  ld_pairs_top = ld_pairs_top,
  subscales = subscale_tbl,
  june_pool_check = june_pool_check,
  june_reference = june_reference,
  headline = headline,
  tetrachoric = map(classical, "rho"),
  boot = imap_dfr(classical, \(cl, nm) cl$boot |> mutate(sample = nm, .before = 1))
)

write_rds(res, out_file, compress = "gz")
msg("saved ", out_file, " (", round(as.numeric(difftime(Sys.time(), t_start, units = "mins")), 1), " min)")
