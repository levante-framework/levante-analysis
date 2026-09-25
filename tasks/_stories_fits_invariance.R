# Stories (Theory of Mind): cross-language measurement invariance and DIF on the
# corrected 2026-09 data. Heavy model fits for the consolidated Stories chapter,
# which only reads the results object this script writes.
#
# Input  (gitignored): data/stories_2026-09/stories_trials.rds
#                      (built by tasks/_build_stories_data.R; levantemodels PR #27)
# Output (gitignored): data/stories_2026-09/results_invariance.rds
#
# Analyses (all multigroup IRT with mirt; one run per child = the child's earliest
# run that contains the analysed stories):
#   A   stories 1-6,   de-DE vs en-US vs es-CO (Colombian datasets pooled)
#   B   stories 13-18, de-DE vs es-AR (es-CO has < 100 responses per item here, so
#       it enters only a lower-threshold sensitivity fit, B_esco)
#   Items need >= 100 responses in every group (and both responses in every group).
#   Primary data = corrected (PR #27) trials, item-policy exclusions applied
#   (`policy_exclude`), Spanish reality_known false-belief items kept, all forms
#   (fixed forms + the 2026 fcat_random3 administrations, whose story draw is
#   random, i.e. missing by design).
#   Sensitivities: (i) policy-excluded trials included; (ii) Spanish
#   reality_known false-belief item dropped; fixed forms only (A); es-CO without
#   the V0 Bogota item-bank runs of May-June 2024 (A); pure Rasch without a
#   guessing floor (the June model); the dimensionality analysis's guessing rule
#   (floor 0 for items at or below chance in a group; A_g0, B_g0); German children
#   under 9 only (B_young); a production-like 2PL with priors (configural / metric /
#   scalar tests, and DIF effect sizes with boundary items never used as anchors);
#   within-language cross-site comparisons (X_esAR: UTDT-AR main vs UTDT-intl;
#   X_esCO: Bogota vs rural) as falsification checks; and a June-style bridge
#   (DE-Leipzig vs Bogota, pure Rasch, June's DIF rule) on release-equivalent vs
#   corrected data.
# ETS classes, adapted: the 0.43 / 0.64 cut-offs (1 and 1.5 delta units) are applied
#   to latent-logit db under the guessing floor, which runs larger than observed-score
#   Mantel-Haenszel D-DIF; significance is the BH-adjusted LR test of the item, and
#   class C does not require |db| to be significantly above 0.43.
#
# Model: Rasch with the guessing floor fixed at the item's chance level (LEVANTE
#   convention). 2PL with fixed guessing is fitted for comparison
#   (`model_choice`): by ML it does not converge configurally and its slopes
#   diverge, so its LR tests are not usable; a production-like 2PL with priors
#   gives effect sizes only (`robust_2pl`). When a whole group answers an item at
#   or below chance, the floor model cannot place it (difficulty -> infinity):
#   such estimates are flagged (`boundary`: |b| > 10; `near_floor`: lowest group
#   accuracy < chance + .05), their LR tests are kept, and their magnitudes should
#   be read from the pure-Rasch sensitivity fit.
# Invariance: configural (item difficulties free, group means fixed at 0, group
#   variances free) vs scalar (difficulties equal; reference group de-DE mean 0,
#   other means and all variances free). LR test + BIC.
# DIF: stage 1 = each item freed in turn with all others as anchors (LR vs
#   scalar); stage 2 = purified anchors (items negligible at stage 1), each item
#   tested against the anchors, iterated until the anchor set is stable. Effect
#   size = difference in item difficulty (logits) at matched ability, group minus
#   de-DE. Classes follow the ETS rule translated to logits: A (negligible) =
#   not significant (BH-adjusted p >= .05) or max |db| < 0.43; C (large) =
#   significant and max |db| >= 0.64; B (moderate) otherwise. Partial scalar
#   model = final anchors constrained, all other items free.
#
# Run from a working directory OUTSIDE the repo (the repo's renv has an mgcv
# binary that breaks mirt):
#   cd <scratch dir> && Rscript <book>/tasks/_stories_fits_invariance.R [<book>]
# Book root: first argument, else env STORIES_BOOK_ROOT, else the directory above this script's tasks/.
# Env STORIES_CORES sets the number of forked workers (default 4); ~15 min with 4.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(tibble)
  library(readr)
  library(mirt)
})
options(warn = 1, dplyr.summarise.inform = FALSE, width = 200)

args <- commandArgs(trailingOnly = TRUE)
script_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
default_root <- if (length(script_file) == 1) dirname(dirname(normalizePath(script_file))) else here::here()
book_root <- normalizePath(
  if (length(args) >= 1) args[1] else
    Sys.getenv("STORIES_BOOK_ROOT", default_root),
  mustWork = TRUE)
if (startsWith(normalizePath(getwd()), book_root))
  stop("Run from a working directory outside the book repo (its renv breaks mirt).")
data_dir <- file.path(book_root, "data", "stories_2026-09")
out_file <- file.path(data_dir, "results_invariance.rds")
n_cores <- as.integer(Sys.getenv("STORIES_CORES", "4"))
t_start <- Sys.time()

# ---- settings -----------------------------------------------------------------

N_MIN      <- 100    # responses per item per group
ES_SMALL   <- 0.43   # ETS 1 delta unit in logits (1 / 2.35)
ES_LARGE   <- 0.64   # ETS 1.5 delta units in logits
ALPHA      <- 0.05   # on BH-adjusted p
N_CYCLES   <- 5000
MAX_PURIFY <- 5
MIN_ANCHOR <- 3
REF_GROUP  <- "de_DE"
B_BOUND    <- 10     # |b| beyond this = not identified (group at/below the floor)

msg <- function(...) message(format(Sys.time(), "%H:%M:%S "), ...)

# ---- data ---------------------------------------------------------------------

tr <- readRDS(file.path(data_dir, "stories_trials.rds"))
data_provenance <- attr(tr, "provenance")

# item descriptors from the uid itself, so release-equivalent uids get them too
uid_meta <- function(uid) {
  tibble(item = uid,
         story = as.integer(str_match(uid, "^tom_story(\\d+)_")[, 2]),
         question_type = case_when(
           str_detect(uid, "reality_check") ~ "reality_check",
           str_detect(uid, "false_belief") ~ "false_belief",
           str_detect(uid, "emotion_reasoning") ~ "emotion_reasoning",
           str_detect(uid, "reference") ~ "reference",
           TRUE ~ "other"),
         is_control = question_type == "reality_check",
         old_group = str_match(uid, paste0(
           "^tom_story\\d+_(reality_known|moral_reasoning|interpretation|",
           "deception|reference|second_order)_"))[, 2],
         short = str_remove(uid, "^tom_"))
}

# prep(): build the response matrix for one analysis
#   stories, groups   which stories; group labels (a named vector language -> group,
#                     or dataset -> group when by = "dataset")
#   policy            "exclude" drops policy_exclude trials; "include" keeps them
#   spanish_fb        "keep" or "drop" (drop the reality_known false-belief items
#                     flagged in Spanish, for every group)
#   forms             "all" or "fixed" (no fcat_random3)
#   uid               "corrected" (story_uid) or "release" (release trials, pre-PR27 uid)
#   persons           "first_run" (earliest run per child) or "all_runs"
#   drop_v0           drop the V0 Bogota item-bank runs (2024-05/06)
#   languages         NULL (all) or the languages to keep (for by = "dataset")
#   age_max           NULL or keep children younger than this (age-restricted variants)
#   g0                TRUE: guessing floor 0 for items at or below chance in any group (the
#                     rule the dimensionality analysis uses), chance otherwise
prep <- function(stories, groups, by = "language", policy = "exclude",
                 spanish_fb = "keep", forms = "all", uid = "corrected",
                 persons = "first_run", drop_v0 = FALSE, n_min = N_MIN,
                 languages = NULL, age_max = NULL, g0 = FALSE) {
  d <- tr |> filter(story %in% stories)
  if (!is.null(languages)) d <- d |> filter(language %in% languages)
  if (!is.null(age_max)) d <- d |> filter(!is.na(age), age < age_max)
  # V0 = the first Bogota content (item_bank runs, May-June 2024): identity rests on
  # a retroactive position map and no V0 corpus survives
  if (drop_v0) d <- d |> filter(!(dataset == "pilot_uniandes_co_bogota" & form == "item_bank"))
  if (uid == "release") {
    d <- d |> filter(in_release, !is.na(story_uid_pre_pr27)) |>
      mutate(story_uid = story_uid_pre_pr27)
  }
  d <- d |> mutate(group = unname(groups[.data[[by]]])) |> filter(!is.na(group))
  if (policy == "exclude") d <- d |> filter(!policy_exclude)
  if (forms == "fixed") d <- d |> filter(form != "fcat_random3")
  if (spanish_fb == "drop") {
    fb_items <- tr |> filter(spanish_fb_flag) |> distinct(story_uid) |> pull()
    d <- d |> filter(!story_uid %in% fb_items)
  }
  if (persons == "first_run") {
    keep_runs <- d |> distinct(user_id, run_id, time_started) |>
      arrange(time_started, run_id) |> group_by(user_id) |> slice(1) |> ungroup()
    d <- d |> semi_join(keep_runs, by = "run_id")
  }
  # one response per run x item (only binds for release uids, which repeat)
  d <- d |> arrange(run_id, trial_number) |> distinct(run_id, story_uid, .keep_all = TRUE)

  lev <- unique(unname(groups))
  cells <- d |> group_by(item = story_uid, group) |>
    summarise(n = n(), acc = mean(correct), n_levels = n_distinct(correct),
              .groups = "drop") |>
    complete(item, group = lev, fill = list(n = 0L, n_levels = 0L))
  item_ok <- cells |> group_by(item) |>
    summarise(min_n = min(n), all_var = all(n_levels == 2), .groups = "drop") |>
    mutate(reason = case_when(min_n < n_min ~ glue_n(min_n, n_min),
                              !all_var ~ "no variance in a group",
                              TRUE ~ NA_character_))
  keep_items <- item_ok |> filter(is.na(reason)) |> pull(item) |> sort(method = "radix")

  w <- d |> filter(story_uid %in% keep_items) |>
    transmute(run_id, group, item = story_uid, correct = as.integer(correct)) |>
    pivot_wider(names_from = item, values_from = correct)
  mat <- as.matrix(w[, keep_items])
  rownames(mat) <- w$run_id
  has <- rowSums(!is.na(mat)) > 0
  grp <- factor(w$group[has], levels = lev)
  mat <- mat[has, , drop = FALSE]

  chance <- d |> filter(story_uid %in% keep_items) |> group_by(item = story_uid) |>
    summarise(n_chance = n_distinct(chance), chance = min(chance), .groups = "drop")
  chance <- chance[match(keep_items, chance$item), ]
  if (g0) {
    low <- cells |> filter(item %in% keep_items) |> left_join(chance |> select(item, ch = chance), by = "item") |>
      group_by(item) |> summarise(low = any(acc <= ch), .groups = "drop") |> filter(low) |> pull(item)
    chance$chance[chance$item %in% low] <- 0
  }
  runs <- d |> filter(run_id %in% rownames(mat)) |>
    distinct(run_id, user_id, group, dataset, form, age, time_started)
  sample <- runs |> group_by(group) |>
    summarise(n_runs = n(), n_children = n_distinct(user_id),
              age_median = median(age, na.rm = TRUE),
              age_q1 = quantile(age, .25, na.rm = TRUE),
              age_q3 = quantile(age, .75, na.rm = TRUE),
              age_missing = sum(is.na(age)),
              first = as.Date(min(time_started)), last = as.Date(max(time_started)),
              datasets = paste(sort(unique(dataset)), collapse = ", "),
              forms = paste(names(sort(table(form), decreasing = TRUE)), collapse = ", "),
              .groups = "drop") |>
    mutate(group = factor(group, levels = lev)) |> arrange(group) |>
    mutate(group = as.character(group))
  sample_forms <- runs |> count(group, dataset, form, name = "n_runs")

  list(mat = mat, grp = grp, guess = chance$chance, items = keep_items, ref = lev[1],
       g0_items = if (g0) keep_items[chance$chance == 0] else character(0),
       cells = cells |> left_join(item_ok |> select(item, reason), by = "item") |>
         mutate(included = item %in% keep_items),
       sample = sample, sample_forms = sample_forms,
       n_chance_conflicts = sum(chance$n_chance > 1))
}
glue_n <- function(min_n, n_min) paste0("n < ", n_min, " in a group (min ", min_n, ")")

# ---- model helpers --------------------------------------------------------------

# multigroup Rasch; model = "rasch_g" (guessing fixed at chance) or "rasch" (none).
# anchors = items constrained equal across groups; with >= 1 anchor the group means
# (except the reference group's) and all variances are free; with 0 anchors this
# is the configural model (means fixed at 0, variances free).
# Warnings are captured (not printed) and kept with the model: mirt's "Log-likelihood
# was decreasing near the ML solution" is counted per fit in the results.
fit_mg <- function(D, anchors, model = "rasch_g", SE = FALSE, quadpts = NULL) {
  inv <- if (length(anchors)) c(anchors, "free_means", "free_var") else ""
  g <- if (model == "rasch_g") D$guess else rep(0, ncol(D$mat))
  w <- character(0)
  m <- withCallingHandlers(
    multipleGroup(D$mat, 1, group = D$grp, itemtype = "Rasch", guess = g,
                  invariance = inv, SE = SE, verbose = FALSE, quadpts = quadpts,
                  technical = list(NCYCLES = N_CYCLES)),
    warning = function(cond) {
      w <<- c(w, conditionMessage(cond)); invokeRestart("muffleWarning")
    })
  attr(m, "warn") <- w
  m
}
n_warn <- function(m) length(attr(m, "warn"))
msum <- function(m) {
  tibble(logLik = extract.mirt(m, "logLik"), npar = extract.mirt(m, "nest"),
         AIC = extract.mirt(m, "AIC"), BIC = extract.mirt(m, "BIC"),
         converged = extract.mirt(m, "converged"),
         iterations = extract.mirt(m, "iterations"),
         n_warnings = n_warn(m),
         warnings = paste(unique(attr(m, "warn")), collapse = " | "))
}
# per-group item difficulty b = -d (Rasch slope 1)
item_b <- function(m) {
  co <- coef(m, simplify = TRUE)
  imap_dfr(co, \(x, g) tibble(group = g, item = rownames(x$items), b = -x$items[, "d"]))
}
item_b_se <- function(m) {
  co <- coef(m, printSE = TRUE)
  imap_dfr(co, \(x, g) {
    x <- x[names(x) != "GroupPars"]
    tibble(group = g, item = names(x),
           se = map_dbl(x, \(p) if ("SE" %in% rownames(p)) p["SE", "d"] else NA_real_))
  })
}
group_pars <- function(m) {
  co <- coef(m, simplify = TRUE)
  imap_dfr(co, \(x, g) tibble(group = g, mean = unname(x$means[1]),
                              var = unname(x$cov[1, 1])))
}
# LR test of a restricted model m0 nested in m1; dBIC = BIC(m0) - BIC(m1), so a
# positive dBIC means BIC prefers the LESS constrained model m1.
lr <- function(m0, m1) {
  s0 <- msum(m0); s1 <- msum(m1)
  x2 <- 2 * (s1$logLik - s0$logLik); df <- s1$npar - s0$npar
  tibble(X2 = x2, df = df, X2_df = x2 / df,
         p = pchisq(x2, df, lower.tail = FALSE),
         dBIC = s0$BIC - s1$BIC, dAIC = s0$AIC - s1$AIC)
}
# contrasts of one item's difficulty: each group minus the reference group
contrasts_of <- function(bt, it, ref = REF_GROUP) {
  x <- bt |> filter(item == it)
  b_ref <- x$b[x$group == ref]
  out <- x |> filter(group != ref) |> transmute(item, group, db = b - b_ref)
  attr(out, "max_pair") <- max(dist(x$b))
  attr(out, "boundary") <- any(abs(x$b) > B_BOUND)
  out
}
es_class <- function(p_adj, max_abs) {
  case_when(is.na(p_adj) ~ NA_character_,
            p_adj >= ALPHA | max_abs < ES_SMALL ~ "A",
            max_abs >= ES_LARGE ~ "C",
            TRUE ~ "B")
}
pmap_items <- function(items, f) {
  res <- parallel::mclapply(items, f, mc.cores = n_cores, mc.preschedule = FALSE)
  bad <- map_lgl(res, \(r) inherits(r, "try-error") || is.null(r))
  if (any(bad)) stop("worker failed for: ", paste(items[bad], collapse = ", "),
                     "\n", paste(res[bad], collapse = "\n"))
  res
}
boundary_items <- function(m, cut = 10) {
  item_b(m) |> filter(abs(b) > cut) |> pull(item) |> unique()
}

# ---- the invariance + DIF procedure ---------------------------------------------

run_invariance <- function(id, D, model = "rasch_g", p_adjust = "BH",
                           es_small = ES_SMALL, iterate = TRUE, subsets = TRUE,
                           se = TRUE, numerics = FALSE, anchor_se = FALSE) {
  msg(id, ": ", nrow(D$mat), " runs x ", ncol(D$mat), " items, model ", model)
  items <- D$items
  conf <- fit_mg(D, character(0), model)
  scal <- fit_mg(D, items, model)
  conv <- list(tibble(model = "configural", msum(conf)),
               tibble(model = "scalar", msum(scal)))

  # stage 1: each item freed in turn, all others as anchors
  s1 <- pmap_items(items, \(it) {
    m <- fit_mg(D, setdiff(items, it), model)
    ct <- contrasts_of(item_b(m), it, ref = D$ref)
    list(test = tibble(item = it, lr(scal, m), max_pair = attr(ct, "max_pair"),
                       boundary = attr(ct, "boundary"),
                       converged = extract.mirt(m, "converged"),
                       iterations = extract.mirt(m, "iterations"), n_warnings = n_warn(m)),
         db = ct)
  })
  stage1 <- map_dfr(s1, "test") |>
    mutate(p_adj = p.adjust(p, p_adjust),
           class = case_when(p_adj >= ALPHA | max_pair < es_small ~ "A",
                             max_pair >= ES_LARGE ~ "C", TRUE ~ "B"))
  stage1_db <- map_dfr(s1, "db")

  anchors <- stage1 |> filter(class == "A") |> pull(item)
  anchor_rule <- "stage-1 class A"
  if (length(anchors) < MIN_ANCHOR) {
    anchors <- stage1 |> arrange(X2) |> slice_head(n = MIN_ANCHOR) |> pull(item)
    anchor_rule <- paste0("fewer than ", MIN_ANCHOR,
                          " class-A items; used the smallest stage-1 X2")
  }

  # stage 2: test every item against the purified anchors; iterate
  history <- list()
  max_round <- if (iterate) MAX_PURIFY else 1
  for (round in seq_len(max_round)) {
    part <- fit_mg(D, anchors, model)
    bt_part <- item_b(part)
    s2 <- pmap_items(items, \(it) {
      if (it %in% anchors) {                      # free this anchor
        m <- fit_mg(D, setdiff(anchors, it), model)
        ct <- contrasts_of(item_b(m), it, ref = D$ref)
        tst <- lr(part, m)
      } else {                                    # constrain this free item
        m <- fit_mg(D, c(anchors, it), model)
        ct <- contrasts_of(bt_part, it, ref = D$ref)
        tst <- lr(m, part)
      }
      list(test = tibble(item = it, anchor = it %in% anchors, tst,
                         max_pair = attr(ct, "max_pair"), boundary = attr(ct, "boundary"),
                         converged = extract.mirt(m, "converged"),
                         iterations = extract.mirt(m, "iterations"), n_warnings = n_warn(m)),
           db = ct)
    })
    stage2 <- map_dfr(s2, "test") |>
      mutate(p_adj = p.adjust(p, p_adjust),
             class = case_when(p_adj >= ALPHA | max_pair < es_small ~ "A",
                               max_pair >= ES_LARGE ~ "C", TRUE ~ "B"))
    stage2_db <- map_dfr(s2, "db")
    new_anchors <- stage2 |> filter(class == "A") |> pull(item)
    history[[round]] <- tibble(round = round, n_anchors = length(anchors),
                               anchors = paste(sort(anchors), collapse = ";"),
                               n_class_A = length(new_anchors))
    if (round == max_round || setequal(new_anchors, anchors) ||
        length(new_anchors) < MIN_ANCHOR) break
    anchors <- new_anchors
    anchor_rule <- paste0(anchor_rule, "; re-purified (round ", round + 1, ")")
  }
  stable <- setequal(stage2 |> filter(class == "A") |> pull(item), anchors)

  # final partial-scalar model with SEs for the free items
  part_se <- if (se) fit_mg(D, anchors, model, SE = TRUE) else part
  bt <- item_b(part) |>
    left_join(if (se) item_b_se(part_se) else tibble(group = character(), item = character(), se = numeric()),
              by = c("group", "item"))
  # SEs for the anchors' own db: each anchor freed in turn (the fit its stage-2 db comes from)
  anchor_se_tbl <- if (anchor_se) {
    bind_rows(pmap_items(anchors, \(it) {
      m <- fit_mg(D, setdiff(anchors, it), model, SE = TRUE)
      item_b_se(m) |> filter(item == it)
    }))
  } else tibble(group = character(), item = character(), se = numeric())
  conv <- c(conv, list(tibble(model = "partial_scalar", msum(part)),
                       tibble(model = "partial_scalar_SE", msum(part_se),
                              secondorder = if (se) isTRUE(extract.mirt(part_se, "secondordertest")) else NA)))

  n_all <- length(items)
  tests <- bind_rows(
    tibble(comparison = "scalar vs configural", items = "all", n_items = n_all, lr(scal, conf)),
    tibble(comparison = "partial scalar vs configural", items = "all", n_items = n_all, lr(part, conf)),
    tibble(comparison = "scalar vs partial scalar", items = "all", n_items = n_all, lr(scal, part)))

  if (subsets) {
    ctl <- tibble(item = items) |> left_join(uid_meta(items), by = "item")
    for (s in c("targets", "controls")) {
      its <- ctl |> filter(if (s == "targets") !is_control else is_control) |> pull(item)
      if (length(its) < 3) next
      Ds <- D; Ds$mat <- D$mat[, its, drop = FALSE]; Ds$guess <- D$guess[match(its, items)]
      Ds$items <- its
      keep <- rowSums(!is.na(Ds$mat)) > 0; Ds$mat <- Ds$mat[keep, , drop = FALSE]
      Ds$grp <- droplevels(D$grp[keep])
      if (nlevels(Ds$grp) < nlevels(D$grp)) next
      c2 <- fit_mg(Ds, character(0), model); s2m <- fit_mg(Ds, its, model)
      n_s <- length(its)
      tests <- bind_rows(tests, tibble(comparison = "scalar vs configural", items = s,
                                       n_items = n_s, lr(s2m, c2)))
      conv <- c(conv, list(tibble(model = paste0("configural_", s), msum(c2)),
                           tibble(model = paste0("scalar_", s), msum(s2m))))
      # partial scalar within the subset: the procedure's anchors that fall in it
      a_s <- intersect(anchors, its)
      if (length(a_s) >= 1 && length(a_s) < length(its)) {
        p2m <- fit_mg(Ds, a_s, model)
        tests <- bind_rows(tests, tibble(comparison = "partial scalar vs configural",
                                         items = s, n_items = n_s, lr(p2m, c2)))
        conv <- c(conv, list(tibble(model = paste0("partial_scalar_", s), msum(p2m))))
      }
    }
  }

  cells <- D$cells |> filter(included) |> select(item, group, n, acc)
  dif <- uid_meta(items) |>
    left_join(tibble(item = items, chance = D$guess), by = "item") |>
    left_join(stage1 |> select(item, s1_X2 = X2, s1_p = p, s1_p_adj = p_adj,
                               s1_max_pair = max_pair, s1_class = class), by = "item") |>
    left_join(stage2 |> select(item, anchor, s2_X2 = X2, s2_p = p, s2_p_adj = p_adj,
                               s2_max_pair = max_pair, boundary, class), by = "item") |>
    left_join(stage2_db |> select(item, group, db) |>
                pivot_wider(names_from = group, values_from = db, names_prefix = "db_"),
              by = "item") |>
    left_join(stage1_db |> select(item, group, db) |>
                pivot_wider(names_from = group, values_from = db, names_prefix = "s1_db_"),
              by = "item") |>
    left_join(bt |> select(item, group, b) |>
                pivot_wider(names_from = group, values_from = b, names_prefix = "b_"),
              by = "item") |>
    left_join(cells |> select(item, group, acc) |>
                pivot_wider(names_from = group, values_from = acc, names_prefix = "acc_"),
              by = "item") |>
    left_join(cells |> select(item, group, n) |>
                pivot_wider(names_from = group, values_from = n, names_prefix = "n_"),
              by = "item") |>
    mutate(analysis = id, .before = 1)
  # distance of the lowest group accuracy from the guessing floor; items at or near
  # the floor in a group have weakly (or, below it, not) identified difficulties
  acc_m <- as.matrix(dif[, paste0("acc_", levels(D$grp))])
  dif$floor_margin <- apply(acc_m, 1, min) - dif$chance
  dif$near_floor <- dif$floor_margin < 0.05
  # June's metric: difficulty gap in the partial-scalar model (0 for anchors)
  bm <- as.matrix(dif[, paste0("b_", levels(D$grp))])
  dif$p_max_pair <- apply(bm, 1, \(x) max(dist(x)))

  # long, plot-ready effect sizes (stage 2, vs the reference group), with an
  # approximate SE (sqrt of the two groups' squared SEs): free items from the final
  # partial-scalar fit, anchors (if anchor_se) from their free-one-anchor fits.
  # mdd = minimum detectable |db| at two-sided alpha .05 with 80% power, (1.96 + 0.84) SE,
  # before the BH adjustment the classes use
  se_w <- bind_rows(bt |> filter(!item %in% anchors) |> select(item, group, se),
                    anchor_se_tbl |> select(item, group, se))
  dif_long <- stage2_db |>
    left_join(se_w |> rename(se_g = se), by = c("item", "group")) |>
    left_join(se_w |> filter(group == D$ref) |> select(item, se_ref = se), by = "item") |>
    mutate(se = sqrt(se_g^2 + se_ref^2), mdd = (qnorm(.975) + qnorm(.8)) * se) |>
    select(item, group, db, se, mdd) |>
    left_join(dif |> select(item, short, story, question_type, is_control, class, anchor,
                            boundary, near_floor),
              by = "item") |>
    mutate(analysis = id, contrast = paste0(group, " - ", D$ref), .before = 1)

  list(id = id, model = model, n_runs = nrow(D$mat), items = items,
       anchors = sort(anchors), anchor_rule = anchor_rule, anchors_stable = stable,
       purification = bind_rows(history),
       fits = bind_rows(conv) |> mutate(analysis = id, .before = 1),
       tests = tests |> mutate(analysis = id, .before = 1),
       group_pars = bind_rows(
         group_pars(scal) |> mutate(model = "scalar"),
         group_pars(part) |> mutate(model = "partial_scalar"),
         group_pars(conf) |> mutate(model = "configural")) |>
         mutate(analysis = id, .before = 1),
       dif = dif, dif_long = dif_long,
       stage1_convergence = stage1 |> summarise(n = n(), all_converged = all(converged),
                                                max_iter = max(iterations),
                                                n_fits_warned = sum(n_warnings > 0)),
       stage2_convergence = stage2 |> summarise(n = n(), all_converged = all(converged),
                                                max_iter = max(iterations),
                                                n_fits_warned = sum(n_warnings > 0)),
       numerics = if (numerics) bind_rows(map(list(configural = list(conf, character(0)),
                                                   scalar = list(scal, items),
                                                   partial_scalar = list(part, anchors)),
         \(x) {
           m121 <- fit_mg(D, x[[2]], model, quadpts = 121)
           b61 <- item_b(x[[1]]); b121 <- item_b(m121)
           ok <- abs(b61$b) <= B_BOUND & abs(b121$b) <= B_BOUND
           tibble(logLik_61 = extract.mirt(x[[1]], "logLik"),
                  logLik_121 = extract.mirt(m121, "logLik"),
                  n_warnings_61 = n_warn(x[[1]]), n_warnings_121 = n_warn(m121),
                  max_abs_b_diff = max(abs(b61$b - b121$b)),
                  max_abs_b_diff_identified = max(abs(b61$b - b121$b)[ok]),
                  n_boundary_params = sum(!ok))
         }), .id = "model") |> mutate(analysis = id, .before = 1) else NULL,
       boundary = list(configural = boundary_items(conf), partial = boundary_items(part)))
}

# ---- analyses -------------------------------------------------------------------

grp_A <- c("de-DE" = "de_DE", "en-US" = "en_US", "es-CO" = "es_CO")
grp_B <- c("de-DE" = "de_DE", "es-AR" = "es_AR")
grp_B3 <- c("de-DE" = "de_DE", "es-AR" = "es_AR", "es-CO" = "es_CO")
grp_J <- c(pilot_mpieva_de_main = "de_DE", pilot_uniandes_co_bogota = "es_CO")
# within-language, cross-site comparisons (falsification checks for the language DIF)
grp_XAR <- c(rfp1_utdt_ar_main = "utdt_ar", rfp1_utdt_intl_ys = "utdt_intl")
grp_XCO <- c(pilot_uniandes_co_bogota = "bogota", pilot_uniandes_co_rural = "rural")

# the first group listed is the reference (mean 0; db = group minus reference);
# langs = "" keeps every language; age_max = NA keeps every age
specs <- tribble(
  ~id,           ~family, ~label,                                                     ~stories, ~groups, ~by,        ~policy,   ~spanish_fb, ~forms,  ~uid,        ~persons,    ~drop_v0, ~n_min, ~model,    ~june_method, ~langs,  ~age_max, ~g0,
  "A",           "A",     "Stories 1-6: primary",                                    1:6,      grp_A,   "language", "exclude", "keep",      "all",   "corrected", "first_run", FALSE,    100,    "rasch_g", FALSE,        "",      NA,       FALSE,
  "A_policy",    "A",     "Stories 1-6: (i) policy-excluded trials included",        1:6,      grp_A,   "language", "include", "keep",      "all",   "corrected", "first_run", FALSE,    100,    "rasch_g", FALSE,        "",      NA,       FALSE,
  "A_nofb",      "A",     "Stories 1-6: (ii) Spanish false belief dropped",          1:6,      grp_A,   "language", "exclude", "drop",      "all",   "corrected", "first_run", FALSE,    100,    "rasch_g", FALSE,        "",      NA,       FALSE,
  "A_fixed",     "A",     "Stories 1-6: fixed forms only (no fcat)",                 1:6,      grp_A,   "language", "exclude", "keep",      "fixed", "corrected", "first_run", FALSE,    100,    "rasch_g", FALSE,        "",      NA,       FALSE,
  "A_noV0",      "A",     "Stories 1-6: es-CO without V0 Bogota (n >= 90)",          1:6,      grp_A,   "language", "exclude", "keep",      "all",   "corrected", "first_run", TRUE,     90,     "rasch_g", FALSE,        "",      NA,       FALSE,
  "A_rasch",     "A",     "Stories 1-6: pure Rasch (no guessing floor)",             1:6,      grp_A,   "language", "exclude", "keep",      "all",   "corrected", "first_run", FALSE,    100,    "rasch",   FALSE,        "",      NA,       FALSE,
  "A_g0",        "A",     "Stories 1-6: floor 0 for items at/below chance in a group", 1:6,    grp_A,   "language", "exclude", "keep",      "all",   "corrected", "first_run", FALSE,    100,    "rasch_g", FALSE,        "",      NA,       TRUE,
  "B",           "B",     "Stories 13-18: primary",                                  13:18,    grp_B,   "language", "exclude", "keep",      "all",   "corrected", "first_run", FALSE,    100,    "rasch_g", FALSE,        "",      NA,       FALSE,
  "B_policy",    "B",     "Stories 13-18: (i) policy-excluded trials included",      13:18,    grp_B,   "language", "include", "keep",      "all",   "corrected", "first_run", FALSE,    100,    "rasch_g", FALSE,        "",      NA,       FALSE,
  "B_nofb",      "B",     "Stories 13-18: (ii) Spanish false belief dropped",        13:18,    grp_B,   "language", "exclude", "drop",      "all",   "corrected", "first_run", FALSE,    100,    "rasch_g", FALSE,        "",      NA,       FALSE,
  "B_rasch",     "B",     "Stories 13-18: pure Rasch (no guessing floor)",           13:18,    grp_B,   "language", "exclude", "keep",      "all",   "corrected", "first_run", FALSE,    100,    "rasch",   FALSE,        "",      NA,       FALSE,
  "B_g0",        "B",     "Stories 13-18: floor 0 for items at/below chance in a group", 13:18, grp_B,  "language", "exclude", "keep",      "all",   "corrected", "first_run", FALSE,    100,    "rasch_g", FALSE,        "",      NA,       TRUE,
  "B_esco",      "B",     "Stories 13-18: + es-CO (n >= 60, underpowered)",          13:18,    grp_B3,  "language", "exclude", "keep",      "all",   "corrected", "first_run", FALSE,    60,     "rasch_g", FALSE,        "",      NA,       FALSE,
  "B_young",     "B",     "Stories 13-18: children under 9 only (n >= 40, underpowered)", 13:18, grp_B, "language", "exclude", "keep",      "all",   "corrected", "first_run", FALSE,    40,     "rasch_g", FALSE,        "",      9,        FALSE,
  "X_esAR",      "X",     "Within es-AR: UTDT-AR main vs UTDT-intl, stories 13-18",  13:18,    grp_XAR, "dataset",  "exclude", "keep",      "all",   "corrected", "first_run", FALSE,    100,    "rasch_g", FALSE,        "es-AR", NA,       FALSE,
  "X_esCO",      "X",     "Within es-CO: Bogota vs rural, stories 1-6 (n >= 60)",    1:6,      grp_XCO, "dataset",  "exclude", "keep",      "all",   "corrected", "first_run", FALSE,    60,     "rasch_g", FALSE,        "es-CO", NA,       FALSE,
  "J_release",   "J",     "June-style DE vs Bogota: release-equivalent data",        1:18,     grp_J,   "dataset",  "include", "keep",      "all",   "release",   "all_runs",  FALSE,    50,     "rasch",   TRUE,         "",      NA,       FALSE,
  "J_corrected", "J",     "June-style DE vs Bogota: corrected (PR #27)",             1:18,     grp_J,   "dataset",  "include", "keep",      "all",   "corrected", "all_runs",  FALSE,    50,     "rasch",   TRUE,         "",      NA,       FALSE,
  "J_policy",    "J",     "June-style DE vs Bogota: corrected + policy exclusions",  1:18,     grp_J,   "dataset",  "exclude", "keep",      "all",   "corrected", "all_runs",  FALSE,    50,     "rasch",   TRUE,         "",      NA,       FALSE
)

data_list <- pmap(specs, \(id, stories, groups, by, policy, spanish_fb, forms, uid,
                           persons, drop_v0, n_min, langs, age_max, g0, ...) {
  prep(stories, groups, by = by, policy = policy, spanish_fb = spanish_fb,
       forms = forms, uid = uid, persons = persons, drop_v0 = drop_v0, n_min = n_min,
       languages = if (nzchar(langs)) langs else NULL,
       age_max = if (is.na(age_max)) NULL else age_max, g0 = g0)
}) |> set_names(specs$id)

# June's procedure (old/tom_dif_multigroup.qmd): raw p < .05 on the drop test, no effect
# size threshold, one anchored refit. Everything else uses the procedure above.
results <- pmap(specs, \(id, model, june_method, ...) {
  if (june_method) {
    run_invariance(id, data_list[[id]], model = model, p_adjust = "none",
                   es_small = 0, iterate = FALSE, subsets = TRUE, se = FALSE)
  } else {
    run_invariance(id, data_list[[id]], model = model, numerics = id %in% c("A", "B"),
                   anchor_se = id %in% c("A", "B"))
  }
}) |> set_names(specs$id)

# ---- model choice: Rasch+g vs 2PL+g (ML) for the primary analyses ----------------

model_choice <- map_dfr(c("A", "B"), \(id) {
  D <- data_list[[id]]; items <- D$items
  f <- \(type, inv) multipleGroup(D$mat, 1, group = D$grp, itemtype = type,
                                  guess = D$guess, invariance = inv, verbose = FALSE,
                                  technical = list(NCYCLES = N_CYCLES))
  ms <- list(
    `Rasch+g configural` = f("Rasch", ""),
    `Rasch+g scalar`     = f("Rasch", c(items, "free_means", "free_var")),
    `2PL+g configural`   = f("2PL", ""),
    `2PL+g metric`       = f("2PL", c("slopes", "free_var")),
    `2PL+g scalar`       = f("2PL", c(items, "free_means", "free_var")))
  imap_dfr(ms, \(m, nm) {
    a <- unlist(map(coef(m, simplify = TRUE), \(g) g$items[, "a1"]))
    tibble(analysis = id, model = nm, msum(m),
           max_slope = max(a), prop_slopes_over_4 = mean(a > 4),
           prop_slopes_negative = mean(a < 0))
  })
})
msg("model choice done")

# ---- production-like 2PL+g with priors -------------------------------------------
# levante-pilots 01_fit_irt_modular.qmd priors: d ~ N(0, 3), a1 ~ N(1, 0.3).
twopl_model <- function(K) mirt.model(paste0("F = 1-", K, "\nPRIOR = (1-", K, ", a1, norm, 1, 0.3), (1-", K,
                                             ", d, norm, 0, 3)"))
fit_2pl_mg <- function(D, inv) {
  w <- character(0)
  m <- withCallingHandlers(
    multipleGroup(D$mat, twopl_model(ncol(D$mat)), group = D$grp, itemtype = "2PL", guess = D$guess,
                  invariance = inv, verbose = FALSE, technical = list(NCYCLES = N_CYCLES)),
    warning = function(cond) { w <<- c(w, conditionMessage(cond)); invokeRestart("muffleWarning") })
  attr(m, "warn") <- w
  m
}

# (a) invariance tests under the 2PL with priors: configural / metric / scalar. The fits are
# MAP, so the LR is computed on the log posterior (logLik + logPrior) as well as on logLik;
# BIC is mirt's (from logLik). dBIC = BIC(constrained) - BIC(less constrained).
model_choice_2pl <- map_dfr(c("A", "B"), \(id) {
  D <- data_list[[id]]; items <- D$items
  ms <- list(configural = fit_2pl_mg(D, ""),
             metric = fit_2pl_mg(D, c("slopes", "free_var")),
             scalar = fit_2pl_mg(D, c("slopes", items, "free_means", "free_var")))
  imap_dfr(ms, \(m, nm) tibble(analysis = id, model = nm, logLik = extract.mirt(m, "logLik"),
                               logPrior = extract.mirt(m, "logPrior"), npar = extract.mirt(m, "nest"),
                               BIC = extract.mirt(m, "BIC"), converged = extract.mirt(m, "converged"),
                               iterations = extract.mirt(m, "iterations"), n_warnings = n_warn(m),
                               max_slope = max(unlist(map(coef(m, simplify = TRUE), \(g) g$items[, "a1"])))))
}) |>
  mutate(logpost = logLik + logPrior)
tests_2pl <- model_choice_2pl |>
  group_by(analysis) |>
  reframe(tibble(comparison = c("metric vs configural", "scalar vs metric"),
                   X2_logLik = 2 * c(logLik[model == "configural"] - logLik[model == "metric"],
                                     logLik[model == "metric"] - logLik[model == "scalar"]),
                   X2_logpost = 2 * c(logpost[model == "configural"] - logpost[model == "metric"],
                                      logpost[model == "metric"] - logpost[model == "scalar"]),
                   df = c(npar[model == "configural"] - npar[model == "metric"],
                          npar[model == "metric"] - npar[model == "scalar"]),
                   dBIC = c(BIC[model == "metric"] - BIC[model == "configural"],
                            BIC[model == "scalar"] - BIC[model == "metric"]))) |>
  mutate(p_logpost = pchisq(X2_logpost, df, lower.tail = FALSE),
         # a nested (more constrained) model reaching a better optimum signals a local optimum
         nested_optimum_better = X2_logLik < 0)
msg("2PL invariance tests done")

# (b) DIF effect sizes under the 2PL with priors (no LR). Slopes equal across groups.
# Effect = -(d_g - d_ref): the log-odds difference at matched ability, the same quantity
# as db in a Rasch model (slope 1). Items whose Rasch + g difficulty is unbounded in a
# group ("boundary", a group at or below the floor) are never anchors:
#   s1       each item freed in turn, all other non-boundary items as anchors
#   purified the Rasch procedure's final anchors minus boundary items; free items from
#            the partial model, each anchor freed in turn
db_2pl <- function(m, it, ref) {
  co <- coef(m, simplify = TRUE)
  x <- imap_dfr(co, \(g, nm) tibble(group = nm, a = g$items[it, "a1"], d = g$items[it, "d"]))
  r <- x |> filter(group == ref)
  x |> filter(group != ref) |> transmute(item = it, group, a = r$a, db = -(d - r$d),
                                          converged = extract.mirt(m, "converged"))
}
robust_2pl <- map_dfr(c("A", "B"), \(id) {
  D <- data_list[[id]]; items <- D$items
  bnd <- results[[id]]$dif |> filter(boundary) |> pull(item)
  s1 <- bind_rows(pmap_items(items, \(it)
    db_2pl(fit_2pl_mg(D, c("slopes", setdiff(items, c(it, bnd)), "free_means", "free_var")), it, D$ref)))
  anc <- setdiff(results[[id]]$anchors, bnd)
  part <- fit_2pl_mg(D, c("slopes", anc, "free_means", "free_var"))
  pur <- bind_rows(
    map_dfr(setdiff(items, anc), \(it) db_2pl(part, it, D$ref)),
    bind_rows(pmap_items(anc, \(it)
      db_2pl(fit_2pl_mg(D, c("slopes", setdiff(anc, it), "free_means", "free_var")), it, D$ref))))
  s1 |> rename(db_2pl = db) |>
    left_join(pur |> select(item, group, db_2pl_purified = db, converged_purified = converged),
              by = c("item", "group")) |>
    mutate(analysis = id, boundary = item %in% bnd, anchor_2pl = item %in% anc, .before = 1)
}) |>
  left_join(bind_rows(results$A$dif_long, results$B$dif_long) |>
              select(analysis, item, group, db_rasch_stage2 = db), by = c("analysis", "item", "group")) |>
  left_join(bind_rows(
    results$A$dif |> select(analysis, item, starts_with("s1_db_")),
    results$B$dif |> select(analysis, item, starts_with("s1_db_"))) |>
      pivot_longer(starts_with("s1_db_"), names_to = "group", values_to = "db_rasch_stage1",
                   names_prefix = "s1_db_"), by = c("analysis", "item", "group"))
msg("2PL robustness done")

# controls vs targets under each model, June's metric (mean |DIF| = mean over items of the
# largest pairwise difficulty gap between groups, the reference counting as 0), boundary
# items excluded; and the Rasch-vs-2PL agreement by item type
max_gap <- \(v) max(dist(c(0, v)))
dif_by_type_models <- robust_2pl |>
  filter(!boundary) |>
  left_join(uid_meta(unique(robust_2pl$item)) |> select(item, is_control, question_type), by = "item") |>
  group_by(analysis, item, is_control, question_type) |>
  summarise(rasch_s1 = max_gap(db_rasch_stage1), rasch_s2 = max_gap(db_rasch_stage2),
            twopl_s1 = max_gap(db_2pl), twopl_purified = max_gap(db_2pl_purified), .groups = "drop") |>
  mutate(kind = if_else(is_control, "control", "target")) |>
  group_by(analysis, kind) |>
  summarise(n_items = n(), across(c(rasch_s1, rasch_s2, twopl_s1, twopl_purified), mean),
            n_large_2pl_purified = sum(twopl_purified >= ES_LARGE), .groups = "drop")
rasch_2pl_agreement <- robust_2pl |>
  left_join(uid_meta(unique(robust_2pl$item)) |> select(item, is_control), by = "item") |>
  group_by(analysis, group) |>
  summarise(r_all = cor(db_2pl, db_rasch_stage1),
            r_no_boundary = cor(db_2pl[!boundary], db_rasch_stage1[!boundary]),
            r_controls_no_boundary = cor(db_2pl[!boundary & is_control], db_rasch_stage1[!boundary & is_control]),
            r_targets_no_boundary = cor(db_2pl[!boundary & !is_control], db_rasch_stage1[!boundary & !is_control]),
            r_purified_no_boundary = cor(db_2pl_purified[!boundary], db_rasch_stage2[!boundary]),
            n_boundary = sum(boundary), .groups = "drop")

# ---- summaries ------------------------------------------------------------------

all_dif <- map_dfr(results, "dif") |> left_join(specs |> select(analysis = id, family), by = "analysis")

dif_by_type <- all_dif |>
  mutate(kind = if_else(is_control, "control", "target"),
         flagged = class %in% c("B", "C")) |>
  group_by(analysis, family) |>
  mutate(total_X2 = sum(s2_X2[!anchor], na.rm = TRUE)) |>
  group_by(analysis, family, kind) |>
  summarise(n_items = n(), n_anchor = sum(anchor), n_flagged = sum(flagged),
            n_large = sum(class == "C"), n_boundary = sum(boundary),
            n_near_floor = sum(near_floor),
            mean_abs_db = mean(s2_max_pair[!boundary]), median_abs_db = median(s2_max_pair),
            mean_abs_db_partial = mean(p_max_pair[!boundary]),
            share_of_dif_X2 = sum(s2_X2[!anchor], na.rm = TRUE) / first(total_X2),
            s1_X2 = sum(s1_X2),
            .groups = "drop") |>
  group_by(analysis) |>
  mutate(share_of_items = n_items / sum(n_items),
         share_of_flagged = n_flagged / sum(n_flagged),
         share_of_s1_X2 = s1_X2 / sum(s1_X2)) |>
  ungroup()

dif_by_qtype <- all_dif |>
  group_by(analysis, family, question_type) |>
  summarise(n_items = n(), n_flagged = sum(class %in% c("B", "C")),
            n_large = sum(class == "C"), n_boundary = sum(boundary),
            mean_abs_db = mean(s2_max_pair[!boundary]),
            median_abs_db = median(s2_max_pair), .groups = "drop")

# class of every item across the variants of each family (robustness of flags)
flag_matrix <- all_dif |>
  select(family, analysis, item, short, question_type, is_control, class) |>
  pivot_wider(names_from = analysis, values_from = class)

# agreement of each variant's flags (class B/C vs A) with its family's primary fit
flag_agreement <- all_dif |>
  filter(family %in% c("A", "B"), !analysis %in% c("A", "B")) |>
  select(family, analysis, item, class) |>
  inner_join(all_dif |> filter(analysis %in% c("A", "B")) |>
               select(family, item, primary = class), by = c("family", "item")) |>
  group_by(family, analysis) |>
  summarise(n_common = n(), agreement = mean((class != "A") == (primary != "A")),
            flagged_primary = sum(primary != "A"), flagged_variant = sum(class != "A"),
            flagged_both = sum(primary != "A" & class != "A"), .groups = "drop")

# June-style bridge (DE-Leipzig vs Bogota): change relative to release-equivalent data
june_bridge <- map_dfr(results, "tests") |>
  filter(str_starts(analysis, "J_"), comparison == "scalar vs configural") |>
  select(analysis, items, n_items, X2, df, X2_df, p, dBIC) |>
  group_by(items) |>
  mutate(X2_change_vs_release = X2 / X2[analysis == "J_release"] - 1,
         X2_df_change_vs_release = X2_df / X2_df[analysis == "J_release"] - 1,
         dBIC_change_vs_release = dBIC / dBIC[analysis == "J_release"] - 1) |>
  ungroup()

# June values as REPORTED in the June chapters' text (quoted, not recomputed here),
# for side-by-side display next to june_bridge / dif_by_type
june_reported <- tribble(
  ~source,                           ~quantity,                                                       ~june_value,
  "old/tom_dif_multigroup.qmd",      "DE vs CO, all items: scalar vs configural X2/df",               "~29",
  "old/tom_dif_multigroup.qmd",      "DE vs CO, all items: BIC preference for configural",            "~580",
  "old/tom_dif_multigroup.qmd",      "DE vs CO, targets only: X2/df",                                 "~8",
  "old/tom_dif_multigroup.qmd",      "DE vs CO, targets only: BIC gap",                               "~30",
  "old/tom_dif_multigroup.qmd",      "partial scalar (targets) vs configural: p",                     "~0.8",
  "old/tom_dif_multigroup.qmd",      "mean |DIF| (anchored model), controls, logits",                 "~1.9",
  "old/tom_dif_multigroup.qmd",      "mean |DIF| (anchored model), targets, logits",                  "~0.5",
  "tasks/tom_reality_check_bug.qmd", "scalar vs configural X2, before -> after repairing defects",    "702 -> 212 (~70% less)",
  "tasks/tom_reality_check_bug.qmd", "scalar vs configural dBIC, before -> after repairing defects",  "550 -> 67")

# accuracy by language x age band for every item of the A/B analyses (policy
# exclusions applied, all included trials), to read DIF against sample age
ab_items <- union(results$A$items, results$B$items)
age_band_acc <- tr |>
  filter(!policy_exclude, !is.na(age), story_uid %in% ab_items,
         language %in% c("de-DE", "en-US", "es-CO", "es-AR")) |>
  mutate(age_band = cut(age, c(0, 6, 7, 8, 10, Inf), right = FALSE,
                        labels = c("<6", "6-7", "7-8", "8-10", "10+"))) |>
  group_by(item = story_uid, language, age_band) |>
  summarise(n = n(), acc = mean(correct), chance = min(chance), .groups = "drop") |>
  left_join(uid_meta(ab_items), by = "item")

tests <- map_dfr(results, "tests") |>
  left_join(specs |> select(analysis = id, family, label), by = "analysis")
fits <- map_dfr(results, "fits")
group_pars_all <- map_dfr(results, "group_pars")
samples <- imap_dfr(data_list, \(D, id) D$sample |> mutate(analysis = id, .before = 1))
sample_forms <- imap_dfr(data_list, \(D, id) D$sample_forms |> mutate(analysis = id, .before = 1))
item_cells <- imap_dfr(data_list, \(D, id) D$cells |> mutate(analysis = id, .before = 1)) |>
  left_join(uid_meta(unique(unlist(map(data_list, \(D) D$cells$item)))), by = "item")
convergence <- bind_rows(
  fits |> transmute(analysis, model, n_fits = 1L, converged, iterations,
                    n_fits_warned = as.integer(n_warnings > 0)),
  imap_dfr(results, \(r, id) bind_rows(
    tibble(analysis = id, model = "stage-1 DIF fits", n_fits = r$stage1_convergence$n,
           converged = r$stage1_convergence$all_converged,
           iterations = r$stage1_convergence$max_iter,
           n_fits_warned = r$stage1_convergence$n_fits_warned),
    tibble(analysis = id, model = "stage-2 DIF fits", n_fits = r$stage2_convergence$n,
           converged = r$stage2_convergence$all_converged,
           iterations = r$stage2_convergence$max_iter,
           n_fits_warned = r$stage2_convergence$n_fits_warned))))
numerics <- map_dfr(results, "numerics")
anchors <- imap_dfr(results, \(r, id) tibble(analysis = id, n_items = length(r$items),
                                             n_anchors = length(r$anchors),
                                             anchor_rule = r$anchor_rule,
                                             anchors_stable = r$anchors_stable,
                                             purification_rounds = nrow(r$purification),
                                             anchors = paste(str_remove(r$anchors, "^tom_"), collapse = "; "),
                                             boundary_configural = paste(r$boundary$configural, collapse = "; "),
                                             boundary_partial = paste(r$boundary$partial, collapse = "; ")))

out <- list(
  meta = list(created = Sys.time(), runtime_min = as.numeric(difftime(Sys.time(), t_start, units = "mins")),
              book_root = book_root, script = "tasks/_stories_fits_invariance.R",
              script_md5 = unname(tools::md5sum(file.path(book_root, "tasks/_stories_fits_invariance.R"))),
              input = file.path(data_dir, "stories_trials.rds"),
              input_md5 = unname(tools::md5sum(file.path(data_dir, "stories_trials.rds"))),
              data_provenance = data_provenance,
              mirt_version = as.character(packageVersion("mirt")), R_version = R.version.string,
              session_info = sessionInfo(),
              settings = list(N_MIN = N_MIN, ES_SMALL = ES_SMALL, ES_LARGE = ES_LARGE,
                              ALPHA = ALPHA, p_adjust = "BH", N_CYCLES = N_CYCLES,
                              MAX_PURIFY = MAX_PURIFY, MIN_ANCHOR = MIN_ANCHOR,
                              REF_GROUP = REF_GROUP, B_BOUND = B_BOUND)),
  specs = specs |> mutate(stories = map_chr(stories, \(s) paste(range(s), collapse = "-")),
                          groups = map_chr(groups, \(g) paste(unique(g), collapse = ", "))),
  samples = samples, sample_forms = sample_forms, item_cells = item_cells,
  tests = tests, fits = fits, convergence = convergence, group_pars = group_pars_all,
  anchors = anchors, dif = all_dif, dif_long = map_dfr(results, "dif_long"),
  dif_by_type = dif_by_type, dif_by_qtype = dif_by_qtype, flag_matrix = flag_matrix,
  flag_agreement = flag_agreement, june_bridge = june_bridge, june_reported = june_reported,
  age_band_acc = age_band_acc,
  model_choice = model_choice, robust_2pl = robust_2pl, numerics = numerics,
  model_choice_2pl = model_choice_2pl, tests_2pl = tests_2pl,
  dif_by_type_models = dif_by_type_models, rasch_2pl_agreement = rasch_2pl_agreement,
  g0_items = map(data_list, "g0_items") |> keep(\(x) length(x) > 0),
  n_chance_conflicts = map_int(data_list, "n_chance_conflicts")
)
saveRDS(out, out_file)
msg("saved ", out_file, " (", round(out$meta$runtime_min, 1), " min)")
