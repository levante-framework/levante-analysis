# _stories_fits_items.R
#
# Stories (Theory of Mind), consolidated chapter: ITEMS & CONTROLS.
#   (a) item table: story_uid x language (and x dataset x form): n, accuracy,
#       chance, exact binomial tests vs chance, item-rest point-biserial
#       (within dataset x form), weak/poor flags
#   (b) verdicts for the 12 generic ToM uids on the Airtable exclusion list,
#       story by story (28 instances), checked against the build's policy flags
#   (c) the June "controls" claims re-tested: person-level correlations,
#       gating of target age slopes by passing the story's control, and
#       marginal reliability with vs without controls (DE item-bank form and
#       UTDT-AR no-HA retest-B form), with LEVANTE-convention IRT
#       (Rasch + fixed guessing and 2PL + fixed guessing, g = chance)
#   (d) do construct schemes (old_group, new_construct) or question_type
#       explain item difficulty and item age slope?
#   (d2) age sensitivity above the guessing floor, compared across question
#       types with item and language random effects (2026-09 revision); also a
#       flipped-key check of the scoped rows and age-matched Spanish checks in (b)
#
# Input : data/stories_2026-09/stories_trials.rds (tasks/_build_stories_data.R)
#         data/stories_2026-09/raw/levante_metadata_items__v3_18.rds (exclusions)
#         data/tom_meta/tom_item_metadata.rds (prompt text only)
# Output: data/stories_2026-09/results_items.rds (the chapter only reads this)
#
# Run from a working directory OUTSIDE the repo (the book's renv has an mgcv
# binary that breaks mirt), passing the book root:
#   Rscript /path/to/levante-analysis/tasks/_stories_fits_items.R /path/to/levante-analysis
# (or set STORIES_BOOK_ROOT; default: the directory above this script's tasks/).
# Takes a few minutes (mirt + glmer fits).

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(stringr)
  library(tibble); library(readr); library(mirt); library(lme4)
})

args <- commandArgs(trailingOnly = TRUE)
script_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
default_root <- if (length(script_file) == 1) dirname(dirname(normalizePath(script_file))) else here::here()
root <- if (length(args) >= 1) args[1] else
  Sys.getenv("STORIES_BOOK_ROOT", default_root)
stopifnot(dir.exists(file.path(root, "data/stories_2026-09")))
out_file <- file.path(root, "data/stories_2026-09/results_items.rds")
set.seed(20260924)

# thresholds (fixed before looking at results, except R_WEAK_CI: see the flag rules)
MIN_N_FLAG   <- 30    # trials needed before an item cell is flagged
MIN_N_R      <- 20    # trials needed for a cell's item-rest r
P_BELOW      <- 0.01  # exact one-sided binomial, accuracy < chance
P_ABOVE      <- 0.05  # exact one-sided binomial, accuracy > chance
R_WEAK       <- 0.10  # item-rest r point estimate below this (reported, not used to flag)
R_WEAK_CI    <- 0.20  # item-rest r 95% CI upper bound below this = weak discrimination
ACC_CEILING  <- 0.95  # accuracy at/above this = ceiling (little information)
MIN_N_SLOPE  <- 50    # trials needed for an item x language age slope
MIN_MINORITY <- 10    # ... and at least this many correct AND incorrect (added after the
                      # first run: a .99-accuracy item gave a separated slope of -170)
MAX_SLOPE_SE <- 1     # slopes with a larger SE are reported but not analysed
MIN_N_CALIB  <- 30    # responses needed for an item in a language calibration
PRIORS_D     <- "norm, 0, 3"    # LEVANTE convention (levante-pilots 01_fit_irt_modular.qmd)
PRIORS_A     <- "norm, 1, 0.3"

# ---- 0. data ---------------------------------------------------------------

trials_raw <- read_rds(file.path(root, "data/stories_2026-09/stories_trials.rds"))
prompts <- read_rds(file.path(root, "data/tom_meta/tom_item_metadata.rds")) |>
  select(story_uid = item_uid, prompt)
meta_items <- read_rds(file.path(root, "data/stories_2026-09/raw/levante_metadata_items__v3_18.rds"))

tr <- trials_raw |>
  mutate(correct = as.integer(correct),
         cell = paste(dataset, form, sep = " / "),
         lang_es = str_starts(language, "es")) |>
  # rest score: proportion correct on the run's OTHER non-policy items
  group_by(run_id) |>
  mutate(n_ok = sum(!policy_exclude), s_ok = sum(correct[!policy_exclude]),
         rest = (s_ok - if_else(policy_exclude, 0L, correct)) /
                (n_ok - if_else(policy_exclude, 0L, 1L))) |>
  ungroup() |>
  mutate(rest = if_else(is.finite(rest), rest, NA_real_))

# items whose policy flag covers only some content versions (story-4 rc_2/_3
# on image 4g, story-2 rc_1 on 2e): item tables report the two parts separately
partial_uids <- tr |> group_by(story_uid) |>
  summarise(partial = any(policy_exclude) & !all(policy_exclude), .groups = "drop") |>
  filter(partial) |> pull(story_uid)
tr <- tr |>
  mutate(content = case_when(
    story_uid %in% partial_uids & policy_exclude ~ paste0("policy-flagged (", item_original, ")"),
    story_uid %in% partial_uids ~ "other content",
    TRUE ~ "all"))

# one response per run x story uid (checked; PR #27 removed the duplicates)
stopifnot(!anyDuplicated(tr[c("run_id", "story_uid")]))
stopifnot(tr |> group_by(story_uid) |> summarise(k = n_distinct(chance)) |> pull(k) |> max() == 1)

binom_p <- function(k, n, p, alt) map_dbl(seq_along(k), \(i) binom.test(k[i], n[i], p[i], alternative = alt)$p.value)
fisher_pool <- function(r, n) {
  ok <- !is.na(r) & n >= MIN_N_R & abs(r) < 1
  if (!any(ok)) return(c(r = NA_real_, lo = NA_real_, hi = NA_real_, n = 0))
  z <- atanh(r[ok]); w <- n[ok] - 3
  zm <- sum(w * z) / sum(w); se <- 1 / sqrt(sum(w))
  c(r = tanh(zm), lo = tanh(zm - 1.96 * se), hi = tanh(zm + 1.96 * se), n = sum(n[ok]))
}
safe_r <- function(x, y) {
  ok <- !is.na(x) & !is.na(y)
  if (sum(ok) < 3 || sd(x[ok]) == 0 || sd(y[ok]) == 0) NA_real_ else cor(x[ok], y[ok])
}

# ---- (a) item tables --------------------------------------------------------

item_keys <- tr |>
  distinct(story_uid, story, generic_uid, entry, old_group, question_type, is_control,
           new_construct, chance) |>
  left_join(prompts, by = "story_uid")

# cell level: story_uid x dataset x form
items_cell <- tr |>
  group_by(story_uid, content, language, dataset, form, cell) |>
  summarise(n = n(), n_correct = sum(correct), accuracy = mean(correct),
            chance = first(chance), median_age = median(age, na.rm = TRUE),
            r_ir = if (n() >= MIN_N_R) safe_r(correct, rest) else NA_real_,
            policy_trials = sum(policy_exclude), .groups = "drop") |>
  mutate(p_below = binom_p(n_correct, n, chance, "less"),
         p_above = binom_p(n_correct, n, chance, "greater"))

# Flag rules. Revised once after the first run (recorded in meta$notes): the
# first version flagged "weak" on the item-rest r POINT estimate (< .10), which
# flagged mostly small fCAT cells (n ~ 30-40, CI half-width ~ .3) and ceiling
# items; it also lumped below-chance items that still discriminate (systematic
# false-belief errors) with broken ones. Final rules:
#   poor         below chance (p < .01) with r_ir not credibly > 0, or r_ir CI < 0
#   below chance, discriminating   below chance but r_ir CI > 0
#   ceiling      accuracy >= .95
#   weak         not above chance (p >= .05), or r_ir CI upper bound < .20
#   ok           otherwise
# r_ir_below_10 keeps the original point-estimate criterion for reference.
flag_items <- function(df) df |>
  mutate(flag = case_when(
    n < MIN_N_FLAG ~ "n < 30",
    (p_below < P_BELOW & !(coalesce(r_ir_lo, 0) > 0)) | coalesce(r_ir_hi, 1) < 0 ~ "poor",
    p_below < P_BELOW ~ "below chance, discriminating",
    accuracy >= ACC_CEILING ~ "ceiling",
    p_above >= P_ABOVE | coalesce(r_ir_hi, 1) < R_WEAK_CI ~ "weak",
    TRUE ~ "ok"),
    flag = factor(flag, levels = c("poor", "below chance, discriminating", "weak", "ceiling", "ok", "n < 30")),
    flag_reason = case_when(
      flag == "poor" & p_below < P_BELOW ~ "below chance, item-rest r not > 0",
      flag == "poor" ~ "item-rest r CI < 0",
      flag == "below chance, discriminating" ~ "below chance, item-rest r CI > 0",
      flag == "weak" & p_above >= P_ABOVE ~ "not above chance",
      flag == "weak" ~ "item-rest r CI upper bound < .20",
      flag == "ceiling" ~ "accuracy >= .95",
      TRUE ~ NA_character_),
    r_ir_below_10 = !is.na(r_ir) & r_ir < R_WEAK)

# cell-level r CI (single correlation)
items_cell <- items_cell |>
  mutate(se_z = 1 / sqrt(pmax(n - 3, 1)),
         r_ir_lo = if_else(is.na(r_ir), NA_real_, tanh(atanh(pmin(r_ir, .999)) - 1.96 * se_z)),
         r_ir_hi = if_else(is.na(r_ir), NA_real_, tanh(atanh(pmin(r_ir, .999)) + 1.96 * se_z))) |>
  select(-se_z) |>
  flag_items() |>
  left_join(item_keys |> select(story_uid, story, entry, old_group, question_type, is_control),
            by = "story_uid") |>
  arrange(story, story_uid, language, dataset, form)

# language level: pooled counts, Fisher-pooled within-cell item-rest r
items_lang <- tr |>
  group_by(story_uid, content, language) |>
  summarise(n = n(), n_runs = n_distinct(run_id), n_correct = sum(correct),
            accuracy = mean(correct), chance = first(chance),
            median_age = median(age, na.rm = TRUE),
            datasets = paste(sort(unique(dataset)), collapse = ", "),
            policy_trials = sum(policy_exclude), spanish_fb_trials = sum(spanish_fb_flag),
            .groups = "drop") |>
  mutate(p_below = binom_p(n_correct, n, chance, "less"),
         p_above = binom_p(n_correct, n, chance, "greater"),
         q_below = p.adjust(p_below, "BH")) |>
  left_join(items_cell |> group_by(story_uid, content, language) |>
              summarise(pooled = list(fisher_pool(r_ir, n)), n_cells_r = sum(!is.na(r_ir) & n >= MIN_N_R),
                        .groups = "drop") |>
              mutate(r_ir = map_dbl(pooled, "r"), r_ir_lo = map_dbl(pooled, "lo"),
                     r_ir_hi = map_dbl(pooled, "hi"), n_r = map_dbl(pooled, "n")) |>
              select(-pooled),
            by = c("story_uid", "content", "language")) |>
  flag_items() |>
  left_join(item_keys |> select(-chance), by = "story_uid") |>
  select(story, story_uid, content, entry, old_group, question_type, is_control, new_construct, prompt,
         language, n, n_runs, accuracy, chance, p_below, q_below, p_above, r_ir, r_ir_lo, r_ir_hi,
         n_cells_r, flag, flag_reason, r_ir_below_10, policy_trials, spanish_fb_trials, median_age, datasets) |>
  arrange(story, story_uid, content, language)

# item level (all languages): for the chapter's one-line-per-item table
items_all <- tr |>
  group_by(story_uid, content) |>
  summarise(n = n(), accuracy = mean(correct), chance = first(chance),
            n_languages = n_distinct(language), policy_trials = sum(policy_exclude),
            .groups = "drop") |>
  left_join(items_cell |> group_by(story_uid, content) |>
              summarise(pooled = list(fisher_pool(r_ir, n)), .groups = "drop") |>
              mutate(r_ir = map_dbl(pooled, "r"), r_ir_lo = map_dbl(pooled, "lo"),
                     r_ir_hi = map_dbl(pooled, "hi")) |> select(-pooled),
            by = c("story_uid", "content")) |>
  left_join(items_lang |> filter(flag != "n < 30") |> group_by(story_uid, content) |>
              summarise(lang_poor = paste(language[flag == "poor"], collapse = ", "),
                        lang_below_disc = paste(language[flag == "below chance, discriminating"], collapse = ", "),
                        lang_weak = paste(language[flag == "weak"], collapse = ", "),
                        lang_ceiling = paste(language[flag == "ceiling"], collapse = ", "),
                        n_lang_flaggable = n(), .groups = "drop"),
            by = c("story_uid", "content")) |>
  left_join(item_keys |> select(-chance), by = "story_uid") |>
  mutate(across(c(lang_poor, lang_below_disc, lang_weak, lang_ceiling), \(x) na_if(x, ""))) |>
  arrange(story, story_uid, content)

flag_summary <- list(
  by_language = items_lang |> count(language, flag) |>
    pivot_wider(names_from = flag, values_from = n, values_fill = 0L),
  by_qtype = items_lang |> filter(flag != "n < 30") |> count(question_type, flag) |>
    pivot_wider(names_from = flag, values_from = n, values_fill = 0L),
  weak_point_rule = items_lang |> filter(flag != "n < 30") |>
    count(language, weak_by_point_estimate = r_ir_below_10),
  poor_not_policy = items_lang |>
    filter(flag %in% c("poor", "below chance, discriminating"), policy_trials == 0) |>
    select(story_uid, language, n, accuracy, chance, p_below, r_ir, r_ir_lo, flag, flag_reason,
           policy_trials, spanish_fb_trials, datasets)
)

# ---- (b) verdicts for the 12 generic exclusion-list uids --------------------

excl_generic <- meta_items$exclusions |> distinct(generic_uid = item_uid) |>
  filter(str_starts(generic_uid, "tom_")) |> pull(generic_uid) |> sort()
stopifnot(length(excl_generic) == 12)

inst_stats <- function(df) {
  cells <- df |> group_by(cell) |>
    summarise(n = n(), r = if (n() >= MIN_N_R) safe_r(correct, rest) else NA_real_, .groups = "drop")
  pr <- fisher_pool(cells$r, cells$n)
  tibble(n = nrow(df), accuracy = mean(df$correct), chance = first(df$chance),
         p_below = binom.test(sum(df$correct), nrow(df), first(df$chance), alternative = "less")$p.value,
         p_above = binom.test(sum(df$correct), nrow(df), first(df$chance), alternative = "greater")$p.value,
         r_ir = pr[["r"]], r_ir_lo = pr[["lo"]], r_ir_hi = pr[["hi"]])
}

excl_tr <- tr |> filter(generic_uid %in% excl_generic)

verdict_evidence <- excl_tr |>
  group_by(generic_uid, story, story_uid) |>
  group_modify(\(d, k) {
    all_s <- inst_stats(d)
    keep <- d |> filter(!policy_exclude, !spanish_fb_flag)
    kept_s <- if (nrow(keep) > 0) inst_stats(keep) else
      tibble(n = 0L, accuracy = NA_real_, chance = NA_real_, p_below = NA_real_, p_above = NA_real_,
             r_ir = NA_real_, r_ir_lo = NA_real_, r_ir_hi = NA_real_)
    flagged <- d |> filter(policy_exclude | spanish_fb_flag)
    lang <- d |> filter(!policy_exclude) |> group_by(language) |>
      summarise(n = n(), k = sum(correct), acc = mean(correct), ch = first(chance), .groups = "drop") |>
      mutate(pb = binom_p(k, n, ch, "less"))
    tibble(all_s |> rename_with(\(x) paste0(x, "_all")),
           kept_s |> rename_with(\(x) paste0(x, "_kept")),
           n_policy = sum(d$policy_exclude), n_spanish = sum(d$spanish_fb_flag),
           acc_flagged = if (nrow(flagged)) mean(flagged$correct) else NA_real_,
           policy_reason = paste(unique(na.omit(d$policy_reason)), collapse = "; "),
           policy_versions = paste(sort(unique(d$item_original[d$policy_exclude])), collapse = ", "),
           versions = paste(sort(unique(d$item_original)), collapse = ", "),
           n_lang_below = sum(lang$n >= MIN_N_FLAG & lang$pb < P_BELOW),
           acc_by_language = paste(sprintf("%s %.2f (%d)", lang$language, lang$acc, lang$n), collapse = "; "))
  }) |>
  ungroup()

# Spanish reality_known false belief (the build's spanish_fb_flag, a sensitivity set):
# does the item still discriminate in each es-* language, and does an age-matched
# comparison with the other languages still find it harder in Spanish?
es_disc <- items_lang |>
  filter(content == "all", str_starts(language, "es"), flag != "n < 30") |>
  group_by(story_uid) |>
  summarise(es_languages = paste(language, collapse = ", "),
            es_r_ir = paste(sprintf("%s %.2f [%.2f, %.2f]", language, r_ir, r_ir_lo, r_ir_hi), collapse = "; "),
            # same thresholds as the weak-discrimination rule: r >= .10 and CI upper bound >= .20
            es_discriminates = all(!is.na(r_ir) & r_ir >= R_WEAK & r_ir_hi >= R_WEAK_CI), .groups = "drop")
AGE_BANDS <- c(0, 6, 7, 8, 10, Inf)
rk_fb_uids <- sort(unique(tr$story_uid[tr$spanish_fb_flag]))
rk_fb_age <- tr |>
  filter(story_uid %in% rk_fb_uids, !is.na(age)) |>
  mutate(age_band = cut(age, AGE_BANDS, right = FALSE, labels = c("<6", "6-7", "7-8", "8-10", "10+"))) |>
  group_by(story_uid, story, language, age_band) |>
  summarise(n = n(), accuracy = mean(correct), chance = first(chance), .groups = "drop")
# age-matched: glm(correct ~ age + Spanish) on the ages both groups cover (5th-95th
# percentiles of each group's ages, intersected); Spanish effect in logits
rk_fb_age_matched <- map_dfr(rk_fb_uids, \(u) {
  d <- tr |> filter(story_uid == u, !is.na(age)) |> mutate(es = as.integer(str_starts(language, "es")))
  rng_es <- quantile(d$age[d$es == 1], c(.05, .95)); rng_ot <- quantile(d$age[d$es == 0], c(.05, .95))
  lo <- max(rng_es[1], rng_ot[1]); hi <- min(rng_es[2], rng_ot[2])
  dm <- d |> filter(age >= lo, age <= hi)
  m <- glm(correct ~ age + es, family = binomial, data = dm)
  ci <- confint.default(m)["es", ]
  at7 <- predict(m, newdata = tibble(age = 7, es = c(0, 1)), type = "response")
  tibble(story_uid = u, age_lo = lo, age_hi = hi, n_es = sum(dm$es), n_other = sum(1 - dm$es),
         other_languages = paste(sort(unique(dm$language[dm$es == 0])), collapse = ", "),
         es_logit = coef(m)[["es"]], es_lo = ci[[1]], es_hi = ci[[2]],
         acc_at_7_other = at7[[1]], acc_at_7_es = at7[[2]], chance = first(d$chance))
})

item_verdicts <- verdict_evidence |>
  left_join(es_disc, by = "story_uid") |>
  left_join(rk_fb_age_matched |> select(story_uid, es_logit, es_lo, es_hi, acc_at_7_es), by = "story_uid") |>
  mutate(
    evidence_kept = case_when(
      n_kept == 0 ~ "none left",
      # same rules as the item flags in (a)
      (p_below_kept < P_BELOW & !(coalesce(r_ir_lo_kept, 0) > 0)) | coalesce(r_ir_hi_kept, 1) < 0 ~ "poor",
      n_lang_below > 0 & n_spanish == 0 ~ "poor in a language",
      !(coalesce(r_ir_lo_kept, 0) > 0) & coalesce(r_ir_kept, 0) < R_WEAK ~ "no credible discrimination",
      p_above_kept >= P_ABOVE | coalesce(r_ir_hi_kept, 1) < R_WEAK_CI ~ "weak",
      TRUE ~ "ok"),
    # Spanish flag: un-exclude if the item discriminates in every es-* language (below
    # chance but informative: the reality bias); otherwise keep the analysis flag, and
    # write no Airtable row until the translation check (the age-matched gap is reported)
    es_failing = n_spanish > 0 & !coalesce(es_discriminates, FALSE),
    verdict = case_when(
      n_policy == n_all ~ "keep excluded",
      n_policy > 0 ~ "scoped exclusion",
      n_spanish > 0 & !es_failing ~ "un-exclude (below chance in Spanish, but discriminates)",
      n_spanish > 0 ~ "un-exclude; flag es-* in analyses, no Airtable row until the translation check",
      evidence_kept == "ok" ~ "un-exclude",
      evidence_kept == "weak" ~ "un-exclude (weak item)",
      TRUE ~ "REVIEW: poor but not policy-flagged"),
    scope = case_when(
      verdict == "keep excluded" ~ "all datasets, languages and content versions",
      verdict == "scoped exclusion" ~ paste0("item_original ", policy_versions,
                                             if_else(story == 2, " before 2024-07-01 (Bogota V0)", "")),
      es_failing ~ "analysis flag on es-AR / es-CO trials (reality_known false belief)",
      TRUE ~ "none"),
    # the build's policy flags were set from the 2026-09 audit; this re-checks them with the
    # item-screen rules. For "keep excluded" and "un-exclude" rows the agreement holds by
    # construction; the informative checks are the scoped rows and the Spanish rows.
    consistent_with_policy = case_when(
      verdict == "keep excluded" ~ TRUE,
      verdict == "scoped exclusion" ~ evidence_kept %in% c("ok", "weak") & acc_flagged < chance_all,
      str_starts(verdict, "un-exclude") ~ TRUE,
      TRUE ~ FALSE),
    check_is_informative = verdict == "scoped exclusion" | n_spanish > 0,
    proposed_airtable_row = case_when(
      verdict == "keep excluded" ~ paste0(story_uid, " | all datasets"),
      verdict == "scoped exclusion" & story == 4 ~
        paste0(story_uid, " | ", scope, ": exclude, or re-key to 'no' if the content team confirms the 4g scene"),
      verdict == "scoped exclusion" ~ paste0(story_uid, " | ", scope, ": exclude"),
      es_failing ~ paste0(story_uid, " | none yet (language row pending the translation check)"),
      TRUE ~ paste0(story_uid, " | remove generic row")),
    across(c(accuracy_all, accuracy_kept, acc_flagged, r_ir_all, r_ir_kept), \(x) round(x, 3))) |>
  arrange(generic_uid, story)

# The scoped rows under a flipped key (2-option items: correct' = 1 - correct). The rest
# score excludes policy items, so the flipped item-rest r is the negative of the original;
# it is recomputed here, within dataset x form and Fisher-pooled, by content and language.
flip_stats <- function(d) {
  cells <- d |> group_by(cell) |>
    summarise(n = n(), r = if (n() >= MIN_N_R) safe_r(correct, rest) else NA_real_,
              r_flip = if (n() >= MIN_N_R) safe_r(1L - correct, rest) else NA_real_, .groups = "drop")
  p <- fisher_pool(cells$r, cells$n); pf <- fisher_pool(cells$r_flip, cells$n)
  tibble(n = nrow(d), accuracy = mean(d$correct), accuracy_flipped = 1 - mean(d$correct),
         r_ir = p[["r"]], r_ir_lo = p[["lo"]], r_ir_hi = p[["hi"]],
         r_ir_flipped = pf[["r"]], r_ir_flipped_lo = pf[["lo"]], r_ir_flipped_hi = pf[["hi"]])
}
flip_uids <- c("tom_story4_deception_reality_check_2", "tom_story4_deception_reality_check_3",
               "tom_story2_moral_reasoning_reality_check_1")
flip_d <- tr |> filter(story_uid %in% flip_uids) |>
  mutate(content_part = if_else(policy_exclude, paste("flagged:", item_original),
                                paste("other content:", if_else(story == 4, item_original, "2f/2g and later"))))
flip_check <- bind_rows(
  flip_d |> group_by(story_uid, content_part) |> group_modify(\(d, k) flip_stats(d)) |>
    ungroup() |> mutate(language = "all"),
  flip_d |> group_by(story_uid, content_part, language) |> group_modify(\(d, k) flip_stats(d)) |>
    ungroup()) |>
  arrange(story_uid, content_part, language != "all", language)
# story 2's two reality checks in V0 Bogota vs later content: which option children chose
story2_rc_responses <- tr |>
  filter(story == 2, str_detect(entry, "reality_check"), !is.na(response),
         !str_starts(answer, "hostile-attribution")) |>
  mutate(content = if_else(item_original == "2e" & dataset == "pilot_uniandes_co_bogota" &
                             timestamp < as.POSIXct("2024-07-01", tz = "UTC"),
                           "V0 Bogota (2e)", "later content")) |>
  count(entry, content, key = answer, response) |>
  group_by(entry, content) |> mutate(share = n / sum(n)) |> ungroup()

# the scoped instances: evidence split by scope (flagged vs not flagged trials)
scoped_split <- excl_tr |>
  filter(story_uid %in% item_verdicts$story_uid[item_verdicts$verdict != "un-exclude" &
                                                  item_verdicts$verdict != "un-exclude (weak item)"]) |>
  mutate(scope_part = case_when(policy_exclude ~ paste("flagged:", item_original),
                                spanish_fb_flag ~ "flagged: es-*",
                                TRUE ~ "not flagged")) |>
  group_by(story_uid, scope_part, language) |>
  summarise(n = n(), k = sum(correct), accuracy = mean(correct), chance = first(chance),
            .groups = "drop") |>
  mutate(p_below = binom_p(k, n, chance, "less")) |>
  select(-k) |>
  arrange(story_uid, scope_part, language)

verdict_counts <- item_verdicts |> count(verdict, consistent_with_policy)

# ---- (c) controls -----------------------------------------------------------

# (c1) person-level correlations
person_scores <- function(df) df |>
  group_by(dataset, form, cell, language, run_id, age) |>
  summarise(n_ctrl = sum(is_control), n_tgt = sum(!is_control),
            control = if (any(is_control)) mean(correct[is_control]) else NA_real_,
            target = if (any(!is_control)) mean(correct[!is_control]) else NA_real_,
            .groups = "drop")
person <- person_scores(tr |> filter(!policy_exclude))
person_allitems <- person_scores(tr)

pooled_cors <- function(p, label) {
  p <- p |> filter(!is.na(age), !is.na(control), !is.na(target))
  tibble(item_set = label, n_runs = nrow(p),
         r_control_target = cor(p$control, p$target),
         r_control_age = cor(p$control, p$age),
         r_target_age = cor(p$target, p$age))
}
cell_cors <- function(p) p |>
  filter(!is.na(age), !is.na(control), !is.na(target)) |>
  group_by(dataset, form, cell, language) |>
  filter(n() >= 30) |>
  summarise(n_runs = n(), median_items = median(n_ctrl + n_tgt), median_ctrl = median(n_ctrl),
            mean_control = mean(control), mean_target = mean(target),
            r_control_target = safe_r(control, target),
            r_control_age = safe_r(control, age),
            r_target_age = safe_r(target, age),
            r_control_target_partial_age = {
              rct <- safe_r(control, target); rca <- safe_r(control, age); rta <- safe_r(target, age)
              (rct - rca * rta) / sqrt((1 - rca^2) * (1 - rta^2))
            }, .groups = "drop")
controls_cells <- cell_cors(person)
pool_cells <- function(cc, label) {
  f <- \(v) fisher_pool(cc[[v]], cc$n_runs)
  tibble(item_set = label, n_cells = nrow(cc), n_runs = sum(cc$n_runs),
         r_control_target = f("r_control_target")[["r"]],
         r_control_target_lo = f("r_control_target")[["lo"]],
         r_control_target_hi = f("r_control_target")[["hi"]],
         r_control_age = f("r_control_age")[["r"]],
         r_target_age = f("r_target_age")[["r"]],
         r_control_target_partial_age = f("r_control_target_partial_age")[["r"]])
}
controls_cor <- bind_rows(
  pooled_cors(person, "policy items dropped") |> mutate(method = "pooled over runs (June method)"),
  pooled_cors(person_allitems, "all items (June item set)") |> mutate(method = "pooled over runs (June method)"),
  pool_cells(controls_cells, "policy items dropped") |>
    mutate(method = "within dataset x form, Fisher-pooled"),
  pool_cells(cell_cors(person_allitems), "all items (June item set)") |>
    mutate(method = "within dataset x form, Fisher-pooled")) |>
  relocate(method, item_set)

# (c2) gating: does passing a story's control(s) sharpen target age slopes?
story_inst <- tr |>
  filter(!policy_exclude, !is.na(age)) |>
  group_by(dataset, form, language, run_id, age, story) |>
  summarise(n_ctrl = sum(is_control), n_tgt = sum(!is_control),
            ctrl_pass = all(correct[is_control] == 1),
            target_acc = mean(correct[!is_control]), .groups = "drop") |>
  filter(n_ctrl > 0, n_tgt > 0)

gating_june <- story_inst |>
  group_by(ctrl_pass) |>
  summarise(n_story_instances = n(), mean_target_acc = mean(target_acc),
            mean_age = mean(age),
            target_age_beta_perSD = coef(lm(target_acc ~ scale(age)))[2], .groups = "drop") |>
  mutate(method = "June method: lm(story target accuracy ~ scale(age)) by control pass, pooled")

gate_trials <- tr |>
  filter(!policy_exclude, !is.na(age), !is_control) |>
  inner_join(story_inst |> select(run_id, story, ctrl_pass), by = c("run_id", "story")) |>
  mutate(age_c = age - 8, ctrl_pass = as.integer(ctrl_pass))

fit_gate <- function(d, label) {
  f <- if (n_distinct(d$dataset) > 1)
    correct ~ age_c * ctrl_pass + dataset + (1 | run_id) + (1 | story_uid) else
    correct ~ age_c * ctrl_pass + (1 | run_id) + (1 | story_uid)
  m <- glmer(f, data = d, family = binomial, control = glmerControl(optimizer = "bobyqa"))
  b <- fixef(m); V <- as.matrix(vcov(m))
  s_fail <- b[["age_c"]]; se_fail <- sqrt(V["age_c", "age_c"])
  s_pass <- b[["age_c"]] + b[["age_c:ctrl_pass"]]
  se_pass <- sqrt(V["age_c", "age_c"] + V["age_c:ctrl_pass", "age_c:ctrl_pass"] +
                    2 * V["age_c", "age_c:ctrl_pass"])
  se_int <- sqrt(V["age_c:ctrl_pass", "age_c:ctrl_pass"])
  tibble(sample = label, n_trials = nrow(d), n_runs = n_distinct(d$run_id),
         pct_instances_fail = 100 * mean(d |> distinct(run_id, story, ctrl_pass) |> pull(ctrl_pass) == 0),
         pass_effect_at_age8 = b[["ctrl_pass"]],
         age_slope_fail = s_fail, age_slope_fail_se = se_fail,
         age_slope_pass = s_pass, age_slope_pass_se = se_pass,
         interaction = b[["age_c:ctrl_pass"]], interaction_se = se_int,
         interaction_p = 2 * pnorm(-abs(b[["age_c:ctrl_pass"]] / se_int)),
         singular = isSingular(m))
}
message("gating glmer fits ...")
gating_glmm <- bind_rows(
  fit_gate(gate_trials, "all languages"),
  map_dfr(sort(unique(gate_trials$language)), \(l) fit_gate(gate_trials |> filter(language == l), l)))

gating_plot <- story_inst |>
  mutate(age_bin = pmin(pmax(floor(age), 4), 13)) |>
  group_by(language, age_bin, ctrl_pass) |>
  summarise(n = n(), target_acc = mean(target_acc), .groups = "drop")

# (c3) reliability with vs without controls
mirt_fit <- function(mat, guess, itemtype, priors = TRUE) {
  J <- ncol(mat)
  prior <- paste0("PRIOR = (1-", J, ", d, ", PRIORS_D, ")",
                  if (itemtype == "2PL") paste0(", (1-", J, ", a1, ", PRIORS_A, ")") else "")
  mod_str <- paste0("F = 1-", J, if (priors) paste0("\n", prior) else "")
  mirt(mat, mirt.model(mod_str), itemtype = itemtype, guess = guess, verbose = FALSE,
       technical = list(NCYCLES = 5000))
}
build_mat <- function(d, min_n = MIN_N_CALIB) {
  keep_items <- d |> group_by(story_uid) |>
    summarise(n = n(), v = n_distinct(correct), .groups = "drop") |>
    filter(n >= min_n, v == 2) |> pull(story_uid)
  w <- d |> filter(story_uid %in% keep_items) |>
    select(run_id, story_uid, correct) |>
    pivot_wider(names_from = story_uid, values_from = correct)
  m <- as.matrix(w[, -1]); rownames(m) <- w$run_id
  m <- m[rowSums(!is.na(m)) > 0, , drop = FALSE]
  g <- d |> distinct(story_uid, chance) |> tibble::deframe()
  list(mat = m, guess = unname(g[colnames(m)]))
}
# mirt::marginal_rxx() integrates TI / (TI + var) over N(0, 1). For Rasch fits
# (latent variance estimated) that is not the marginal reliability; this uses
# the fitted N(mu, var): E[ var * I / (var * I + 1) ]. Identical for 2PL (var = 1).
marg_rxx_model <- function(mod) {
  cf <- coef(mod, simplify = TRUE)
  mu <- as.vector(cf$means); v <- as.vector(cf$cov)
  th <- seq(mu - 6 * sqrt(v), mu + 6 * sqrt(v), length.out = 801)
  I <- testinfo(mod, matrix(th))
  w <- dnorm(th, mu, sqrt(v)); w <- w / sum(w)
  sum(w * v * I / (v * I + 1))
}
alpha_cc <- function(mat) {
  m <- mat[, colMeans(!is.na(mat)) >= 0.9, drop = FALSE]
  m <- m[complete.cases(m), , drop = FALSE]
  if (ncol(m) < 3 || nrow(m) < 30) return(c(alpha = NA_real_, n = nrow(m), k = ncol(m)))
  a <- suppressWarnings(psych::alpha(m, check.keys = FALSE, warnings = FALSE)$total$raw_alpha)
  c(alpha = a, n = nrow(m), k = ncol(m))
}
rel_row <- function(d, label, sample, itemtype, use_guess = TRUE, min_n = MIN_N_CALIB, priors = TRUE) {
  b <- build_mat(d, min_n)
  guess <- if (use_guess) b$guess else rep(0, ncol(b$mat))
  mod <- mirt_fit(b$mat, guess, itemtype, priors)
  fs <- fscores(mod, method = "EAP", full.scores.SE = TRUE)
  ages <- d |> distinct(run_id, age) |> tibble::deframe()
  th <- tibble(run_id = rownames(b$mat), theta = fs[, "F"], se = fs[, "SE_F"])
  list(row = tibble(sample = sample, items = label, model = itemtype,
                    guessing = if (use_guess) "fixed at chance" else "none",
                    n_runs = nrow(b$mat), n_items = ncol(b$mat),
                    n_controls = sum(colnames(b$mat) %in% d$story_uid[d$is_control]),
                    mean_items_answered = mean(rowSums(!is.na(b$mat))),
                    marginal_rxx_mirt = as.numeric(marginal_rxx(mod)),
                    marginal_rxx = marg_rxx_model(mod),
                    empirical_rxx = as.numeric(empirical_rxx(fs)),
                    latent_var = as.vector(coef(mod, simplify = TRUE)$cov),
                    alpha_complete_cases = alpha_cc(b$mat)[["alpha"]],
                    alpha_n_runs = alpha_cc(b$mat)[["n"]], alpha_n_items = alpha_cc(b$mat)[["k"]],
                    r_theta_age = safe_r(th$theta, unname(ages[th$run_id])),
                    converged = extract.mirt(mod, "converged")),
       theta = th)
}
rel_set <- function(d, sample) {
  out <- list()
  for (it in c("Rasch", "2PL")) {
    a <- rel_row(d, "all items", sample, it)
    t <- rel_row(d |> filter(!is_control), "targets only", sample, it)
    r_at <- inner_join(a$theta, t$theta, by = "run_id")
    out[[it]] <- bind_rows(a$row, t$row) |>
      mutate(r_theta_all_vs_targets = cor(r_at$theta.x, r_at$theta.y))
  }
  bind_rows(out)
}
message("reliability fits ...")
de_fixed <- tr |> filter(dataset == "pilot_mpieva_de_main", form == "item_bank", !policy_exclude)
ar_fixed <- tr |> filter(dataset == "rfp1_utdt_ar_main", form == "no_ha_retest_B", !policy_exclude)
reliability <- bind_rows(
  rel_set(de_fixed, "DE Leipzig, item-bank form (stories 1-6)"),
  rel_set(ar_fixed, "UTDT-AR main, no-HA retest-B form (stories 13-18)"),
  rel_set(ar_fixed |> filter(!spanish_fb_flag),
          "UTDT-AR main, no-HA retest-B form, without the flagged es false-belief item"))
# June-method replica, on June's item set: the Leipzig forms that existed in June
# (item_bank, retest_A, retest_B), all runs, released (pre-PR #27) uids, policy items
# kept, items answered by > 15% of runs (58 items; June's rendered table used 58 and,
# for targets only, 42); pure Rasch without guessing or priors, as June fitted it. The same pool as
# june_pool_check in tasks/_stories_fits_dimensionality.R.
june_pool <- tr |>
  filter(dataset == "pilot_mpieva_de_main", form %in% c("item_bank", "retest_A", "retest_B"),
         !is.na(story_uid_pre_pr27)) |>
  arrange(run_id, trial_number) |>
  distinct(run_id, story_uid_pre_pr27, .keep_all = TRUE) |>
  mutate(story_uid = story_uid_pre_pr27,
         is_control = str_detect(story_uid_pre_pr27, "reality_check"))
june_min_n <- floor(0.15 * n_distinct(june_pool$run_id)) + 1   # > 15% of runs
june_a <- rel_row(june_pool, "all items", "DE Leipzig, June item pool (June method)", "Rasch",
                  use_guess = FALSE, min_n = june_min_n, priors = FALSE)
june_t <- rel_row(june_pool |> filter(!is_control), "targets only",
                  "DE Leipzig, June item pool (June method)", "Rasch",
                  use_guess = FALSE, min_n = june_min_n, priors = FALSE)
jr <- inner_join(june_a$theta, june_t$theta, by = "run_id")
reliability <- bind_rows(reliability,
                         bind_rows(june_a$row, june_t$row) |>
                           mutate(r_theta_all_vs_targets = cor(jr$theta.x, jr$theta.y)))

# ---- (d) construct schemes vs item difficulty and age slope ------------------

message("per-language calibrations ...")
calib <- tr |> filter(!policy_exclude) |>
  group_by(language) |>
  group_modify(\(d, k) {
    b <- build_mat(d)
    mod <- mirt_fit(b$mat, b$guess, "Rasch")
    cf <- coef(mod, simplify = TRUE)$items
    tibble(story_uid = colnames(b$mat), d = cf[, "d"], g = cf[, "g"],
           n_resp = colSums(!is.na(b$mat)), n_runs_model = nrow(b$mat),
           converged = extract.mirt(mod, "converged"))
  }) |>
  ungroup() |>
  mutate(difficulty = -d) |>
  group_by(language) |>
  mutate(difficulty_z = as.numeric(scale(difficulty))) |>
  ungroup()

age_slopes <- tr |> filter(!policy_exclude, !is.na(age)) |>
  group_by(story_uid, language) |>
  filter(n() >= MIN_N_SLOPE, sum(correct) >= MIN_MINORITY, sum(1 - correct) >= MIN_MINORITY) |>
  group_modify(\(d, k) {
    f <- if (n_distinct(d$dataset) > 1) correct ~ age + dataset else correct ~ age
    m <- glm(f, data = d, family = binomial)
    tibble(n_slope = nrow(d), age_slope = coef(m)[["age"]],
           age_slope_se = sqrt(vcov(m)["age", "age"]))
  }) |>
  ungroup() |>
  mutate(slope_ok = age_slope_se <= MAX_SLOPE_SE)

item_lang_d <- tr |>
  filter(!policy_exclude) |>
  group_by(story_uid, language) |>
  summarise(n = n(), accuracy = mean(correct), chance = first(chance), .groups = "drop") |>
  filter(n >= MIN_N_CALIB) |>
  left_join(item_keys |> select(story, story_uid, question_type, old_group, new_construct, is_control),
            by = "story_uid") |>
  left_join(calib |> select(language, story_uid, difficulty, difficulty_z, n_resp), by = c("language", "story_uid")) |>
  left_join(age_slopes, by = c("story_uid", "language")) |>
  mutate(age_slope_raw = age_slope,
         age_slope = if_else(coalesce(slope_ok, FALSE), age_slope, NA_real_)) |>
  mutate(cc_acc = pmin(pmax((accuracy - chance) / (1 - chance), 0.01), 0.99),
         difficulty_ccacc = -qlogis(cc_acc)) |>
  group_by(language) |>
  mutate(difficulty_ccacc_z = as.numeric(scale(difficulty_ccacc))) |>
  ungroup()

schemes <- list("old_group (6)" = "old_group", "new_construct (6)" = "new_construct",
                "question_type (4)" = "question_type", "control vs target (2)" = "is_control",
                "question_type + old_group" = c("question_type", "old_group"))
r2_row <- function(df, y, terms, base = NULL) {
  df <- df |> filter(!is.na(.data[[y]]))
  if (nrow(df) < 10 || any(map_int(terms, \(t) n_distinct(df[[t]])) < 2))
    return(tibble(n_items = nrow(df), R2 = NA_real_, adj_R2 = NA_real_, dR2 = NA_real_,
                  F = NA_real_, df1 = NA_real_, df2 = NA_real_, p = NA_real_))
  m0 <- lm(reformulate(if (is.null(base)) "1" else base, y), df)
  m1 <- lm(reformulate(c(base, terms), y), df)
  a <- anova(m0, m1)
  tibble(n_items = nrow(df), R2 = summary(m1)$r.squared, adj_R2 = summary(m1)$adj.r.squared,
         dR2 = summary(m1)$r.squared - summary(m0)$r.squared,
         F = a$F[2], df1 = a$Df[2], df2 = a$Res.Df[2], p = a$`Pr(>F)`[2])
}
item_means <- item_lang_d |>
  group_by(story_uid, question_type, old_group, new_construct, is_control) |>
  summarise(difficulty_z = mean(difficulty_z, na.rm = TRUE),
            age_slope = mean(age_slope, na.rm = TRUE), n_lang = n(), .groups = "drop") |>
  mutate(across(c(difficulty_z, age_slope), \(x) if_else(is.nan(x), NA_real_, x)))
construct_r2 <- imap_dfr(schemes, \(terms, lab) bind_rows(
  r2_row(item_lang_d, "difficulty_z", terms) |>
    mutate(outcome = "difficulty (IRT, z within language)", unit = "item x language"),
  r2_row(item_lang_d, "difficulty_ccacc_z", terms) |>
    mutate(outcome = "difficulty (chance-corrected accuracy, z within language)", unit = "item x language"),
  r2_row(item_means, "difficulty_z", terms) |>
    mutate(outcome = "difficulty (IRT, z within language)", unit = "item (mean over languages)"),
  r2_row(item_lang_d, "age_slope", terms, base = "language") |>
    mutate(outcome = "age slope (logit per year)", unit = "item x language (language FE; dR2 over it)"),
  r2_row(item_means, "age_slope", terms) |>
    mutate(outcome = "age slope (logit per year)", unit = "item (mean over languages)")) |>
  mutate(scheme = lab)) |>
  relocate(outcome, unit, scheme)

construct_r2_by_lang <- item_lang_d |>
  group_by(language) |>
  group_modify(\(d, k) imap_dfr(schemes[1:3], \(terms, lab) bind_rows(
    r2_row(d, "difficulty_z", terms) |> mutate(outcome = "difficulty"),
    r2_row(d, "age_slope", terms) |> mutate(outcome = "age slope")) |>
      mutate(scheme = lab))) |>
  ungroup() |>
  select(language, outcome, scheme, n_items, R2, adj_R2, p)

# sensitivity: without the Spanish-flagged reality_known false-belief items
construct_r2_sens <- imap_dfr(schemes[1:3], \(terms, lab) {
  d <- item_lang_d |> filter(!(str_starts(language, "es") & str_detect(story_uid, "reality_known_false_belief")))
  bind_rows(
    r2_row(d, "difficulty_z", terms) |> mutate(outcome = "difficulty (IRT, z within language)"),
    r2_row(d, "age_slope", terms, base = "language") |> mutate(outcome = "age slope (logit per year)")) |>
    mutate(scheme = lab, unit = "item x language, es reality_known FB dropped")
}) |> relocate(outcome, unit, scheme)

# level means for each scheme (for the construct figure / prose)
scheme_means <- map_dfr(c("old_group", "new_construct", "question_type"), \(v) item_lang_d |>
  group_by(level = .data[[v]]) |>
  summarise(n_item_lang = n(), n_items = n_distinct(story_uid),
            mean_difficulty_z = mean(difficulty_z, na.rm = TRUE),
            mean_age_slope = mean(age_slope, na.rm = TRUE),
            se_age_slope = sd(age_slope, na.rm = TRUE) / sqrt(sum(!is.na(age_slope))),
            n_slopes = sum(!is.na(age_slope)), .groups = "drop") |>
  mutate(scheme = v, .before = 1))

qtype_summary <- item_lang_d |>
  group_by(question_type) |>
  summarise(n_item_lang = n(), n_items = n_distinct(story_uid),
            mean_accuracy = mean(accuracy), mean_difficulty_z = mean(difficulty_z, na.rm = TRUE),
            mean_age_slope = mean(age_slope, na.rm = TRUE),
            median_age_slope = median(age_slope, na.rm = TRUE),
            n_slopes = sum(!is.na(age_slope)), .groups = "drop") |>
  arrange(desc(mean_age_slope))

# ---- (d2) age sensitivity above the guessing floor (added in the 2026-09 revision) ----
# The logit slopes above come from glm on raw correctness (no floor), and their
# filter (>= 10 correct and >= 10 incorrect) removes ceiling cells but keeps cells near
# the floor, where slopes are attenuated. Here each item x language cell is refitted
# with the floor fixed at chance, P = c + (1 - c) logistic(a + b age [+ dataset]), and
# cells are kept symmetrically: chance-corrected accuracy (acc - c) / (1 - c) within
# [.10, .90], n >= 50, SE(b) <= 1. The probability-scale slope is the average marginal
# effect dP/dage over the cell's trials. Question types are compared with a
# random-effects meta-regression (metafor::rma.mv; sampling variances SE^2, crossed
# random intercepts for item and language), which does not treat item x language cells
# as independent.
CC_LO <- 0.10; CC_HI <- 0.90
# direct ML (BFGS with the analytic gradient; SE from the Hessian); age centred at 8 years
fit_floor <- function(X, y, c0, start) {
  nll <- function(b) {
    p <- pmin(pmax(c0 + (1 - c0) * plogis(drop(X %*% b)), 1e-12), 1 - 1e-12)
    -sum(y * log(p) + (1 - y) * log1p(-p))
  }
  gr <- function(b) {
    eta <- drop(X %*% b); p <- c0 + (1 - c0) * plogis(eta)
    -colSums(X * ((y / p - (1 - y) / (1 - p)) * (1 - c0) * dlogis(eta)))
  }
  o <- tryCatch(optim(start, nll, gr, method = "BFGS", hessian = TRUE,
                      control = list(maxit = 2000, reltol = 1e-12)), error = \(e) NULL)
  if (is.null(o)) return(list(ok = FALSE, value = Inf))
  V <- tryCatch(solve(o$hessian), error = \(e) NULL)
  if (!is.null(V)) dimnames(V) <- list(colnames(X), colnames(X))
  list(coef = setNames(o$par, colnames(X)), V = V, value = o$value,
       ok = o$convergence == 0 && !is.null(V) && all(is.finite(diag(V))) && all(diag(V) > 0))
}
age_slopes_floor <- tr |> filter(!policy_exclude, !is.na(age)) |>
  mutate(age_c = age - 8) |>
  group_by(story_uid, language) |>
  filter(n() >= MIN_N_SLOPE) |>
  group_modify(\(d, k) {
    c0 <- first(d$chance)
    f <- if (n_distinct(d$dataset) > 1) correct ~ age_c + dataset else correct ~ age_c
    X <- model.matrix(f, d); y <- d$correct
    m_logit <- suppressWarnings(glm(f, data = d, family = binomial))
    eta_l <- predict(m_logit, type = "link")
    cc <- (mean(y) - c0) / (1 - c0)
    out <- tibble(n = nrow(d), accuracy = mean(y), chance = c0, cc_accuracy = cc,
                  slope_logit = coef(m_logit)[["age_c"]],
                  slope_logit_se = sqrt(vcov(m_logit)["age_c", "age_c"]),
                  ame_logit = mean(dlogis(eta_l)) * coef(m_logit)[["age_c"]])
    # two starts (flat at the cell's level; the plain logit fit); keep the better optimum
    starts <- list(c(qlogis(min(max(cc, .05), .95)), rep(0, ncol(X) - 1)), unname(coef(m_logit)))
    fits <- map(starts, \(st) fit_floor(X, y, c0, if (anyNA(st)) rep(0, ncol(X)) else st))
    fits <- keep(fits, "ok")
    if (!length(fits))
      return(out |> mutate(slope_floor = NA_real_, slope_floor_se = NA_real_, ame_floor = NA_real_))
    m <- fits[[which.min(map_dbl(fits, "value"))]]
    eta <- drop(X %*% m$coef)
    out |> mutate(slope_floor = m$coef[["age_c"]], slope_floor_se = sqrt(m$V["age_c", "age_c"]),
                  ame_floor = mean((1 - c0) * dlogis(eta)) * m$coef[["age_c"]])
  }) |>
  ungroup() |>
  left_join(item_keys |> select(story_uid, story, question_type, is_control, new_construct), by = "story_uid") |>
  mutate(kept_symmetric = cc_accuracy >= CC_LO & cc_accuracy <= CC_HI & !is.na(slope_floor) &
           is.finite(slope_floor_se) & slope_floor_se <= MAX_SLOPE_SE) |>
  # the cells the original (asymmetric) filter keeps, for comparison
  left_join(item_lang_d |> transmute(story_uid, language, kept_asymmetric = !is.na(age_slope)),
            by = c("story_uid", "language")) |>
  mutate(kept_asymmetric = coalesce(kept_asymmetric, FALSE))

slope_by_qtype <- bind_rows(
  age_slopes_floor |> filter(kept_symmetric) |> mutate(filter = "symmetric (floor model)"),
  age_slopes_floor |> filter(kept_asymmetric) |> mutate(filter = "original (>= 10 correct and incorrect)")) |>
  group_by(filter, question_type) |>
  summarise(n_cells = n(), n_items = n_distinct(story_uid), mean_accuracy = mean(accuracy),
            mean_slope_logit = mean(slope_logit), mean_slope_floor = mean(slope_floor, na.rm = TRUE),
            mean_ame_logit = mean(ame_logit), mean_ame_floor = mean(ame_floor, na.rm = TRUE),
            .groups = "drop")

re_test <- function(df, y, se, scheme) {
  df <- df |> filter(!is.na(.data[[y]]), !is.na(.data[[se]]))
  df$scheme <- factor(df[[scheme]])
  if (scheme == "question_type") df$scheme <- relevel(df$scheme, ref = "reality_check")
  m0 <- metafor::rma.mv(df[[y]], df[[se]]^2, random = list(~ 1 | story_uid, ~ 1 | language),
                        data = df, method = "ML")
  m1 <- metafor::rma.mv(df[[y]], df[[se]]^2, mods = ~ scheme, random = list(~ 1 | story_uid, ~ 1 | language),
                        data = df, method = "ML")
  lr <- as.numeric(2 * (logLik(m1) - logLik(m0)))
  dfree <- length(coef(m1)) - 1
  fb <- if (scheme == "question_type") {
    m1r <- metafor::rma.mv(df[[y]], df[[se]]^2, mods = ~ scheme, random = list(~ 1 | story_uid, ~ 1 | language),
                           data = df, method = "REML")
    cf <- coef(summary(m1r))["schemefalse_belief", ]
    c(est = cf[["estimate"]], lo = cf[["ci.lb"]], hi = cf[["ci.ub"]])
  } else c(est = NA_real_, lo = NA_real_, hi = NA_real_)
  tibble(outcome = y, scheme = scheme, n_cells = nrow(df), n_items = n_distinct(df$story_uid),
         LR = lr, df = dfree, p = pchisq(lr, dfree, lower.tail = FALSE),
         fb_minus_control = fb[["est"]], fb_minus_control_lo = fb[["lo"]], fb_minus_control_hi = fb[["hi"]])
}
sym <- age_slopes_floor |> filter(kept_symmetric)
asym <- item_lang_d |> filter(!is.na(age_slope))
slope_tests <- bind_rows(
  re_test(sym, "slope_floor", "slope_floor_se", "question_type") |> mutate(filter = "symmetric"),
  re_test(sym, "slope_floor", "slope_floor_se", "new_construct") |> mutate(filter = "symmetric"),
  re_test(sym, "slope_logit", "slope_logit_se", "question_type") |> mutate(filter = "symmetric"),
  re_test(asym, "age_slope", "age_slope_se", "question_type") |> mutate(filter = "original"),
  re_test(asym, "age_slope", "age_slope_se", "new_construct") |> mutate(filter = "original")) |>
  relocate(filter, .after = outcome)

calib_summary <- calib |>
  group_by(language) |>
  summarise(n_runs = first(n_runs_model), n_items = n(), converged = all(converged),
            median_resp_per_item = median(n_resp), min_resp = min(n_resp), .groups = "drop")

# ---- save -------------------------------------------------------------------

results <- list(
  # (a)
  items_lang = items_lang,
  items_cell = items_cell,
  items_all = items_all,
  flag_summary = flag_summary,
  # (b)
  item_verdicts = item_verdicts,
  scoped_split = scoped_split,
  verdict_counts = verdict_counts,
  flip_check = flip_check,
  story2_rc_responses = story2_rc_responses,
  rk_fb_age = rk_fb_age,
  rk_fb_age_matched = rk_fb_age_matched,
  # (c)
  controls_cor = controls_cor,
  controls_cells = controls_cells,
  person_scores = person,
  gating_june = gating_june,
  gating_glmm = gating_glmm,
  gating_plot = gating_plot,
  reliability = reliability,
  # (d)
  item_difficulty = item_lang_d,
  construct_r2 = construct_r2,
  construct_r2_by_lang = construct_r2_by_lang,
  construct_r2_sens = construct_r2_sens,
  scheme_means = scheme_means,
  qtype_summary = qtype_summary,
  calib_summary = calib_summary,
  age_slopes_floor = age_slopes_floor,
  slope_by_qtype = slope_by_qtype,
  slope_tests = slope_tests,
  meta = list(
    built_at = Sys.time(), script = "tasks/_stories_fits_items.R",
    input = file.path("data/stories_2026-09", c("stories_trials.rds", "raw/levante_metadata_items__v3_18.rds")),
    input_provenance = attr(trials_raw, "provenance"),
    n_trials = nrow(tr), n_runs = n_distinct(tr$run_id),
    thresholds = list(MIN_N_FLAG = MIN_N_FLAG, MIN_N_R = MIN_N_R, P_BELOW = P_BELOW, P_ABOVE = P_ABOVE,
                      R_WEAK = R_WEAK, ACC_CEILING = ACC_CEILING, MIN_N_SLOPE = MIN_N_SLOPE,
                      MIN_N_CALIB = MIN_N_CALIB, priors = c(d = PRIORS_D, a1 = PRIORS_A)),
    versions = c(R = R.version.string, mirt = as.character(packageVersion("mirt")),
                 lme4 = as.character(packageVersion("lme4")),
                 metafor = as.character(packageVersion("metafor"))),
    session_info = sessionInfo(),
    notes = c(
      "flag rules revised once after the first run: weak discrimination now = item-rest r 95% CI upper bound < .20 (was point estimate < .10, which mostly flagged small fCAT cells); ceiling checked before weak; below-chance items with a credibly positive item-rest r get their own level ('below chance, discriminating'); the original criterion is kept as r_ir_below_10",
      "age slopes: glm(correct ~ age [+ dataset]) per item x language with n >= 50 and >= 10 correct and >= 10 incorrect (minority rule added after a separated slope of -170 in the first run); slopes with SE > 1 are kept in age_slope_raw but not analysed",
      "marginal_rxx = E[var I / (var I + 1)] over the fitted N(mu, var); marginal_rxx_mirt = mirt::marginal_rxx(), which uses N(0, 1) and TI / (TI + var) and so is off for Rasch fits with var != 1. Both assume every item in the matrix is answered; empirical_rxx (EAP) reflects the items each run actually answered",
      "rest score = proportion correct on the run's other non-policy items; item-rest r computed within dataset x form and Fisher-pooled (weights n - 3, cells with n >= 20)",
      "flags (n >= 30): poor = below chance (exact binomial p < .01) without a credibly positive item-rest r, or item-rest r CI entirely < 0; below chance, discriminating = below chance with item-rest r CI > 0; ceiling = accuracy >= .95; weak = not above chance (p >= .05) or item-rest r CI upper bound < .20; ok otherwise",
      "policy_exclude items are dropped from (c) and (d) and from rest scores; spanish_fb_flag items are kept except in the flagged-item sensitivity rows",
      "IRT: mirt, guess fixed at the story-question chance, d ~ N(0, 3), a1 ~ N(1, 0.3) (2PL), EAP scores; June-method rows use pure Rasch without guessing on June's 58-item Leipzig pool (item_bank/retest_A/retest_B, released uids, > 15% coverage)",
      "2026-09 revision: floor-corrected age slopes (P = c + (1 - c) logistic(a + b age)) per item x language with a symmetric filter (chance-corrected accuracy in [.10, .90]); question types compared by rma.mv with crossed item and language random effects; flipped-key check of the scoped rows; Spanish reality_known verdicts by per-language discrimination (r >= .10 and CI upper bound >= .20) and an age-matched glm"))
)
write_rds(results, out_file, compress = "gz")
message("saved ", out_file)
