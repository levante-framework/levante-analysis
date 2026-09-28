# _stories_fits_monotonicity.R
#
# Stories (Theory of Mind), consolidated chapter: DO HIGHER SCORERS DO BETTER?
#   (a) monotonicity screen for every story item: accuracy in the bottom, middle
#       and top third of the rest score (the run's other non-policy items, as in
#       _stories_fits_items.R), thirds formed within dataset x form; the
#       top-minus-bottom accuracy difference is pooled over cells (Mantel-Haenszel
#       weights) by language, by content version (item_original) and over all
#       languages
#   (b) the reference questions of stories 5 (cups), 11 (balls) and 17 (book
#       stacks): which option children choose, by site; item-rest r against the
#       story's other questions; choice by rest-score third; overlap with the
#       reality check; the flipped key; age
#
# Input : data/stories_2026-09/stories_trials.rds (tasks/_build_stories_data.R)
#         data/tom_meta/tom_item_metadata.rds (prompt text only)
# Output: data/stories_2026-09/results_monotonicity.rds (the chapter only reads this)
#
# Run with Rscript, passing the book root (or set STORIES_BOOK_ROOT):
#   Rscript /path/to/levante-analysis/tasks/_stories_fits_monotonicity.R /path/to/levante-analysis
# No model fits; takes seconds.

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(stringr)
  library(tibble); library(readr)
})

args <- commandArgs(trailingOnly = TRUE)
script_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
default_root <- if (length(script_file) == 1) dirname(dirname(normalizePath(script_file))) else here::here()
root <- if (length(args) >= 1) args[1] else
  Sys.getenv("STORIES_BOOK_ROOT", default_root)
stopifnot(dir.exists(file.path(root, "data/stories_2026-09")))
out_file <- file.path(root, "data/stories_2026-09/results_monotonicity.rds")
set.seed(20260928)

# thresholds (fixed before looking at the screen's results)
MIN_N_CELL  <- 30    # responses an item needs in a dataset x form cell to enter the screen
MIN_N_FLAG  <- 90    # responses (over qualifying cells) before an item x language is classified
ACC_CEILING <- 0.95  # as in _stories_fits_items.R
MIN_N_SITE  <- 20    # reference-story tables: site cells shown
MIN_N_R     <- 20    # item-rest r needs this many responses in a cell (as in _stories_fits_items.R)
GAIN_MIN    <- 0.10  # added after the first run (see notes): a top-minus-bottom gain this
                     # small or smaller is treated as no gain when the CI rules out anything larger
notes <- c(
  "Revised once after the first run. The first classification had one class for every item whose top-minus-bottom CI included 0 ('no credible gain'); it mixed items that are flat with precision (e.g. story 17 emotion reasoning 1, n = 1,374) with small fCAT-only items whose CIs are wide. The class is now split: 'flat' when the CI upper bound is below GAIN_MIN = .10, otherwise 'inconclusive'.",
  "Added after the first run: the screen by content version (item_original), because story 2's reality check 2 was flat overall but reversed on the V0 Bogota scene 2e and increasing on 2f.")

# ---- 0. data ---------------------------------------------------------------

trials_raw <- read_rds(file.path(root, "data/stories_2026-09/stories_trials.rds"))
prompts <- read_rds(file.path(root, "data/tom_meta/tom_item_metadata.rds")) |>
  select(story_uid = item_uid, prompt)

tr <- trials_raw |>
  mutate(correct = as.integer(correct), cell = paste(dataset, form, sep = " / ")) |>
  # rest score: proportion correct on the run's OTHER non-policy items (as in _stories_fits_items.R)
  group_by(run_id) |>
  mutate(n_ok = sum(!policy_exclude), s_ok = sum(correct[!policy_exclude]),
         rest = (s_ok - if_else(policy_exclude, 0L, correct)) /
                (n_ok - if_else(policy_exclude, 0L, 1L))) |>
  ungroup() |>
  mutate(rest = if_else(is.finite(rest), rest, NA_real_))
stopifnot(!anyDuplicated(tr[c("run_id", "story_uid")]))

# items whose policy flag covers only some content (story-4 rc_2/_3 on image 4g,
# story-2 rc_1 on 2e) are screened in two parts, as in _stories_fits_items.R
partial_uids <- tr |> group_by(story_uid) |>
  summarise(partial = any(policy_exclude) & !all(policy_exclude), .groups = "drop") |>
  filter(partial) |> pull(story_uid)
tr <- tr |>
  mutate(content = case_when(
    story_uid %in% partial_uids & policy_exclude ~ paste0("policy-flagged (", item_original, ")"),
    story_uid %in% partial_uids ~ "other content",
    TRUE ~ "all"))

# ---- (a) monotonicity screen -----------------------------------------------

# thirds of the rest score within each item x dataset x form cell; ties are
# broken at random so that every third has a third of the responses
thirds <- tr |>
  filter(!is.na(rest)) |>
  group_by(story_uid, content, cell) |>
  filter(n() >= MIN_N_CELL) |>
  mutate(third = ceiling(3 * rank(rest, ties.method = "random") / n())) |>
  ungroup()

cell_tab <- thirds |>
  group_by(story_uid, content, language, dataset, form, cell, third) |>
  summarise(n = n(), x = sum(correct), .groups = "drop") |>
  pivot_wider(names_from = third, values_from = c(n, x), names_glue = "{.value}{third}")
# by content version: thirds formed within item x version x cell
version_tab <- tr |>
  filter(!is.na(rest)) |>
  group_by(story_uid, item_original, cell) |>
  filter(n() >= MIN_N_CELL) |>
  mutate(third = ceiling(3 * rank(rest, ties.method = "random") / n())) |>
  group_by(story_uid, item_original, cell, third) |>
  summarise(n = n(), x = sum(correct), .groups = "drop") |>
  pivot_wider(names_from = third, values_from = c(n, x), names_glue = "{.value}{third}")

# Mantel-Haenszel pooled risk difference (top minus bottom third) with the
# Sato variance; accuracy by third pooled with the same cells
mh_rd <- function(n1, x1, n3, x3) {
  N <- n1 + n3; w <- n1 * n3 / N
  rd <- sum((x3 * n1 - x1 * n3) / N) / sum(w)
  P <- (n3^2 * x1 - n1^2 * x3 + n3 * n1 * (n1 - n3) / 2) / N^2
  Q <- (x3 * (n1 - x1) + x1 * (n3 - x3)) / (2 * N)
  se <- sqrt(rd * sum(P) + sum(Q)) / sum(w)
  if (!is.finite(se) || se == 0) se <- sqrt(sum(w^2 * (x3 / n3 * (1 - x3 / n3) / n3 + x1 / n1 * (1 - x1 / n1) / n1))) / sum(w)
  c(rd = rd, lo = rd - 1.96 * se, hi = rd + 1.96 * se)
}
summarise_mono <- function(d) {
  d |> summarise(n = sum(n1 + n2 + n3), n_cells = n(),
                 acc_low = sum(x1) / sum(n1), acc_mid = sum(x2) / sum(n2), acc_high = sum(x3) / sum(n3),
                 acc = sum(x1 + x2 + x3) / sum(n1 + n2 + n3),
                 rd = list(mh_rd(n1, x1, n3, x3)), .groups = "drop") |>
    mutate(diff = map_dbl(rd, "rd"), diff_lo = map_dbl(rd, "lo"), diff_hi = map_dbl(rd, "hi")) |>
    select(-rd)
}
mono_levels <- c("reversed", "flat", "inconclusive", "ceiling", "higher scorers do better", "n < 90")
classify <- function(d) d |>
  mutate(mono = factor(case_when(
    n < MIN_N_FLAG ~ "n < 90",
    diff_hi < 0 ~ "reversed",
    acc >= ACC_CEILING ~ "ceiling",
    diff_lo > 0 ~ "higher scorers do better",
    diff_hi < GAIN_MIN ~ "flat",
    TRUE ~ "inconclusive"), levels = mono_levels))

keys <- tr |> distinct(story_uid, story, entry, question_type, is_control, chance)
mono_lang <- cell_tab |> group_by(story_uid, content, language) |> summarise_mono() |>
  classify() |> left_join(keys, by = "story_uid")
mono_all <- cell_tab |> group_by(story_uid, content) |> summarise_mono() |>
  classify() |> left_join(keys, by = "story_uid") |>
  left_join(prompts, by = "story_uid") |>
  left_join(mono_lang |> filter(mono %in% c("reversed", "flat")) |>
              group_by(story_uid, content) |>
              summarise(langs_failing = paste0(language, " (", mono, ")", collapse = "; "), .groups = "drop"),
            by = c("story_uid", "content")) |>
  mutate(policy = story_uid %in% unique(tr$story_uid[tr$policy_exclude]) & content != "other content")

# by content version, only for items deployed in more than one version
multi_version <- tr |> group_by(story_uid) |> filter(n_distinct(item_original) > 1) |> ungroup() |>
  distinct(story_uid) |> pull()
mono_version <- version_tab |> filter(story_uid %in% multi_version) |>
  group_by(story_uid, item_original) |> summarise_mono() |> classify() |>
  left_join(keys, by = "story_uid") |>
  left_join(tr |> group_by(story_uid, item_original) |>
              summarise(datasets = paste(sort(unique(dataset)), collapse = ", "),
                        policy_share = mean(policy_exclude), .groups = "drop"),
            by = c("story_uid", "item_original"))

# the two non-policy items that fail with enough data (see the chapter)
# story 17 emotion reasoning 1: which emotion children choose, by third, German vs the rest
er17 <- thirds |> filter(story_uid == "tom_story17_reference_emotion_reasoning_1") |>
  mutate(emotion = str_remove(response, "-fem$"), lang = if_else(language == "de-DE", "de-DE", "other languages")) |>
  count(lang, third, emotion) |> group_by(lang, third) |> mutate(n_third = sum(n), share = n / n_third) |> ungroup()
# story 2 reality check 2: V0 Bogota scene 2e vs later content
s2rc2 <- tr |> filter(story_uid == "tom_story2_moral_reasoning_reality_check_2", !is.na(rest)) |>
  mutate(part = if_else(item_original == "2e", "V0 Bogota (2e)", "later content")) |>
  group_by(part, cell) |> filter(n() >= MIN_N_CELL) |>
  mutate(third = ceiling(3 * rank(rest, ties.method = "random") / n())) |>
  group_by(part, cell, third) |> summarise(n = n(), x = sum(correct), .groups = "drop") |>
  pivot_wider(names_from = third, values_from = c(n, x), names_glue = "{.value}{third}") |>
  group_by(part) |> summarise_mono() |> classify()

mono_counts <- list(
  all = count(mono_all, mono),
  lang = count(mono_lang, mono),
  cells_excluded = tr |> filter(!is.na(rest)) |> count(story_uid, content, cell) |> summarise(
    cells = n(), cells_used = sum(n >= MIN_N_CELL), resp = sum(n), resp_used = sum(n[n >= MIN_N_CELL])))

# ---- (b) reference questions of stories 5, 11 and 17 -------------------------

site_lab <- c(pilot_mpieva_de_main = "Leipzig (de)", rfp1_mpib_de_main = "Leipzig (de)",
              pilot_uniandes_co_bogota = "Bogotá (es-CO)", pilot_uniandes_co_rural = "CO rural (es-CO)",
              pilot_western_ca_main = "Western (en-US)", pilot_langcog_us_downex = "LangCog (en-US)",
              rfp1_sheffield_gb_main = "Sheffield (en-GB)", rfp1_utdt_ar_main = "UTDT-AR (es-AR)",
              rfp1_utdt_intl_ys = "UTDT-intl (es)", rfp1_mpib_intl_ys = "MPIB-intl (es-CO)")
opt <- tribble(~story, ~keyed_opt, ~wanted_opt,
               5, "medium_cup", "large_cup",
               11, "soccerball", "baseball",
               17, "redstack", "yellowstack")
choice_levels <- c("listener's misreading (keyed correct)",
                   "what the speaker meant (= reality-check answer)", "other option")

ref <- tr |> filter(story %in% c(5, 11, 17)) |> mutate(site = site_lab[dataset])
stopifnot(!anyNA(ref$site))

fisher_pool <- function(r, n) {
  ok <- !is.na(r) & n >= MIN_N_R & abs(r) < 1
  if (!any(ok)) return(tibble(r = NA_real_, lo = NA_real_, hi = NA_real_))
  z <- atanh(r[ok]); w <- n[ok] - 3; zm <- sum(w * z) / sum(w); se <- 1 / sqrt(sum(w))
  tibble(r = tanh(zm), lo = tanh(zm - 1.96 * se), hi = tanh(zm + 1.96 * se))
}
safe_r <- function(x, y) {
  ok <- !is.na(x) & !is.na(y)
  if (sum(ok) < 3 || sd(x[ok]) == 0 || sd(y[ok]) == 0) NA_real_ else cor(x[ok], y[ok])
}

ref_cells <- ref |> group_by(story, entry, site, dataset, form) |>
  summarise(n = n(), acc = mean(correct), chance = first(chance), r = safe_r(correct, rest), .groups = "drop")
pool_ref <- function(d) d |>
  summarise(acc = weighted.mean(acc, n), chance = first(chance), fisher_pool(r, n),
            n = sum(n), .groups = "drop")
ref_ir <- bind_rows(ref_cells |> group_by(story, entry, site) |> pool_ref(),
                    ref_cells |> group_by(story, entry) |> pool_ref() |> mutate(site = "All sites"))

rr <- ref |> filter(entry == "reference") |> left_join(opt, by = "story") |>
  mutate(choice = factor(case_when(response == keyed_opt ~ choice_levels[1],
                                   response == wanted_opt ~ choice_levels[2],
                                   TRUE ~ choice_levels[3]), levels = choice_levels))
stopifnot(all(rr$correct == (rr$choice == choice_levels[1])))
ref_choices <- bind_rows(rr, rr |> mutate(site = "All sites")) |>
  count(story, site, choice) |> group_by(story, site) |> mutate(n_site = sum(n), share = n / n_site) |> ungroup()

# choice by rest-score third (thirds within dataset, pooled over sites)
ref_icc <- rr |> filter(!is.na(rest)) |>
  group_by(dataset) |> mutate(third = ceiling(3 * rank(rest, ties.method = "random") / n())) |> ungroup() |>
  count(story, third, choice) |> group_by(story, third) |> mutate(n_third = sum(n), share = n / n_third) |> ungroup()

# the same child's reality check
rc <- ref |> filter(entry == "reality_check") |> select(run_id, story, rc_correct = correct)
ref_rc <- rr |> inner_join(rc, by = c("run_id", "story")) |>
  group_by(story) |>
  summarise(n = n(),
            wanted_if_rc_pass = mean(choice[rc_correct == 1] == choice_levels[2]),
            keyed_if_rc_pass = mean(correct[rc_correct == 1]),
            keyed_if_rc_fail = mean(correct[rc_correct == 0]),
            r_wanted_rc = cor(as.numeric(choice == choice_levels[2]), rc_correct), .groups = "drop")

# flipped key: score "what the speaker wanted" as correct
ref_flip <- rr |> mutate(wanted = as.integer(choice == choice_levels[2])) |>
  group_by(story, dataset, form) |> summarise(n = n(), r = safe_r(wanted, rest), .groups = "drop") |>
  group_by(story) |> summarise(fisher_pool(r, n), .groups = "drop")

ref_age <- rr |> filter(!is.na(age)) |>
  mutate(age_band = cut(age, c(0, 6, 8, 10, 99), labels = c("<6", "6-8", "8-10", "10+"), right = FALSE)) |>
  group_by(story, age_band) |>
  summarise(n = n(), keyed = mean(correct), wanted = mean(choice == choice_levels[2]), .groups = "drop")

ref_prompts <- prompts |> filter(str_detect(story_uid, "story(5|11|17)_reference_(reference|reality_check)$"))

# ---- write --------------------------------------------------------------------

results <- list(
  meta = list(created = Sys.time(), root = root, notes = notes,
              thresholds = list(MIN_N_CELL = MIN_N_CELL, MIN_N_FLAG = MIN_N_FLAG, GAIN_MIN = GAIN_MIN,
                                ACC_CEILING = ACC_CEILING, MIN_N_SITE = MIN_N_SITE, MIN_N_R = MIN_N_R)),
  mono_all = mono_all, mono_lang = mono_lang, mono_version = mono_version, mono_counts = mono_counts,
  er17 = er17, s2rc2 = s2rc2,
  ref = list(ir = ref_ir, choices = ref_choices, icc = ref_icc, rc = ref_rc, flip = ref_flip,
             age = ref_age, prompts = ref_prompts, choice_levels = choice_levels))
write_rds(results, out_file, compress = "gz")

options(width = 220)
cat("\n== classification, pooled\n"); print(mono_counts$all)
cat("\n== classification, item x language\n"); print(mono_counts$lang)
cat("\n== cells used\n"); print(mono_counts$cells_excluded)
cat("\n== pooled: not 'higher scorers do better'\n")
print(as.data.frame(mono_all |> filter(mono != "higher scorers do better") |>
  select(story_uid, content, n, acc, chance, acc_low, acc_mid, acc_high, diff, diff_lo, diff_hi, mono, policy, langs_failing) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |> arrange(mono, diff)))
cat("\n== by content version: not 'higher scorers do better' (n >= 90)\n")
print(as.data.frame(mono_version |> filter(!mono %in% c("higher scorers do better", "n < 90", "ceiling")) |>
  select(story_uid, item_original, n, acc, acc_low, acc_high, diff, diff_lo, diff_hi, mono, policy_share, datasets) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |> arrange(mono, diff)))
cat("\n== item x language: reversed or flat\n")
print(as.data.frame(mono_lang |> filter(mono %in% c("reversed", "flat")) |>
  select(story_uid, content, language, n, acc, chance, acc_low, acc_high, diff, diff_lo, diff_hi, mono) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |> arrange(mono, diff)))
