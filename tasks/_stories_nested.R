# Does story type explain item difficulty / age sensitivity BEYOND question
# type? (Rebecca Saxe's framing, 2026-10.) Nested models on the 2026-09 item
# tables (results_items.rds), with the corrected question types from
# _stories_qtype_recode.R. Writes data/stories_2026-10/results_nested.rds.
suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(lme4); library(metafor)})
root <- Sys.getenv("LEVANTE_ANALYSIS", here::here())
source(file.path(root, "tasks/_stories_qtype_recode.R"))
ri <- read_rds(file.path(root, "data/stories_2026-09/results_items.rds"))
keys <- read_rds(file.path(root, "data/stories_2026-09/stories_trials.rds")) |>
  distinct(story_uid, story, entry, old_group, question_type, is_control) |>
  qtype_recode()

diff <- ri$item_difficulty |>
  select(story_uid, language, difficulty_z) |>
  inner_join(keys, by = "story_uid")

lrt <- function(m0, m1, label) {
  a <- anova(m0, m1)
  tibble(comparison = label, df = a$npar[2] - a$npar[1], chisq = a$Chisq[2], p = a$`Pr(>Chisq)`[2],
         dAIC = AIC(m1) - AIC(m0), dBIC = BIC(m1) - BIC(m0))
}
fit <- \(f, d) lmer(f, data = d, REML = FALSE)

difficulty_tests <- purrr::map_dfr(c(original = "question_type", corrected = "question_type_v2"), \(qt) {
  d <- diff |> mutate(qt = .data[[qt]])
  m_q  <- fit(difficulty_z ~ qt + (1 | story_uid) + (1 | language), d)
  m_qs <- fit(difficulty_z ~ qt + old_group + (1 | story_uid) + (1 | language), d)
  m_qr <- fit(difficulty_z ~ qt + (1 | story) + (1 | story_uid) + (1 | language), d)
  m_s  <- fit(difficulty_z ~ old_group + (1 | story_uid) + (1 | language), d)
  m_sq <- m_qs
  bind_rows(lrt(m_q, m_qs, "story type beyond question type"),
            lrt(m_q, m_qr, "individual story (random) beyond question type"),
            lrt(m_s, m_sq, "question type beyond story type")) |>
    mutate(scheme = qt, .before = 1)
}, .id = "qtype_scheme")

# item-level R2 (one row per item, mean over languages): how much of the
# between-item variance each adds
item_means <- diff |>
  group_by(story_uid, story, old_group, question_type, question_type_v2) |>
  summarise(difficulty_z = mean(difficulty_z), n_lang = n(), .groups = "drop")
r2 <- \(f) summary(lm(f, data = item_means))$r.squared
n_qt <- n_distinct(item_means$question_type_v2)
item_r2 <- tibble(
  model = c("question type (original, 4)", paste0("question type (corrected, ", n_qt, ")"), "story type (6)",
            "corrected question type + story type", "corrected question type + story (18)"),
  R2 = c(r2(difficulty_z ~ question_type), r2(difficulty_z ~ question_type_v2), r2(difficulty_z ~ old_group),
         r2(difficulty_z ~ question_type_v2 + old_group), r2(difficulty_z ~ question_type_v2 + factor(story))))
item_f <- anova(lm(difficulty_z ~ question_type_v2, item_means), lm(difficulty_z ~ question_type_v2 + old_group, item_means))
item_r2$F_story_type_beyond_qt <- c(NA, NA, NA, item_f$F[2], NA)
item_r2$p <- c(NA, NA, NA, item_f$`Pr(>F)`[2], NA)

# which story types differ, given question type (corrected)
m_full <- fit(difficulty_z ~ question_type_v2 + old_group + (1 | story_uid) + (1 | language), diff)
story_type_effects <- broom.mixed::tidy(m_full, effects = "fixed") |>
  filter(grepl("old_group", term)) |> mutate(term = sub("old_group", "", term))

# question type x story type cell means (where both exist)
cell_means <- diff |>
  group_by(question_type_v2, old_group) |>
  summarise(items = n_distinct(story_uid), mean_difficulty_z = mean(difficulty_z), .groups = "drop")

# age sensitivity above the floor: same nested comparison as a meta-regression
sl <- ri$age_slopes_floor |>
  filter(!is.na(slope_floor), cc_accuracy >= .1, cc_accuracy <= .9, n >= 50, slope_floor_se <= 1) |>
  select(story_uid, language, slope_floor, slope_floor_se) |>
  inner_join(keys, by = "story_uid")
rma_fit <- \(mods) rma.mv(slope_floor, slope_floor_se^2, mods = mods, data = sl,
                          random = list(~ 1 | story_uid, ~ 1 | language), method = "ML")
s_q <- rma_fit(~ question_type_v2); s_qs <- rma_fit(~ question_type_v2 + old_group)
s_s <- rma_fit(~ old_group)
a1 <- anova(s_qs, s_q); a2 <- anova(s_qs, s_s)
slope_tests <- tibble(comparison = c("story type beyond question type", "question type beyond story type"),
                      n_cells = nrow(sl), n_items = n_distinct(sl$story_uid),
                      LRT = c(a1$LRT, a2$LRT), df = c(a1$parms.f - a1$parms.r, a2$parms.f - a2$parms.r),
                      p = c(a1$pval, a2$pval))
slope_by_qtype_v2 <- sl |> group_by(question_type_v2) |>
  summarise(cells = n(), items = n_distinct(story_uid), mean_slope_floor = mean(slope_floor), .groups = "drop")

write_rds(list(keys = keys, difficulty_tests = difficulty_tests, item_r2 = item_r2,
               story_type_effects = story_type_effects, cell_means = cell_means,
               slope_tests = slope_tests, slope_by_qtype_v2 = slope_by_qtype_v2),
          file.path(root, "data/stories_2026-10/results_nested.rds"))
