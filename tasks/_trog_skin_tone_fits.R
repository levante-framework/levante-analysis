# _trog_skin_tone_fits.R
#
# Sentence Understanding (TROG): does the skin tone of the depicted characters
# function differently across samples?
#   (1) item DIF: explanatory-IRT GLMM (Rasch + fixed 4AFC guessing floor),
#       correct ~ group * target skin tone + (1 | run) + (1 + group | item),
#       one model per contrast against Bogotá, on items whose target picture
#       shows people. The item random slope makes the skin-tone test answer to
#       ordinary item-level DIF.
#   (2) choice: conditional logit over the pictures on screen, with image fixed
#       effects, asking whether groups differ in how strongly they are drawn to
#       pictures of lighter / darker characters. Fit on all trials (with
#       group-specific pull toward the target) and on errors only (which
#       distractor was picked). SEs clustered by item.
#
# Input : tasks/trog_skin_tone/trog_image_skin.csv, trog_item_images.csv
#         (skin measurement pipeline + README in tasks/trog_skin_tone/)
#         TROG trials via common.R::load_levante_trials() (2026-08-24 snapshot)
# Output: data/trog_skin_tone_fits.rds (the chapter only reads this)
# Runtime: ~25 min (four glmer fits with item random slopes, two clogit fits).
#   Rscript tasks/_trog_skin_tone_fits.R   (from the repo root)

suppressMessages({source(here::here("common.R")); library(lme4); library(survival)})
dir <- here("tasks/trog_skin_tone")

# ---- item skin features ------------------------------------------------------------
img <- read_csv(file.path(dir, "trog_image_skin.csv"), show_col_types = FALSE)
item_imgs <- read_csv(file.path(dir, "trog_item_images.csv"), show_col_types = FALSE) |>
  mutate(images = map2(target, str_split(distractors, ","), c)) |>
  select(item_uid, target, images) |> unnest(images) |>
  mutate(role = if_else(images == target, "target", "distractor")) |>
  left_join(img, by = c("images" = "image"))
item_feat <- item_imgs |> group_by(item_uid) |>
  summarise(n_img_person = sum(n_person > 0, na.rm = TRUE),
            target_ita   = ita_mean[role == "target"],
            distr_ita    = mean(ita_mean[role == "distractor"], na.rm = TRUE),
            .groups = "drop") |>
  mutate(distr_ita = if_else(is.finite(distr_ita), distr_ita, NA_real_))

# ---- trials: each child's first TROG run ---------------------------------------------
grp_of <- c(pilot_uniandes_co_bogota = "Bogotá", pilot_uniandes_co_rural = "Rural CO",
            rfp1_mpib_intl_ys = "CO (MPIB intl)", pilot_mpieva_de_main = "Leipzig",
            pilot_western_ca_main = "Western CA")
tr <- load_levante_trials(task_ids = "trog") |>
  filter(dataset %in% names(grp_of)) |>
  mutate(grp = grp_of[dataset], correct = as.numeric(correct))
first_runs <- tr |> group_by(user_id, run_id) |> summarise(t = min(timestamp), .groups = "drop") |>
  group_by(user_id) |> slice_min(t, n = 1, with_ties = FALSE) |> pull(run_id)
tr <- tr |> filter(run_id %in% first_runs, !is.na(correct)) |>
  distinct(run_id, item_uid, .keep_all = TRUE) |>
  left_join(item_feat, by = "item_uid")

# identity check: the corpus image set contains the child's response, and
# corpus-keyed correctness matches the recorded `correct`
id_check <- tr |> filter(!is.na(response)) |>
  left_join(item_imgs |> group_by(item_uid) |> summarise(set = list(images), tgt = first(target)), by = "item_uid") |>
  mutate(in_set = map2_lgl(response, set, \(r, s) r %in% s)) |>
  group_by(grp) |> summarise(n = n(), in_set = mean(in_set), agree = mean((response == tgt) == (correct == 1)))

# 4AFC guessing-floor logit (LEVANTE "Rasch" = Rasch + g = 1/4)
mafc_logit <- function(m = 4) structure(list(
  linkfun  = function(mu) qlogis((m * pmin(pmax(mu, 1/m + 1e-6), 1 - 1e-6) - 1) / (m - 1)),
  linkinv  = function(eta) 1/m + (m - 1)/m * plogis(eta),
  mu.eta   = function(eta) (m - 1)/m * dlogis(eta),
  valideta = function(eta) TRUE, name = "mafc.logit"), class = "link-glm")
ita_ref <- mean(item_feat$target_ita, na.rm = TRUE)
contrasts_vs <- c("Rural CO", "Leipzig", "Western CA", "CO (MPIB intl)")

# ---- 1. item DIF by target skin tone -------------------------------------------------
d1 <- tr |> filter(!is.na(target_ita)) |> mutate(ita10 = (target_ita - ita_ref) / 10)
dif_fits <- map(set_names(contrasts_vs), \(g) {
  d <- d1 |> filter(grp %in% c("Bogotá", g)) |> mutate(g = as.numeric(grp == g))
  glmer(correct ~ g * ita10 + (1 | run_id) + (1 + g | item_uid), data = d,
        family = binomial(link = mafc_logit(4)), control = glmerControl(optimizer = "bobyqa"))
})
dif_tab <- imap_dfr(dif_fits, \(m, g) {
  cf <- summary(m)$coefficients
  tibble(contrast = g, n_runs = n_distinct(m@frame$run_id), n_items = n_distinct(m@frame$item_uid),
         term = rownames(cf), est = cf[, 1], se = cf[, 2], p = cf[, 4],
         sd_item_dif = attr(VarCorr(m)$item_uid, "stddev")[["g"]])
})
item_dif <- imap_dfr(dif_fits, \(m, g) {
  re <- ranef(m)$item_uid |> rownames_to_column("item_uid") |> select(item_uid, re_g = g)
  fe <- fixef(m)
  d1 |> distinct(item_uid, target_ita, ita10) |> inner_join(re, by = "item_uid") |>
    mutate(contrast = g, dif = fe["g"] + fe["g:ita10"] * ita10 + re_g)
})

# ---- 2. choice among the pictures ----------------------------------------------------
multi <- item_imgs |> distinct(item_uid, target, images, n_person, ita_mean) |>
  group_by(item_uid) |> filter(sum(n_person > 0) >= 2) |> ungroup()
d2 <- tr |> filter(!is.na(response), item_uid %in% multi$item_uid) |>
  select(run_id, item_uid, grp, response) |> mutate(trial = row_number()) |>
  inner_join(multi, by = "item_uid", relationship = "many-to-many") |>
  mutate(chosen = as.numeric(response == images), person = as.numeric(n_person > 0),
         ita10 = if_else(person == 1, (ita_mean - ita_ref) / 10, 0),
         is_target = as.numeric(images == target)) |>
  group_by(trial) |> filter(sum(chosen) == 1) |> ungroup()
for (g in contrasts_vs) {  # explicit dummies so Bogotá is the reference
  k <- make.names(g); x <- as.numeric(d2$grp == g)
  d2[[paste0("gT_", k)]] <- x * d2$is_target; d2[[paste0("gP_", k)]] <- x * d2$person
  d2[[paste0("gS_", k)]] <- x * d2$ita10
}
rhs <- \(d, pat) paste("chosen ~ factor(images) +", paste(grep(pat, names(d), value = TRUE), collapse = " + "), "+ strata(trial)")
fit_all <- clogit(as.formula(rhs(d2, "^g[TPS]_")), data = d2, cluster = item_uid, method = "efron")
d3 <- d2 |> group_by(trial) |> filter(sum(chosen * is_target) == 0) |> ungroup() |> filter(is_target == 0) |>
  group_by(trial) |> filter(n_distinct(ita10) > 1 | n_distinct(person) > 1) |> ungroup()
fit_err <- clogit(as.formula(rhs(d3, "^g[PS]_")), data = d3, cluster = item_uid, method = "efron")
tidy_c <- \(f, lab) as_tibble(summary(f)$coefficients, rownames = "term") |>
  filter(str_detect(term, "^g[TPS]_")) |> mutate(model = lab)
choice_tab <- bind_rows(tidy_c(fit_all, "all trials"), tidy_c(fit_err, "errors only"))

# ---- 3. age-matched check: MPIB intl (ages 4.5-8.4) vs Bogotá + rural children of the same ages
age_of <- read_rds(here("data/scores_all_sites.rds")) |> filter(task_id == "trog") |> distinct(run_id, age)
young <- age_of |> filter(age >= 4.5, age < 8.5) |> pull(run_id)
d2y <- d2 |> filter(run_id %in% young, grp %in% c("Bogotá", "Rural CO", "CO (MPIB intl)"))
fit_young <- clogit(as.formula(rhs(d2y, "^g[TPS]_(Rural|CO)")), data = d2y, cluster = item_uid, method = "efron")
d1y <- d1 |> filter(run_id %in% young, grp %in% c("Bogotá", "CO (MPIB intl)")) |> mutate(g = as.numeric(grp != "Bogotá"))
dif_young <- glmer(correct ~ g * ita10 + (1 | run_id) + (1 + g | item_uid), data = d1y,
                   family = binomial(link = mafc_logit(4)), control = glmerControl(optimizer = "bobyqa"))
# developmental check within Bogotá + rural: does the pull toward darker/lighter characters change with age?
d2a <- d2 |> filter(grp %in% c("Bogotá", "Rural CO")) |> inner_join(age_of, by = "run_id") |>
  mutate(age_c = age - 9, tgt_age = is_target * age_c, skin_age = ita10 * age_c, skin_rural = ita10 * (grp == "Rural CO"),
         tgt_rural = is_target * (grp == "Rural CO"))
fit_age <- clogit(chosen ~ factor(images) + tgt_age + tgt_rural + skin_age + skin_rural + strata(trial),
                  data = d2a, cluster = item_uid, method = "efron")
young_tab <- bind_rows(tidy_c(fit_young, "all trials, ages 4.5-8.4"),
  as_tibble(summary(fit_age)$coefficients, rownames = "term") |> filter(str_detect(term, "^(skin|tgt)_")) |>
    mutate(model = "Bogotá + rural, age interaction"),
  as_tibble(summary(dif_young)$coefficients, rownames = "term") |> filter(str_detect(term, "g")) |>
    mutate(model = "item DIF, MPIB vs Bogotá ages 4.5-8.4"))
young_n <- d2y |> distinct(grp, run_id) |> count(grp)

write_rds(list(young_tab = young_tab, young_n = young_n, item_feat = item_feat, item_imgs = item_imgs, id_check = id_check,
               n_runs = tr |> distinct(grp, run_id) |> count(grp),
               dif_tab = dif_tab, item_dif = item_dif, ita_ref = ita_ref, choice_tab = choice_tab,
               n_choice = list(all = n_distinct(d2$trial), err = n_distinct(d3$trial),
                               items = n_distinct(d2$item_uid), err_items = n_distinct(d3$item_uid))),
          here("data/trog_skin_tone_fits.rds"))
