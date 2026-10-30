# Concurrent recalibration of H&F v1 and v2 on one scale (sourced by
# studies/2026-10_hf-v2-redesign/hearts_and_flowers_v2.qmd, section 11). Run with `Rscript --vanilla`
# (mirt can't load inside this project's renv).
#
# The released model scores v2 with item parameters calibrated on untimed v1
# trials, so a pass on a 1.5-s trial earns the same credit as an untimed pass.
# Here:
#   - one trial coding for both forms: correct = right side, answered, and
#     200 <= RT <= window, with window = 3000 ms for v1 (all blocks) and v2's
#     pure blocks, and v2's actual 3000 / 2000 / 1500 ms in the mixed segments
#     (no 2000 ms recode cutoff)
#   - items are the released items (block x stimulus x start/stay/switch);
#     the pure-block items (heart and flower, start and stay) are the anchors,
#     shared across forms; mixed items are form-specific (v1: untimed; v2:
#     split by window)
#   - multigroup by form; v1 mean 0, both variances and v2's mean free
#   - Rasch slopes; guessing fixed at 0.5 ("g_fixed") or, for v2's timed
#     mixed items, estimated per item group ("g_estimated")
# Item parameters are tied within item group (all occurrences of e.g. a 2-s
# switch trial share one difficulty). All runs with trials are included;
# early stops are ignorable missing data.
# Writes data/hf_v2/refit_results.rds.

source(here::here("studies/2026-10_hf-v2-redesign/_hf_v2_common.R"))
suppressPackageStartupMessages(library(mirt))

dl <- readRDS(here::here("data/hf_v2/deadline_items.rds"))

trials <- dl$typed |>
  filter(!is.na(block)) |>
  arrange(run_id, trial_index) |>
  mutate(
    win = if_else(form == "v1", 3000, window),
    y = as.integer(correct %in% TRUE & !timeout & rt_numeric >= 200 & rt_numeric <= win),
    group = case_when(block != "mixed" ~ item_class,
                      form == "v1" ~ paste0("v1_", item_class),
                      TRUE ~ paste0("v2_", item_class, "_", window))) |>
  group_by(run_id, group) |>
  mutate(k = row_number()) |>
  ungroup() |>
  mutate(item = sprintf("%s_%02d", group, k))

item_n <- trials |> count(item, group)
keep <- item_n |> filter(n >= 100) |> pull(item)
trials <- trials |> filter(item %in% keep)

runs_meta <- trials |> distinct(run_id, form, age)
wide <- trials |>
  select(run_id, item, y) |>
  pivot_wider(names_from = item, values_from = y)
y <- as.matrix(wide[, -1])
rownames(y) <- wide$run_id
items <- colnames(y)
groups <- sub("_[0-9]+$", "", items)
form_of <- runs_meta$form[match(rownames(y), runs_meta$run_id)]

timed_v2 <- grepl("^v2_heartsflowers", groups)

fit_refit <- function(estimate_g) {
  type <- if_else(timed_v2 & estimate_g, "3PL", "Rasch")
  sv <- multipleGroup(y, 1, group = form_of, itemtype = type, pars = "values")
  # slopes are fixed at 1, so the scale comes from the latent variance: v1
  # has mean 0 and a free variance; v2's mean and variance are free
  # (identified by the anchors)
  sv$est[sv$name == "COV_11"] <- TRUE
  sv$est[sv$group == "v2" & sv$name == "MEAN_1"] <- TRUE
  # Rasch + guessing: slopes fixed at 1, g fixed at 0.5 unless estimated
  sv$value[sv$name == "a1"] <- 1
  sv$est[sv$name == "a1"] <- FALSE
  gi <- sv$name == "g"
  sv$value[gi] <- 0.5
  sv$est[gi] <- estimate_g & sv$item[gi] %in% items[timed_v2]
  # one difficulty (and g) per item group, equal in both forms' groups
  cons <- list()
  for (g in unique(groups)) {
    its <- items[groups == g]
    for (p in c("d", "g")) {
      nums <- sv$parnum[sv$item %in% its & sv$name == p & sv$est]
      if (length(nums) > 1) cons[[length(cons) + 1]] <- nums
    }
  }
  multipleGroup(y, 1, group = form_of, itemtype = type, pars = sv,
                constrain = cons,
                verbose = FALSE, technical = list(NCYCLES = 4000))
}

fits <- list(g_fixed = fit_refit(FALSE), g_estimated = fit_refit(TRUE))

score <- function(fit, label) {
  fs <- fscores(fit, method = "EAP", full.scores.SE = TRUE)
  tibble(run_id = rownames(y), form = form_of, theta = fs[, "F1"],
         se = fs[, "SE_F1"], n_items = rowSums(!is.na(y)), model = label)
}
scores <- purrr::imap_dfr(fits, score)

group_pars <- purrr::imap_dfr(fits, \(f, label) {
  cf <- coef(f, simplify = TRUE)
  purrr::imap_dfr(cf, \(x, frm) {
    it <- as.data.frame(x$items)
    it$item <- rownames(it)
    it |>
      mutate(group = sub("_[0-9]+$", "", item)) |>
      group_by(group) |>
      summarise(d = first(d), g = first(g), cols = n(), .groups = "drop") |>
      mutate(form = frm, mean = x$means[1], var = x$cov[1, 1], model = label)
  })
}) |>
  # a group-specific item is only informed by its own form
  filter(!(grepl("^v1_", group) & form == "v2"), !(grepl("^v2_", group) & form == "v1"))

# test information over theta for a complete run of each form, built from the
# average number of trials per item group in complete runs
complete <- hf_run_struct |> filter(grepl("complete \\(", layout))
per_run <- trials |>
  semi_join(complete, by = "run_id") |>
  count(form, run_id, group) |>
  group_by(form, group) |>
  summarise(trials = sum(n) / n_distinct(trials$run_id[trials$form == first(form) &
                                                         trials$run_id %in% complete$run_id]),
            .groups = "drop")
theta_grid <- seq(-6, 8, by = 0.05)
info_curves <- purrr::imap_dfr(fits, \(f, label) {
  gp <- group_pars |> filter(model == label)
  purrr::map_dfr(c("v1", "v2"), \(frm) {
    pr <- per_run |> filter(form == frm) |>
      inner_join(gp |> filter(form == frm), by = c("group", "form"))
    info <- vapply(theta_grid, \(t) {
      p <- pr$g + (1 - pr$g) * plogis(t + pr$d)
      sum(pr$trials * ((p - pr$g) / (1 - pr$g))^2 * (1 - p) / p)
    }, numeric(1))
    tibble(form = frm, theta = theta_grid, info = info, model = label)
  })
})

saveRDS(list(scores = scores, group_pars = group_pars, info_curves = info_curves,
             per_run = per_run, n_items = length(items),
             logLik = sapply(fits, \(f) extract.mirt(f, "logLik")),
             df = sapply(fits, \(f) extract.mirt(f, "nest"))),
        here::here("data/hf_v2/refit_results.rds"))
