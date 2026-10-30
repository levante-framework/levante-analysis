# ROAR-team suggestion: zero-fill unreached items and fit a standard IRT
# model, so speed enters the score through the block of zeros. One Rasch fit
# per item bank (es = the Colombian and Argentine sites share the Spanish
# bank; de; en), dense runs x items matrices; items with no variance dropped.
# Data: levante_data_latest v3_17, pinned for this study. Saves run-level EAP scores
# to data/sre_zerofill_scores.rds.

source(here::here("common.R"))

sre <- load_levante_latest("trials", version = "v3_17") |>
  filter(task_id == "sre") |>
  mutate(correct = as.integer(correct))

# Canada's 20 runs tagged es-CO are early (v1.15) runs on the English lab/ai
# corpora, so all of Canada goes in the English bank
banks <- list(
  es = c("pilot_uniandes_co_bogota", "pilot_uniandes_co_rural",
         "rfp1_utdt_ar_main"),
  de = "pilot_mpieva_de_main",
  en = c("pilot_western_ca_main", "rfp1_sheffield_gb_main",
         "pilot_capecoast_gh_main")
)

models <- list()
scores <- purrr::imap_dfr(banks, function(datasets, bank) {
  wide <- sre |>
    filter(dataset %in% datasets) |>
    select(run_id, item_uid, correct) |>
    pivot_wider(names_from = item_uid, values_from = correct,
                values_fill = 0L)   # <- unreached/unseen items scored 0
  resp <- as.data.frame(wide[, -1])
  rownames(resp) <- wide$run_id
  keep <- vapply(resp, \(x) var(x) > 0, logical(1))
  message(bank, ": ", nrow(resp), " runs x ", sum(keep), " items (",
          sum(!keep), " constant items dropped)")
  mod <- mirt::mirt(resp[, keep], 1, itemtype = "Rasch", verbose = FALSE)
  models[[bank]] <<- mod
  fs <- mirt::fscores(mod, method = "EAP", full.scores.SE = TRUE)
  tibble(bank = bank, run_id = rownames(resp),
         theta_zerofill = fs[, 1], theta_se = fs[, 2],
         rxx = mirt::empirical_rxx(fs))
})

scores <- scores |>
  left_join(distinct(sre, run_id, dataset, user_id), by = "run_id")

write_rds(scores, here::here("data/sre_zerofill_scores.rds"), compress = "gz")
write_rds(models, here::here("data/sre_zerofill_models.rds"), compress = "gz")
message("saved: ", nrow(scores), " run scores + fitted models")
