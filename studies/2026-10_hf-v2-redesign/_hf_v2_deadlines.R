# Counterfactual-deadline calibration for H&F (sourced by
# studies/2026-10_hf-v2-redesign/hearts_and_flowers_v2.qmd, section on timing). v1 was untimed, so any
# deadline D can be imposed after the fact: correct_D = right side AND
# 200 <= rt <= D. Item difficulties under each D are estimated with persons'
# released theta held fixed (Rasch, guessing 0.5, the released model's
# conventions), which puts every hypothetical deadline on the released scale.
# Writes data/hf_v2/deadline_items.rds.

source(here::here("studies/2026-10_hf-v2-redesign/_hf_v2_common.R"))

scores <- readRDS(here::here("data/scores_latest_v3_17.rds")) |>
  filter(task_id == "hearts-and-flowers", !is.na(score)) |>
  distinct(run_id, .keep_all = TRUE) |>
  select(run_id, theta = score)

# item_class = the released item (block x stimulus x start/stay/switch,
# from hf_test's item_uid), e.g. "heartsflowers_heart_switch"
typed <- hf_test |>
  mutate(item_class = sub("^hf_", "", item_uid))

g <- 0.5
p_correct <- \(theta, b) g + (1 - g) * plogis(theta - b)
fit_b <- function(theta, y) {
  nll <- \(b) -sum(dbinom(y, 1, p_correct(theta, b), log = TRUE))
  optimize(nll, c(-8, 8))$minimum
}

deadlines <- c(Inf, 3000, 2500, 2000, 1750, 1500, 1250, 1000, 800)

v1_typed <- typed |>
  filter(form == "v1") |>
  inner_join(scores, by = "run_id")

v1_items <- purrr::map_dfr(deadlines, \(D) {
  v1_typed |>
    mutate(y = as.integer(correct %in% TRUE & rt_numeric >= 200 & rt_numeric <= D)) |>
    group_by(item_class) |>
    summarise(deadline = D, n = n(), accuracy = mean(y),
              b = fit_b(theta, y), .groups = "drop")
})

# validation: impose 2000 / 1500 ms on v2 segment-1 responses (3 s window)
# and compare with what the same children did when actually given those
# windows in segments 2 and 3
v2_valid <- typed |>
  filter(form == "v2", block == "mixed") |>
  group_by(run_id) |>
  filter(all(1:3 %in% segment)) |>
  ungroup() |>
  mutate(ans_ok = correct %in% TRUE & !timeout)

validation <- bind_rows(
  v2_valid |> filter(segment == 1) |>
    mutate(imposed_2000 = ans_ok & rt_numeric <= 2000,
           imposed_1500 = ans_ok & rt_numeric <= 1500) |>
    summarise(seg1_untruncated = mean(ans_ok), imposed_2000 = mean(imposed_2000),
              imposed_1500 = mean(imposed_1500)),
  v2_valid |> filter(segment > 1) |>
    group_by(segment) |> summarise(actual = mean(ans_ok), .groups = "drop") |>
    tidyr::pivot_wider(names_from = segment, values_from = actual,
                       names_prefix = "actual_seg"))

saveRDS(list(v1_items = v1_items, validation = validation, typed = typed),
        here::here("data/hf_v2/deadline_items.rds"))
