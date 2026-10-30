# Follow-ups to the 2026-09 Stories chapter (2026-10, after Rebecca Saxe's
# review): why development looks flat in Leipzig and Colombia, and how far
# Argentina and Colombia can be compared. Reads the 2026-09 data build;
# writes data/stories_2026-10/results_followups.rds.
suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr)})
root <- Sys.getenv("LEVANTE_ANALYSIS", here::here())
tr <- read_rds(file.path(root, "data/stories_2026-09/stories_trials.rds"))

site4 <- c(Leipzig = "pilot_mpieva_de_main", `Bogotá` = "pilot_uniandes_co_bogota",
           `rural Colombia` = "pilot_uniandes_co_rural", Western = "pilot_western_ca_main")
v0_end <- as.Date("2024-07-01")
grp_of <- \(d) d |> mutate(group = case_when(
  dataset == site4[["Bogotá"]] & as.Date(time_started) < v0_end ~ "Bogotá V0 (May–Jun 2024)",
  dataset == site4[["Bogotá"]] ~ "Bogotá, later",
  dataset %in% site4 ~ names(site4)[match(dataset, site4)],
  TRUE ~ NA_character_))
age_band <- \(a) cut(a, c(0, 8, 10, 20), right = FALSE, labels = c("< 8", "8–10", "10+"))

core <- tr |> filter(story <= 6, !policy_exclude, !is.na(age)) |> grp_of() |> filter(!is.na(group))

# runs and forms behind each group
group_runs <- core |>
  distinct(group, run_id, form, task_version, age) |>
  group_by(group) |>
  summarise(runs = n(), median_age = median(age),
            forms = paste(sort(unique(form)), collapse = ", "),
            task_versions = paste(head(names(sort(table(task_version), decreasing = TRUE)), 2), collapse = ", "),
            .groups = "drop")

# control accuracy (reality checks + emotion questions) by group x age
controls_by_age <- core |>
  filter(question_type %in% c("reality_check", "emotion_reasoning")) |>
  group_by(group, age = age_band(age)) |>
  summarise(runs = n_distinct(run_id), accuracy = mean(correct), .groups = "drop")

# items: accuracy of children 10+, by group
items_10plus <- core |>
  filter(age >= 10) |>
  group_by(story, entry, question_type, group) |>
  summarise(n = n(), accuracy = mean(correct), .groups = "drop")

# answer splits on items older V0 children miss (children 9+)
split_items <- tribble(~story, ~entry,
  2, "reality_check_2", 3, "reality_check_1", 5, "reality_check", 4, "emotion_reasoning_1",
  2, "false_belief_2", 1, "false_belief")
answer_splits <- tr |>
  filter(!is.na(age), age >= 9) |> grp_of() |> filter(!is.na(group), group != "Bogotá, later") |>
  mutate(group = if_else(startsWith(group, "Bogotá"), group, "Leipzig, Western, rural Colombia")) |>
  semi_join(split_items, by = c("story", "entry")) |>
  # share from `correct`: some Leipzig trials (Sept-Oct 2024) carry a bad
  # `answer` field but a valid `correct` (see the item audit)
  group_by(story, entry, group) |>
  summarise(n = n(), keyed = names(which.max(table(response[correct == 1]))),
            share_correct = mean(correct),
            answers = paste(names(sort(table(response), decreasing = TRUE)),
                            sort(table(response), decreasing = TRUE), sep = " ", collapse = ", "),
            .groups = "drop")

# do V0 items still track the rest of the child's answers?
item_rest <- core |>
  group_by(run_id) |>
  mutate(rest = (sum(correct) - correct) / (n() - 1)) |>
  ungroup() |>
  group_by(group, story, entry) |>
  filter(n() >= 40) |>
  summarise(r = cor(correct, rest), .groups = "drop") |>
  group_by(group) |>
  summarise(items = n(), median_item_rest_r = median(r, na.rm = TRUE), .groups = "drop")

# response-time floors: could children answer before the question audio ended?
rt_floor <- tr |>
  filter(story == 1, entry == "reality_check", !is.na(rt_numeric)) |>
  grp_of() |> filter(!is.na(group)) |>
  group_by(group) |>
  summarise(n = n(), rt_q01_s = quantile(rt_numeric, .01) / 1000,
            rt_q10_s = quantile(rt_numeric, .10) / 1000,
            rt_median_s = median(rt_numeric) / 1000,
            share_under_2s = mean(rt_numeric < 2000), .groups = "drop")
v0_rt_terciles <- core |>
  filter(group == "Bogotá V0 (May–Jun 2024)", question_type %in% c("reality_check", "emotion_reasoning"),
         !is.na(rt_numeric)) |>
  group_by(story, entry) |>
  mutate(rt_tercile = ntile(rt_numeric, 3)) |>
  group_by(rt_tercile) |>
  summarise(n = n(), median_rt_s = median(rt_numeric) / 1000, accuracy = mean(correct), .groups = "drop")

# Argentina vs Colombia: stories 13-18 at matched ages
ac_sites <- c(`UTDT-AR` = "rfp1_utdt_ar_main", `rural Colombia` = "pilot_uniandes_co_rural",
              Leipzig = "pilot_mpieva_de_main", Sheffield = "rfp1_sheffield_gb_main")
ac <- tr |>
  filter(story >= 13, !is.na(age), age >= 5.5, age < 9, dataset %in% ac_sites) |>
  mutate(site = names(ac_sites)[match(dataset, ac_sites)])
ac_samples <- ac |>
  distinct(site, run_id, age, form, task_version) |>
  group_by(site) |>
  summarise(runs = n(), median_age = median(age), forms = paste(sort(unique(form)), collapse = ", "),
            main_version = names(sort(table(task_version), decreasing = TRUE))[1], .groups = "drop")
ac_items <- ac |>
  group_by(site, story, entry, question_type) |>
  summarise(n = n(), accuracy = mean(correct), .groups = "drop")
ac_wide <- ac_items |>
  select(-n) |>
  pivot_wider(names_from = site, values_from = accuracy) |>
  left_join(ac_items |> filter(site == "rural Colombia") |> select(story, entry, n_colombia = n),
            by = c("story", "entry"))
pair_r <- \(a, b) cor(ac_wide[[a]], ac_wide[[b]], use = "complete.obs")
ac_profile <- tibble(
  pair = c("UTDT-AR vs rural Colombia", "UTDT-AR vs Leipzig", "UTDT-AR vs Sheffield", "rural Colombia vs Leipzig"),
  r = c(pair_r("UTDT-AR", "rural Colombia"), pair_r("UTDT-AR", "Leipzig"),
        pair_r("UTDT-AR", "Sheffield"), pair_r("rural Colombia", "Leipzig")))
ac_means <- ac_wide |> summarise(across(all_of(names(ac_sites)), \(x) mean(x, na.rm = TRUE)))

write_rds(list(group_runs = group_runs, controls_by_age = controls_by_age, items_10plus = items_10plus,
               answer_splits = answer_splits, item_rest = item_rest, rt_floor = rt_floor,
               v0_rt_terciles = v0_rt_terciles, ac_samples = ac_samples, ac_wide = ac_wide,
               ac_profile = ac_profile, ac_means = ac_means),
          file.path(root, "data/stories_2026-10/results_followups.rds"))
