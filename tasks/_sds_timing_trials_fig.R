# Mean SDS trials of each type per run: (A) by age, block-timed runs
# (v1.3.18+); (B) before vs after the per-block timer, by age group.
# 6-minute CAT runs on the current flow (v1.3.12+). Counts are trials, not
# saved rows: practice trials and something-same test trials save two rows
# each, and a match trial counts once however many selections it takes.
source(here::here("tasks/_sds_timing_common.R"))
suppressPackageStartupMessages({library(ggplot2); library(patchwork)})

runs <- sds_budget |>
  filter(n_dims >= 5, !is.na(age), vnum(task_version) >= vnum("1.3.12")) |>
  select(run_id, age, young, block_timer)

type_levels <- c("Test-dimensions", "Two-match practice", "Something-same practice",
                 "Something-same test", "2-match", "3-match", "4-match")
counts <- sds_trials |>
  semi_join(runs, by = "run_id") |>
  mutate(type = case_when(
    segment == "dimensions" ~ "Test-dimensions",
    practice & grepl("match", corpus_trial_type) & assessment_stage == "practice_response" ~ "Two-match practice",
    practice & corpus_trial_type == "something-same-2" ~ "Something-same practice",
    segment == "something_same" & corpus_trial_type == "something-same-2" ~ "Something-same test",
    segment == "match" & assessment_stage == "first_response" ~ paste0(substr(corpus_trial_type, 1, 1), "-match"),
    TRUE ~ NA_character_),
    # two-match practice saves a row per selection (two per trial)
    w = if_else(type == "Two-match practice", 0.5, 1)) |>
  filter(!is.na(type)) |>
  group_by(run_id, type) |>
  summarise(n = sum(w), .groups = "drop") |>
  right_join(runs |> tidyr::crossing(type = type_levels), by = c("run_id", "type")) |>
  mutate(n = coalesce(n, 0),
         type = factor(type, type_levels),
         kind = if_else(grepl("practice", type), "Practice", "Test"))

age_lab <- \(a) cut(a, c(0, 5, 6, 8, 10, 20), right = FALSE,
                    labels = c("Under 5", "5–6", "6–8", "8–10", "10+"))
# facet label with run count, kept in the order of `lab`'s factor levels
facet_n <- \(df) df |>
  group_by(lab) |>
  mutate(facet = paste0(lab, "\n(", n_distinct(run_id), " runs)")) |>
  ungroup() |>
  mutate(facet = forcats::fct_reorder(facet, as.integer(lab)))

pal <- c(Test = "#2a78d6", Practice = "#eb6834")
base_theme <- theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(colour = "#e6e5e1", linewidth = 0.3),
        strip.text = element_text(face = "bold", colour = "#0b0b0b"),
        axis.text = element_text(colour = "#52514e"), legend.position = "top",
        legend.title = element_blank(), plot.title = element_text(face = "bold"),
        plot.background = element_rect(fill = "#fcfcfb", colour = NA))
bars <- \(df) ggplot(df, aes(x = mean, y = forcats::fct_rev(type), fill = kind)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.1f", mean)), hjust = -0.15, size = 3, colour = "#52514e") +
  scale_fill_manual(values = pal) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(x = "Mean trials per run", y = NULL) + base_theme

a <- counts |>
  filter(block_timer) |>
  mutate(lab = age_lab(age)) |>
  facet_n() |>
  group_by(facet, type, kind) |> summarise(mean = mean(n), .groups = "drop") |>
  bars() + facet_wrap(~facet, nrow = 1) +
  labs(title = "A. By age (block-timed runs, v1.3.18+)")

b <- counts |>
  mutate(lab = factor(paste0(if_else(young, "Under 5", "5 and older"), ": ",
                            if_else(block_timer, "new (per-block timer, v1.3.18+)", "old (whole-task timer, v1.3.12–1.3.17)")),
                     levels = c("Under 5: old (whole-task timer, v1.3.12–1.3.17)", "Under 5: new (per-block timer, v1.3.18+)",
                                "5 and older: old (whole-task timer, v1.3.12–1.3.17)", "5 and older: new (per-block timer, v1.3.18+)"))) |>
  facet_n() |>
  group_by(facet, type, kind) |> summarise(mean = mean(n), .groups = "drop") |>
  bars() + facet_wrap(~facet, nrow = 1) +
  labs(title = "B. Old vs new timing, by age group")

fig <- a / b + plot_annotation(
  title = "Same & Different: trials of each type per 6-minute run",
  caption = "6-minute CAT runs, v1.3.12+. Practice and something-same trials save two rows each; counted here as one trial.",
  theme = theme(plot.title = element_text(face = "bold", size = 14),
                plot.background = element_rect(fill = "#fcfcfb", colour = NA)))
out <- Sys.getenv("SDS_FIG_OUT", here::here("data/sds_timing/sds_trials_by_type.png"))
ggsave(out, fig, width = 15, height = 9, dpi = 150, bg = "#fcfcfb")
message("saved ", out)
