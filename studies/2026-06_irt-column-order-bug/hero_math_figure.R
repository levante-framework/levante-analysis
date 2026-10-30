# Hero longitudinal math figure for a slide: CO-Bogotá, CO-rural, DE.
# Corrected (bug-fixed) EAP scores. Points colored by LEVANTE site convention,
# faint within-child trajectory lines where two-wave data exists, GAM smooth+SE.
suppressMessages({library(tidyverse); library(here); library(mirt); library(levantemodels); library(mgcv)})
source(here("common.R"))

fig_dir <- here("figures"); fs::dir_create(fig_dir)
scores  <- read_rds(here("data", "scores_all_sites.rds"))
mr_math <- read_rds(here("data", "math_model_record_v1_14.rds"))

datasets <- c("pilot_uniandes_co_bogota", "pilot_mpieva_de_main")

# corrected EAP per dataset
corrected <- map_dfr(datasets, function(ds) {
  tr <- load_levante_trials(task_ids = "egma-math") |> filter(dataset == ds)
  score_task_irt(tr, "math", ds, mr_math, method = "EAP") |>
    transmute(run_id, score_corrected = pmin(pmax(score, -6), 6))
})

dat <- scores |>
  filter(task_id == "egma-math", dataset %in% datasets) |>
  select(run_id, user_id, age, site, dataset, dataset_label) |>
  inner_join(corrected, by = "run_id") |>
  group_by(dataset, user_id) |> arrange(age) |> mutate(wave = row_number()) |>
  ungroup()

# nicer, ordered panel labels
dat <- dat |>
  mutate(panel = recode(dataset,
           pilot_uniandes_co_bogota = "Colombia — Bogotá",
           pilot_mpieva_de_main     = "Germany — Leipzig") |>
         factor(levels = c("Colombia — Bogotá", "Germany — Leipzig")))

# lines are the star: only kids with two waves and a real age gap
traj_lines <- dat |>
  group_by(dataset, user_id) |>
  filter(n_distinct(wave) >= 2, (max(age) - min(age)) > 1/12) |>
  ungroup()

# per-child change, then per-panel summaries
child_change <- traj_lines |>
  group_by(panel, user_id) |>
  summarise(d_theta = score_corrected[wave == 2] - score_corrected[wave == 1],
            d_age   = age[wave == 2] - age[wave == 1],
            .groups = "drop")

stats <- child_change |>
  group_by(panel) |>
  summarise(n          = n(),
            mean_dtheta = mean(d_theta),
            se_dtheta   = sd(d_theta) / sqrt(n()),
            mean_dage   = mean(d_age),
            # Δθ/year: ratio of means (stable; avoids dividing by tiny gaps)
            dtheta_per_year = mean(d_theta) / mean(d_age),
            .groups = "drop")

cat("=== per-panel growth stats ===\n"); print(stats, width = 200)

# on-figure annotation: N and overall Δθ (NOT the per-year rate)
labs_df <- stats |>
  mutate(label = sprintf("N = %d\nΔθ = %+.2f", n, mean_dtheta))

p <- ggplot(traj_lines, aes(x = age, y = score_corrected)) +
  geom_line(aes(group = user_id, color = site), alpha = 0.45, linewidth = 0.4) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 4),
              color = "black", fill = "grey75", linewidth = 1.1) +
  geom_text(data = labs_df, aes(x = -Inf, y = Inf, label = label),
            hjust = -0.12, vjust = 1.15, size = 4.6, lineheight = 0.95,
            family = .levante_font) +
  facet_wrap(~panel) +
  scale_colour_site(guide = "none") +
  labs(x = "Age (years)", y = "Math ability (IRT θ)") +
  theme_bw(base_size = 18, base_family = .levante_font) +
  theme(panel.grid = element_blank(), strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        panel.border = element_blank(), axis.line = element_line())

ggsave(file.path(fig_dir, "hero_math_trajectories.png"), p,
       width = 7.5, height = 4, dpi = 300, bg = "white")
cat("saved figures/hero_math_trajectories.png\n")
