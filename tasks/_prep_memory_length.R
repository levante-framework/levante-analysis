# Memory (Corsi) length-reduction precompute for tasks/memory_length.qmd.
#
# Staircase (reverse-engineered from trials, modal modern rule): forward
# starts at len 2; advance +1 after 3 consecutive correct; errors stay;
# block ends at 3 cumulative errors; backward restarts at len 2; run ends
# after backward's 3 errors. Items are mg_{dir}_{grid}_len{N}-{attempt} with
# difficulties TIED across attempts, so scoring only needs the multiset of
# (dir, grid, len) responses. g = 0, Rasch, multigroup_dataset scalar.
#
# Two evaluation modes:
#  - REPLAY (exact): error budgets E < 3 are prefixes of the observed
#    sequence -> truncate and rescore.
#  - SIMULATION (model-based): path-changing policies (faster promotion,
#    higher starts) simulated from each run's full-data EAP theta; the
#    current rule (S0) is simulated too as the calibration anchor.
#
# Saves data/memory_length_results.rds.

library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(purrr)

set.seed(42)
suppressMessages(library(levantemodels))

datasets_used <- c("pilot_mpieva_de_main", "pilot_uniandes_co_bogota",
                   "pilot_uniandes_co_rural", "pilot_western_ca_main")

mg <- read_rds(here::here("data/latest_r2_trials_mg.rds")) |>
  filter(dataset %in% datasets_used) |>
  mutate(dir = str_extract(item_uid, "(?<=mg_)[a-z]+"),
         grid = str_extract(item_uid, "[23]grid"),
         len = as.integer(str_extract(item_uid, "(?<=len)[0-9]+")),
         correct = as.logical(correct)) |>
  filter(!is.na(correct)) |>
  group_by(run_id) |>
  arrange(trial_number, .by_group = TRUE) |>
  ungroup()

sc <- read_rds(here::here("data/levante_data_latest__v1_2__scores.rds")) |>
  filter(task_id == "memory-game", !is.na(score), dataset %in% datasets_used)

# ---- per-trial duration model: seconds to next trial ~ length ----
dt_dat <- mg |>
  group_by(run_id) |>
  mutate(dt = as.numeric(difftime(lead(timestamp), timestamp, units = "secs"))) |>
  ungroup() |>
  filter(!is.na(dt), dt > 0, dt < 120)
dt_med <- dt_dat |> group_by(len) |> summarise(med = median(dt), .groups = "drop")
dt_fit <- lm(med ~ len, data = dt_med)
trial_secs <- \(len) predict(dt_fit, newdata = data.frame(len = len))

# ---- item difficulties (easiness d; P = plogis(theta + d)) + priors ----
mr <- readRDS(here::here("data/mg_model_record_current.rds"))
mv <- model_vals(mr)
g1 <- mv$group[1]
dlook <- mv |>
  filter(name == "d", group == g1) |>
  transmute(dir = str_extract(item, "(?<=mg_)[a-z]+"),
            grid = str_extract(item, "[23]grid"),
            len = as.integer(str_extract(item, "(?<=len)[0-9]+")),
            d = value) |>
  distinct(dir, grid, len, .keep_all = TRUE)
priors <- mv |>
  filter(name %in% c("MEAN_1", "COV_11")) |>
  select(group, name, value) |>
  pivot_wider(names_from = name, values_from = value) |>
  rename(mu = MEAN_1, v = COV_11)

max_len <- dlook |> group_by(dir, grid) |> summarise(mx = max(len), .groups = "drop")

# ---- quadrature EAP: responses (x) on easinesses (d), prior N(mu, v) ----
theta_grid <- seq(-6, 6, length.out = 121)
eap <- function(x, d, mu, v) {
  lp <- dnorm(theta_grid, mu, sqrt(v), log = TRUE)
  for (i in seq_along(x)) {
    p <- plogis(theta_grid + d[i])
    # NB: not ifelse() — a scalar condition would collapse the grid vector
    lp <- lp + if (x[i] == 1) log(p) else log1p(-p)
  }
  w <- exp(lp - max(lp)); w <- w / sum(w)
  m <- sum(w * theta_grid)
  c(est = m, se = sqrt(sum(w * (theta_grid - m)^2)))
}

mg <- mg |> left_join(dlook, by = c("dir", "grid", "len"))
# a handful of 2grid len-8 trials are uncalibrated (no model item) — drop
message("dropping ", sum(is.na(mg$d)), " uncalibrated trials")
mg <- mg |> filter(!is.na(d))
# 3 runs mix grids (deployment oddity); key each run by its modal grid
run_key <- mg |>
  count(run_id, dataset, user_id, grid) |>
  group_by(run_id) |>
  slice_max(n, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(-n) |>
  left_join(priors, by = c("dataset" = "group"))

score_runs <- function(trials) {
  trials |>
    group_by(run_id) |>
    summarise(x = list(as.integer(correct)), d = list(d),
              n_tr = n(), mins = sum(trial_secs(len)) / 60, .groups = "drop") |>
    inner_join(run_key, by = "run_id") |>
    mutate(res = pmap(list(x, d, mu, v), \(x, d, mu, v) eap(x, d, mu, v)),
           est = map_dbl(res, 1), se = map_dbl(res, 2)) |>
    select(run_id, dataset, user_id, n_tr, mins, est, se)
}

message("scoring full runs...")
full <- score_runs(mg)

# validation vs published pipeline scores
val <- full |> inner_join(sc |> select(run_id, published = score), by = "run_id")
validation <- val |> group_by(dataset) |>
  summarise(n = n(), r_vs_published = cor(est, published), .groups = "drop")
print(validation)

# ---- REPLAY: error budgets 1, 2 (3 = observed/full) ----
replay <- map_dfr(c(1, 2), function(E) {
  trunc <- mg |>
    group_by(run_id, dir) |>
    arrange(trial_number, .by_group = TRUE) |>
    mutate(err_before = lag(cumsum(!correct), default = 0)) |>
    filter(err_before < E) |>   # keep through the Eth error, drop the rest
    ungroup()
  score_runs(trunc) |> mutate(policy = paste0("budget", E))
})

# ---- Rasch calibration check for the simulator ----
calib <- mg |>
  inner_join(full |> select(run_id, est), by = "run_id") |>
  mutate(p_hat = plogis(est + d), q = ntile(est, 5)) |>
  group_by(q, len) |>
  filter(n() >= 100) |>
  summarise(n = n(), obs_acc = mean(correct), pred_acc = mean(p_hat),
            .groups = "drop")

# ---- SIMULATION ----
policies <- tribble(
  ~policy, ~promote, ~budget, ~fwd_start, ~bwd_start,
  "S0_current",           3, 3, 2, 2,
  "S1_promote2",          2, 3, 2, 2,
  "S2_bwd_start3",        3, 3, 2, 3,
  "S3_promote2_bwd3",     2, 3, 2, 3,
  "S4_promote2_starts3",  2, 3, 3, 3,
  "S5_promote2_budget2_bwd3", 2, 2, 2, 3
)
REPS <- 10

sim_block <- function(theta, grid_, dir_, promote, budget, start_len) {
  mx <- max_len$mx[max_len$dir == dir_ & max_len$grid == grid_]
  len <- min(start_len, mx); errors <- 0L; consec <- 0L
  lens <- integer(0); xs <- integer(0)
  while (errors < budget && length(lens) < 60) {
    d_ <- dlook$d[dlook$dir == dir_ & dlook$grid == grid_ & dlook$len == len]
    x <- rbinom(1, 1, plogis(theta + d_))
    lens <- c(lens, len); xs <- c(xs, x)
    if (x == 1) {
      consec <- consec + 1L
      if (consec >= promote) { len <- min(len + 1L, mx); consec <- 0L }
    } else { errors <- errors + 1L; consec <- 0L }
  }
  tibble(dir = dir_, len = lens, x = xs)
}

sim_run <- function(theta, grid_, pol) {
  bind_rows(
    sim_block(theta, grid_, "forward", pol$promote, pol$budget, pol$fwd_start),
    sim_block(theta, grid_, "backward", pol$promote, pol$budget, pol$bwd_start))
}

sim_key <- run_key |> inner_join(full |> select(run_id, theta = est), by = "run_id")
message("simulating ", nrow(sim_key), " runs x ", nrow(policies),
        " policies x ", REPS, " reps...")

sims <- map_dfr(seq_len(nrow(policies)), function(pi) {
  pol <- policies[pi, ]
  map_dfr(seq_len(REPS), function(rep) {
    sim_key |>
      mutate(sim = pmap(list(theta, grid, mu, v), \(th, gr, mu_, v_) {
        tr <- sim_run(th, gr, pol)
        tr <- tr |> left_join(dlook |> filter(grid == gr) |> select(-grid),
                              by = c("dir", "len"))
        r <- eap(tr$x, tr$d, mu_, v_)
        tibble(n_tr = nrow(tr), mins = sum(trial_secs(tr$len)) / 60,
               est = r[1], se = r[2])
      })) |>
      unnest(sim) |>
      select(run_id, dataset, user_id, theta, n_tr, mins, est, se) |>
      mutate(policy = pol$policy, rep = rep)
  })
})

write_rds(list(full = full, replay = replay, sims = sims,
               validation = validation, calib = calib,
               dt_med = dt_med, dt_coef = coef(dt_fit),
               policies = policies, priors = priors, dlook = dlook),
          here::here("data/memory_length_results.rds"), compress = "gz")
message("saved data/memory_length_results.rds")
