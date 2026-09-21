# Round 3: cap-aware policies, after dev confirmation that deployed blocks
# end at min(hardcoded trial cap = 21, 3 cumulative errors). Rounds 1-2
# simulated without the cap; this rerun adds it, plus the dev's cap-
# reduction proposal and an age-safe variant of S4.
#   C0_current  : promote-3, starts 2/2, caps 21/21  (validation anchor)
#   C1_devcaps  : promote-3, starts 2/2, caps 14/12  (dev proposal)
#   C2_promote2 : promote-2, starts 2/2, caps 21/21
#   C3_safe     : promote-2, fwd start 2, bwd start 3 if fwd cleared len 3
#                 else 2, caps 14/12  (age-safe package)
#   C4_S4caps   : promote-2, starts 3/3, caps 14/12  (round-1 rec + caps)
# Saves data/memory_length_results3.rds (same sims format).

library(dplyr)
library(readr)
library(stringr)
library(purrr)

set.seed(44)
res <- read_rds(here::here("data/memory_length_results.rds"))
dlook <- res$dlook
dt_coef <- res$dt_coef
trial_secs <- \(len) dt_coef[1] + dt_coef[2] * len
theta_grid <- seq(-6, 6, length.out = 121)

eap <- function(x, d, mu, v) {
  lp <- dnorm(theta_grid, mu, sqrt(v), log = TRUE)
  for (i in seq_along(x)) {
    p <- plogis(theta_grid + d[i])
    lp <- lp + if (x[i] == 1) log(p) else log1p(-p)
  }
  w <- exp(lp - max(lp)); w <- w / sum(w)
  m <- sum(w * theta_grid)
  c(est = m, se = sqrt(sum(w * (theta_grid - m)^2)))
}

mg_key <- read_rds(here::here("data/latest_r2_trials_mg.rds")) |>
  filter(dataset %in% res$priors$group) |>
  mutate(grid = str_extract(item_uid, "[23]grid")) |>
  count(run_id, dataset, user_id, grid) |>
  group_by(run_id) |>
  slice_max(n, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(-n) |>
  left_join(res$priors, by = c("dataset" = "group")) |>
  inner_join(res$full |> select(run_id, theta = est), by = "run_id")

max_len <- dlook |> group_by(dir, grid) |> summarise(mx = max(len), .groups = "drop")
dget <- function(dir_, grid_, len_)
  dlook$d[dlook$dir == dir_ & dlook$grid == grid_ & dlook$len == len_]

sim_block_cap <- function(theta, grid_, dir_, promote, start_len, cap) {
  mx <- max_len$mx[max_len$dir == dir_ & max_len$grid == grid_]
  len <- min(start_len, mx); errors <- 0L; consec <- 0L
  lens <- integer(0); xs <- integer(0)
  while (errors < 3L && length(lens) < cap) {
    x <- rbinom(1, 1, plogis(theta + dget(dir_, grid_, len)))
    lens <- c(lens, len); xs <- c(xs, x)
    if (x == 1) {
      consec <- consec + 1L
      if (consec >= promote) { len <- min(len + 1L, mx); consec <- 0L }
    } else { errors <- errors + 1L; consec <- 0L }
  }
  tibble(dir = dir_, len = lens, x = xs)
}

sim_policy3 <- function(theta, grid_, policy) {
  if (policy == "C0_current") {
    fwd <- sim_block_cap(theta, grid_, "forward", 3, 2, 21)
    bwd <- sim_block_cap(theta, grid_, "backward", 3, 2, 21)
  } else if (policy == "C1_devcaps") {
    fwd <- sim_block_cap(theta, grid_, "forward", 3, 2, 14)
    bwd <- sim_block_cap(theta, grid_, "backward", 3, 2, 12)
  } else if (policy == "C2_promote2") {
    fwd <- sim_block_cap(theta, grid_, "forward", 2, 2, 21)
    bwd <- sim_block_cap(theta, grid_, "backward", 2, 2, 21)
  } else if (policy == "C3_safe") {
    fwd <- sim_block_cap(theta, grid_, "forward", 2, 2, 14)
    cleared3 <- max(fwd$len) >= 4  # promoted past len 3
    bwd <- sim_block_cap(theta, grid_, "backward", 2, if (cleared3) 3 else 2, 12)
  } else if (policy == "C4_S4caps") {
    fwd <- sim_block_cap(theta, grid_, "forward", 2, 3, 14)
    bwd <- sim_block_cap(theta, grid_, "backward", 2, 3, 12)
  }
  bind_rows(fwd, bwd)
}

policies <- c("C0_current", "C1_devcaps", "C2_promote2", "C3_safe", "C4_S4caps")
REPS <- 10
message("simulating ", nrow(mg_key), " runs x ", length(policies),
        " policies x ", REPS, " reps...")

sims3 <- map_dfr(policies, function(pol) {
  map_dfr(seq_len(REPS), function(rep) {
    mg_key |>
      mutate(sim = pmap(list(theta, grid, mu, v), \(th, gr, mu_, v_) {
        tr <- sim_policy3(th, gr, pol)
        tr <- tr |> left_join(dlook |> filter(grid == gr) |> select(-grid),
                              by = c("dir", "len"))
        r <- eap(tr$x, tr$d, mu_, v_)
        tibble(n_tr = nrow(tr), mins = sum(trial_secs(tr$len)) / 60,
               n_succ_fwd = sum(tr$x[tr$dir == "forward"]),
               est = r[1], se = r[2])
      })) |>
      tidyr::unnest(sim) |>
      select(run_id, dataset, user_id, theta, n_tr, mins, n_succ_fwd, est, se) |>
      mutate(policy = pol, rep = rep)
  })
})

write_rds(list(sims = sims3, policies = policies),
          here::here("data/memory_length_results3.rds"), compress = "gz")
message("saved data/memory_length_results3.rds")
