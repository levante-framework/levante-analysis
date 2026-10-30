# Round 2 of memory staircase policies (team feedback + exotic variants),
# extending _prep_memory_length.R (reuses its cached difficulties, priors,
# per-trial time model, and full-data thetas). Policies:
#   S6_ramp1_verify : promote after 1 correct; fail-out = 3 errors at the
#                     current length; then 2 verification trials at len-1;
#                     both blocks start at len 2 (team proposal as stated)
#   S7_cat{8,12,16} : max-information CAT over (dir, len) at the run's grid,
#                     fixed trial count — the infeasible-but-topline bound
#   S8_fasttrack    : S4 rules (promote-2, starts at 3) + clearing forward
#                     len 5 exits forward immediately; backward starts at 4
# Saves data/memory_length_results2.rds (same sims format as round 1).

library(dplyr)
library(readr)
library(stringr)
library(purrr)

set.seed(43)
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

# ---- team proposal: ramp on 1 correct, fail-out at 3-at-length, verify ----
sim_block_ramp1 <- function(theta, grid_, dir_, start_len = 2) {
  mx <- max_len$mx[max_len$dir == dir_ & max_len$grid == grid_]
  len <- min(start_len, mx); err_here <- 0L
  lens <- integer(0); xs <- integer(0)
  while (length(lens) < 60) {
    x <- rbinom(1, 1, plogis(theta + dget(dir_, grid_, len)))
    lens <- c(lens, len); xs <- c(xs, x)
    if (x == 1) {
      # promotion resets the at-length error count; at the bank ceiling the
      # child stays (errors keep accumulating toward fail-out)
      if (len < mx) { len <- len + 1L; err_here <- 0L }
    } else {
      err_here <- err_here + 1L
      if (err_here >= 3L) {
        if (len > 2) {  # verification: two trials at len - 1
          for (k in 1:2) {
            xv <- rbinom(1, 1, plogis(theta + dget(dir_, grid_, len - 1L)))
            lens <- c(lens, len - 1L); xs <- c(xs, xv)
          }
        }
        break
      }
    }
  }
  tibble(dir = dir_, len = lens, x = xs)
}

# ---- CAT: max-info length selection, sequential EAP update ----
sim_cat <- function(theta, grid_, mu, v, n_trials) {
  bank <- dlook |> filter(grid == grid_)
  lp <- dnorm(theta_grid, mu, sqrt(v), log = TRUE)
  out <- vector("list", n_trials)
  for (t in seq_len(n_trials)) {
    w <- exp(lp - max(lp)); w <- w / sum(w)
    est <- sum(w * theta_grid)
    p_at <- plogis(est + bank$d)
    pick <- which.max(p_at * (1 - p_at))
    p_true <- plogis(theta + bank$d[pick])
    x <- rbinom(1, 1, p_true)
    p_grid <- plogis(theta_grid + bank$d[pick])
    lp <- lp + if (x == 1) log(p_grid) else log1p(-p_grid)
    out[[t]] <- tibble(dir = bank$dir[pick], len = bank$len[pick], x = x)
  }
  bind_rows(out)
}

# ---- fast-track: S4 + forward early exit ----
sim_block_s4 <- function(theta, grid_, dir_, start_len, exit_after_len = Inf) {
  mx <- max_len$mx[max_len$dir == dir_ & max_len$grid == grid_]
  len <- min(start_len, mx); errors <- 0L; consec <- 0L
  lens <- integer(0); xs <- integer(0)
  while (errors < 3L && length(lens) < 60) {
    x <- rbinom(1, 1, plogis(theta + dget(dir_, grid_, len)))
    lens <- c(lens, len); xs <- c(xs, x)
    if (x == 1) {
      consec <- consec + 1L
      if (consec >= 2L) {
        if (len >= exit_after_len) break  # cleared the exit threshold
        len <- min(len + 1L, mx); consec <- 0L
      }
    } else { errors <- errors + 1L; consec <- 0L }
  }
  tibble(dir = dir_, len = lens, x = xs)
}

sim_policy <- function(theta, grid_, mu, v, policy) {
  if (policy == "S6_ramp1_verify") {
    bind_rows(sim_block_ramp1(theta, grid_, "forward"),
              sim_block_ramp1(theta, grid_, "backward"))
  } else if (str_detect(policy, "^S7_cat")) {
    sim_cat(theta, grid_, mu, v, as.integer(str_extract(policy, "[0-9]+$")))
  }
}

# S8: backward start depends on whether forward fast-tracked
sim_policy_s8 <- function(theta, grid_) {
  fwd <- sim_block_s4(theta, grid_, "forward", 3, exit_after_len = 5)
  fast <- max(fwd$len) >= 5 && sum(fwd$x[fwd$len == 5]) >= 2
  bwd <- sim_block_s4(theta, grid_, "backward", if (fast) 4 else 3)
  bind_rows(fwd, bwd)
}

policies <- c("S6_ramp1_verify", "S7_cat8", "S7_cat12", "S7_cat16",
              "S8_fasttrack")
REPS <- 10
message("simulating ", nrow(mg_key), " runs x ", length(policies),
        " policies x ", REPS, " reps...")

sims2 <- map_dfr(policies, function(pol) {
  map_dfr(seq_len(REPS), function(rep) {
    mg_key |>
      mutate(sim = pmap(list(theta, grid, mu, v), \(th, gr, mu_, v_) {
        tr <- if (pol == "S8_fasttrack") sim_policy_s8(th, gr)
              else sim_policy(th, gr, mu_, v_, pol)
        tr <- tr |> left_join(dlook |> filter(grid == gr) |> select(-grid),
                              by = c("dir", "len"))
        r <- eap(tr$x, tr$d, mu_, v_)
        tibble(n_tr = nrow(tr), mins = sum(trial_secs(tr$len)) / 60,
               est = r[1], se = r[2])
      })) |>
      tidyr::unnest(sim) |>
      select(run_id, dataset, user_id, theta, n_tr, mins, est, se) |>
      mutate(policy = pol, rep = rep)
  })
})

write_rds(list(sims = sims2, policies = policies),
          here::here("data/memory_length_results2.rds"), compress = "gz")
message("saved data/memory_length_results2.rds")
