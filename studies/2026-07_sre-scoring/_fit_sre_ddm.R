# Pooled DDM-IRT for SRE via brms wiener family.
#   drift:    ~ 0 + dataset + (1|run_id) + (1|item_uid)   <- item REs = the IRT part
#   boundary: ~ 0 + dataset + (1|run_id)
#   ndt:      ~ 0 + dataset + (1|run_id)
#   bias fixed at 0.5 (accuracy coding, balanced true/false)
# Item banks are disjoint across sites, so datasets are linked via priors only.
# Saves run/item posterior-mean estimates to data/sre_ddm_estimates.rds
# and the (draws-trimmed) fit summary to data/sre_ddm_summary.rds.

library(dplyr)
library(readr)
library(brms)

options(mc.cores = 4, brms.backend = "cmdstanr")

sre <- read_rds(here::here("data/hackathon_v1_1_trials_roar.rds")) |>
  filter(task_id == "sre") |>
  mutate(correct = as.integer(as.logical(correct)), rt_s = rt_numeric / 1000) |>
  filter(rt_s >= 0.25, rt_s <= 30)

message(nrow(sre), " trials, ", n_distinct(sre$run_id), " runs, ",
        n_distinct(sre$item_uid), " items")

f <- bf(rt_s | dec(correct) ~ 0 + dataset + (1 | run_id) + (1 | item_uid),
        bs ~ 0 + dataset + (1 | run_id),
        ndt ~ 0 + dataset + (1 | run_id),
        bias = 0.5)

priors <- c(
  prior(normal(1, 1.5), class = "b"),
  prior(normal(0.4, 0.5), class = "b", dpar = "bs"),
  prior(normal(-1.9, 0.3), class = "b", dpar = "ndt"),  # exp(-1.9) ~ 0.15 s
  prior(exponential(2), class = "sd"),
  prior(exponential(4), class = "sd", dpar = "bs"),
  prior(exponential(6), class = "sd", dpar = "ndt")
)

# ndt inits must satisfy exp(eta_ndt) < min(rt) for every trial; find the
# stan names of the ndt coefficients and its RE sd from the generated code.
scode <- stancode(f, data = sre, family = wiener(), prior = priors)
ndt_id <- regmatches(scode, regexpr("r_[0-9]+_ndt_1", scode)) |>
  sub(pattern = "r_([0-9]+)_ndt_1", replacement = "\\1")
message("ndt grouping id: ", ndt_id)

init_fun <- function() {
  out <- list(b_ndt = array(log(0.08), dim = n_distinct(sre$dataset)))
  out[[paste0("sd_", ndt_id)]] <- array(0.01, dim = 1)
  out[[paste0("z_", ndt_id)]] <- matrix(0, nrow = 1,
                                        ncol = n_distinct(sre$run_id))
  out
}

t0 <- Sys.time()
fit <- brm(f, data = sre, family = wiener(),
           prior = priors, init = init_fun,
           chains = 4, cores = 4, threads = threading(3),
           iter = 1000, warmup = 500, seed = 42,
           file = here::here("data/sre_ddm_fit"),
           file_refit = "on_change")
message("sampling done: ", format(Sys.time() - t0))

re <- ranef(fit)
fe <- fixef(fit)

run_est <- tibble(run_id = rownames(re$run_id),
                  drift_re = re$run_id[, "Estimate", "Intercept"],
                  bs_re = re$run_id[, "Estimate", "bs_Intercept"],
                  ndt_re = re$run_id[, "Estimate", "ndt_Intercept"]) |>
  left_join(distinct(sre, run_id, dataset, user_id), by = "run_id")

item_est <- tibble(item_uid = rownames(re$item_uid),
                   drift_re = re$item_uid[, "Estimate", "Intercept"])

write_rds(list(run = run_est, item = item_est, fixef = fe),
          here::here("data/sre_ddm_estimates.rds"), compress = "gz")
message("saved data/sre_ddm_estimates.rds")
