# Export the SRE DDM-IRT model for fitting on Sherlock without brms:
# generates Stan code, data JSON, per-chain inits, and a grouping-level
# lookup into studies/2026-07_sre-scoring/sherlock_sre_ddm/. Companion files there: run_fit.R
# (cmdstanr driver) and fit_sre_ddm.slurm (submission script).

library(dplyr)
library(readr)
library(brms)
library(jsonlite)

out_dir <- here::here("studies/2026-07_sre-scoring/sherlock_sre_ddm")
dir.create(out_dir, showWarnings = FALSE)

sre <- read_rds(here::here("data/hackathon_v1_1_trials_roar.rds")) |>
  filter(task_id == "sre") |>
  mutate(correct = as.integer(as.logical(correct)), rt_s = rt_numeric / 1000) |>
  filter(rt_s >= 0.25, rt_s <= 30)

f <- bf(rt_s | dec(correct) ~ 0 + dataset + (1 | run_id) + (1 | item_uid),
        bs ~ 0 + dataset + (1 | run_id),
        ndt ~ 0 + dataset + (1 | run_id),
        bias = 0.5)

priors <- c(
  prior(normal(1, 1.5), class = "b"),
  prior(normal(0.4, 0.5), class = "b", dpar = "bs"),
  prior(normal(-1.9, 0.3), class = "b", dpar = "ndt"),
  prior(exponential(2), class = "sd"),
  prior(exponential(4), class = "sd", dpar = "bs"),
  prior(exponential(6), class = "sd", dpar = "ndt")
)

thr <- threading(8)
scode <- make_stancode(f, data = sre, family = wiener(), prior = priors,
                       threads = thr)
sdata <- make_standata(f, data = sre, family = wiener(), prior = priors,
                       threads = thr)

writeLines(scode, file.path(out_dir, "sre_ddm.stan"))
class(sdata) <- NULL
cmdstanr::write_stan_json(sdata, file.path(out_dir, "standata.json"))

# grouping-id -> variable/levels lookup (indices in r_<id>_* match these)
ndt_id <- sub("r_([0-9]+)_ndt_1.*", "\\1",
              regmatches(scode, regexpr("r_[0-9]+_ndt_1", scode)))
bs_id <- sub("r_([0-9]+)_bs_1.*", "\\1",
             regmatches(scode, regexpr("r_[0-9]+_bs_1", scode)))
run_levels <- sort(unique(sre$run_id))
item_levels <- sort(unique(sre$item_uid))
# drift groups: the two ids that aren't bs/ndt; disambiguate by size
all_ids <- as.character(1:4)
drift_ids <- setdiff(all_ids, c(ndt_id, bs_id))
sizes <- sapply(drift_ids, \(i) sdata[[paste0("N_", i)]])
run_drift_id <- drift_ids[sizes == length(run_levels)]
item_drift_id <- drift_ids[sizes == length(item_levels)]

lookup <- list(groups = list(), datasets = sort(unique(sre$dataset)))
lookup$groups[[run_drift_id]] <- list(var = "run_id", dpar = "drift",
                                      levels = run_levels)
lookup$groups[[item_drift_id]] <- list(var = "item_uid", dpar = "drift",
                                       levels = item_levels)
lookup$groups[[bs_id]] <- list(var = "run_id", dpar = "bs",
                               levels = run_levels)
lookup$groups[[ndt_id]] <- list(var = "run_id", dpar = "ndt",
                                levels = run_levels)
write_json(lookup, file.path(out_dir, "lookup.json"), auto_unbox = TRUE)

# inits: keep every run's ndt safely below min(rt) = 0.25 s
# (sd_* is vector[1] in the Stan code — write it as a JSON array, not a
# scalar, or cmdstan rejects the init with a dims mismatch)
init <- list(b_ndt = rep(log(0.08), length(lookup$datasets)))
init[[paste0("sd_", ndt_id)]] <- list(0.01)
init[[paste0("z_", ndt_id)]] <-
  matrix(0, nrow = 1, ncol = length(run_levels))
for (k in 1:4)
  write_json(init, file.path(out_dir, paste0("init_", k, ".json")),
             auto_unbox = FALSE, digits = 10)

message("exported to ", out_dir, ": N=", nrow(sre), " trials, ",
        length(run_levels), " runs, ", length(item_levels), " items; ",
        "ndt gid=", ndt_id, " bs gid=", bs_id,
        " drift run gid=", run_drift_id, " drift item gid=", item_drift_id)
