# Sherlock driver for the SRE DDM-IRT fit (pure cmdstanr, no brms needed).
# Expects to run in the directory containing sre_ddm.stan, standata.json,
# init_[1-4].json, lookup.json. Writes fit CSVs to ./csv and summarized
# estimates to sre_ddm_estimates.rds (small; rsync this back).

user_lib <- Sys.getenv("R_LIBS_USER")
if (nzchar(user_lib)) dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
for (p in c("cmdstanr", "posterior", "jsonlite")) {
  if (!requireNamespace(p, quietly = TRUE))
    install.packages(p, repos = c("https://stan-dev.r-universe.dev",
                                  "https://cloud.r-project.org"))
}
library(cmdstanr)
library(posterior)

if (is.null(tryCatch(cmdstan_path(), error = \(e) NULL))) {
  message("installing cmdstan under $HOME...")
  install_cmdstan(cores = 4)
}

threads <- as.integer(Sys.getenv("STAN_THREADS_PER_CHAIN", "8"))
dir.create("csv", showWarnings = FALSE)

mod <- cmdstan_model("sre_ddm.stan", cpp_options = list(stan_threads = TRUE))

fit <- mod$sample(
  data = "standata.json",
  init = paste0("init_", 1:4, ".json"),
  chains = 4, parallel_chains = 4, threads_per_chain = threads,
  iter_warmup = 500, iter_sampling = 500,
  adapt_delta = 0.9, seed = 42,
  output_dir = "csv", refresh = 25
)

message("sampling finished; summarizing...")

lookup <- jsonlite::read_json("lookup.json", simplifyVector = TRUE)

summ_b <- fit$summary(variables = c("b", "b_bs", "b_ndt"))
sds <- fit$summary(variables = grep("^sd_", fit$metadata()$stan_variables,
                                    value = TRUE))

re_est <- list()
for (gid in names(lookup$groups)) {
  g <- lookup$groups[[gid]]
  suffix <- switch(g$dpar, drift = "1", bs = "bs_1", ndt = "ndt_1")
  var <- paste0("r_", gid, "_", suffix)
  s <- fit$summary(variables = var, "mean", "sd")
  re_est[[paste(g$dpar, g$var, sep = "_")]] <-
    data.frame(level = g$levels, est = s$mean, se = s$sd,
               dpar = g$dpar, var = g$var)
}

diagn <- fit$diagnostic_summary()

saveRDS(list(fixef = summ_b, sd = sds, re = re_est, diag = diagn,
             time = fit$time()),
        "sre_ddm_estimates.rds")
message("saved sre_ddm_estimates.rds")
