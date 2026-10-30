# Hierarchical speed-accuracy model for SRE (van der Linden 2007 style,
# two-part crossed-random-effects approximation):
#   accuracy: logit GLMM  correct ~ 0 + dataset + (1|run_id) + (1|item_uid)
#   speed:    LMM     log(rt) ~ 0 + dataset + (1|run_id) + (1|item_uid)
# Run-level ability = accuracy run RE; speed = -(RT run RE).
# Data: levante_data_latest (pinned in common.R).
# Saves run-level estimates to data/sre_sa_estimates.rds.

source(here::here("common.R"))
library(lme4)

sre <- load_levante_latest("trials") |>
  filter(task_id == "sre") |>
  mutate(rt_s = rt_numeric / 1000) |>
  filter(rt_s >= 0.25, rt_s <= 30)

message(nrow(sre), " trials, ", n_distinct(sre$run_id), " runs, ",
        n_distinct(sre$item_uid), " items")

t0 <- Sys.time()
acc_mod <- glmer(correct ~ 0 + dataset + (1 | run_id) + (1 | item_uid),
                 data = sre, family = binomial, nAGQ = 0,
                 control = glmerControl(optimizer = "bobyqa"))
message("accuracy GLMM done: ", format(Sys.time() - t0))

t0 <- Sys.time()
rt_mod <- lmer(log(rt_s) ~ 0 + dataset + (1 | run_id) + (1 | item_uid),
               data = sre, REML = FALSE)
message("RT LMM done: ", format(Sys.time() - t0))

re_acc <- ranef(acc_mod, condVar = TRUE)$run_id
re_rt <- ranef(rt_mod, condVar = TRUE)$run_id
run_est <- tibble(run_id = rownames(re_acc),
                  theta = re_acc[[1]],
                  theta_se = sqrt(as.vector(attr(re_acc, "postVar")))) |>
  full_join(tibble(run_id = rownames(re_rt),
                   tau = -re_rt[[1]],  # higher = faster
                   tau_se = sqrt(as.vector(attr(re_rt, "postVar")))),
            by = "run_id") |>
  left_join(distinct(sre, run_id, dataset, user_id), by = "run_id")

item_est <- tibble(item_uid = rownames(ranef(acc_mod)$item_uid),
                   easiness = ranef(acc_mod)$item_uid[[1]]) |>
  full_join(tibble(item_uid = rownames(ranef(rt_mod)$item_uid),
                   time_intensity = ranef(rt_mod)$item_uid[[1]]),
            by = "item_uid")

write_rds(list(run = run_est, item = item_est,
               vc_acc = as.data.frame(VarCorr(acc_mod)),
               vc_rt = as.data.frame(VarCorr(rt_mod))),
          here::here("data/sre_sa_estimates.rds"), compress = "gz")
message("saved data/sre_sa_estimates.rds")
