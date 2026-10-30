# Replay SDS runs under alternative block-timing policies (2026-10-29).
# Each run's own timeline (component durations; expected per-response Fisher
# information at the child's re-estimated theta under the v2.3 Rasch model; see
# _sds_timing_common.R) is replayed
# through the core-tasks timing rules (catBlockTimer.ts / catTimeline.ts as of
# v1.3.21):
#   - blocks: 0 = intro + test-dimensions; 1 = two-match practice +
#     something-same practice + something-same test (test only for children
#     under 5, who get heavyInstructions); 2 = "new game" instruction + match.
#   - timed blocks (all but the last) get maxTime / (number of effective
#     blocks), counted from the block start and INCLUDING its practice; a
#     block's CAT trials stop once its budget is spent. The last block runs on
#     the whole-task timer. Practice and instructions always run.
# Where a child's observed sequence ran out because the clock stopped it, the
# sequence is extended: with the child's own mean duration and information over
# their last 6 responses in that block, or, for blocks never reached, with the
# age group's median duration and the information the block's items give at the
# child's theta. Blocks that ended any other way (finished, error-out) are not
# extended. Writes data/sds_timing/sim_results.rds.
source(here::here("studies/2026-10_sds-block-timing/_sds_timing_common.R"))
suppressPackageStartupMessages(library(purrr))

# ---- base runs: current SDS flow (>= 1.3.12), with theta -------------------
base <- sds_budget |>
  filter(n_dims >= 5, !is.na(age), vnum(task_version) >= vnum("1.3.12")) |>
  inner_join(theta |> select(run_id, theta), by = "run_id") |>
  mutate(band = age_band(age)) |>
  left_join(prior_by_age |> select(band, prior_sd = sd), by = "band") |>
  filter(young | block_timer)   # 5+: block-timed runs only (current flow)
ev <- sds_events |> semi_join(base, by = "run_id")

# group pools for synthesising unobserved blocks
grp_med <- ev |>
  left_join(base |> select(run_id, young), by = "run_id") |>
  group_by(young, kind) |>
  summarise(med_item = median(dur), med_total = median(dur) * median(table(run_id)), .groups = "drop")
comp_med <- ev |>
  left_join(base |> select(run_id, young), by = "run_id") |>
  group_by(young, run_id, kind) |> summarise(s = sum(dur), .groups = "drop") |>
  group_by(young, kind) |> summarise(med = median(s), .groups = "drop")
gm <- \(y, k, col = "med") { x <- comp_med[[col]][comp_med$young == y & comp_med$kind == k]; if (length(x)) x else NA_real_ }
gi <- \(y, k) grp_med$med_item[grp_med$young == y & grp_med$kind == k]

# expected information per response at theta, by block, from all scored
# responses (theta-binned), used only for synthetic responses
theta_bins <- c(-Inf, -4, -3, -2, -1, 0, 1, Inf)
info_rate <- sds_trials |>
  filter(!practice, segment %in% c("something_same", "match"), !is.na(theta)) |>
  mutate(kind = if_else(segment == "match", "match", "ss"), bin = cut(theta, theta_bins)) |>
  group_by(kind, bin) |> summarise(info = mean(info), .groups = "drop")
# (info already includes the share of responses the recode drops)
exp_info <- \(k, th) {
  i <- info_rate$info[info_rate$kind == k & info_rate$bin == cut(th, theta_bins)]
  if (!length(i)) 0 else i
}
max_match <- ev |> filter(kind == "match") |> count(run_id) |> pull(n) |> quantile(.99)
max_ss <- 10

# ---- per-run structure --------------------------------------------------------
build_run <- function(r) {
  e <- ev |> filter(run_id == r$run_id)
  y <- r$young
  ended_by_clock <- r$stop_reason %in% "timeOut" | (is.na(r$stop_reason) & r$task_end >= 350)
  last_kind <- if (nrow(e)) e$kind[nrow(e)] else NA
  seq_of <- \(k) e |> filter(kind == k) |> select(dur, info)
  extend <- \(s, k, cap) {
    # extend only if the clock (not completion or error-out) ended this block
    cut <- ended_by_clock & (identical(last_kind, k) | (nrow(s) == 0))
    if (!cut || nrow(s) >= cap) return(s)
    add <- cap - nrow(s)
    if (nrow(s) >= 3) {
      t6 <- tail(s, 6); bind_rows(s, tibble(dur = rep(mean(t6$dur), add), info = rep(mean(t6$info), add)))
    } else {
      bind_rows(s, tibble(dur = rep(gi(y, k), add), info = rep(exp_info(k, r$theta), add)))
    }
  }
  ss <- if (y) extend(seq_of("ss"), "ss", max_ss) else tibble(dur = numeric(), info = numeric())
  list(
    young = y, theta = r$theta,
    intro = coalesce(r$intro, gm(y, "dims") * 0 + 20),
    dims = seq_of("dims"),
    pm = { s <- sum(e$dur[e$kind == "pm"]); if (s > 0) s else gm(y, "pm") },
    pss = { s <- sum(e$dur[e$kind == "pss"]); if (s > 0) s else gm(y, "pss") },
    ss = ss,
    ig = { s <- sum(e$dur[e$kind == "ig"]); if (s > 0) s else coalesce(gm(y, "ig"), 6) },
    match = extend(seq_of("match"), "match", max_match))
}

# ---- policy engine ------------------------------------------------------------
# policy fields: T (s), move_pm (two-match practice runs in the match block),
# spread (timed-block budget = remaining time / remaining blocks), skip_pss_older,
# pause_practice (practice does not run the clock), dims_cap (max dims items)
simulate <- function(run, pol) {
  T <- pol$T; nb <- if (run$young && !pol$skip_ss_young) 3 else 2
  clock <- 0; info <- 0; ended <- FALSE
  ss_n <- 0; match_n <- 0
  practice <- function(d) {
    if (ended) return()
    if (pol$pause_practice) return()
    clock <<- clock + d; if (clock >= T) ended <<- TRUE
  }
  instr <- function(d) { if (ended) return(); clock <<- clock + d; if (clock >= T) ended <<- TRUE }
  cat_trials <- function(s, deadline, counter = NULL) {
    for (i in seq_len(nrow(s))) {
      if (ended || clock >= deadline) break
      clock <<- clock + s$dur[i]; info <<- info + s$info[i]
      if (!is.null(counter)) assign(counter, get(counter, envir = parent.frame(2)) + 1, envir = parent.frame(2))
      if (clock >= T) { ended <<- TRUE; break }
    }
  }
  budget <- \(k, start) if (pol$spread) (T - start) / (nb - k) else T / nb
  # block 0
  s0 <- clock; L0 <- budget(0, s0)
  instr(run$intro)
  cat_trials(head(run$dims, pol$dims_cap), s0 + L0)
  # block 1
  s1 <- clock; L1 <- if (run$young) budget(1, s1) else Inf
  if (!pol$move_pm) practice(run$pm)
  ss_on <- run$young && !pol$skip_ss_young
  if (ss_on || (!run$young && !pol$skip_pss_older)) practice(run$pss)
  if (ss_on) { before <- info; cat_trials(run$ss, s1 + L1); ss_info <- info - before } else ss_info <- 0
  # block 2 (last; whole-task timer)
  if (pol$move_pm) practice(run$pm)
  instr(run$ig)
  before <- info; cat_trials(run$match, T); match_info <- info - before
  tibble(info = info, ss_info = ss_info, match_info = match_info, minutes = min(clock, T) / 60,
         active_minutes = (min(clock, T) + if (pol$pause_practice) run$pm + run$pss else 0) / 60)
}

policies <- tribble(
  ~policy, ~T, ~move_pm, ~spread, ~skip_pss_older, ~pause_practice, ~dims_cap,
  "current (6 min)",                          360, FALSE, FALSE, FALSE, FALSE, 19,
  "A: two-match practice moved to match block", 360, TRUE,  FALSE, FALSE, FALSE, 19,
  "B: spread leftover time over later blocks", 360, FALSE, TRUE,  FALSE, FALSE, 19,
  "A + B",                                    360, TRUE,  TRUE,  FALSE, FALSE, 19,
  "C: no something-same practice for 5+",     360, FALSE, FALSE, TRUE,  FALSE, 19,
  "A + B + C",                                360, TRUE,  TRUE,  TRUE,  FALSE, 19,
  "D: clock paused during practice",          360, FALSE, FALSE, FALSE, TRUE,  19,
  "E: max time 7 min",                        420, FALSE, FALSE, FALSE, FALSE, 19,
  "E: max time 8 min",                        480, FALSE, FALSE, FALSE, FALSE, 19,
  "A + B + C, max time 7 min",                420, TRUE,  TRUE,  TRUE,  FALSE, 19,
  "F (contrast): only 10 test-dimensions items", 360, FALSE, FALSE, FALSE, FALSE, 10,
  "G: no something-same block for under-5s either", 360, FALSE, FALSE, TRUE, FALSE, 19) |>
  mutate(skip_ss_young = policy == "G: no something-same block for under-5s either")

runs <- map(seq_len(nrow(base)), \(i) build_run(base[i, ]))
res <- map_dfr(seq_len(nrow(policies)), \(p) {
  pol <- as.list(policies[p, ])
  map_dfr(seq_along(runs), \(i) simulate(runs[[i]], pol) |>
            mutate(run_id = base$run_id[i], young = base$young[i], age = base$age[i],
                   theta = base$theta[i], prior_sd = base$prior_sd[i], policy = pol$policy))
})

# replay check: under the current policy, block-timed runs should reproduce
# their observed information (no extension involved for blocks they finished)
obs_info <- sds_trials |> filter(!practice) |> group_by(run_id) |> summarise(obs_info = sum(info), .groups = "drop")
check <- res |> filter(policy == "current (6 min)") |>
  inner_join(base |> select(run_id, block_timer), by = "run_id") |>
  filter(block_timer) |> inner_join(obs_info, by = "run_id")

saveRDS(list(res = res, policies = policies, check = check, base = base, mix = mix,
             max_match = max_match),
        file.path(sds_dir, "sim_results.rds"))
message("simulated ", length(runs), " runs x ", nrow(policies), " policies")
