# SDS scoring: recodes, audits, and all model fits for studies/2026-07_sds-scoring/same_different.qmd
# ------------------------------------------------------------------
# Written by Claude Code (Anthropic), directed by Mike Frank, July 2026.
# Ported from levante-pilots/03_explore_tasks/blockCAT/sds/model_tree_mirt/
# (_fit_sds_report_models.R, _fit_sds_extensions.R, _audit_recode.R,
# _replicate_hackathon.R), re-based on the model calibration dataset
# (levante-pilots/01_fetched_data/task_data_nested.rds, same source used by
# studies/2026-07_mrot-item-sets/).
#
# Everything caches to data/sds_scoring/ — delete that dir to recompute.
# Runtime from scratch: ~15-20 min (the 3-factor tree and the multigroup
# fits dominate).
#
# Two independent recode implementations are run and cross-validated:
#   A. a direct port of the levante-pilots `_recode_sds.R` logic (JSON parsing
#      + the levantemodels helper semantics) — the code slated for production;
#   B. the independent audit implementation (regex tokens, direct attribute
#      tables) from `_audit_recode.R`.
# Models are fit on A's output.

suppressPackageStartupMessages({
  library(tidyverse)
  library(glue)
  library(mirt)
  library(jsonlite)
})

data_dir <- here::here("data")
cache_dir <- file.path(data_dir, "sds_scoring")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

pilots_data_dir <- here::here("..", "levante-pilots", "01_fetched_data")
raw <- read_rds(file.path(pilots_data_dir, "task_data_nested.rds")) |>
  filter(item_task == "sds") |>
  unnest(data)

# ============================================================================
# Recode A: port of _recode_sds.R (levantemodels semantics)
# ============================================================================
parse_response <- function(resp) {
  if (is.na(resp) || str_trim(resp) == "") return(character(0))
  fromJSON(str_replace_all(resp, "'", '"'), simplifyVector = TRUE) |>
    as.character() |> unname()
}
code_stim <- function(stim_parts) {
  num <- str_extract(stim_parts, "\\d") |> discard(is.na)
  if (length(num) == 0) num <- "1"
  bg <- str_extract(stim_parts, "gray|black|striped") |> discard(is.na)
  if (length(bg) == 0) bg <- "white"
  set_names(c(stim_parts[1:3], num, bg),
            c("size", "color", "shape", "number", "background"))
}
code_dims <- function(stims) {
  stims |> str_split("-") |> map(code_stim)
}
match_opts_dims <- function(opts) {
  opts |> transpose() |> map(unlist) |> map(table) |>
    discard(\(x) length(x) == 1) |> map(\(x) sum(choose(x, 2))) |> unlist()
}
match_resp_dims <- function(resp, opts_dims) {
  resp_t <- resp |> transpose() |> map(unlist)
  resp_t[names(opts_dims)] |> map(n_distinct) |>
    keep(\(x) x < length(resp)) |> names() |> sort()
}

coded_file <- file.path(cache_dir, "coded.rds")
if (file.exists(coded_file)) coded <- read_rds(coded_file) else {
  sds_data <- raw |>
    filter(str_detect(item_group, "match")) |>
    filter(!str_detect(response, "mittel|rote|gelb|blau|grün")) |>
    filter(!(dataset == "pilot_western_ca_main" & timestamp < "2025-02-21"))

  sds_indexed <- sds_data |>
    mutate(different = str_detect(item_original, "different")) |>
    group_by(run_id, item_group) |>
    arrange(timestamp, .by_group = TRUE) |>
    mutate(trial_index = cumsum(item == "choice1"),
           trial_index_s = cumsum(!different)) |>
    ungroup()

  sds_match <- sds_indexed |>
    filter(trial_index != 0) |>
    group_by(run_id, item_group) |>
    filter(all(trial_index == trial_index_s)) |>
    mutate(match_k = as.numeric(str_extract(item_group, "^."))) |>
    group_by(run_id, item_group, trial_index) |>
    filter(!any(response == "{}"),
           !any(str_count(response, ":") != 2),
           n() == unique(match_k),
           n_distinct(distractors) == 1) |>
    mutate(choice_i = 1:n()) |>
    ungroup() |>
    select(dataset, run_id, trial_index, item_group, match_k, choice_i,
           trial_id, resp = response, opts = distractors)

  sds_dims <- sds_match |>
    mutate(resp_parsed = map(resp, parse_response) |> map(sort),
           opts_parsed = map(opts, parse_response) |> map(sort)) |>
    mutate(resp_coded = map(resp_parsed, \(r) code_dims(r)),
           opts_coded = map(opts_parsed, \(o) code_dims(o))) |>
    mutate(opts_dims = map(opts_coded, match_opts_dims),
           resp_dims = map2(resp_coded, opts_dims, match_resp_dims),
           n_matches = map_int(opts_dims, sum),
           sig = map_chr(opts_dims, \(od) paste(sort(names(od)[od > 0]), collapse = "+")),
           resp_norm = map_chr(resp_parsed, paste, collapse = " "))

  coded <- sds_dims |>
    mutate(subtrial_match = map_int(resp_dims, length) > 0) |>
    group_by(run_id, item_group, trial_index) |>
    mutate(new = map_lgl(row_number(),
                         \(i) i == 1 || !(resp_norm[i] %in% resp_norm[1:(i - 1)])),
           correct = subtrial_match & new) |>
    ungroup() |>
    filter(match_k == n_matches) |>
    group_by(run_id, item_group, trial_index) |>
    mutate(prev_m = lag(cumsum(subtrial_match & new), default = 0),
           prev_n = lag(cumsum(!subtrial_match & new), default = 0)) |>
    ungroup() |>
    mutate(match = subtrial_match,
           n_pairs = match_k * (match_k + 1) / 2,
           chance_match = match_k / n_pairs,
           chance_newM = 1 - prev_m / match_k,
           chance_newN = 1 - prev_n / (n_pairs - match_k),
           chance_correct = (match_k - prev_m) / n_pairs,
           item_choice = paste0("choice", choice_i),
           item_status = paste0(prev_m, "m", prev_n, "n")) |>
    select(dataset, run_id, trial_index, item_group, match_k, choice_i,
           item_choice, item_status, sig, match, new, correct,
           starts_with("chance"), prev_m, prev_n, n_pairs)
  write_rds(coded, coded_file, compress = "gz")
}

# ============================================================================
# Recode B: independent audit implementation; cross-validate against A
# ============================================================================
audit_file <- file.path(cache_dir, "audit.rds")
if (!file.exists(audit_file)) {
  extract_tokens <- \(s) str_match_all(s, "'[0-9]+': '([^']*)'")[[1]][, 2]
  decode_stim <- function(stim) {
    parts <- str_split_1(stim, "-")
    num <- parts[str_detect(parts, "^[0-9]$")]
    tex <- parts[parts %in% c("gray", "black", "striped")]
    tibble(size = parts[1], color = parts[2], shape = parts[3],
           number = if (length(num)) num else "1",
           texture = if (length(tex)) tex else "white")
  }
  audit_trial <- function(resps, opts_str) {
    opts <- sort(extract_tokens(opts_str))
    opts_attrs <- map(opts, decode_stim) |> list_rbind()
    varying <- names(opts_attrs)[map_lgl(opts_attrs, \(v) n_distinct(v) > 1)]
    pair_norm <- map_chr(resps, \(r) paste(sort(extract_tokens(r)), collapse = "|"))
    match_i <- map_lgl(resps, \(r) {
      cards <- sort(extract_tokens(r))
      if (length(cards) != 2) return(NA)
      a <- decode_stim(cards[1]); b <- decode_stim(cards[2])
      any(map_lgl(varying, \(d) a[[d]] == b[[d]]))
    })
    new_i <- map_lgl(seq_along(pair_norm),
                     \(i) i == 1 || !(pair_norm[i] %in% pair_norm[1:(i - 1)]))
    n_match_pairs <- sum(map_dbl(varying, \(d) sum(choose(table(opts_attrs[[d]]), 2))))
    tibble(choice_i = seq_along(resps), match_b = match_i, new_b = new_i,
           n_matches_b = n_match_pairs)
  }

  # grammar check over every stimulus token
  stim_regex <- "^(sm|med|lg)-(yellow|blue|red|green)-(triangle|circle|square|star)(-[0-9])?(-(gray|black|striped))?$"
  match_raw <- raw |> filter(str_detect(item_group, "match"))
  all_tokens <- c(unlist(map(match_raw$response, \(r) if (str_count(r, ":") >= 1) extract_tokens(r) else character(0))),
                  unlist(map(match_raw$distractors, extract_tokens)))
  bad_tokens <- unique(all_tokens[!str_detect(all_tokens, stim_regex)])

  # NB: index trials BEFORE dropping invalid responses (see levante-pilots
  # _audit_recode.R -- filtering rows first shifts trial indices)
  audited <- match_raw |>
    group_by(run_id, item_group) |> arrange(timestamp, .by_group = TRUE) |>
    mutate(trial_index = cumsum(item == "choice1")) |> ungroup() |>
    filter(trial_index > 0) |>
    group_by(run_id, item_group, trial_index) |>
    filter(!any(response == "{}"), !any(str_count(response, ":") != 2)) |>
    filter(n_distinct(distractors) == 1) |>
    arrange(timestamp, .by_group = TRUE) |>
    summarise(audit = list(audit_trial(response, distractors[1])), .groups = "drop") |>
    unnest(audit) |>
    group_by(run_id, item_group, trial_index) |>
    mutate(prev_m_b = lag(cumsum(match_b & new_b), default = 0),
           prev_n_b = lag(cumsum(!match_b & new_b), default = 0)) |>
    ungroup()

  cmp <- coded |>
    inner_join(audited, by = c("run_id", "item_group", "trial_index", "choice_i"))
  agreement <- cmp |>
    summarise(n = n(),
              match_agree = mean(match == match_b),
              new_agree = mean(new == new_b),
              correct_agree = mean(correct == (match_b & new_b)),
              status_m_agree = mean(prev_m == prev_m_b),
              status_n_agree = mean(prev_n == prev_n_b),
              n_matches_agree = mean(match_k == n_matches_b),
              coverage = n() / nrow(coded))
  write_rds(list(n_tokens = length(all_tokens), bad_tokens = bad_tokens,
                 agreement = agreement),
            audit_file, compress = "gz")
}

# ============================================================================
# Monte Carlo validation of the conditional chance formulas
# ============================================================================
mc_file <- file.path(cache_dir, "mc.rds")
if (!file.exists(mc_file)) {
  mc_chance <- function(k, n_sim) {
    n_pairs <- choose(k + 1, 2)
    is_match <- c(rep(TRUE, k), rep(FALSE, n_pairs - k))
    map(1:n_sim, \(s) {
      sel <- sample(n_pairs, k, replace = TRUE)
      prev <- integer(0)
      map(seq_along(sel), \(i) {
        p <- sel[i]
        prev_uniq <- unique(prev)
        out <- tibble(match = is_match[p], new = !(p %in% prev),
                      prev_m = sum(is_match[prev_uniq]),
                      prev_n = sum(!is_match[prev_uniq]))
        prev <<- c(prev, p)
        out
      }) |> list_rbind()
    }) |> list_rbind() |>
      mutate(status = paste0(prev_m, "m", prev_n, "n")) |>
      group_by(status, prev_m, prev_n) |>
      summarise(p_match_mc = mean(match),
                p_newM_mc = mean(new[match]),
                p_newN_mc = mean(new[!match]), n_mc = n(), .groups = "drop") |>
      mutate(match_k = k, n_pairs = n_pairs,
             p_match_formula = k / n_pairs,
             p_newM_formula = 1 - prev_m / k,
             p_newN_formula = 1 - prev_n / (n_pairs - k))
  }
  set.seed(1)
  write_rds(map(2:4, \(k) mc_chance(k, 30000)) |> list_rbind(),
            mc_file, compress = "gz")
}

# ============================================================================
# Model datasets
# ============================================================================
corpus_chance <- tribble(
  ~item_uid, ~chance_corpus,
  "sds_2match_choice1", 0.5, "sds_2match_choice2", 0.75,
  "sds_3match_choice1", 0, "sds_3match_choice2", 0, "sds_3match_choice3", 0,
  "sds_4match_choice1", 0, "sds_4match_choice2", 0, "sds_4match_choice3", 0,
  "sds_4match_choice4", 0)

sitefy <- \(d) d |> mutate(site = dataset |> str_remove("_bogota$|_rural$|_main$"))

data_m0 <- coded |> sitefy() |>
  mutate(item_uid = glue("sds_{item_group}_{item_choice}") |> as.character()) |>
  left_join(corpus_chance, by = "item_uid") |>
  mutate(chance = chance_corpus) |>
  select(site, run_id, item_uid, correct, chance)
data_m0c <- data_m0 |>
  select(-chance) |>
  left_join(coded |>
              mutate(item_uid = glue("sds_{item_group}_{item_choice}") |> as.character()) |>
              summarise(chance = mean(chance_correct), .by = item_uid),
            by = "item_uid")
data_m0z <- data_m0 |> mutate(chance = 0)
data_m1 <- coded |> sitefy() |>
  mutate(item_uid = glue("sds_{item_group}_{item_choice}_{item_status}") |> as.character(),
         chance = chance_correct) |>
  select(site, run_id, item_uid, correct, chance)
data_m1z <- data_m1 |> mutate(chance = 0)
data_m1z_sig <- coded |> sitefy() |>
  mutate(item_uid = glue("sds_{item_group}_{item_choice}_{item_status}_{sig}") |> as.character(),
         chance = 0) |>
  select(site, run_id, item_uid, correct, chance)
data_m1z_no2 <- data_m1z |> filter(!str_detect(item_uid, "2match"))

data_gate <- raw |>
  filter(item_group %in% c("same", "dimensions"), run_id %in% unique(coded$run_id)) |>
  mutate(chance = replace_na(chance, 0)) |> sitefy() |>
  select(site, run_id, item_uid, correct, chance) |>
  filter(!is.na(correct))
data_m1z_gate <- bind_rows(data_m1z, data_gate)

# tree long data (match + conditional new nodes; deterministic nodes dropped)
tree <- coded |> sitefy() |>
  mutate(value_match = match,
         value_newM = if_else(match, new, NA),
         value_newN = if_else(!match, new, NA)) |>
  select(site, run_id, item_group, item_status, trial_index,
         starts_with("chance_"), starts_with("value_")) |>
  pivot_longer(cols = c(starts_with("chance_"), starts_with("value_")),
               names_to = c(".value", "node"), names_sep = "_") |>
  filter(!is.na(value), node != "correct", chance != 1, chance != 0) |>
  mutate(item = glue("{item_group}_{item_status}") |> as.character())
write_rds(tree, file.path(cache_dir, "tree.rds"), compress = "gz")

data_t1 <- tree |>
  mutate(item_uid = paste(node, item, sep = "."), correct = value) |>
  select(site, run_id, node, item_uid, correct, chance)
data_t1z <- data_t1 |> mutate(chance = 0)
data_t2z <- data_t1z |>
  mutate(item_uid = paste0(if_else(node == "match", "match", "new"), ".",
                           str_remove(item_uid, "^[A-Za-z]+\\."),
                           if_else(node == "newN", "_nn", "")),
         node = if_else(node == "match", "match", "new"))

# ============================================================================
# Fitting machinery
# ============================================================================
dedupe_items <- function(df, item_sep = "-") {
  df |> group_by(run_id, item_uid) |>
    mutate(instance = seq_along(item_uid)) |> ungroup() |>
    mutate(item_inst = as.character(glue("{item_uid}{item_sep}{instance}")))
}
remove_no_var_items <- function(df) {
  df |> group_by(item_inst) |> filter(n_distinct(correct) > 1) |> ungroup()
}
to_mirt_shape <- function(df) {
  df |> mutate(correct = as.numeric(correct)) |>
    select(run_id, item_inst, correct) |>
    pivot_wider(names_from = item_inst, values_from = correct) |>
    column_to_rownames("run_id")
}
paste_c <- \(...) paste(..., collapse = ",")
gen_model_str <- function(df, df_prepped, item_type, priors = NULL,
                          by_node = FALSE, free_cov = FALSE) {
  items <- df |> pull(item_uid) |> unique()
  if (by_node) {
    nodes <- df |> pull(node) |> unique()
    factors <- nodes |> set_names() |> map(\(nd) {
      idx <- which(str_detect(colnames(df_prepped), glue("^{nd}\\.")))
      paste(idx, collapse = ",")
    })
  } else factors <- list(glue("1-{ncol(df_prepped)}")) |> set_names("F1")
  slopes <- paste0("a", 1:length(factors)) |> set_names(names(factors))
  item_params <- items |> set_names() |> map(\(iu) {
    params <- "d"
    if (item_type != "Rasch") {
      if (by_node) {
        nd <- str_extract(iu, "^[A-Za-z]+(?=\\.)")
        params <- c(params, slopes[[nd]])
      } else params <- c(params, slopes[["F1"]])
    }
    params
  })
  constraints <- items |> map(\(iu) {
    prefixes <- str_sub(colnames(df_prepped), 1, str_length(iu) + 1)
    matched_idx <- which(prefixes == paste0(iu, "-"))
    if (length(matched_idx) > 1)
      map_chr(item_params[[iu]], \(p) glue("({paste_c(matched_idx)},{p})")) |> paste_c()
  }) |> compact() |> paste_c()
  constraint <- if (str_length(constraints) > 1) paste0("CONSTRAIN=", constraints) else ""
  prior_terms <- priors |>
    imap(\(pr, param) glue("(1-{ncol(df_prepped)},{paste(c(param, pr), collapse = ',')})"))
  prior <- if (length(prior_terms) > 0) glue("PRIOR={paste(prior_terms, collapse = ',')}") else ""
  cov_str <- if (free_cov && length(factors) > 1) {
    prs <- combn(names(factors), 2)
    paste0("COV = ", paste(map_chr(1:ncol(prs), \(j) paste(prs[, j], collapse = "*")),
                           collapse = ", "))
  } else ""
  factor_str <- imap_chr(factors, \(idx, nm) glue("{nm} = {idx}")) |> unname()
  paste(c(factor_str, constraint, cov_str, prior), collapse = "\n")
}

priors_rasch  <- list(d = c("norm", 0, 3))
priors_2pl_1f <- list(d = c("norm", 0, 3), a1 = c("norm", 1, 0.3))
priors_2pl_2f <- list(d = c("norm", 0, 3), a1 = c("norm", 1, 0.3), a2 = c("norm", 1, 0.3))

extract_row <- function(mod, name, wide_rownames, invariance = NA, itemtype = NA) {
  nf <- extract.mirt(mod, "nfact")
  fs <- fscores(mod, method = "EAP", full.scores.SE = TRUE, verbose = FALSE)
  # mirt >= 1.46 classes fscores output as mirt_matrix, which survives
  # as_tibble() as matrix columns and breaks vctrs rbinds -- strip it
  fs <- matrix(as.numeric(fs), nrow = nrow(fs), dimnames = dimnames(fs))
  sc <- as_tibble(fs) |> mutate(run_id = wide_rownames, .before = 1)
  fnames <- colnames(fs)[1:nf]
  emp <- map_dbl(fnames, \(fn) {
    th <- fs[, fn]; se <- fs[, paste0("SE_", fn)]
    var(th) / (var(th) + mean(se^2))
  })
  mrxx <- if (nf == 1) tryCatch(marginal_rxx(mod), error = \(e) NA) else NA
  tibble(model = name, factor = fnames, nfact = nf,
         invariance = invariance, itemtype = itemtype,
         converged = extract.mirt(mod, "converged"),
         AIC = extract.mirt(mod, "AIC"), BIC = extract.mirt(mod, "BIC"),
         npar = extract.mirt(mod, "nest"), logLik = extract.mirt(mod, "logLik"),
         emp_rxx = emp, marg_rxx = mrxx) |>
    mutate(scores = list(sc))
}

fit_uni <- function(dat, itemtype, priors, name, by_node = FALSE, free_cov = FALSE) {
  f <- file.path(cache_dir, paste0("fit_", name, ".rds"))
  df_f <- dat |> dedupe_items() |> remove_no_var_items()
  df_w <- to_mirt_shape(df_f)
  guess_map <- df_f |> distinct(item_inst, chance) |> deframe()
  guess <- unname(guess_map[colnames(df_w)])
  if (file.exists(f)) mod <- read_rds(f) else {
    message("fitting ", name, " ...")
    ms <- gen_model_str(df_f, df_w, itemtype, priors, by_node, free_cov)
    mod <- mirt(data = df_w, itemtype = itemtype, model = mirt.model(ms),
                guess = guess, SE = FALSE, verbose = FALSE,
                technical = list(NCYCLES = 5000))
    write_rds(mod, f, compress = "gz")
  }
  extract_row(mod, name, rownames(df_w), itemtype = itemtype)
}

invariances <- list(configural = "", metric = c("slopes"),
                    scalar = c("free_means", "free_var", "intercepts", "slopes"))
fit_mg <- function(dat, itemtype, priors, name, invariance) {
  f <- file.path(cache_dir, paste0("fit_", name, ".rds"))
  df_f <- dat |> dedupe_items() |>
    group_by(item_inst) |> filter(n_distinct(correct) > 1) |>
    group_by(item_inst, site) |> mutate(ncat_g = n_distinct(correct)) |>
    group_by(item_inst) |> filter(n_distinct(ncat_g) == 1, all(ncat_g > 1)) |> ungroup()
  df_w <- df_f |> mutate(correct = as.numeric(correct)) |>
    select(run_id, site, item_inst, correct) |>
    pivot_wider(names_from = item_inst, values_from = correct) |>
    column_to_rownames("run_id")
  groups <- df_w$site; df_w2 <- df_w |> select(-site)
  guess_map <- df_f |> distinct(item_inst, chance) |> deframe()
  guess <- unname(guess_map[colnames(df_w2)])
  if (file.exists(f)) mod <- read_rds(f) else {
    message("fitting ", name, " ...")
    ms <- gen_model_str(df_f, df_w2, itemtype, priors, by_node = FALSE)
    mod <- multipleGroup(data = df_w2, itemtype = itemtype, model = mirt.model(ms),
                         group = groups, invariance = invariances[[invariance]],
                         guess = guess, verbose = FALSE,
                         technical = list(NCYCLES = 5000))
    write_rds(mod, f, compress = "gz")
  }
  extract_row(mod, name, rownames(df_w2), invariance = invariance, itemtype = itemtype)
}

# ============================================================================
# Fits
# ============================================================================
results_file <- file.path(cache_dir, "results.rds")
if (!file.exists(results_file)) {
  results <- bind_rows(
    # selection universe, pooled
    fit_uni(data_m0,      "Rasch", priors_rasch,  "m0_rasch"),
    fit_uni(data_m0,      "2PL",   priors_2pl_1f, "m0_2pl"),
    fit_uni(data_m0c,     "Rasch", priors_rasch,  "m0c_rasch"),
    fit_uni(data_m0z,     "Rasch", priors_rasch,  "m0z_rasch"),
    fit_uni(data_m1,      "Rasch", priors_rasch,  "m1_rasch"),
    fit_uni(data_m1,      "2PL",   priors_2pl_1f, "m1_2pl"),
    fit_uni(data_m1z,     "Rasch", priors_rasch,  "m1z_rasch"),
    fit_uni(data_m1z,     "2PL",   priors_2pl_1f, "m1z_2pl"),
    fit_uni(data_m1z_sig, "Rasch", priors_rasch,  "m1z_sig_rasch"),
    fit_uni(data_m1z_no2, "Rasch", priors_rasch,  "m1z_no2_rasch"),
    fit_uni(data_m1z_gate, "Rasch", priors_rasch, "m1z_gate_rasch"),
    # tree universe, pooled
    fit_uni(data_t1,  "Rasch", priors_rasch,  "t1_rasch"),
    fit_uni(data_t1z, "Rasch", priors_rasch,  "t1z_rasch"),
    fit_uni(data_t1z, "2PL",   priors_2pl_1f, "t1z_2pl"),
    fit_uni(data_t2z, "Rasch", priors_rasch,  "t2zc_rasch", by_node = TRUE, free_cov = TRUE),
    fit_uni(data_t1z, "Rasch", priors_rasch,  "t3z_rasch",  by_node = TRUE, free_cov = TRUE),
    # multigroup by site
    fit_mg(data_m0z, "Rasch", priors_rasch,  "mg_m0z_rasch_scalar", "scalar"),
    fit_mg(data_m1z, "Rasch", priors_rasch,  "mg_m1z_rasch_config", "configural"),
    fit_mg(data_m1z, "Rasch", priors_rasch,  "mg_m1z_rasch_scalar", "scalar"),
    fit_mg(data_m1z, "2PL",   priors_2pl_1f, "mg_m1z_2pl_config",   "configural"),
    fit_mg(data_m1z, "2PL",   priors_2pl_1f, "mg_m1z_2pl_metric",   "metric"),
    fit_mg(data_m1z, "2PL",   priors_2pl_1f, "mg_m1z_2pl_scalar",   "scalar"),
    fit_mg(data_t1z, "Rasch", priors_rasch,  "mg_t1z_rasch_scalar", "scalar"),
    fit_mg(data_m1z_gate, "Rasch", priors_rasch, "mg_m1z_gate_rasch_config", "configural"),
    fit_mg(data_m1z_gate, "Rasch", priors_rasch, "mg_m1z_gate_rasch_scalar", "scalar"))
  write_rds(results, results_file, compress = "gz")
}
results <- read_rds(results_file)

# factor correlations for the correlated trees
fc_file <- file.path(cache_dir, "factor_cors.rds")
if (!file.exists(fc_file)) {
  fc <- map(c("t2zc_rasch", "t3z_rasch"), \(nm) {
    mod <- read_rds(file.path(cache_dir, paste0("fit_", nm, ".rds")))
    tibble(model = nm, cor = list(cov2cor(coef(mod, simplify = TRUE)$cov)))
  }) |> list_rbind()
  write_rds(fc, fc_file, compress = "gz")
}

# ============================================================================
# Production-recode bug: order-variant repeats + score impact
# ============================================================================
bug_file <- file.path(cache_dir, "bug.rds")
if (!file.exists(bug_file)) {
  cmp <- raw |>
    filter(str_detect(item_group, "match")) |>
    group_by(run_id, item_group) |> arrange(timestamp, .by_group = TRUE) |>
    mutate(trial_index = cumsum(item == "choice1")) |> ungroup() |>
    filter(trial_index > 0) |>
    group_by(run_id, item_group, trial_index) |>
    filter(!any(response == "{}"), !any(str_count(response, ":") != 2)) |>
    mutate(choice_i = row_number(),
           resp_norm = map_chr(response, \(r) paste(sort(parse_response(r)), collapse = " ")),
           new_raw  = map_lgl(row_number(), \(i) i == 1 || !(response[i] %in% response[1:(i - 1)])),
           new_sort = map_lgl(row_number(), \(i) i == 1 || !(resp_norm[i] %in% resp_norm[1:(i - 1)]))) |>
    ungroup() |>
    inner_join(coded |> select(run_id, item_group, trial_index, choice_i, match, new),
               by = c("run_id", "item_group", "trial_index", "choice_i"))

  data_m0bug <- cmp |>
    mutate(correct = match & new_raw,
           item_uid = glue("sds_{item_group}_choice{choice_i}") |> as.character(),
           chance = 0) |>
    select(run_id, item_uid, correct, chance)
  row_bug <- fit_uni(data_m0bug, "Rasch", priors_rasch, "m0bug_rasch")
  bug_scores <- row_bug$scores[[1]] |> select(run_id, theta_bug = F1) |>
    inner_join(results |> filter(model == "m0z_rasch") |> pull(scores) |> pluck(1) |>
                 select(run_id, theta_fix = F1), by = "run_id")
  write_rds(list(cmp = cmp |> select(run_id, dataset, item_group, trial_index,
                                     choice_i, match, new_raw, new_sort),
                 bug_scores = bug_scores),
            bug_file, compress = "gz")
}

message("done: all SDS scoring artifacts cached in ", cache_dir)
