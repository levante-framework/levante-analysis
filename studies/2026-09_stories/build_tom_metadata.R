# Build consolidated ToM item metadata table.
# Sources:
#  (1) item-level sheet mapping (data/tom_meta/item_level_sheet.tsv) — from the
#      shared Google Sheet: item_uid -> story, version, old_group, entry, prompt,
#      and which corpus forms it appears in.
#  (2) story-level new-construct classification + exclusion flags (Sheet table 2).
#  (3) the deployed trial data (which items actually appear, n, accuracy per site).
suppressMessages({library(tidyverse); library(here)})
source(here("common.R"))

# ---- (1) item-level sheet --------------------------------------------------
sheet <- read_tsv(here("data/tom_meta/item_level_sheet.tsv"), show_col_types = FALSE)

# corpora each item_uid appears in (collapse the CO_/non-CO duplicates)
corpus_membership <- sheet |>
  mutate(corpus = corpus_id |> str_remove("^CO_") |>
           recode("theory-of-mind-v2" = "v2",
                  "theory-of-mind_retest-A" = "retestA",
                  "theory-of-mind_retest-B" = "retestB")) |>
  distinct(item_uid, corpus) |>
  group_by(item_uid) |>
  summarise(corpora = paste(sort(unique(corpus)), collapse = ","), .groups = "drop")

item_sheet <- sheet |>
  mutate(tom_scenario = as.character(tom_scenario)) |>
  distinct(item_uid, tom_scenario, version, old_group, entry, prompt) |>
  left_join(corpus_membership, by = "item_uid")

# ---- (2) story-level new construct + exclusion -----------------------------
story_lookup <- tribble(
  ~tom_scenario, ~old_group,        ~new_construct,                              ~sheet_excluded,
  "1",  "reality_known",   "False Belief",                              "N",
  "2",  "moral_reasoning", "Diverse belief plus reality known",         "N",
  "3",  "interpretation",  "Interpretation",                            "N",
  "4",  "deception",       "Fairness / belief + moral judgment",        "Y",
  "5",  "reference",       "VPT",                                       "Y",
  "6",  "second_order",    "Diverse belief plus reality known",         "N",
  "7",  "reality_known",   "False Belief",                              "N",
  "8",  "moral_reasoning", "Diverse belief plus reality known",         "N",
  "9",  "interpretation",  "Interpretation",                            "N",
  "10", "deception",       "False belief plus moral/emotional judgment","N",
  "11", "reference",       "VPT",                                       "N",
  "12", "second_order",    "Diverse belief plus reality known",         "N",
  "13", "reality_known",   "False Belief",                              "N",
  "14", "moral_reasoning", "Diverse belief plus reality known",         "N",
  "15", "interpretation",  "Interpretation",                            "N",
  "16", "deception",       "False belief plus moral/emotional judgment","N",
  "17", "reference",       "VPT",                                       "N",
  "18", "second_order",    "Diverse belief plus reality known",         "N"
)

# corpus a story was *designed* into (from bankID): the "home" form
story_home <- tibble(
  tom_scenario = as.character(1:18),
  home_corpus = c("v2","retestA","v2","v2","v2","v2",
                  "retestA","retestA","retestA","retestA","retestA","retestA",
                  "retestB","retestB","retestB","retestB","retestB","retestB")
)

# ---- (3) deployed data ------------------------------------------------------
tr <- load_levante_trials(task_ids = "theory-of-mind")

deployed <- tr |>
  group_by(item_uid) |>
  summarise(
    in_data        = TRUE,
    n_resp         = n(),
    n_de           = sum(dataset == "pilot_mpieva_de_main"),
    n_co_bogota    = sum(dataset == "pilot_uniandes_co_bogota"),
    n_co_rural     = sum(dataset == "pilot_uniandes_co_rural"),
    n_western      = sum(dataset == "pilot_western_ca_main"),
    overall_acc    = mean(correct, na.rm = TRUE),
    chance         = first(chance),
    .groups = "drop"
  )

# data-side parse for items NOT in the sheet (e.g. ha_* hostile-attribution)
data_items <- tr |>
  distinct(item_uid, data_item_group = item_group, data_entry = item) |>
  mutate(
    is_hostile_attribution = str_starts(item_uid, "ha_"),
    data_story = str_extract(item_uid, "(?<=tom_story)[0-9]+")
  )

# ---- consolidate ------------------------------------------------------------
meta <- deployed |>
  left_join(data_items, by = "item_uid") |>
  left_join(item_sheet, by = "item_uid") |>
  # fall back to data-derived story/group when sheet has no row
  mutate(tom_scenario = coalesce(tom_scenario, data_story),
         old_group    = coalesce(old_group, data_item_group),
         entry        = coalesce(entry, data_entry)) |>
  left_join(story_lookup |> select(tom_scenario, new_construct, sheet_excluded),
            by = "tom_scenario") |>
  left_join(story_home, by = "tom_scenario") |>
  mutate(
    question_type = case_when(
      is_hostile_attribution ~ "hostile_attribution",
      str_detect(entry, "reality_check") ~ "reality_check (control)",
      str_detect(entry, "false_belief")  ~ "false_belief",
      str_detect(entry, "emotion")       ~ "emotion_reasoning",
      str_detect(entry, "reference")     ~ "reference",
      TRUE ~ entry
    ),
    is_control = str_detect(coalesce(entry, ""), "reality_check"),
    in_sheet   = !is.na(version)
  ) |>
  arrange(suppressWarnings(as.integer(tom_scenario)), item_uid) |>
  select(item_uid, tom_scenario, old_group, new_construct, question_type, entry,
         is_control, is_hostile_attribution, version, corpora, home_corpus,
         sheet_excluded, in_sheet, in_data, n_resp,
         n_de, n_co_bogota, n_co_rural, n_western, overall_acc, chance, prompt)

write_csv(meta, here("data/tom_meta/tom_item_metadata.csv"))
write_rds(meta, here("data/tom_meta/tom_item_metadata.rds"))

cat("=== ToM item metadata:", nrow(meta), "items ===\n")
cat("\nby new_construct (sheet items only):\n")
meta |> filter(in_sheet) |> count(new_construct) |> print()
cat("\nquestion types:\n"); meta |> count(question_type) |> print()
cat("\nhostile-attribution items (not in ToM sheet):\n")
meta |> filter(is_hostile_attribution) |> select(item_uid, overall_acc, n_resp) |> print()
cat("\nitems in data but NOT in sheet:\n")
meta |> filter(!in_sheet) |> count(is_hostile_attribution) |> print()
cat("\nstories flagged excluded in sheet:\n")
meta |> filter(sheet_excluded == "Y") |> distinct(tom_scenario, old_group, new_construct) |> print()
