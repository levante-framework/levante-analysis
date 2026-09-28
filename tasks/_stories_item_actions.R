# _stories_item_actions.R
#
# Stories (Theory of Mind), consolidated chapter: ONE TABLE OF RECOMMENDED ITEM ACTIONS.
# Every recommendation the chapter makes about individual story items, with its
# exact scope, the trials it touches, the evidence and the rationale:
#   exclude (everywhere or scoped), hold, correct (chance), un-exclude (remove a
#   generic Airtable row), review (not proposed for exclusion).
# The scope columns follow the Airtable schema proposed in levantemodels PR #28
# (item_uid, dataset, language, start/end date, exclude), plus the content
# version (item_original), which is how the chapter defines the scoped rows.
#
# Input : data/stories_2026-09/stories_trials.rds, results_items.rds,
#         results_monotonicity.rds (run _stories_fits_items.R and
#         _stories_fits_monotonicity.R first)
# Output: tasks/stories_item_actions.csv (tracked in git; the chapter reads it)
#
#   Rscript /path/to/levante-analysis/tasks/_stories_item_actions.R /path/to/levante-analysis

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(stringr); library(tibble); library(readr)
})

args <- commandArgs(trailingOnly = TRUE)
script_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
default_root <- if (length(script_file) == 1) dirname(dirname(normalizePath(script_file))) else here::here()
root <- if (length(args) >= 1) args[1] else Sys.getenv("STORIES_BOOK_ROOT", default_root)
dir26 <- file.path(root, "data/stories_2026-09")
out_file <- file.path(root, "tasks/stories_item_actions.csv")

tr   <- read_rds(file.path(dir26, "stories_trials.rds"))
ri   <- read_rds(file.path(dir26, "results_items.rds"))
mono <- read_rds(file.path(dir26, "results_monotonicity.rds"))

ref_why <- paste(
  "Asks what the listener will do; the key is the listener's misreading. Children answer with what",
  "the speaker meant (the reality-check answer), more so the higher they score and the older they are.",
  "Below or at chance in every language; negative item-rest r; reversed or flat in the monotonicity",
  "screen. A flipped key only duplicates the reality check: a design problem, so redesign rather than re-key.")
s4g_why <- paste(
  "On image 4g the 'yes' key conflicts with the scene (Mother, Isabel and Joshua smiling; the story",
  "never says Mother learns what Isabel did). Works on the earlier image 4d. With the key flipped to",
  "'no' the 4g item discriminates, but weakly; re-key only if the content team confirms 'no' for 4g.")
v0_why <- paste(
  "First Bogota content (2e, May-June 2024). Cause undetermined: an inverted key, a question identity",
  "swapped in the retroactive V0 map, or the content itself; no V0 corpus survives.")

# ---- the actions (scope blank = all) --------------------------------------------------------------
actions <- tribble(
  ~item_uid, ~action, ~dataset, ~language, ~content_version, ~date_start, ~date_end, ~rationale, ~status, ~source,
  # exclude everywhere
  "tom_story5_reference_reference", "exclude", "", "", "", "", "", ref_why,
    "proposed", "PR #28 record 1; chapter: The reference questions",
  "tom_story11_reference_reference", "exclude", "", "", "", "", "", ref_why,
    "proposed", "PR #28 record 1; chapter: The reference questions",
  "tom_story17_reference_reference", "exclude", "", "", "", "", "", ref_why,
    "proposed", "PR #28 record 1; chapter: The reference questions",
  "tom_story10_deception_false_belief_1", "exclude", "", "", "", "", "",
    paste("Does not discriminate: item-rest r CI spans 0 in every language; inconclusive in the monotonicity",
          "screen. The same question type works in stories 4 and 16."),
    "proposed", "PR #28 record 6; chapter: The exclusion list",
  "tom_story17_reference_emotion_reasoning_1", "exclude (or rewrite)", "", "", "", "", "",
    paste("'How do you think Mom feels about not finding the book she wanted?' (keyed sad). Flat in the",
          "monotonicity screen; reversed in German, where higher scorers answer 'surprised' (defensible for",
          "someone who expected the book to be there); elsewhere answers split between sad, scared and",
          "surprised at every level."),
    "new 2026-09-28 (not in PR #28)", "chapter: Do higher scorers do better?; recommendation 5",
  # scoped
  "tom_story4_deception_reality_check_2", "exclude, or re-key to 'no' (scoped)", "", "", "4g", "2024-10-24", "",
    paste("'Is Mother mad at Isabel now?' Below chance in de-DE and en-US, flat in the monotonicity screen.", s4g_why),
    "proposed", "PR #28 record 2; chapter: The exclusion list",
  "tom_story4_deception_reality_check_3", "exclude, or re-key to 'no' (scoped)", "", "", "4g", "2024-10-24", "",
    paste("'Is Joshua mad at Isabel now?' Below chance in every language; reversed in the monotonicity screen.", s4g_why),
    "proposed", "PR #28 record 3; chapter: The exclusion list",
  "tom_story2_moral_reasoning_reality_check_1", "exclude (scoped)", "pilot_uniandes_co_bogota", "", "2e", "", "2024-07-01",
    paste("Below chance on this content only; V0 children chose the mirror image of later children ('coats'",
          "for key 'shelf'). Flat in the monotonicity screen.", v0_why),
    "proposed", "PR #28 record 4; chapter: The exclusion list",
  "tom_story2_moral_reasoning_reality_check_2", "exclude (scoped)", "pilot_uniandes_co_bogota", "", "2e", "", "2024-07-01",
    paste("On the same V0 content higher scorers do worse (reversed); on later content it is near ceiling and",
          "rises with score. Also non-invariant only because of V0.", v0_why),
    "new 2026-09-28 (not in PR #28)", "chapter: Do higher scorers do better?; recommendation 3",
  # hold
  "tom_story13_reality_known_false_belief", "hold: analysis flag only, no Airtable row yet", "", "es-AR, es-CO", "", "", "",
    paste("Below chance and non-discriminating in Spanish; Spanish children also lower at matched age; fine in",
          "German. Wait for the translation check before writing a language-scoped row (the row would be",
          "unchecked meanwhile). Stories 1 and 7 (same question) still discriminate in Spanish."),
    "proposed (unchecked row)", "PR #28 record 5; chapter: Item screen"
)

# chance corrections (not exclusions)
chance_fix <- tribble(
  ~item_uid, ~chance_now,
  "tom_story6_second_order_false_belief_3", 0.33,
  "tom_story12_second_order_false_belief_2", 0.33,
  "tom_story18_second_order_false_belief_2", 0.33) |>
  transmute(item_uid, action = "correct: chance .33 -> .5", dataset = "", language = "", content_version = "",
            date_start = "", date_end = "",
            rationale = paste("Yes/no question stored with the chance of its generic uid (.33, a 3-option question",
                              "in other stories), so it is fit with the wrong guessing floor."),
            status = "in PR #27 (code); the #27 proposal moves it to the item bank",
            source = "PR #27; chapter: Data and provenance")

# un-exclusions: the story-level items the 12 generic Airtable rows cover but should not
unexclude <- ri$item_verdicts |>
  filter(str_starts(verdict, "un-exclude"), story_uid != "tom_story13_reality_known_false_belief") |>
  transmute(item_uid = story_uid,
            action = "un-exclude: remove the generic row",
            dataset = "", language = "", content_version = "", date_start = "", date_end = "",
            rationale = case_when(
              str_detect(verdict, "weak") ~ "Above chance but discriminates weakly; no reason to exclude.",
              str_detect(verdict, "Spanish") ~ paste("Below chance in Spanish but discriminates (children answer",
                                                     "with the real location); no reason to exclude."),
              TRUE ~ "Behaves normally: above chance and discriminating."),
            status = "proposed", source = "chapter: The exclusion list")

# review, not proposed for exclusion (chapter recommendation 5)
review <- tribble(
  ~item_uid, ~why,
  "tom_story4_deception_false_belief_3", "near-chance yes/no 'Is [sibling] mad at [sibling]?'",
  "tom_story16_deception_reference_2", "near-chance yes/no 'Is [sibling] mad at [sibling]?'",
  "tom_story16_deception_reference_5", "barely discriminates") |>
  transmute(item_uid, action = "review (not proposed for exclusion)", dataset = "", language = "",
            content_version = "", date_start = "", date_end = "",
            rationale = paste0("Candidate for revision: ", why, "."),
            status = "for the content team", source = "chapter: Item screen; recommendation 5")

all_actions <- bind_rows(actions, chance_fix, unexclude, review)

# ---- evidence -------------------------------------------------------------------------------------
generic_rows <- read_rds(file.path(dir26, "raw/levante_metadata_items__v3_18.rds"))
airtable_generic <- tr |> distinct(story_uid, generic_uid)

scope_trials <- function(uid, ds, lang, cv, end) {
  d <- tr |> filter(story_uid == uid)
  if (ds != "") d <- d |> filter(dataset %in% str_split_1(ds, ",\\s*"))
  if (lang != "") d <- d |> filter(language %in% str_split_1(lang, ",\\s*"))
  if (cv != "") d <- d |> filter(item_original == cv)
  if (end != "") d <- d |> filter(timestamp < as.POSIXct(end, tz = "UTC"))
  d
}
evidence <- function(uid, ds, lang, cv, end) {
  d <- scope_trials(uid, ds, lang, cv, end)
  content <- case_when(cv == "4g" ~ "policy-flagged (4g)",
                       cv == "2e" & uid == "tom_story2_moral_reasoning_reality_check_1" ~ "policy-flagged (2e)",
                       TRUE ~ "all")
  ia <- ri$items_all |> filter(story_uid == uid, content == !!content)
  ma <- mono$mono_all |> filter(story_uid == uid, content == !!content)
  if (uid == "tom_story2_moral_reasoning_reality_check_2" && cv == "2e") {
    ma <- mono$s2rc2 |> filter(part == "V0 Bogota (2e)"); ia <- ia[0, ]
  }
  if (lang != "") {   # language-scoped: pooled over the scope's languages
    il <- ri$items_lang |> filter(story_uid == uid, content == "all", language %in% str_split_1(lang, ",\\s*"))
    ml <- mono$mono_lang |> filter(story_uid == uid, content == "all", language %in% str_split_1(lang, ",\\s*"))
    ia <- il |> slice_max(n, n = 1); ma <- ml |> slice_max(n, n = 1)
  }
  tibble(n_trials = nrow(d), n_runs = n_distinct(d$run_id),
         datasets = paste(sort(unique(d$dataset)), collapse = ", "),
         accuracy = mean(as.logical(d$correct)), chance = first(d$chance),
         item_rest_r = if (nrow(ia)) ia$r_ir else NA_real_,
         item_rest_r_lo = if (nrow(ia)) ia$r_ir_lo else NA_real_,
         item_rest_r_hi = if (nrow(ia)) ia$r_ir_hi else NA_real_,
         monotonicity = if (nrow(ma)) as.character(ma$mono) else NA_character_,
         gain_top_minus_bottom = if (nrow(ma)) ma$diff else NA_real_,
         gain_lo = if (nrow(ma)) ma$diff_lo else NA_real_,
         gain_hi = if (nrow(ma)) ma$diff_hi else NA_real_)
}

out <- all_actions |>
  mutate(ev = pmap(list(item_uid, dataset, language, content_version, date_end), evidence)) |>
  unnest(ev) |>
  left_join(airtable_generic |> rename(item_uid = story_uid), by = "item_uid") |>
  left_join(generic_rows$exclusions |> distinct(item_uid, dataset) |>
              group_by(generic_uid = item_uid) |>
              summarise(generic_datasets = paste(sort(unique(dataset)), collapse = ", "), .groups = "drop"),
            by = "generic_uid") |>
  mutate(airtable_now = if_else(!is.na(generic_datasets),
                                paste0(generic_uid, " for ", generic_datasets, " (generic row; matches nothing today)"),
                                "none"),
         evidence_note = case_when(
           item_uid == "tom_story13_reality_known_false_belief" ~ "item-rest r and gain for es-AR (largest Spanish sample)",
           item_uid == "tom_story2_moral_reasoning_reality_check_2" ~ "gain on the V0 content only; no item-rest r for this scope",
           TRUE ~ "")) |>
  mutate(order = match(action, unique(all_actions$action))) |>
  arrange(order, item_uid) |>
  mutate(id = row_number()) |>
  select(id, item_uid, action, dataset, language, content_version, date_start, date_end,
         n_trials, n_runs, accuracy, chance, item_rest_r, item_rest_r_lo, item_rest_r_hi,
         monotonicity, gain_top_minus_bottom, gain_lo, gain_hi, evidence_note,
         rationale, status, source, airtable_now, datasets) |>
  mutate(across(c(accuracy, item_rest_r, item_rest_r_lo, item_rest_r_hi, gain_top_minus_bottom, gain_lo, gain_hi),
                \(x) round(x, 3)))

# one row per item x action (the three chance corrections are also un-exclusions)
stopifnot(!anyDuplicated(out[c("item_uid", "action")]), nrow(out) == nrow(all_actions), all(out$n_trials > 0))
write_csv(out, out_file, na = "")
options(width = 220)
print(count(out, action))
print(as.data.frame(out |> select(id, item_uid, action, dataset, language, content_version, date_end, n_trials,
                                  accuracy, item_rest_r, monotonicity, gain_top_minus_bottom, airtable_now)))
