# Writes tasks/stories_qtype_relabels.csv: the proposed question-type
# relabels for the Stories items (applied in tasks/_stories_qtype_recode.R)
# plus borderline cases listed for discussion but not applied. Run from the
# repo root after tasks/_build_stories_data.R.
suppressPackageStartupMessages({library(dplyr); library(readr); library(tibble)})
source("tasks/_stories_qtype_recode.R")

tr <- read_rds("data/stories_2026-09/stories_trials.rds")
prompts <- read_rds("data/tom_meta/tom_item_metadata.rds") |> select(story_uid = item_uid, prompt)
items <- tr |>
  group_by(story_uid, story, entry, old_group, question_type, is_control) |>
  summarise(keyed_answer = first(answer), responses = n(), accuracy = mean(correct), .groups = "drop") |>
  left_join(prompts, by = "story_uid") |>
  qtype_recode()

why <- tribble(
  ~story, ~entry, ~rationale,
  2,  "false_belief_2", "Moral judgment of the actor (blameworthiness); keyed no because the act was not intentional",
  2,  "false_belief_3", "Moral judgment (deserved punishment); keyed no",
  8,  "false_belief_2", "Moral judgment of the actor (blameworthiness); keyed no",
  8,  "false_belief_3", "Moral judgment (deserved punishment); keyed no",
  14, "false_belief_2", "Moral judgment of the actor (blameworthiness); keyed no",
  14, "false_belief_3", "Moral judgment (deserved punishment); keyed no",
  4,  "false_belief_1", "Fairness judgment about another character's anger; keyed no",
  16, "false_belief_1", "Fairness judgment about another character's anger; keyed no",
  10, "false_belief_1", "Lie judgment: a false statement made in good faith is not a lie; keyed no",
  10, "reality_check_1", "Lie judgment (the deceiver); keyed yes. Corpus treats it as the control for the item above",
  16, "reference_1", "Belief-based attribution, the same structure as story 4 false_belief_2 (not a listener/reference question)",
  16, "reference_2", "Belief-based attribution, the same structure as story 4 false_belief_3",
  16, "reference_3", "Post-reveal check, the same structure as story 4 reality_check_1",
  16, "reference_4", "Post-reveal check, the same structure as story 4 reality_check_2",
  16, "reference_5", "Post-reveal check, the same structure as story 4 reality_check_3")

applied <- items |>
  filter(question_type_v2 != question_type) |>
  left_join(why, by = c("story", "entry")) |>
  transmute(status = "proposed (applied in the chapter)", story, story_type = old_group, entry,
            item_uid = story_uid, prompt, keyed_answer,
            current_type = question_type, proposed_type = question_type_v2, rationale,
            responses, accuracy = round(accuracy, 3))
stopifnot(nrow(applied) == nrow(why), !anyNA(applied$rationale))

discuss <- tribble(
  ~story, ~entry, ~proposed_type, ~rationale,
  4,  "emotion_reasoning_2", "counterfactual emotion (split?)", "\"How would X feel if...\" counterfactual emotion items (all stories' emotion_reasoning_2) are harder than actual-emotion items; a separate type may be warranted",
  4,  "false_belief_2", "false_belief (keep?) or emotion attribution", "\"Is Mother mad at Isabel?\" attributes an emotion that depends on what Mother knows; story 4 false_belief_3 and story 16 reference_1/_2 are the same structure",
  6,  "false_belief_1", "knowledge access (split?)", "\"Does Sam know that...?\" asks about knowledge, not a false belief; also story 12 false_belief_2 and story 18 false_belief_2",
  10, "reality_check_2", "reality_check (duplicate prompt)", "Identical prompt to story 10 reality_check_3; one of the two is redundant")
discuss <- discuss |>
  left_join(items |> select(story, entry, old_group, story_uid, prompt, keyed_answer, question_type, responses, accuracy),
            by = c("story", "entry")) |>
  transmute(status = "for discussion (not applied)", story, story_type = old_group, entry, item_uid = story_uid,
            prompt, keyed_answer, current_type = question_type, proposed_type, rationale, responses,
            accuracy = round(accuracy, 3))

out <- bind_rows(applied |> arrange(proposed_type, story, entry), discuss)
write_csv(out, "tasks/stories_qtype_relabels.csv")
message("wrote ", nrow(out), " rows to tasks/stories_qtype_relabels.csv")
