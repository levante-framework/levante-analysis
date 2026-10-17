# Corrected question-type scheme for the Stories items (2026-10, after
# Rebecca Saxe's review). The corpus entry names (reality_check_N,
# false_belief_N, ...) are what the 2026-09 chapter used as "question type",
# but they mislabel several questions; reading the prompts:
#   - moral judgments are carried as false_belief entries: stories 2/8/14
#     false_belief_2/_3 ("Is X being mean and naughty...?", "Should X get in
#     trouble...?") and stories 4/16 false_belief_1 ("Is it fair that ... is
#     mad at ...?"); story 10 false_belief_1 ("Is Teacher Sarah lying?", a
#     false statement made in good faith) is a lie judgment, grouped with them.
#     Story 10 reality_check_1 ("Is Brian lying?") stays a reality check
#     (DCC decisions, 2026-10-17/18);
#   - story 16's five questions are labelled reference_1..5 but are the
#     story-4 structure: reference_1/_2 ("Is Dad/Sophie mad at James?") are
#     belief-based attributions (story 4's false_belief_2/_3) and
#     reference_3/_4/_5 ("... now?") are post-reveal checks (story 4's
#     reality_check_1/_2/_3). Like story 4's, they are scored by logic, not
#     as a separate emotion-inference type: only two stories (one scenario)
#     carry them, and the belief-dependent ones track false belief more than
#     emotion within children;
#   - knowledge access: the four "Does X know that ...?" questions in the
#     second-order stories (story 6 false_belief_1 and reality_check, story
#     12 false_belief_2, story 18 false_belief_2), a step of the Wellman &
#     Liu theory-of-mind scale.
# Returns a lookup keyed on story x entry.
suppressPackageStartupMessages(library(dplyr))

qtype_recode <- function(items) {
  items |>
    mutate(question_type_v2 = case_when(
      story %in% c(2, 8, 14) & entry %in% c("false_belief_2", "false_belief_3") ~ "moral_judgment",
      story %in% c(4, 16) & entry == "false_belief_1" ~ "moral_judgment",
      story == 10 & entry == "false_belief_1" ~ "moral_judgment",
      (story == 6 & entry %in% c("false_belief_1", "reality_check")) |
        (story %in% c(12, 18) & entry == "false_belief_2") ~ "knowledge_access",
      story == 16 & entry %in% c("reference_1", "reference_2") ~ "false_belief",
      story == 16 & entry %in% c("reference_3", "reference_4", "reference_5") ~ "reality_check",
      TRUE ~ question_type),
      is_control_v2 = question_type_v2 == "reality_check" |
        (question_type_v2 == "emotion_reasoning" & is_control))
}
