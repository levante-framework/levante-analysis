# Corrected question-type scheme for the Stories items (2026-10, after
# Rebecca Saxe's review). The corpus entry names (reality_check_N,
# false_belief_N, ...) are what the 2026-09 chapter used as "question type",
# but they mislabel several questions; reading the prompts:
#   - moral judgments are carried as false_belief entries: stories 2/8/14
#     false_belief_2/_3 ("Is X being mean and naughty...?", "Should X get in
#     trouble...?") and stories 4/16 false_belief_1 ("Is it fair that ... is
#     mad at ...?"); both story-10 lie questions ("Is Teacher Sarah lying?",
#     corpus false_belief_1; "Is Brian lying?", corpus reality_check_1) are
#     lie judgments, grouped with them (DCC decision, 2026-10-17);
#   - story 16's five questions are labelled reference_1..5 but are the
#     story-4 structure: reference_1/_2 ("Is Dad/Sophie mad at James?") are
#     belief-based attributions (story 4's false_belief_2/_3) and
#     reference_3/_4/_5 ("... now?") are post-reveal checks (story 4's
#     reality_check_1/_2/_3).
# Returns a lookup keyed on story x entry.
suppressPackageStartupMessages(library(dplyr))

qtype_recode <- function(items) {
  items |>
    mutate(question_type_v2 = case_when(
      story %in% c(2, 8, 14) & entry %in% c("false_belief_2", "false_belief_3") ~ "moral_judgment",
      story %in% c(4, 16) & entry == "false_belief_1" ~ "moral_judgment",
      story == 10 & entry %in% c("false_belief_1", "reality_check_1") ~ "moral_judgment",
      story == 16 & entry %in% c("reference_1", "reference_2") ~ "false_belief",
      story == 16 & entry %in% c("reference_3", "reference_4", "reference_5") ~ "reality_check",
      TRUE ~ question_type),
      is_control_v2 = question_type_v2 == "reality_check" |
        (question_type_v2 == "emotion_reasoning" & is_control))
}
