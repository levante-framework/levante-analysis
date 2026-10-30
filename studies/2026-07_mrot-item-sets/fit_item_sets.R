library(tidyverse)
library(here)
library(glue)
library(mirt)
library(levantemodels)

pilots_data_dir <- here("..", "levante-pilots", "01_fetched_data")
task_data_nested <- read_rds(file.path(pilots_data_dir, "task_data_nested.rds"))
run_data <- read_rds(file.path(pilots_data_dir, "run_data.rds"))

task_data_mrot <- task_data_nested |> filter(item_task == "mrot")

# at least for this task, NA responses seem to be incorrectly coded as incorrect
task_data <- task_data_mrot |>
  unnest(data) |>
  filter(!is.na(response)) |>
  left_join(run_data |> select(run_id, age))

items_coded <- task_data |>
  select(contains("item")) |> distinct() |>
  separate(item, into = c("stimulus", "angle")) |>
  mutate(angle = as.numeric(angle)) |>
  rename(item = item_original) |>
  mutate(angle_full = item |> str_sub(str_length(item) - 2, str_length(item)) |> as.numeric(),
         stimulus_code = item |> str_sub(1, 1),
         chirality = item |> str_sub(2, 2),
         exemplar = item |> str_sub(3, str_length(item) - 3),
         stimulus_code = paste0(stimulus_code, exemplar)) |>
  filter(!stimulus %in% c("polygon1", "polygon2")) |>
  rename(item_original = item) |>
  select(item_task, item_group, item_original, stimulus, stimulus_code, chirality, angle, angle_full)

make_item_uid_map <- function(angle_var, stimulus_var, use_chirality) {
  items_coded |>
    mutate(angle_str = sprintf("%03d", .data[[angle_var]]),
           stim_val = .data[[stimulus_var]]) |>
    mutate(item_uid_new = if (use_chirality) {
      paste("mrot", item_group, stim_val, chirality, angle_str, sep = "_")
    } else {
      paste("mrot", item_group, stim_val, angle_str, sep = "_")
    }) |>
    select(item_original, item_uid_new)
}

item_set_specs <- expand_grid(
  angle_var = c("angle", "angle_full"),
  stimulus_var = c("stimulus", "stimulus_code"),
  chirality = c(FALSE, TRUE)
) |>
  mutate(item_set = paste(angle_var, stimulus_var,
                           if_else(chirality, "chir", "nochir"), sep = "_"),
         item_uid_map = pmap(list(angle_var, stimulus_var, chirality), make_item_uid_map),
         n_items = map_dbl(item_uid_map, ~ n_distinct(.x$item_uid_new)))

build_task_data_x <- function(item_uid_map) {
  task_data |>
    inner_join(item_uid_map, by = "item_original") |>
    select(-item_uid) |>
    rename(item_uid = item_uid_new) |>
    nest(data = -c(dataset, item_task, language))
}

item_set_specs <- item_set_specs |>
  mutate(task_data_x = map(item_uid_map, build_task_data_x))

models_multigroup <- tribble(
  ~nfact, ~itemtype, ~invariance,
  1,   "Rasch", "configural",
  1,   "Rasch", "scalar",
  1,     "2PL", "configural",
  1,     "2PL", "metric",
  1,     "2PL", "scalar",
)

priors <- list(d = c("norm", 0, 3), a1 = c("norm", 1, 0.3))

is_item_set_done <- \(item_set) {
  # scalar 2PL all_items file is the last one fit_task_models_multigroup writes
  # per item set (fit last, since models_multigroup is ordered) -- use it as
  # a completion marker so a relaunch can skip finished item sets
  file.exists(file.path("models/item_sets", item_set, "mrot",
                         "multigroup_dataset", "all_items", "mrot_2pl_f1_scalar.rds"))
}

walk2(item_set_specs$item_set, item_set_specs$task_data_x, \(item_set, task_data_x) {
  if (is_item_set_done(item_set)) {
    message(glue::glue(">>> skipping already-fit item set: {item_set}"))
    return(invisible())
  }
  message(glue::glue(">>> fitting item set: {item_set}"))
  fit_task_models_multigroup(task_data = task_data_x,
                              models = models_multigroup,
                              priors = priors,
                              task = "mrot",
                              group = dataset,
                              registry_dir = file.path("models/item_sets", item_set))
  message(glue::glue(">>> done item set: {item_set}"))
})

message(">>> ALL DONE")
