## Preprocessing pipeline for the continuous-response experiments (Exp14, Exp18)
##
## Each trial: a sampling screen (Task "response", Response is always 0 = continue),
## followed by a slider asking how likely the correct colour is blue or red.
##
## Output (saved to OUTPUT_DIR):
##   data_processed   one row per trial with a slider answer
##
## Some participants ran the task more than once under the same ID. Each run gets
## its own number (Run), all runs are kept, and these participants are printed.
##   performance      per participant: accuracy, optimal-observer accuracy, optimality
##   likelihood_blue  P(choose blue) and mean slider by net number of blue samples, per participant
##   likelihood_final same, pooled per experiment (Done.by)
##   feedback_data    any other task rows (e.g. colour feedback in Exp18), kept for later

library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

# ---- Settings ----------------------------------------------------------------

OUTPUT_DIR    <- "./results"
NET_RANGE     <- -6:6               # possible net counts (6 samples per trial)
RELIABILITIES <- c("50", "55", "65")

input_files <- tribble(
  ~Done.by, ~Experiment.Name, ~file,
  "Exp14",  "egvg",           "data/Continuous_Response/Exp14_NoFeedback/data_exp_236042-v3_task-egvg.csv",
  "Exp14",  "lsjo",           "data/Continuous_Response/Exp14_NoFeedback/data_exp_236042-v3_task-lsjo.csv",
  "Exp18",  "tvvn",           "data/Continuous_Response/Exp18_CorrectColour/data_exp_263579-v6_task-tvvn.csv",
  "Exp18",  "u1cn",           "data/Continuous_Response/Exp18_CorrectColour/data_exp_263579-v6_task-u1cn.csv"
)

# Columns taken from every raw Gorilla file
raw_cols <- c("Participant.Private.ID", "UTC.Timestamp", "randomiser.475f", "Manipulation..Prior.Color",
              "Block", "Trial.Index", "Internal.Node.ID", "Task", "Stimulus",
              "Response", "Correct.Response", "Stimulus.Info")

# ---- Helpers -----------------------------------------------------------------

# Numeric conversion where empty cells or text become NA (checked explicitly below)
to_num <- function(x) suppressWarnings(as.numeric(x))

read_task_file <- function(file, Experiment.Name, Done.by) {
  # everything as character so files with different column types bind cleanly
  d <- read.csv(file, colClasses = "character")[, raw_cols]
  d$Experiment.Name <- Experiment.Name
  d$Done.by <- Done.by
  d
}

# Internal.Node.ID looks like "0.0-3.0-0.0-0.5": the last part is "<screen>.<repetition>".
# A response ("...-0.k") and its slider ("...-1.k") share the prefix and the repetition k.
add_node_keys <- function(d) {
  last_part <- str_extract(d$Internal.Node.ID, "[^-]+$")
  d$node_prefix     <- str_remove(d$Internal.Node.ID, "-[^-]+$")
  d$node_repetition <- str_extract(last_part, "[^.]+$")
  d
}

# Features of the six samples shown in one trial
parse_stimulus_info <- function(txt) {
  d <- fromJSON(txt)
  if (!is.null(d$displayed) && !all(d$displayed)) stop("Sample with displayed = false: ", txt)
  is_blue <- d$color == "blue"
  is_red  <- d$color == "red"
  rel     <- d$reliability
  tibble(
    blue_rel = list(ifelse(is_blue, rel, 1 - rel)),  # P(blue) implied by each sample
    blue_50  = sum(is_blue & rel == 0.50),
    blue_55  = sum(is_blue & rel == 0.55),
    blue_65  = sum(is_blue & rel == 0.65),
    red_50   = sum(is_red  & rel == 0.50),
    red_55   = sum(is_red  & rel == 0.55),
    red_65   = sum(is_red  & rel == 0.65)
  )
}

# Number each run of the task per participant: in time order, a new run starts
# whenever Trial.Index does not increase (it restarts from 0 in every run).
add_run_number <- function(d) {
  d |>
    mutate(.file_row = row_number()) |>
    group_by(Participant.Private.ID, Experiment.Name) |>
    arrange(UTC.Timestamp, .file_row, .by_group = TRUE) |>
    mutate(.previous_index = lag(Trial.Index)) |>
    fill(.previous_index, .direction = "down") |>        # skip rows without a Trial.Index
    mutate(Run = 1 + cumsum(coalesce(Trial.Index <= .previous_index, FALSE))) |>
    ungroup() |>
    arrange(.file_row) |>
    select(-.file_row, -.previous_index)
}

report_rows <- function(d, what) {
  if (nrow(d) == 0) return(invisible())
  message(nrow(d), " ", what, ":")
  print(as.data.frame(d), row.names = FALSE)
}

summarise_performance <- function(data_processed, participants) {
  scores <- data_processed |>
    group_by(Participant.Private.ID, Experiment.Name, Done.by) |>
    summarise(Accuracy       = mean(Accuracy, na.rm = TRUE),        # slider at 50 excluded
              BDT_accuracy   = mean(accuracy_bayes, na.rm = TRUE),  # tie trials excluded
              Optimal_Choice = mean(Optimality),                    # 50 on a tie trial is optimal
              n_trials       = n(),
              n_neutral      = sum(is.na(Response)),                # slider answers of exactly 50
              .groups = "drop")
  participants |> left_join(scores, by = c("Participant.Private.ID", "Experiment.Name", "Done.by"))
}

summarise_likelihood <- function(data_processed, participants) {
  # one row per trial x reliability level
  long <- data_processed |>
    select(Participant.Private.ID, Experiment.Name, Done.by, Response, Optimalchoice,
           slider_blue, starts_with("Net.Blue.")) |>
    pivot_longer(starts_with("Net.Blue."), names_to = "Reliability",
                 names_prefix = "Net.Blue.", values_to = "Net_blue")
  stopifnot("Net counts outside NET_RANGE" = all(long$Net_blue %in% NET_RANGE))
  
  count_on_grid <- function(grid, by) {
    counts <- long |>
      group_by(across(all_of(by))) |>
      summarise(Count            = n(),
                Count_choice     = sum(!is.na(Response)),   # trials with a side (slider not 50)
                Blue_choice      = sum(Response, na.rm = TRUE),
                Blue_BDT_choice  = sum(Optimalchoice),      # tie trials count as 0.5
                Mean_slider_blue = mean(slider_blue),
                .groups = "drop")
    grid |>
      left_join(counts, by = by) |>
      mutate(across(c(Count, Count_choice, Blue_choice, Blue_BDT_choice), \(x) replace_na(x, 0)),
             P_blue     = Blue_choice / Count_choice,      # NaN where there are no trials
             P_BDT_blue = Blue_BDT_choice / Count)
  }
  
  net_by_reliability <- expand_grid(Net_blue = NET_RANGE, Reliability = RELIABILITIES)
  
  list(
    likelihood_blue = count_on_grid(
      grid = cross_join(participants, net_by_reliability),
      by   = c("Participant.Private.ID", "Experiment.Name", "Done.by", "Reliability", "Net_blue")),
    likelihood_final = count_on_grid(
      grid = expand_grid(Net_blue = NET_RANGE, Reliability = RELIABILITIES,
                         Done.by = unique(participants$Done.by)),
      by   = c("Net_blue", "Reliability", "Done.by"))
  )
}

# ---- 1. Load -----------------------------------------------------------------

raw <- pmap(input_files, read_task_file) |>
  list_rbind() |>
  rename(Prior.Color = Manipulation..Prior.Color) |>
  mutate(across(c(Participant.Private.ID, UTC.Timestamp, Block, Trial.Index, Prior.Color), to_num)) |>
  filter(!is.na(Participant.Private.ID)) |>
  add_run_number()

report_rows(raw |>
              filter(Task == "response") |>
              count(Done.by, Experiment.Name, Participant.Private.ID, Run, name = "n_trials") |>
              group_by(Participant.Private.ID, Experiment.Name) |>
              filter(n() > 1) |>
              ungroup(),
            "runs from participants who did the task more than once (all kept)")

participants <- raw |> distinct(Participant.Private.ID, Experiment.Name, Done.by)

feedback_data <- raw |> filter(!Task %in% c("response", "slider", "break", ""))
if (nrow(feedback_data) > 0) {
  message("Other task rows kept in feedback_data:")
  print(table(feedback_data$Done.by, feedback_data$Task))
}

# ---- 2. Response and slider rows ---------------------------------------------

responses <- raw |>
  filter(Task == "response") |>
  group_by(Participant.Private.ID, Experiment.Name, Run) |>
  mutate(Trial = row_number(Trial.Index)) |>     # trial order within each run
  ungroup() |>
  add_node_keys() |>
  mutate(correct_raw     = to_num(Correct.Response),
         Correct.Response = as.numeric(Stimulus == "blue"))   # true colour, 1 = blue

sliders <- raw |>
  filter(Task == "slider") |>
  add_node_keys() |>
  transmute(Participant.Private.ID, Experiment.Name, Run, Block, node_prefix, node_repetition,
            slider_response = to_num(Response))

stopifnot(
  "Stimulus on a response row is not 'blue' or 'red'" = all(responses$Stimulus %in% c("blue", "red")),
  "Prior.Color missing or not 0/1"                     = all(responses$Prior.Color %in% c(0, 1)),
  "Slider answer missing or outside 0-100"             = all(between(sliders$slider_response, 0, 100))
)

# Cross-check: Gorilla's Correct.Response (button coded) should agree with the true colour
button_coded_blue <- with(responses, ifelse(randomiser.475f == "Button0Blue", 1 - correct_raw, correct_raw))
n_mismatch <- sum(button_coded_blue != responses$Correct.Response, na.rm = TRUE)
if (n_mismatch > 0) warning(n_mismatch, " trials where Gorilla's Correct.Response disagrees with Stimulus")

# ---- 3. Pair each response with its slider -----------------------------------

pair_keys <- c("Participant.Private.ID", "Experiment.Name", "Run", "Block", "node_prefix", "node_repetition")

report_rows(anti_join(responses, sliders, by = pair_keys) |>
              select(Done.by, Experiment.Name, Participant.Private.ID, Run, Trial, Internal.Node.ID),
            "response trials without a slider (dropped)")
report_rows(anti_join(sliders, responses, by = pair_keys),
            "slider rows without a response trial (dropped)")

trials <- responses |>
  inner_join(sliders, by = pair_keys, relationship = "one-to-one")

# ---- 4. Sample features, optimal observer, participant's answer --------------

features <- map(trials$Stimulus.Info, parse_stimulus_info) |> list_rbind()

log_odds <- map_dbl(features$blue_rel, \(p) round(sum(log(p / (1 - p))), digits = 6))

data_processed <- trials |>
  bind_cols(features) |>
  mutate(
    PosteriorBlue  = log_odds,
    Optimalchoice  = (sign(log_odds) + 1) / 2,            # 1 = blue, 0 = red, 0.5 = tie
    slider_blue    = ifelse(Prior.Color == 1, slider_response, 100 - slider_response),  # 100 = certainly blue
    Response       = ifelse(slider_blue == 50, NA, as.numeric(slider_blue > 50)),       # side chosen, NA at 50
    Accuracy       = as.numeric(Response == Correct.Response),                          # NA at 50
    accuracy_bayes = ifelse(log_odds == 0, NA, as.numeric(Optimalchoice == Correct.Response)),
    Optimality     = as.numeric(sign(slider_blue - 50) == sign(log_odds)),             # 50 on a tie is optimal
    Net.Blue.50    = blue_50 - red_50,
    Net.Blue.55    = blue_55 - red_55,
    Net.Blue.65    = blue_65 - red_65
  ) |>
  select(Participant.Private.ID, Experiment.Name, Done.by, randomiser.475f, Prior.Color,
         Run, Block, Trial, Stimulus, Stimulus.Info, Correct.Response,
         slider_response, slider_blue, Response, PosteriorBlue, Optimalchoice,
         Accuracy, accuracy_bayes, Optimality,
         blue_rel, blue_50, blue_55, blue_65, red_50, red_55, red_65,
         Net.Blue.50, Net.Blue.55, Net.Blue.65)

# ---- 5. Summaries ------------------------------------------------------------

performance <- summarise_performance(data_processed, participants)

likelihood       <- summarise_likelihood(data_processed, participants)
likelihood_blue  <- likelihood$likelihood_blue
likelihood_final <- likelihood$likelihood_final

# ---- 6. Save -----------------------------------------------------------------

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
save(data_processed,   file = file.path(OUTPUT_DIR, "data_preprocessed_ContinuousResp.rdata"))
save(performance,      file = file.path(OUTPUT_DIR, "performance_ContinuousResp.rdata"))
# save(likelihood_blue,  file = file.path(OUTPUT_DIR, "likelihood_blue.rdata"))
# save(likelihood_final, file = file.path(OUTPUT_DIR, "likelihood_final.rdata"))
# save(feedback_data,    file = file.path(OUTPUT_DIR, "feedback_data.rdata"))