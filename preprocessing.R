## Preprocessing pipeline for the Alex, Divyaj, Exp6 and mitigation3 data
##
## Output (saved to OUTPUT_DIR):
##   data_processed   one row per response trial, with slider answer and sample features
##   performance      accuracy per participant (participant vs. optimal observer)
##   likelihood_blue  P(choose blue) by net number of blue samples, per participant
##   likelihood_final same, pooled per dataset (Done.by)
##   feedback_data    mitigation3 feedback rows, kept for later analyses

library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

# ---- Settings ----------------------------------------------------------------

SEED          <- 2026             # seed for breaking ties in the optimal choice
OUTPUT_DIR    <- "./results"  # switch to "./results" once the pipeline is validated
NET_RANGE     <- -6:6             # possible net counts (6 samples per trial)
RELIABILITIES <- c("50", "55", "65")

input_files <- tribble(
  ~Done.by,      ~Experiment.Name, ~file,
  "Alex",        "czkz",           "data/Alex/data_exp_178975-v10_task-czkz.csv",
  "Alex",        "r9tq",           "data/Alex/data_exp_178975-v10_task-r9tq.csv",
  "Divyaj",      "czkz",           "data/Divyaj/data_exp_179659-v6_task-czkz.csv",
  "Divyaj",      "r9tq",           "data/Divyaj/data_exp_179659-v6_task-r9tq.csv",
  "Exp6",        "dr3e",           "data/Exp6/data_exp_52555-v10_task-dr3e.csv",
  "Exp6",        "dr3e",           "data/Exp6/data_exp_52555-v11_task-dr3e.csv",
  "Exp6",        "t5fq",           "data/Exp6/data_exp_52555-v10_task-t5fq.csv",
  "Exp6",        "t5fq",           "data/Exp6/data_exp_52555-v11_task-t5fq.csv",
  "mitigation3", "a5xi",           "data/mitigation3/data_exp_273055-v7_task-a5xi.csv",
  "mitigation3", "sxrr",           "data/mitigation3/data_exp_273055-v7_task-sxrr.csv"
)

# Columns taken from every raw Gorilla file
raw_cols <- c("Participant.Private.ID", "Trial.Number", "randomiser.475f", "Stimulus",
              "Response", "Stimulus.Info", "Task", "Correct.Response", "Accuracy")

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

# Recode Button0Blue sessions so that 1 = blue for everyone
flip_button0blue <- function(d) {
  flip <- d$randomiser.475f == "Button0Blue"
  flip_response <- flip & d$Task == "response"
  d$Response[flip_response] <- 1 - d$Response[flip_response]
  d$Correct.Response[flip] <- 1 - d$Correct.Response[flip]
  d$randomiser.475f[flip] <- "Button0Red"
  d
}

# Features of the six samples shown in one trial
parse_stimulus_info <- function(txt) {
  d <- fromJSON(txt)
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

summarise_performance <- function(data_processed, participants) {
  scores <- data_processed |>
    group_by(Participant.Private.ID, Experiment.Name, Done.by) |>
    summarise(Accuracy       = mean(Response == Correct.Response),
              BDT_accuracy   = mean(Optimalchoice == Correct.Response),
              Optimal_Choice = mean(Optimalchoice == Response),
              .groups = "drop")
  # keep every participant, including any without response trials
  participants |> left_join(scores, by = c("Participant.Private.ID", "Experiment.Name", "Done.by"))
}

summarise_likelihood <- function(data_processed, participants) {
  # one row per trial x reliability level
  long <- data_processed |>
    select(Participant.Private.ID, Experiment.Name, Done.by, Response, Optimalchoice,
           starts_with("Net.Blue.")) |>
    pivot_longer(starts_with("Net.Blue."), names_to = "Reliability",
                 names_prefix = "Net.Blue.", values_to = "Net_blue")
  stopifnot("Net counts outside NET_RANGE" = all(long$Net_blue %in% NET_RANGE))
  
  count_on_grid <- function(grid, by) {
    counts <- long |>
      group_by(across(all_of(by))) |>
      summarise(Count = n(), Blue_choice = sum(Response),
                Blue_BDT_choice = sum(Optimalchoice), .groups = "drop")
    grid |>
      left_join(counts, by = by) |>
      mutate(across(c(Count, Blue_choice, Blue_BDT_choice), \(x) replace_na(x, 0)),
             P_blue     = Blue_choice / Count,       # NaN where Count is 0
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
  mutate(Participant.Private.ID = to_num(Participant.Private.ID),
         Trial.Number           = to_num(Trial.Number)) |>
  filter(!is.na(Participant.Private.ID))

participants <- raw |> distinct(Participant.Private.ID, Experiment.Name, Done.by)

# ---- 2. Keep task rows, convert types, recode buttons ------------------------

trials <- raw |>
  filter(Task %in% c("response", "slider", "feedback")) |>
  mutate(across(c(Response, Correct.Response, Accuracy), to_num)) |>
  flip_button0blue()

responses     <- trials |> filter(Task == "response")
feedback_data <- trials |> filter(Task == "feedback")

sliders <- trials |>
  filter(Task == "slider") |>
  transmute(Participant.Private.ID, Experiment.Name, Trial.Number,
            slider_reliability = as.integer(str_match(Stimulus, "(\\d+)% reliable")[, 2]),
            slider_response    = Response)

stopifnot(
  "Non-numeric Response or Correct.Response in response rows" =
    !anyNA(responses$Response) && !anyNA(responses$Correct.Response),
  "Slider reliability could not be read from Stimulus" =
    !anyNA(sliders$slider_reliability)
)

# ---- 3. Sample features and optimal observer ---------------------------------

features <- map(responses$Stimulus.Info, parse_stimulus_info) |> list_rbind()

log_odds <- map_dbl(features$blue_rel, \(p) round(sum(log(p / (1 - p))), digits = 6))

optimal_choice <- as.numeric(log_odds > 0)   # 1 = blue
is_tie <- log_odds == 0
set.seed(SEED)
optimal_choice[is_tie] <- sample(c(0, 1), sum(is_tie), replace = TRUE)

# ---- 4. Assemble one row per response trial ----------------------------------

data_processed <- responses |>
  mutate(Optimalchoice  = optimal_choice,
         PosteriorBlue  = log_odds,
         accuracy_bayes = as.numeric(Correct.Response == optimal_choice)) |>
  bind_cols(features) |>
  # each response trial must have exactly one slider with the same Trial.Number
  inner_join(sliders, by = c("Participant.Private.ID", "Experiment.Name", "Trial.Number"),
             relationship = "one-to-one", unmatched = "error") |>
  mutate(Net.Blue.50 = blue_50 - red_50,
         Net.Blue.55 = blue_55 - red_55,
         Net.Blue.65 = blue_65 - red_65) |>
  select(Participant.Private.ID, randomiser.475f, Stimulus, Response, Stimulus.Info, Task,
         Correct.Response, Accuracy, Optimalchoice, PosteriorBlue, accuracy_bayes,
         Experiment.Name, Done.by, slider_reliability, slider_response,
         blue_rel, blue_50, blue_55, blue_65, red_50, red_55, red_65,
         Net.Blue.50, Net.Blue.55, Net.Blue.65, Trial.Number)

# ---- 5. Summaries ------------------------------------------------------------

performance <- summarise_performance(data_processed, participants)

likelihood       <- summarise_likelihood(data_processed, participants)
likelihood_blue  <- likelihood$likelihood_blue
likelihood_final <- likelihood$likelihood_final

# ---- 6. Save -----------------------------------------------------------------

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
save(data_processed,   file = file.path(OUTPUT_DIR, "data_preprocessed.rdata"))
save(performance,      file = file.path(OUTPUT_DIR, "performance.rdata"))
# save(likelihood_blue,  file = file.path(OUTPUT_DIR, "likelihood_blue.rdata"))
# save(likelihood_final, file = file.path(OUTPUT_DIR, "likelihood_final.rdata"))
# save(feedback_data,    file = file.path(OUTPUT_DIR, "feedback_data.rdata"))