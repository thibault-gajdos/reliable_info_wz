## ============================================================
## Simulation to verify the learning model
## ============================================================

set.seed(1)


## ============================================================
## Settings
## ============================================================

params <- list(
  delta      = 0.02,             # learning rate
  theta      = 1,                # inverse temperature of the choice rule
  lambda     = 0,                # recency weighting of samples (0 = equal weights)
  n_trial    = 9600,
  n_info     = 6,                # samples per trial
  levels     = c(50, 55, 65),    # sample reliability (% of samples matching the stimulus)
  init_value = 0.1,              # starting value for every level
  
  # What the choice is compared against to give reward:
  #   "optimal"      - the ideal observer's answer
  #   "correct"      - the true stimulus
  #   "optimal_prob" - to be implemented
  feedback_target = "optimal",
  
  # What the prediction error subtracts from the reward:
  #   "choice" - posterior belief in the chosen colour
  #   "policy" - belief averaged over the choice policy:
  #              belief_blue * choice_prob_blue + (1 - belief_blue) * (1 - choice_prob_blue)
  expectation_source = "policy"
)

# Plot colours per level. Levels without an entry get an automatic colour.
level_colors <- c("50" = "#0099FF", "55" = "#00CC33", "65" = "#009933")


## ============================================================
## Trial generation
## ============================================================

# Returns the stimuli for all trials at once:
#   stimulus  - true colour per trial
#   color_mat - n_trial x n_info matrix of sample colours
#   level_mat - n_trial x n_info matrix of sample levels
#   optimal   - ideal observer's answer (ties broken at random)
generate_trials <- function(n_trial, n_info, levels) {
  
  stimulus    <- sample(c("Red", "Blue"), n_trial, replace = TRUE)
  false_color <- ifelse(stimulus == "Red", "Blue", "Red")
  
  level_mat <- matrix(sample(levels, n_trial * n_info, replace = TRUE), nrow = n_trial)
  
  # Each sample shows the true colour with probability level / 100
  matches   <- matrix(runif(n_trial * n_info), nrow = n_trial) < level_mat / 100
  color_mat <- ifelse(matches, stimulus, false_color)
  
  # Ideal observer: sum of signed log-odds (positive favours Blue)
  sign_blue  <- ifelse(color_mat == "Blue", 1, -1)
  ideal_diff <- rowSums(sign_blue * qlogis(level_mat / 100))
  
  is_tie  <- abs(ideal_diff) < 1e-10   # tolerance avoids floating-point false non-ties
  optimal <- ifelse(ideal_diff > 0, "Blue", "Red")
  optimal[is_tie] <- sample(c("Blue", "Red"), sum(is_tie), replace = TRUE)
  
  list(
    stimulus  = stimulus,
    color_mat = color_mat,
    level_mat = level_mat,
    optimal   = optimal
  )
}


## ============================================================
## Learning model
## ============================================================

simulate_learning <- function(params, trials) {
  
  ## --- Check settings ---------------------------------------
  
  if (ncol(trials$color_mat) != params$n_info) {
    stop("trials were generated with a different n_info than params$n_info")
  }
  
  if (params$feedback_target == "optimal_prob") {
    stop("feedback_target = 'optimal_prob' is to be implemented")
  }
  
  target <- switch(params$feedback_target,
                   optimal = trials$optimal,
                   correct = trials$stimulus,
                   stop("Unknown feedback_target: ", params$feedback_target)
  )
  
  if (!params$expectation_source %in% c("choice", "policy")) {
    stop("Unknown expectation_source: ", params$expectation_source)
  }
  
  ## --- Setup ------------------------------------------------
  
  n_trial     <- length(trials$stimulus)
  level_names <- as.character(params$levels)
  
  # One value per level, labelled by level
  V <- setNames(rep(params$init_value, length(level_names)), level_names)
  
  # Weight of each sample position (the last sample has weight 1)
  discount <- exp(params$lambda * (seq_len(params$n_info) - params$n_info))
  
  # Preallocated outputs
  value_hist  <- matrix(NA_real_, n_trial, length(level_names),
                        dimnames = list(NULL, level_names))
  credit_hist <- value_hist
  
  choice                <- character(n_trial)
  reward                <- numeric(n_trial)
  expected_reward       <- numeric(n_trial)
  pe                    <- numeric(n_trial)
  blue_evidence         <- numeric(n_trial)
  red_evidence          <- numeric(n_trial)
  posterior_belief_blue <- numeric(n_trial)
  choice_prob_blue      <- numeric(n_trial)
  
  ## --- Trial loop -------------------------------------------
  
  for (t in seq_len(n_trial)) {
    
    value_hist[t, ] <- V
    
    sample_levels <- as.character(trials$level_mat[t, ])
    sample_colors <- trials$color_mat[t, ]
    
    ## Evidence accumulation
    weighted_values  <- discount * V[sample_levels]
    blue_evidence[t] <- sum(weighted_values[sample_colors == "Blue"])
    red_evidence[t]  <- sum(weighted_values[sample_colors == "Red"])
    evidence_diff    <- blue_evidence[t] - red_evidence[t]
    
    ## Belief and choice
    # posterior_belief_blue is not affected by theta; choice_prob_blue is
    posterior_belief_blue[t] <- plogis(evidence_diff)
    choice_prob_blue[t]      <- plogis(params$theta * evidence_diff)
    
    choice[t] <- if (runif(1) < choice_prob_blue[t]) "Blue" else "Red"
    
    ## Reward
    reward[t] <- as.numeric(choice[t] == target[t])
    
    ## Prediction error
    b <- posterior_belief_blue[t]
    c_prob <- choice_prob_blue[t]
    
    expected_reward[t] <- switch(params$expectation_source,
                                 choice = if (choice[t] == "Blue") b else 1 - b,
                                 policy = b * c_prob + (1 - b) * (1 - c_prob)
    )
    
    pe[t] <- reward[t] - expected_reward[t]
    
    ## Credit assignment
    # +1 for each sample agreeing with the choice, -1 otherwise, summed per level
    contribution <- ifelse(sample_colors == choice[t], 1, -1)
    credit <- vapply(level_names,
                     function(l) sum(contribution[sample_levels == l]),
                     numeric(1))
    
    total_credit <- sum(abs(credit))
    if (total_credit > 0) credit <- credit / total_credit
    
    credit_hist[t, ] <- credit
    
    ## Value update
    V <- V + params$delta * credit * pe[t]
  }
  
  ## --- Assemble output (wide format) ------------------------
  
  n_info <- params$n_info
  
  out <- data.frame(
    Trial    = seq_len(n_trial),
    Stimulus = trials$stimulus,
    setNames(as.data.frame(trials$color_mat), paste0("Color", seq_len(n_info))),
    setNames(as.data.frame(trials$level_mat), paste0("Prob",  seq_len(n_info))),
    Optimal  = trials$optimal,
    setNames(as.data.frame(value_hist), paste0("Value", level_names)),
    Choice              = choice,
    Reward              = reward,
    ExpectedReward      = expected_reward,
    PE                  = pe,
    BlueEvidence        = blue_evidence,
    RedEvidence         = red_evidence,
    PosteriorBeliefBlue = posterior_belief_blue,
    ChoiceProbBlue      = choice_prob_blue,
    setNames(as.data.frame(credit_hist), paste0("Credit", level_names)),
    OptimalAccuracy     = as.numeric(choice == trials$optimal),
    ChoiceAccuracy      = as.numeric(choice == trials$stimulus),
    stringsAsFactors    = FALSE
  )
  
  out
}


## ============================================================
## Plotting helpers
## ============================================================

get_level_colors <- function(level_names) {
  cols <- unname(level_colors[level_names])
  missing <- is.na(cols)
  if (any(missing)) cols[missing] <- hcl.colors(sum(missing), "Dark 3")
  setNames(cols, level_names)
}

# Trailing rolling mean: value at t is the mean of x[(t - window + 1):t]
roll_mean <- function(x, window) {
  as.numeric(stats::filter(x, rep(1 / window, window), sides = 1))
}

# Learned values per level, with the ideal log-odds as dotted lines
plot_values <- function(df, levels) {
  level_names <- as.character(levels)
  cols    <- get_level_colors(level_names)
  targets <- qlogis(levels / 100)
  value_cols <- paste0("Value", level_names)
  
  y_range <- range(unlist(df[value_cols]), targets, na.rm = TRUE)
  
  plot(NA, xlim = range(df$Trial), ylim = y_range, xlab = "Trial", ylab = "Value")
  for (i in seq_along(level_names)) {
    lines(df$Trial, df[[value_cols[i]]], col = cols[i])
    abline(h = targets[i], col = cols[i], lty = 3)
  }
  legend("topleft", legend = value_cols, col = cols, lty = 1)
}

# Ratio of two learned values against the ideal log-odds ratio
plot_value_ratio <- function(df, numerator, denominator) {
  ratio  <- df[[paste0("Value", numerator)]] / df[[paste0("Value", denominator)]]
  target <- qlogis(numerator / 100) / qlogis(denominator / 100)
  
  plot(df$Trial, ratio, type = "l", ylim = range(ratio, target, na.rm = TRUE),
       xlab = "Trial", ylab = paste0("Value", numerator, " / Value", denominator))
  abline(h = target, lty = 3)
}

# Rolling accuracy against the optimal answer and against the true stimulus
plot_accuracy <- function(df, window = 30) {
  plot(df$Trial, roll_mean(df$ChoiceAccuracy, window), type = "l", col = "red",
       ylim = c(0, 1), xlab = "Trial (window end)", ylab = "Accuracy")
  lines(df$Trial, roll_mean(df$OptimalAccuracy, window), col = "blue")
  legend("bottomright", legend = c("Optimality", "Choice"),
         col = c("blue", "red"), lty = 1)
}

# Posterior belief in Blue, coloured by the optimal answer
plot_belief_by_optimal <- function(df) {
  is_blue <- df$Optimal == "Blue"
  plot(df$Trial[is_blue], df$PosteriorBeliefBlue[is_blue], col = "blue",
       ylim = c(0, 1), xlab = "Trial", ylab = "Posterior belief (Blue)")
  points(df$Trial[!is_blue], df$PosteriorBeliefBlue[!is_blue], col = "red")
}

# Credit per level over trials; optionally multiplied by the PE
plot_credit <- function(df, levels, xlim, weight_by_pe = FALSE) {
  level_names <- as.character(levels)
  cols <- get_level_colors(level_names)
  weight <- if (weight_by_pe) df$PE else 1
  
  plot(NA, xlim = xlim, ylim = c(-1, 1), xlab = "Trial",
       ylab = if (weight_by_pe) "Credit x PE" else "Credit")
  for (i in seq_along(level_names)) {
    y <- df[[paste0("Credit", level_names[i])]] * weight
    lines(df$Trial, y, type = "o", col = cols[i])
  }
  legend("topright", legend = paste0("Credit", level_names), col = cols, lty = 1)
}


## ============================================================
## Run
## ============================================================

trials <- generate_trials(params$n_trial, params$n_info, params$levels)
sim_df <- simulate_learning(params, trials)

# Final values
final_values <- unlist(sim_df[nrow(sim_df), paste0("Value", params$levels)])
print(final_values)

# Learned vs ideal ratio for the two highest levels
top_two <- sort(params$levels, decreasing = TRUE)[1:2]
print(c(
  ideal   = qlogis(top_two[1] / 100) / qlogis(top_two[2] / 100),
  learned = final_values[[paste0("Value", top_two[1])]] /
    final_values[[paste0("Value", top_two[2])]]
))


## ============================================================
## Plots
## ============================================================

par(mfrow = c(1, 3))
plot_values(sim_df, params$levels)
plot_value_ratio(sim_df, top_two[1], top_two[2])
plot_accuracy(sim_df, window = 30)

par(mfrow = c(1, 3))
hist(sim_df$PE, breaks = 30, main = "Prediction error", xlab = "PE")
plot(sim_df$Trial, sim_df$PE, xlab = "Trial", ylab = "PE")
plot_belief_by_optimal(sim_df)

par(mfrow = c(1, 3))
plot_credit(sim_df, params$levels, xlim = c(0, 20))
plot_credit(sim_df, params$levels, xlim = c(0, 20), weight_by_pe = TRUE)
plot_credit(sim_df, "55", xlim = c(0, 2000), weight_by_pe = TRUE)

