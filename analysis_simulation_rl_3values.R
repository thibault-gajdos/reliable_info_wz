# =============================================================================
# Full Pipeline — Bayesian RL Model Simulation and Visualisation
# =============================================================================

library(dplyr)
library(tidyr)
library(jsonlite)
library(posterior)
library(readr)

load('./results/data_preprocessed.rdata')
load('./results/performance.rdata')

# =============================================================================
# Module 1 — Extract trials per participant, per experiment
# =============================================================================

experiments <- unique(data_processed$Done.by)

trials_nested <- lapply(setNames(experiments, experiments), function(exp) {
  
  df_exp <- data_processed |> filter(Done.by == exp)
  participants <- unique(df_exp$Participant.Private.ID)
  
  lapply(setNames(participants, participants), function(pid) {
    
    df_p <- df_exp |> filter(Participant.Private.ID == pid)
    
    parsed <- lapply(seq_len(nrow(df_p)), function(t) {
      d       <- fromJSON(df_p$Stimulus.Info[t])
      levels  <- as.integer(d$reliability * 100)
      colours <- ifelse(d$color == "blue", "Blue", "Red")
      color_wide <- setNames(as.list(colours), paste0("Color", 1:6))
      prob_wide  <- setNames(as.list(levels),  paste0("Prob",  1:6))
      as.data.frame(c(color_wide, prob_wide))
    })
    
    parsed_df <- bind_rows(parsed)
    
    bind_cols(
      df_p |> select(Participant.Private.ID, Done.by,
                     Correct.Response, Optimalchoice) |>
        mutate(
          trial            = seq_len(n()),
          Correct.Response = ifelse(Correct.Response == 1, "Blue", "Red"),
          Optimalchoice    = ifelse(Optimalchoice    == 1, "Blue", "Red")
        ),
      parsed_df
    )
  })
})

# --- Sanity check ---
cat("Experiments found:", paste(names(trials_nested), collapse = ", "), "\n")
for (exp in names(trials_nested)) {
  cat(sprintf("  %s — %d participants, %d trials for first participant\n",
              exp,
              length(trials_nested[[exp]]),
              nrow(trials_nested[[exp]][[1]])))
}


# =============================================================================
# Load fits
# =============================================================================

exp_list   <- c("Alex", "Divyaj", "Exp6", "mitigation3")
exp_labels <- c("Mitigation 1", "Mitigation 2", "Control", "Mitigation 3")

model_config <- list(
  base = list(
    file        = "rl.rds",
    indiv_pars  = c("delta", "theta"),
    theta_fixed = NA,
    has_lambda  = FALSE
  ),
  t1 = list(
    file        = "rl_theta1.rds",
    indiv_pars  = c("delta"),
    theta_fixed = 1,
    has_lambda  = FALSE
  ),
  t5 = list(
    file        = "rl_theta5.rds",
    indiv_pars  = c("delta"),
    theta_fixed = 5,
    has_lambda  = FALSE
  ),
  t1s = list(
    file        = "rl_theta1_seq.rds",
    indiv_pars  = c("delta", "lambda"),
    theta_fixed = 1,
    has_lambda  = TRUE
  ),
  t5s = list(
    file        = "rl_theta5_seq.rds",
    indiv_pars  = c("delta", "lambda"),
    theta_fixed = 5,
    has_lambda  = TRUE
  ),
  t3s = list(
    file        = "rl_theta3_seq.rds",
    indiv_pars  = c("delta", "lambda"),
    theta_fixed = 3,
    has_lambda  = TRUE
  )
)


# =============================================================================
# Module 2 — Extract posterior parameters per experiment, per model variant
# =============================================================================

extract_params <- function(fit, model_variant, exp) {
  
  cfg <- model_config[[model_variant]]
  
  indiv_draws <- fit$draws(variables = cfg$indiv_pars) |>
    as_draws_df() |>
    summarise_draws(mean) |>
    filter(grepl(
      paste0("^(", paste(cfg$indiv_pars, collapse = "|"), ")\\["),
      variable
    )) |>
    mutate(
      param = sub("\\[.*", "", variable),
      n     = as.integer(sub(".*\\[(\\d+)\\]", "\\1", variable))
    ) |>
    select(param, n, mean) |>
    pivot_wider(names_from = param, values_from = mean)
  
  pid_order <- unique(data_processed |>
                        filter(Done.by == exp) |>
                        pull(Participant.Private.ID))
  
  indiv_draws <- indiv_draws |>
    mutate(Participant.Private.ID = pid_order[n]) |>
    select(n, Participant.Private.ID, any_of(c("delta", "theta", "lambda")))
  
  if (!is.na(cfg$theta_fixed)) indiv_draws$theta  <- cfg$theta_fixed
  if (!cfg$has_lambda)         indiv_draws$lambda <- 0
  
  indiv_draws <- indiv_draws |>
    select(n, Participant.Private.ID, delta, theta, lambda)
  
  group_means <- fit$draws(variables = c("V50_init", "V55_init", "V65_init")) |>
    as_draws_df() |>
    summarise_draws(mean) |>
    { function(d) setNames(d$mean, d$variable) }()
  
  list(indiv = indiv_draws, group = group_means)
}

params_nested <- lapply(names(model_config), function(model_variant) {
  
  cfg <- model_config[[model_variant]]
  cat(sprintf("\nExtracting parameters for model: %s (%s)\n",
              model_variant, cfg$file))
  
  exp_params <- lapply(exp_list, function(exp) {
    
    fit_path <- file.path(".", "results", exp, cfg$file)
    
    if (!file.exists(fit_path)) {
      warning(sprintf("Fit not found: %s — skipping.", fit_path))
      return(NULL)
    }
    
    cat(sprintf("  Loading %s ...\n", fit_path))
    fit    <- read_rds(fit_path, refhook = NULL)
    params <- extract_params(fit, model_variant, exp)
    rm(fit); gc()
    params
  })
  
  names(exp_params) <- exp_list
  exp_params
})

names(params_nested) <- names(model_config)

# --- Sanity check ---
for (model_variant in names(params_nested)) {
  cat(sprintf("\n===== %s =====\n", model_variant))
  for (exp in exp_list) {
    p <- params_nested[[model_variant]][[exp]]
    if (is.null(p)) { cat(sprintf("  %s: MISSING\n", exp)); next }
    indiv <- p$indiv; group <- p$group
    cat(sprintf("  %s — N: %d\n", exp, nrow(indiv)))
    cat(sprintf("    delta  mean: %.3f  range: [%.3f, %.3f]\n",
                mean(indiv$delta), min(indiv$delta), max(indiv$delta)))
    cat(sprintf("    theta  mean: %.3f  range: [%.3f, %.3f]\n",
                mean(indiv$theta), min(indiv$theta), max(indiv$theta)))
    cat(sprintf("    lambda mean: %.3f  range: [%.3f, %.3f]\n",
                mean(indiv$lambda), min(indiv$lambda), max(indiv$lambda)))
    cat(sprintf("    V50_init: %.3f  V55_init: %.3f  V65_init: %.3f\n",
                group["V50_init"], group["V55_init"], group["V65_init"]))
  }
}


# =============================================================================
# Module 3 — Simulation across all model variants and experiments
# =============================================================================

feedback_map <- c(
  Alex        = "stimulus",
  Exp6        = "stimulus",
  Divyaj      = "optimal",
  mitigation3 = "optimal"
)

simulate_participant <- function(trials_n, delta, theta, lambda,
                                 V50_init, V55_init, V65_init,
                                 feedback_type) {
  
  n_trials <- nrow(trials_n)
  Choice   <- character(n_trials)
  V50_rec  <- numeric(n_trials)
  V55_rec  <- numeric(n_trials)
  V65_rec  <- numeric(n_trials)
  PE_rec   <- numeric(n_trials)
  
  V50 <- V50_init; V55 <- V55_init; V65 <- V65_init
  
  for (t in seq_len(n_trials)) {
    
    V50_rec[t] <- V50; V55_rec[t] <- V55; V65_rec[t] <- V65
    
    blueEvidence <- 0; redEvidence <- 0
    
    for (i in 1:6) {
      level    <- trials_n[[paste0("Prob",  i)]][t]
      color    <- trials_n[[paste0("Color", i)]][t]
      value    <- switch(as.character(level), "50" = V50, "55" = V55, "65" = V65)
      discount <- exp(lambda * (i - 6))
      
      if (color == "Blue") {
        blueEvidence <- blueEvidence + discount * value
      } else {
        redEvidence  <- redEvidence  + discount * value
      }
    }
    
    pBlue_PE       <- plogis(blueEvidence - redEvidence)
    pBlue_decision <- plogis((blueEvidence - redEvidence) * theta)
    
    choice    <- sample(c("Blue", "Red"), 1,
                        prob = c(pBlue_decision, 1 - pBlue_decision))
    Choice[t] <- choice
    
    feedback_col <- if (feedback_type == "stimulus") {
      trials_n$Correct.Response[t]
    } else {
      trials_n$Optimalchoice[t]
    }
    
    reward   <- if (choice == feedback_col) 1.0 else 0.0
    pCorrect <- if (choice == "Blue") pBlue_PE else 1 - pBlue_PE
    PE       <- reward - pCorrect
    PE_rec[t] <- PE
    
    credit50 <- 0; credit55 <- 0; credit65 <- 0
    
    for (i in 1:6) {
      level        <- trials_n[[paste0("Prob",  i)]][t]
      color        <- trials_n[[paste0("Color", i)]][t]
      contribution <- if (color == choice) 1.0 else -1.0
      
      if (level == 50) {
        credit50 <- credit50 + contribution
      } else if (level == 55) {
        credit55 <- credit55 + contribution
      } else {
        credit65 <- credit65 + contribution
      }
    }
    
    totalCredit <- abs(credit50) + abs(credit55) + abs(credit65)
    if (totalCredit > 0) {
      credit50 <- credit50 / totalCredit
      credit55 <- credit55 / totalCredit
      credit65 <- credit65 / totalCredit
    }
    
    V50 <- max(-10, min(10, V50 + delta * credit50 * PE))
    V55 <- max(-10, min(10, V55 + delta * credit55 * PE))
    V65 <- max(-10, min(10, V65 + delta * credit65 * PE))
    
  } # end trial loop
  
  data.frame(
    Participant.Private.ID = trials_n$Participant.Private.ID[1],
    Done.by                = trials_n$Done.by[1],
    trial                  = seq_len(n_trials),
    Choice                 = Choice,
    Value50                = V50_rec,
    Value55                = V55_rec,
    Value65                = V65_rec,
    PE                     = PE_rec,
    CorrectAccuracy        = as.integer(Choice == trials_n$Correct.Response),
    OptimalAccuracy        = as.integer(Choice == trials_n$Optimalchoice)
  )
}

sim_results <- lapply(names(params_nested), function(model_variant) {
  
  cat(sprintf("\nSimulating model: %s\n", model_variant))
  
  exp_results <- lapply(exp_list, function(exp) {
    
    cat(sprintf("  Experiment: %s\n", exp))
    p <- params_nested[[model_variant]][[exp]]
    if (is.null(p)) {
      warning(sprintf("No params for %s / %s — skipping.", model_variant, exp))
      return(NULL)
    }
    
    feedback_type <- feedback_map[exp]
    group         <- p$group
    indiv         <- p$indiv
    
    participant_dfs <- lapply(names(trials_nested[[exp]]), function(pid) {
      trials_n <- trials_nested[[exp]][[pid]]
      params_n <- indiv |> filter(Participant.Private.ID == as.integer(pid))
      simulate_participant(
        trials_n      = trials_n,
        delta         = params_n$delta,
        theta         = params_n$theta,
        lambda        = params_n$lambda,
        V50_init      = group["V50_init"],
        V55_init      = group["V55_init"],
        V65_init      = group["V65_init"],
        feedback_type = feedback_type
      )
    })
    
    bind_rows(participant_dfs)
  })
  
  names(exp_results) <- exp_list
  exp_results
})

names(sim_results) <- names(params_nested)

# --- Sanity check ---
for (model_variant in names(sim_results)) {
  cat(sprintf("\n===== %s =====\n", model_variant))
  for (exp in exp_list) {
    df <- sim_results[[model_variant]][[exp]]
    if (is.null(df)) { cat(sprintf("  %s: MISSING\n", exp)); next }
    cat(sprintf("  %s — Rows: %d  Participants: %d\n",
                exp, nrow(df), n_distinct(df$Participant.Private.ID)))
    cat(sprintf("    Mean CorrectAccuracy: %.3f  OptimalAccuracy: %.3f\n",
                mean(df$CorrectAccuracy, na.rm = TRUE),
                mean(df$OptimalAccuracy, na.rm = TRUE)))
    cat(sprintf("    V50 final: %.3f  V55: %.3f  V65: %.3f\n",
                mean(df |> group_by(Participant.Private.ID) |> slice_tail(n=1) |> pull(Value50)),
                mean(df |> group_by(Participant.Private.ID) |> slice_tail(n=1) |> pull(Value55)),
                mean(df |> group_by(Participant.Private.ID) |> slice_tail(n=1) |> pull(Value65))))
  }
}


# =============================================================================
# Module 4 — Organise and merge simulated output
# =============================================================================

sim_per_model <- lapply(names(sim_results), function(model_variant) {
  
  df     <- bind_rows(sim_results[[model_variant]])
  suffix <- model_variant
  
  df |>
    rename_with(
      ~ paste0("Sim.", ., ".", suffix),
      .cols = c(Choice, CorrectAccuracy, OptimalAccuracy,
                Value50, Value55, Value65, PE)
    ) |>
    select(Participant.Private.ID, Done.by, trial, starts_with("Sim."))
})

names(sim_per_model) <- names(sim_results)

data_processed_indexed <- data_processed |>
  group_by(Participant.Private.ID, Done.by) |>
  mutate(trial = seq_len(n())) |>
  ungroup()

data_simulated <- data_processed_indexed
for (model_variant in names(sim_per_model)) {
  data_simulated <- data_simulated |>
    left_join(sim_per_model[[model_variant]],
              by = c("Participant.Private.ID", "Done.by", "trial"))
}

# --- Sanity check ---
cat("Original rows:  ", nrow(data_processed), "\n")
cat("Simulated rows: ", nrow(data_simulated),  "\n")
new_cols <- setdiff(colnames(data_simulated), colnames(data_processed))
cat("\nNAs in simulated columns (should be 0):\n")
print(colSums(is.na(data_simulated[new_cols])))

cat("\nPer-experiment mean CorrectAccuracy by model variant:\n")
for (model_variant in names(sim_results)) {
  col <- paste0("Sim.CorrectAccuracy.", model_variant)
  cat(sprintf("\n  %s\n", model_variant))
  data_simulated |>
    group_by(Done.by) |>
    summarise(Mean = mean(.data[[col]], na.rm = TRUE), .groups = "drop") |>
    print()
}

save(data_simulated, file = './results/data_simulated.rdata')


# =============================================================================
# Visualisation setup
# =============================================================================
# Change this one line to switch between: "base", "t1", "t5", "t1s", "t5s", "t3s"
load('./results/data_simulated.rdata')
model_variant <- "t3s"

suffix <- model_variant
data_simulated_plot <- data_simulated |>
  rename(
    Sim.CorrectAccuracy = paste0("Sim.CorrectAccuracy.", suffix),
    Sim.OptimalAccuracy = paste0("Sim.OptimalAccuracy.", suffix),
    Sim.Value50         = paste0("Sim.Value50.",         suffix),
    Sim.Value55         = paste0("Sim.Value55.",         suffix),
    Sim.Value65         = paste0("Sim.Value65.",         suffix),
    Sim.PE              = paste0("Sim.PE.",               suffix),
    Sim.Choice          = paste0("Sim.Choice.",           suffix)
  )

exp_list_plot  <- c("Exp6", "Alex", "Divyaj", "mitigation3")
exp_labels     <- c("Control", "Mitigation 1", "Mitigation 2", "Mitigation 3")

col_correct <- "#2166AC"
col_optimal <- "#D6604D"
col_50      <- "#99CCFF"
col_55      <- "#99DD99"
col_65      <- "#338833"
col_levels  <- c("50" = col_50, "55" = col_55, "65" = col_65)

performance$Participant.Private.ID <- as.integer(performance$Participant.Private.ID)

performance_simulated <- performance |>
  left_join(
    data_simulated_plot |>
      group_by(Participant.Private.ID) |>
      summarise(
        Sim.CorrectAccuracy = mean(Sim.CorrectAccuracy, na.rm = TRUE),
        Sim.OptimalAccuracy = mean(Sim.OptimalAccuracy, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "Participant.Private.ID"
  )


# =============================================================================
# Performance plots — A: Scatter
# =============================================================================

plot_scatter <- function(performance, metric_real, metric_sim,
                         metric_label, point_col) {
  
  par(mfrow = c(1, 4), oma = c(0, 0, 4, 0), mar = c(4, 4, 3, 1))
  
  for (k in seq_along(exp_list_plot)) {
    df   <- performance[performance$Done.by == exp_list_plot[k], ]
    real <- df[[metric_real]]
    sim  <- df[[metric_sim]]
    lims <- c(0.4, 1)
    
    plot(real, sim,
         xlim = lims, ylim = lims,
         pch  = 19, col = adjustcolor(point_col, alpha.f = 0.7),
         xlab = "Real", ylab = "Simulated",
         main = exp_labels[k])
    
    abline(a = 0, b = 1, lty = 2, col = "gray40")
    r <- cor(real, sim, use = "complete.obs")
    legend("topleft", legend = sprintf("r = %.2f", r), bty = "n", cex = 0.85)
  }
  
  mtext(sprintf("Scatter: Real vs Simulated — %s", metric_label),
        outer = TRUE, cex = 1.3, font = 2)
}

plot_scatter(performance_simulated, "Accuracy",       "Sim.CorrectAccuracy",
             "Correct Accuracy", col_correct)
plot_scatter(performance_simulated, "Optimal_Choice", "Sim.OptimalAccuracy",
             "Optimal Accuracy", col_optimal)


# =============================================================================
# Performance plots — B: Paired
# =============================================================================

plot_paired <- function(performance, metric_real, metric_sim,
                        metric_label, point_col) {
  
  par(mfrow = c(1, 4), oma = c(0, 0, 4, 0), mar = c(4, 4, 3, 1))
  
  for (k in seq_along(exp_list_plot)) {
    df   <- performance[performance$Done.by == exp_list_plot[k], ]
    df   <- df[order(df[[metric_real]]), ]
    real <- df[[metric_real]]
    sim  <- df[[metric_sim]]
    x    <- seq_along(real)
    
    plot(x, real,
         type = "p", pch = 19, col = point_col,
         ylim = c(0.5, 1),
         xlab = "Participants (sorted by real)",
         ylab = metric_label,
         main = exp_labels[k])
    
    points(x, sim, pch = 1, col = point_col)
    segments(x, real, x, sim,
             col = adjustcolor(point_col, alpha.f = 0.3), lwd = 0.8)
    abline(h = mean(real), lty = 2, col = "gray40")
    abline(h = mean(sim),  lty = 3, col = point_col)
    
    if (k == 1) {
      legend("topleft", legend = c("Real", "Simulated"),
             pch = c(19, 1), col = point_col, bty = "n", cex = 0.85)
    }
  }
  
  mtext(sprintf("Paired: Real vs Simulated — %s", metric_label),
        outer = TRUE, cex = 1.3, font = 2)
}

plot_paired(performance_simulated, "Accuracy",       "Sim.CorrectAccuracy",
            "Correct Accuracy", col_correct)
plot_paired(performance_simulated, "Optimal_Choice", "Sim.OptimalAccuracy",
            "Optimal Accuracy", col_optimal)


# =============================================================================
# Performance plots — D: Bland-Altman
# =============================================================================

plot_bland_altman <- function(performance, metric_real, metric_sim,
                              metric_label, point_col) {
  
  par(mfrow = c(1, 4), oma = c(0, 0, 4, 0), mar = c(4, 4, 3, 1))
  
  for (k in seq_along(exp_list_plot)) {
    df   <- performance[performance$Done.by == exp_list_plot[k], ]
    real <- df[[metric_real]]
    sim  <- df[[metric_sim]]
    avg  <- (real + sim) / 2
    diff <- real - sim
    
    mean_diff <- mean(diff, na.rm = TRUE)
    sd_diff   <- sd(diff,   na.rm = TRUE)
    loa_upper <- mean_diff + 1.96 * sd_diff
    loa_lower <- mean_diff - 1.96 * sd_diff
    ylims     <- c(min(loa_lower, diff) - 0.02, max(loa_upper, diff) + 0.02)
    
    plot(avg, diff,
         pch = 19, col = adjustcolor(point_col, alpha.f = 0.7),
         xlim = c(0.4, 1), ylim = ylims,
         xlab = "Average of real and simulated",
         ylab = "Real - Simulated",
         main = exp_labels[k])
    
    abline(h = 0,         lty = 2, col = "gray40")
    abline(h = mean_diff, lty = 1, col = point_col)
    abline(h = loa_upper, lty = 3, col = point_col)
    abline(h = loa_lower, lty = 3, col = point_col)
    
    legend("topleft",
           legend = c(sprintf("Mean diff = %.3f", mean_diff),
                      sprintf("+-1.96 SD = [%.3f, %.3f]", loa_lower, loa_upper)),
           bty = "n", cex = 0.75)
  }
  
  mtext(sprintf("Bland-Altman: Real vs Simulated — %s", metric_label),
        outer = TRUE, cex = 1.3, font = 2)
}

plot_bland_altman(performance_simulated, "Accuracy",       "Sim.CorrectAccuracy",
                  "Correct Accuracy", col_correct)
plot_bland_altman(performance_simulated, "Optimal_Choice", "Sim.OptimalAccuracy",
                  "Optimal Accuracy", col_optimal)


# =============================================================================
# Value evolution helper
# =============================================================================

get_V_long <- function(exp) {
  data_simulated_plot |>
    filter(Done.by == exp) |>
    select(Participant.Private.ID, trial, Sim.Value50, Sim.Value55, Sim.Value65) |>
    pivot_longer(cols = c(Sim.Value50, Sim.Value55, Sim.Value65),
                 names_to  = "Level",
                 values_to = "Value") |>
    mutate(Level = recode(Level,
                          Sim.Value50 = "50",
                          Sim.Value55 = "55",
                          Sim.Value65 = "65"))
}


# =============================================================================
# Value evolution — Plot 1: Full trial-by-trial
# =============================================================================

plot_evolution_full <- function() {
  
  # universal y limits across all experiments and reliability levels
  all_vals <- bind_rows(lapply(exp_list_plot, get_V_long))
  ylims    <- range(all_vals$Value, na.rm = TRUE)
  
  par(mfrow = c(1, 4), oma = c(0, 0, 4, 0), mar = c(4, 4, 3, 1))
  
  for (k in seq_along(exp_list_plot)) {
    exp      <- exp_list_plot[k]
    df       <- get_V_long(exp)
    n_trials <- max(df$trial)
    
    plot(NULL, xlim = c(1, n_trials), ylim = ylims,
         xlab = "Trial", ylab = "V value", main = exp_labels[k])
    
    for (lvl in c("50", "55", "65")) {
      df_lvl <- df |> filter(Level == lvl)
      pids   <- unique(df_lvl$Participant.Private.ID)
      
      for (pid in pids) {
        df_pid <- df_lvl |> filter(Participant.Private.ID == pid)
        lines(df_pid$trial, df_pid$Value,
              col = adjustcolor(col_levels[lvl], alpha.f = 0.2), lwd = 0.6)
      }
      
      mean_line <- df_lvl |>
        group_by(trial) |>
        summarise(mean_V = mean(Value, na.rm = TRUE), .groups = "drop")
      lines(mean_line$trial, mean_line$mean_V, col = col_levels[lvl], lwd = 2.5)
    }
    
    if (k == 1) {
      legend("topleft", legend = c("V50", "V55", "V65"),
             col = col_levels, lty = 1, lwd = 2, bty = "n", cex = 0.85)
    }
  }
  
  mtext("V Value Evolution — All Trials", outer = TRUE, cex = 1.3, font = 2)
}

plot_evolution_full()


# =============================================================================
# Value evolution — Plot 2: Initial -> Last paired
# =============================================================================

plot_evolution_paired <- function() {
  
  # universal y limits
  all_endpoints <- bind_rows(lapply(exp_list_plot, function(exp) {
    get_V_long(exp) |>
      group_by(Participant.Private.ID, Level) |>
      summarise(Initial = first(Value), Last = last(Value), .groups = "drop")
  }))
  ylims <- c(min(all_endpoints$Initial, all_endpoints$Last) - 0.05,
             max(all_endpoints$Initial, all_endpoints$Last) + 0.05)
  
  par(mfrow = c(1, 4), oma = c(0, 0, 4, 0), mar = c(4, 4, 3, 1))
  
  for (k in seq_along(exp_list_plot)) {
    exp <- exp_list_plot[k]
    df  <- get_V_long(exp)
    
    endpoints <- df |>
      group_by(Participant.Private.ID, Level) |>
      summarise(Initial = first(Value), Last = last(Value), .groups = "drop")
    
    plot(NULL, xlim = c(0.5, 2.5), ylim = ylims,
         xaxt = "n", xlab = "", ylab = "V value", main = exp_labels[k])
    axis(1, at = c(1, 2), labels = c("Initial", "Last"))
    
    for (lvl in c("50", "55", "65")) {
      df_lvl <- endpoints |> filter(Level == lvl)
      
      for (i in seq_len(nrow(df_lvl))) {
        lines(c(1, 2), c(df_lvl$Initial[i], df_lvl$Last[i]),
              col = adjustcolor(col_levels[lvl], alpha.f = 0.25), lwd = 0.7)
      }
      
      points(rep(1, nrow(df_lvl)), df_lvl$Initial,
             pch = 19, col = adjustcolor(col_levels[lvl], alpha.f = 0.4), cex = 0.7)
      points(rep(2, nrow(df_lvl)), df_lvl$Last,
             pch = 19, col = adjustcolor(col_levels[lvl], alpha.f = 0.4), cex = 0.7)
      
      lines(c(1, 2), c(mean(df_lvl$Initial), mean(df_lvl$Last)),
            col = col_levels[lvl], lwd = 2.5)
      points(c(1, 2), c(mean(df_lvl$Initial), mean(df_lvl$Last)),
             pch = 19, col = col_levels[lvl], cex = 1.2)
    }
    
    if (k == 1) {
      legend("topleft", legend = c("V50", "V55", "V65"),
             col = col_levels, lty = 1, lwd = 2, bty = "n", cex = 0.85)
    }
  }
  
  mtext("V Value Evolution — Initial to Last", outer = TRUE, cex = 1.3, font = 2)
}

plot_evolution_paired()


# =============================================================================
# Value evolution — Plot 3: Cross-experiment violin/box
# =============================================================================

plot_evolution_cross <- function() {
  
  levels_list  <- c("50", "55", "65")
  level_labels <- c("Reliability 50%", "Reliability 55%", "Reliability 65%")
  
  # universal y limits across all levels and experiments
  all_last <- bind_rows(lapply(seq_along(exp_list_plot), function(k) {
    bind_rows(lapply(levels_list, function(lvl) {
      get_V_long(exp_list_plot[k]) |>
        filter(Level == lvl) |>
        group_by(Participant.Private.ID) |>
        summarise(Last = last(Value), .groups = "drop")
    }))
  }))
  all_inits <- unlist(lapply(exp_list_plot, function(exp) {
    sapply(levels_list, function(lvl) {
      params_nested[[model_variant]][[exp]]$group[paste0("V", lvl, "_init")]
    })
  }))
  ylims <- c(min(all_last$Last, all_inits, na.rm = TRUE) - 0.05,
             max(all_last$Last, all_inits, na.rm = TRUE) + 0.05)
  
  par(mfrow = c(1, 3), oma = c(0, 0, 4, 0), mar = c(5, 4, 3, 1))
  
  for (lv in seq_along(levels_list)) {
    lvl <- levels_list[lv]
    
    last_vals <- lapply(seq_along(exp_list_plot), function(k) {
      df <- get_V_long(exp_list_plot[k]) |>
        filter(Level == lvl) |>
        group_by(Participant.Private.ID) |>
        summarise(Last = last(Value), .groups = "drop")
      df$exp_idx <- k
      df
    })
    last_df <- bind_rows(last_vals)
    
    init_vals <- sapply(exp_list_plot, function(exp) {
      params_nested[[model_variant]][[exp]]$group[paste0("V", lvl, "_init")]
    })
    
    plot(NULL,
         xlim = c(0.5, length(exp_list_plot) + 0.5), ylim = ylims,
         xaxt = "n", xlab = "", ylab = "V value", main = level_labels[lv])
    axis(1, at = seq_along(exp_list_plot), labels = exp_labels, las = 2, cex.axis = 0.85)
    
    for (k in seq_along(exp_list_plot)) {
      vals <- last_df$Last[last_df$exp_idx == k]
      
      if (requireNamespace("vioplot", quietly = TRUE)) {
        vioplot::vioplot(vals, at = k, add = TRUE,
                         col    = adjustcolor(col_levels[lvl], alpha.f = 0.5),
                         border = col_levels[lvl],
                         drawMedian = TRUE)
      } else {
        boxplot(vals, at = k, add = TRUE,
                col    = adjustcolor(col_levels[lvl], alpha.f = 0.4),
                border = col_levels[lvl],
                width  = 0.4, boxwex = 0.4)
      }
      
      points(k, init_vals[k], pch = 18, cex = 1.5, col = "gray30")
    }
    
    legend("topleft",
           legend = c("Last V (distribution)", "Initial V"),
           pch    = c(15, 18),
           col    = c(adjustcolor(col_levels[lvl], alpha.f = 0.7), "gray30"),
           bty    = "n", cex = 0.85)
  }
  
  mtext("Cross-Experiment V Value Comparison (Last Trial)",
        outer = TRUE, cex = 1.3, font = 2)
}

plot_evolution_cross()

# =============================================================================
# Value Ratio Evolution — V65 / V55 across trials
# =============================================================================

plot_ratio_evolution <- function() {
  
  # --- compute ratio data for all experiments ---
  get_ratio_long <- function(exp) {
    data_simulated_plot |>
      filter(Done.by == exp) |>
      select(Participant.Private.ID, trial, Sim.Value55, Sim.Value65) |>
      mutate(Ratio = Sim.Value65 / Sim.Value55)
  }
  
  # universal y limits
  all_ratios <- bind_rows(lapply(exp_list_plot, get_ratio_long))
  # ylims      <- range(all_ratios$Ratio, na.rm = TRUE)
  ylims      <- range(c(-0.1,5), na.rm = TRUE)
  
  par(mfrow = c(1, 4), oma = c(0, 0, 4, 0), mar = c(4, 4, 3, 1))
  
  for (k in seq_along(exp_list_plot)) {
    exp      <- exp_list_plot[k]
    df       <- get_ratio_long(exp)
    n_trials <- max(df$trial)
    
    plot(NULL, xlim = c(1, n_trials), ylim = ylims,
         xlab = "Trial", ylab = "V65 / V55",
         main = exp_labels[k])
    
    # reference line: logodds(0.65) / logodds(0.55)
    abline(h = log(0.65 / 0.35) / log(0.55 / 0.45),
           lty = 2, col = "gray40")
    
    pids <- unique(df$Participant.Private.ID)
    
    # individual lines (semi-transparent)
    for (pid in pids) {
      df_pid <- df |> filter(Participant.Private.ID == pid)
      lines(df_pid$trial, df_pid$Ratio,
            col = adjustcolor("#338833", alpha.f = 0.2), lwd = 0.6)
    }
    
    # mean line on top
    mean_line <- df |>
      group_by(trial) |>
      summarise(mean_ratio = mean(Ratio, na.rm = TRUE), .groups = "drop")
    
    lines(mean_line$trial, mean_line$mean_ratio,
          col = "#338833", lwd = 2.5)
    
    if (k == 1) {
      legend("topleft",
             legend = c("V65 / V55", "Ideal ratio"),
             col    = c("#338833", "gray40"),
             lty    = c(1, 2), lwd = c(2, 1),
             bty    = "n", cex = 0.85)
    }
  }
  
  mtext("V65 / V55 Ratio Evolution — All Trials",
        outer = TRUE, cex = 1.3, font = 2)
}

plot_ratio_evolution()


# =============================================================================
# Simulated Optimal Accuracy — Sliding Window Evolution
# =============================================================================

library(dplyr)

window_length <- 20

acc_window_sim <- data_simulated_plot |>
  group_by(Done.by) |>
  group_modify(function(df, key) {
    max_trial    <- max(df$trial, na.rm = TRUE)
    window_start <- 1:(max_trial - window_length + 1)
    data.frame(
      Window_end = window_start + window_length - 1,
      Accuracy   = sapply(window_start, function(start) {
        mean(
          df$Sim.OptimalAccuracy[
            df$trial >= start & df$trial <= start + window_length - 1
          ],
          na.rm = TRUE
        )
      })
    )
  }) |>
  ungroup() |>
  mutate(Done.by = factor(Done.by,
                          levels = c("Exp6", "Alex", "Divyaj", "mitigation3")))

ggplot(acc_window_sim, aes(Window_end, Accuracy)) +
  geom_line(aes(colour = Done.by)) +
  theme_classic() +
  labs(
    x      = "Trial",
    y      = "Simulated Optimal Accuracy",
    colour = "Experiment",
    title  = "Simulated"
  ) +
  scale_colour_discrete(labels = c("Exp6"        = "Control",
                                   "Alex"        = "Mitigation 1",
                                   "Divyaj"      = "Mitigation 2",
                                   "mitigation3" = "Mitigation 3")) +
  ylim(0.7, 1)

