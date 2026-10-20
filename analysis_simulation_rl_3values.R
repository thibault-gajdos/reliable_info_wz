load('./results/data_simulated.rdata')
load('./results/performance.rdata')


# =============================================================================
# Select model variant to visualise
# Change this one line to switch between: "base", "t1", "t5", "t1s", "t5s"
# =============================================================================
model_variant <- "t5s"

# Remap selected variant's columns to generic names for plotting
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





##################### performance comparison #######################

library(dplyr)
library(tidyr)
performance$Participant.Private.ID = as.integer(performance$Participant.Private.ID)
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


exp_list    <- c("Exp6", "Alex", "Divyaj", "mitigation3")
exp_labels  <- c("Control", "Mitigation 1", "Mitigation 2", "Mitigation 3")

# colour pair: real (filled) vs simulated (open)
col_correct <- "#2166AC"   # blue  — correct accuracy
col_optimal <- "#D6604D"   # red   — optimal accuracy



# =============================================================================
# A — Scatter: real vs simulated (one dot per participant, y = x reference)
# =============================================================================

plot_scatter <- function(performance, metric_real, metric_sim,
                         metric_label, point_col) {
  
  par(mfrow = c(1, 4), oma = c(0, 0, 4, 0),
      mar = c(4, 4, 3, 1))
  
  for (k in seq_along(exp_list)) {
    df <- performance[performance$Done.by == exp_list[k], ]
    
    real <- df[[metric_real]]
    sim  <- df[[metric_sim]]
    
    lims <- c(0.4, 1)
    
    plot(real, sim,
         xlim = lims, ylim = lims,
         pch  = 19, col = adjustcolor(point_col, alpha.f = 0.7),
         xlab = "Real", ylab = "Simulated",
         main = exp_labels[k])
    
    abline(a = 0, b = 1, lty = 2, col = "gray40")   # y = x reference
    
    # per-experiment correlation
    r <- cor(real, sim, use = "complete.obs")
    legend("topleft", legend = sprintf("r = %.2f", r),
           bty = "n", cex = 0.85)
  }
  
  mtext(sprintf("Scatter: Real vs Simulated — %s", metric_label),
        outer = TRUE, cex = 1.3, font = 2)
}

plot_scatter(performance_simulated,
             metric_real  = "Accuracy",
             metric_sim   = "Sim.CorrectAccuracy",
             metric_label = "Correct Accuracy",
             point_col    = col_correct)

plot_scatter(performance_simulated,
             metric_real  = "Optimal_Choice",
             metric_sim   = "Sim.OptimalAccuracy",
             metric_label = "Optimal Accuracy",
             point_col    = col_optimal)




# =============================================================================
# B — Paired dot plot: sorted by real accuracy, lines connecting pairs
# =============================================================================

plot_paired <- function(performance, metric_real, metric_sim,
                        metric_label, point_col) {
  
  par(mfrow = c(1, 4), oma = c(0, 0, 4, 0),
      mar = c(4, 4, 3, 1))
  
  for (k in seq_along(exp_list)) {
    df <- performance[performance$Done.by == exp_list[k], ]
    df <- df[order(df[[metric_real]]), ]
    
    real <- df[[metric_real]]
    sim  <- df[[metric_sim]]
    x    <- seq_along(real)
    
    ylims <- c(0.5, 1)
    
    plot(x, real,
         type = "p", pch = 19,
         col  = point_col,
         ylim = ylims,
         xlab = "Participants (sorted by real)",
         ylab = metric_label,
         main = exp_labels[k])
    
    points(x, sim, pch = 1, col = point_col)
    
    # connecting lines
    segments(x, real, x, sim,
             col = adjustcolor(point_col, alpha.f = 0.3), lwd = 0.8)
    
    abline(h = mean(real), lty = 2, col = "gray40")
    abline(h = mean(sim),  lty = 3, col = point_col)
    
    if (k == 1) {
      legend("topleft",
             legend = c("Real", "Simulated"),
             pch    = c(19, 1),
             col    = point_col,
             bty    = "n", cex = 0.85)
    }
  }
  
  mtext(sprintf("Paired: Real vs Simulated — %s", metric_label),
        outer = TRUE, cex = 1.3, font = 2)
}

plot_paired(performance_simulated,
            metric_real  = "Accuracy",
            metric_sim   = "Sim.CorrectAccuracy",
            metric_label = "Correct Accuracy",
            point_col    = col_correct)

plot_paired(performance_simulated,
            metric_real  = "Optimal_Choice",
            metric_sim   = "Sim.OptimalAccuracy",
            metric_label = "Optimal Accuracy",
            point_col    = col_optimal)







# =============================================================================
# D — Bland-Altman: difference (real - sim) vs average
# =============================================================================

plot_bland_altman <- function(performance, metric_real, metric_sim,
                              metric_label, point_col) {
  
  par(mfrow = c(1, 4), oma = c(0, 0, 4, 0),
      mar = c(4, 4, 3, 1))
  
  for (k in seq_along(exp_list)) {
    df <- performance[performance$Done.by == exp_list[k], ]
    
    real <- df[[metric_real]]
    sim  <- df[[metric_sim]]
    
    avg  <- (real + sim) / 2
    diff <- real - sim
    
    mean_diff <- mean(diff, na.rm = TRUE)
    sd_diff   <- sd(diff,   na.rm = TRUE)
    loa_upper <- mean_diff + 1.96 * sd_diff   # limits of agreement
    loa_lower <- mean_diff - 1.96 * sd_diff
    
    ylims <- c(min(loa_lower, diff) - 0.02,
               max(loa_upper, diff) + 0.02)
    
    plot(avg, diff,
         pch  = 19, col = adjustcolor(point_col, alpha.f = 0.7),
         xlim = c(0.4, 1),
         ylim = ylims,
         xlab = "Average of real and simulated",
         ylab = "Real − Simulated",
         main = exp_labels[k])
    
    abline(h = 0,          lty = 2, col = "gray40")        # zero line
    abline(h = mean_diff,  lty = 1, col = point_col)       # mean difference
    abline(h = loa_upper,  lty = 3, col = point_col)       # +1.96 SD
    abline(h = loa_lower,  lty = 3, col = point_col)       # -1.96 SD
    
    legend("topleft",
           legend = c(sprintf("Mean diff = %.3f", mean_diff),
                      sprintf("±1.96 SD = [%.3f, %.3f]", loa_lower, loa_upper)),
           bty = "n", cex = 0.75)
  }
  
  mtext(sprintf("Bland-Altman: Real vs Simulated — %s", metric_label),
        outer = TRUE, cex = 1.3, font = 2)
}

plot_bland_altman(performance_simulated,
                  metric_real  = "Accuracy",
                  metric_sim   = "Sim.CorrectAccuracy",
                  metric_label = "Correct Accuracy",
                  point_col    = col_correct)

plot_bland_altman(performance_simulated,
                  metric_real  = "Optimal_Choice",
                  metric_sim   = "Sim.OptimalAccuracy",
                  metric_label = "Optimal Accuracy",
                  point_col    = col_optimal)



##################### Values Evolution #######################



library(dplyr)
library(tidyr)

exp_list   <- c("Exp6", "Alex", "Divyaj", "mitigation3")
exp_labels <- c("Control", "Mitigation 1", "Mitigation 2", "Mitigation 3")

col_50 <- "#99CCFF"   # light blue  — reliability 50
col_55 <- "#99DD99"   # light green — reliability 55
col_65 <- "#338833"   # darker green — reliability 65
col_levels <- c("50" = col_50, "55" = col_55, "65" = col_65)



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
# Plot 1 — Full trial-by-trial evolution, all participants, per experiment
# =============================================================================

plot_evolution_full <- function() {
  
  par(mfrow = c(1, 4), oma = c(0, 0, 4, 0), mar = c(4, 4, 3, 1))
  
  all_vals <- bind_rows(lapply(exp_list, get_V_long))
  ylims <- range(all_vals$Value, na.rm = TRUE)
  
  for (k in seq_along(exp_list)) {
    exp <- exp_list[k]
    df  <- get_V_long(exp)
    
    n_trials <- max(df$trial)

    # empty plot
    plot(NULL,
         xlim = c(1, n_trials), ylim = ylims,
         xlab = "Trial", ylab = "V value",
         main = exp_labels[k])
    
    # individual lines per participant per level (semi-transparent)
    for (lvl in c("50", "55", "65")) {
      df_lvl <- df |> filter(Level == lvl)
      pids   <- unique(df_lvl$Participant.Private.ID)
      
      for (pid in pids) {
        df_pid <- df_lvl |> filter(Participant.Private.ID == pid)
        lines(df_pid$trial, df_pid$Value,
              col = adjustcolor(col_levels[lvl], alpha.f = 0.2),
              lwd = 0.6)
      }
      
      # mean line on top
      mean_line <- df_lvl |>
        group_by(trial) |>
        summarise(mean_V = mean(Value, na.rm = TRUE), .groups = "drop")
      
      lines(mean_line$trial, mean_line$mean_V,
            col = col_levels[lvl], lwd = 2.5)
    }
    
    if (k == 1) {
      legend("topleft",
             legend = c("V50", "V55", "V65"),
             col    = col_levels,
             lty    = 1, lwd   = 2,
             bty    = "n", cex = 0.85)
    }
  }
  
  mtext("V Value Evolution — All Trials",
        outer = TRUE, cex = 1.3, font = 2)
}

plot_evolution_full()


# =============================================================================
# Plot 2 — Initial -> Last paired dot plot, per experiment
# =============================================================================

plot_evolution_paired <- function() {
  
  par(mfrow = c(1, 4), oma = c(0, 0, 4, 0), mar = c(4, 4, 3, 1))
  
  all_endpoints <- bind_rows(lapply(exp_list, function(exp) {
    get_V_long(exp) |>
      group_by(Participant.Private.ID, Level) |>
      summarise(Initial = first(Value), Last = last(Value), .groups = "drop")
  }))
  ylims <- c(min(all_endpoints$Initial, all_endpoints$Last) - 0.05,
             max(all_endpoints$Initial, all_endpoints$Last) + 0.05)
  
  for (k in seq_along(exp_list)) {
    exp <- exp_list[k]
    df  <- get_V_long(exp)
    
    # first and last trial per participant per level
    endpoints <- df |>
      group_by(Participant.Private.ID, Level) |>
      summarise(
        Initial = first(Value),
        Last    = last(Value),
        .groups = "drop"
      )
    

    plot(NULL,
         xlim = c(0.5, 2.5), ylim = ylims,
         xaxt = "n",
         xlab = "", ylab = "V value",
         main = exp_labels[k])
    axis(1, at = c(1, 2), labels = c("Initial", "Last"))
    
    for (lvl in c("50", "55", "65")) {
      df_lvl <- endpoints |> filter(Level == lvl)
      
      # individual paired lines
      for (i in seq_len(nrow(df_lvl))) {
        lines(c(1, 2),
              c(df_lvl$Initial[i], df_lvl$Last[i]),
              col = adjustcolor(col_levels[lvl], alpha.f = 0.25),
              lwd = 0.7)
      }
      
      # individual dots
      points(rep(1, nrow(df_lvl)), df_lvl$Initial,
             pch = 19, col = adjustcolor(col_levels[lvl], alpha.f = 0.4),
             cex = 0.7)
      points(rep(2, nrow(df_lvl)), df_lvl$Last,
             pch = 19, col = adjustcolor(col_levels[lvl], alpha.f = 0.4),
             cex = 0.7)
      
      # mean paired line (thick)
      lines(c(1, 2),
            c(mean(df_lvl$Initial), mean(df_lvl$Last)),
            col = col_levels[lvl], lwd = 2.5)
      
      points(c(1, 2),
             c(mean(df_lvl$Initial), mean(df_lvl$Last)),
             pch = 19, col = col_levels[lvl], cex = 1.2)
    }
    
    if (k == 1) {
      legend("topleft",
             legend = c("V50", "V55", "V65"),
             col    = col_levels,
             lty    = 1, lwd   = 2,
             bty    = "n", cex = 0.85)
    }
  }
  
  mtext("V Value Evolution — Initial to Last",
        outer = TRUE, cex = 1.3, font = 2)
}

plot_evolution_paired()


# =============================================================================
# Plot 3 — Cross-experiment comparison: violin/box of last V values
#          one panel per reliability level, x-axis = experiments,
#          initial value marked as a reference dot
# =============================================================================

plot_evolution_cross <- function() {
  
  par(mfrow = c(1, 3), oma = c(0, 0, 4, 0), mar = c(5, 4, 3, 1))
  
  levels_list  <- c("50", "55", "65")
  level_labels <- c("Reliability 50%", "Reliability 55%", "Reliability 65%")
  
  all_last <- bind_rows(lapply(seq_along(exp_list), function(k) {
    bind_rows(lapply(levels_list, function(lvl) {
      get_V_long(exp_list[k]) |>
        filter(Level == lvl) |>
        group_by(Participant.Private.ID) |>
        summarise(Last = last(Value), .groups = "drop")
    }))
  }))
  all_inits <- unlist(lapply(exp_list, function(exp) {
    sapply(levels_list, function(lvl) {
      params_nested[[model_variant]][[exp]]$group[paste0("V", lvl, "_init")]
    })
  }))
  ylims <- c(min(all_last$Last, all_inits, na.rm = TRUE) - 0.05,
             max(all_last$Last, all_inits, na.rm = TRUE) + 0.05)
  
  for (lv in seq_along(levels_list)) {
    lvl <- levels_list[lv]
    
    # collect last V value per participant per experiment for this level
    last_vals <- lapply(seq_along(exp_list), function(k) {
      df <- get_V_long(exp_list[k]) |>
        filter(Level == lvl) |>
        group_by(Participant.Private.ID) |>
        summarise(Last = last(Value), .groups = "drop")
      df$exp_idx <- k
      df
    })
    last_df <- bind_rows(last_vals)
    
    # initial V per experiment (group-level from params_nested)
    init_key <- paste0("V", lvl, "_init")
    init_vals <- sapply(exp_list, function(exp) {
      params_nested[[model_variant]][[exp]]$group[init_key]
    })
    
    plot(NULL,
         xlim = c(0.5, length(exp_list) + 0.5),
         ylim = ylims,
         xaxt = "n",
         xlab = "", ylab = "V value",
         main = level_labels[lv])
    axis(1, at = seq_along(exp_list), labels = exp_labels, las = 2, cex.axis = 0.85)
    
    for (k in seq_along(exp_list)) {
      vals <- last_df$Last[last_df$exp_idx == k]
      
      # violin via vioplot if available, else boxplot
      # if (requireNamespace("vioplot", quietly = TRUE)) {
      #   vioplot::vioplot(vals, at = k, add = TRUE,
      #                    col  = adjustcolor(col_levels[lvl], alpha.f = 0.5),
      #                    border = col_levels[lvl],
      #                    drawMedian = TRUE)
      # } else {
      boxplot(vals, at = k, add = TRUE,
                col    = adjustcolor(col_levels[lvl], alpha.f = 0.4),
                border = col_levels[lvl],
                width  = 0.4, boxwex = 0.4)
      # }
      
      # initial value as reference point
      points(k, init_vals[k],
             pch = 18, cex = 1.5,
             col = "gray30")
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



