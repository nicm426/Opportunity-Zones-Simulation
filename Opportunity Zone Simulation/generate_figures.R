################################################################################
# OZ Simulation: Figure Generation Script
#
# Run this AFTER simulations complete to generate figures for the paper
#
# Author: William McWilliams
# Date: November 2025
################################################################################

#===============================================================================
# SETUP
#===============================================================================

# Load packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

# Set theme for publication-quality figures
theme_set(theme_bw(base_size = 12) +
            theme(
              panel.grid.minor = element_blank(),
              legend.position = "bottom",
              plot.title = element_text(hjust = 0.5, face = "bold")
            ))

# Directory with simulation results
INPUT_DIR <- "./oz_simulation_output"
OUTPUT_DIR <- "./figures"

# Create output directory
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

#===============================================================================
# LOAD DATA
#===============================================================================

cat("Loading simulation results...\n")

# Load all scenario results
scenario1 <- readRDS(file.path(INPUT_DIR, "scenario1_baseline_final.rds"))
scenario2 <- readRDS(file.path(INPUT_DIR, "scenario2_heterogeneous_final.rds"))
scenario3 <- readRDS(file.path(INPUT_DIR, "scenario3_confounding_final.rds"))
scenario4 <- readRDS(file.path(INPUT_DIR, "scenario4_combined_final.rds"))

# Add scenario labels
scenario1$scenario <- "Baseline"
scenario2$scenario <- "Heterogeneous Effects"
scenario3$scenario <- "Unobserved Confounding"
scenario4$scenario <- "Combined"

# Combine all scenarios
all_results <- bind_rows(scenario1, scenario2, scenario3, scenario4)

cat("Loaded", nrow(all_results), "total simulations across 4 scenarios.\n\n")

#===============================================================================
# CALCULATE SUMMARY STATISTICS
#===============================================================================

# True treatment effect
TRUE_TE <- 0.5

# Calculate bias for each estimator
calc_summary <- function(df, scenario_name) {
  data.frame(
    scenario = scenario_name,
    estimator = c("RD (Optimal BW)", "RD (Half BW)", "RD (Double BW)",
                  "CF Standard", "CF Hybrid"),
    bias = c(
      mean(df$rd_optimal - TRUE_TE, na.rm = TRUE),
      mean(df$rd_half - TRUE_TE, na.rm = TRUE),
      mean(df$rd_double - TRUE_TE, na.rm = TRUE),
      mean(df$cf_ate - TRUE_TE, na.rm = TRUE),
      mean(df$cf_hybrid_ate - TRUE_TE, na.rm = TRUE)
    ),
    bias_pct = c(
      100 * mean(df$rd_optimal - TRUE_TE, na.rm = TRUE) / TRUE_TE,
      100 * mean(df$rd_half - TRUE_TE, na.rm = TRUE) / TRUE_TE,
      100 * mean(df$rd_double - TRUE_TE, na.rm = TRUE) / TRUE_TE,
      100 * mean(df$cf_ate - TRUE_TE, na.rm = TRUE) / TRUE_TE,
      100 * mean(df$cf_hybrid_ate - TRUE_TE, na.rm = TRUE) / TRUE_TE
    ),
    rmse = c(
      sqrt(mean((df$rd_optimal - TRUE_TE)^2, na.rm = TRUE)),
      sqrt(mean((df$rd_half - TRUE_TE)^2, na.rm = TRUE)),
      sqrt(mean((df$rd_double - TRUE_TE)^2, na.rm = TRUE)),
      sqrt(mean((df$cf_ate - TRUE_TE)^2, na.rm = TRUE)),
      sqrt(mean((df$cf_hybrid_ate - TRUE_TE)^2, na.rm = TRUE))
    ),
    rmse_pct = c(
      100 * sqrt(mean((df$rd_optimal - TRUE_TE)^2, na.rm = TRUE)) / TRUE_TE,
      100 * sqrt(mean((df$rd_half - TRUE_TE)^2, na.rm = TRUE)) / TRUE_TE,
      100 * sqrt(mean((df$rd_double - TRUE_TE)^2, na.rm = TRUE)) / TRUE_TE,
      100 * sqrt(mean((df$cf_ate - TRUE_TE)^2, na.rm = TRUE)) / TRUE_TE,
      100 * sqrt(mean((df$cf_hybrid_ate - TRUE_TE)^2, na.rm = TRUE)) / TRUE_TE
    ),
    variance = c(
      var(df$rd_optimal, na.rm = TRUE),
      var(df$rd_half, na.rm = TRUE),
      var(df$rd_double, na.rm = TRUE),
      var(df$cf_ate, na.rm = TRUE),
      var(df$cf_hybrid_ate, na.rm = TRUE)
    )
  )
}

summary_stats <- bind_rows(
  calc_summary(scenario1, "Baseline"),
  calc_summary(scenario2, "Heterogeneous Effects"),
  calc_summary(scenario3, "Unobserved Confounding"),
  calc_summary(scenario4, "Combined")
)

# Set factor levels for ordering
summary_stats$scenario <- factor(summary_stats$scenario, 
                                  levels = c("Baseline", "Heterogeneous Effects", 
                                            "Unobserved Confounding", "Combined"))
summary_stats$estimator <- factor(summary_stats$estimator,
                                   levels = c("RD (Optimal BW)", "RD (Half BW)", 
                                             "RD (Double BW)", "CF Standard", "CF Hybrid"))

#===============================================================================
# FIGURE 1: BIAS COMPARISON ACROSS SCENARIOS
#===============================================================================

cat("Creating Figure 1: Bias comparison...\n")

# Focus on main estimators (optimal RD, CF Standard, CF Hybrid)
fig1_data <- summary_stats %>%
  filter(estimator %in% c("RD (Optimal BW)", "CF Standard", "CF Hybrid"))

fig1 <- ggplot(fig1_data, aes(x = scenario, y = bias_pct, fill = estimator)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_fill_manual(values = c("RD (Optimal BW)" = "#E41A1C", 
                                "CF Standard" = "#377EB8", 
                                "CF Hybrid" = "#4DAF4A"),
                    name = "Estimator") +
  labs(
    title = "Bias by Estimation Method and Scenario",
    x = "",
    y = "Bias (% of True Effect)"
  ) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

ggsave(file.path(OUTPUT_DIR, "fig1_bias_comparison.pdf"), fig1, 
       width = 8, height = 5)
ggsave(file.path(OUTPUT_DIR, "fig1_bias_comparison.png"), fig1, 
       width = 8, height = 5, dpi = 300)

#===============================================================================
# FIGURE 2: RMSE COMPARISON ACROSS SCENARIOS
#===============================================================================

cat("Creating Figure 2: RMSE comparison...\n")

fig2_data <- summary_stats %>%
  filter(estimator %in% c("RD (Optimal BW)", "CF Standard", "CF Hybrid"))

fig2 <- ggplot(fig2_data, aes(x = scenario, y = rmse_pct, fill = estimator)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("RD (Optimal BW)" = "#E41A1C", 
                                "CF Standard" = "#377EB8", 
                                "CF Hybrid" = "#4DAF4A"),
                    name = "Estimator") +
  labs(
    title = "Root Mean Squared Error by Estimation Method and Scenario",
    x = "",
    y = "RMSE (% of True Effect)"
  ) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

ggsave(file.path(OUTPUT_DIR, "fig2_rmse_comparison.pdf"), fig2, 
       width = 8, height = 5)
ggsave(file.path(OUTPUT_DIR, "fig2_rmse_comparison.png"), fig2, 
       width = 8, height = 5, dpi = 300)

#===============================================================================
# FIGURE 3: DENSITY PLOTS OF ESTIMATES (Baseline Scenario)
#===============================================================================

cat("Creating Figure 3: Density plots (baseline)...\n")

# Reshape data for density plot
density_data <- scenario1 %>%
  select(rd_optimal, cf_ate, cf_hybrid_ate) %>%
  pivot_longer(cols = everything(), names_to = "estimator", values_to = "estimate") %>%
  mutate(estimator = case_when(
    estimator == "rd_optimal" ~ "RD (Optimal BW)",
    estimator == "cf_ate" ~ "CF Standard",
    estimator == "cf_hybrid_ate" ~ "CF Hybrid"
  )) %>%
  filter(!is.na(estimate))

fig3 <- ggplot(density_data, aes(x = estimate, fill = estimator, color = estimator)) +
  geom_density(alpha = 0.3, linewidth = 1) +
  geom_vline(xintercept = TRUE_TE, linetype = "dashed", color = "black", linewidth = 1) +
  scale_fill_manual(values = c("RD (Optimal BW)" = "#E41A1C", 
                                "CF Standard" = "#377EB8", 
                                "CF Hybrid" = "#4DAF4A"),
                    name = "Estimator") +
  scale_color_manual(values = c("RD (Optimal BW)" = "#E41A1C", 
                                 "CF Standard" = "#377EB8", 
                                 "CF Hybrid" = "#4DAF4A"),
                     name = "Estimator") +
  labs(
    title = "Distribution of Estimates: Baseline Scenario",
    subtitle = "Dashed line indicates true treatment effect (0.5)",
    x = "Estimated Treatment Effect",
    y = "Density"
  ) +
  xlim(-1, 2)

ggsave(file.path(OUTPUT_DIR, "fig3_density_baseline.pdf"), fig3, 
       width = 8, height = 5)
ggsave(file.path(OUTPUT_DIR, "fig3_density_baseline.png"), fig3, 
       width = 8, height = 5, dpi = 300)

#===============================================================================
# FIGURE 4: DENSITY PLOTS (Confounding Scenario)
#===============================================================================

cat("Creating Figure 4: Density plots (confounding)...\n")

density_data_conf <- scenario3 %>%
  select(rd_optimal, cf_ate, cf_hybrid_ate) %>%
  pivot_longer(cols = everything(), names_to = "estimator", values_to = "estimate") %>%
  mutate(estimator = case_when(
    estimator == "rd_optimal" ~ "RD (Optimal BW)",
    estimator == "cf_ate" ~ "CF Standard",
    estimator == "cf_hybrid_ate" ~ "CF Hybrid"
  )) %>%
  filter(!is.na(estimate))

fig4 <- ggplot(density_data_conf, aes(x = estimate, fill = estimator, color = estimator)) +
  geom_density(alpha = 0.3, linewidth = 1) +
  geom_vline(xintercept = TRUE_TE, linetype = "dashed", color = "black", linewidth = 1) +
  scale_fill_manual(values = c("RD (Optimal BW)" = "#E41A1C", 
                                "CF Standard" = "#377EB8", 
                                "CF Hybrid" = "#4DAF4A"),
                    name = "Estimator") +
  scale_color_manual(values = c("RD (Optimal BW)" = "#E41A1C", 
                                 "CF Standard" = "#377EB8", 
                                 "CF Hybrid" = "#4DAF4A"),
                     name = "Estimator") +
  labs(
    title = "Distribution of Estimates: Unobserved Confounding Scenario",
    subtitle = "Dashed line indicates true treatment effect (0.5)",
    x = "Estimated Treatment Effect",
    y = "Density"
  ) +
  xlim(-1, 2)

ggsave(file.path(OUTPUT_DIR, "fig4_density_confounding.pdf"), fig4, 
       width = 8, height = 5)
ggsave(file.path(OUTPUT_DIR, "fig4_density_confounding.png"), fig4, 
       width = 8, height = 5, dpi = 300)

#===============================================================================
# FIGURE 5: BIAS-VARIANCE TRADEOFF
#===============================================================================

cat("Creating Figure 5: Bias-variance tradeoff...\n")

fig5_data <- summary_stats %>%
  filter(estimator %in% c("RD (Optimal BW)", "CF Standard", "CF Hybrid"))

fig5 <- ggplot(fig5_data, aes(x = abs(bias_pct), y = sqrt(variance) * 100 / TRUE_TE, 
                               color = estimator, shape = scenario)) +
  geom_point(size = 4) +
  scale_color_manual(values = c("RD (Optimal BW)" = "#E41A1C", 
                                 "CF Standard" = "#377EB8", 
                                 "CF Hybrid" = "#4DAF4A"),
                     name = "Estimator") +
  scale_shape_manual(values = c(16, 17, 15, 18), name = "Scenario") +
  labs(
    title = "Bias-Variance Tradeoff Across Methods and Scenarios",
    x = "Absolute Bias (% of True Effect)",
    y = "Standard Deviation (% of True Effect)"
  )

ggsave(file.path(OUTPUT_DIR, "fig5_bias_variance.pdf"), fig5, 
       width = 9, height = 6)
ggsave(file.path(OUTPUT_DIR, "fig5_bias_variance.png"), fig5, 
       width = 9, height = 6, dpi = 300)

#===============================================================================
# TABLE: SUMMARY STATISTICS (for LaTeX)
#===============================================================================

cat("Creating summary table for LaTeX...\n")

# Create table for paper
table_data <- summary_stats %>%
  filter(estimator %in% c("RD (Optimal BW)", "CF Standard", "CF Hybrid")) %>%
  select(scenario, estimator, bias_pct, rmse_pct) %>%
  mutate(
    bias_pct = sprintf("%.1f", bias_pct),
    rmse_pct = sprintf("%.1f", rmse_pct)
  )

# Reshape for LaTeX table format
table_wide <- table_data %>%
  pivot_wider(names_from = estimator, values_from = c(bias_pct, rmse_pct))

# Save as CSV for easy LaTeX conversion
write.csv(table_wide, file.path(OUTPUT_DIR, "summary_table.csv"), row.names = FALSE)

# Also create LaTeX table directly
latex_table <- "\\begin{table}[htbp]
\\centering
\\caption{Simulation Results: Bias and RMSE by Scenario and Estimator}
\\label{tab:simulation_results}
\\begin{tabular}{lcccccc}
\\toprule
& \\multicolumn{2}{c}{RD (Optimal BW)} & \\multicolumn{2}{c}{CF Standard} & \\multicolumn{2}{c}{CF Hybrid} \\\\
\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7}
Scenario & Bias & RMSE & Bias & RMSE & Bias & RMSE \\\\
\\midrule
"

for (scen in c("Baseline", "Heterogeneous Effects", "Unobserved Confounding", "Combined")) {
  row_data <- table_data %>% filter(scenario == scen)
  rd_row <- row_data %>% filter(estimator == "RD (Optimal BW)")
  cf_row <- row_data %>% filter(estimator == "CF Standard")
  hybrid_row <- row_data %>% filter(estimator == "CF Hybrid")
  
  latex_table <- paste0(latex_table, 
                        scen, " & ", 
                        rd_row$bias_pct, "\\% & ", rd_row$rmse_pct, "\\% & ",
                        cf_row$bias_pct, "\\% & ", cf_row$rmse_pct, "\\% & ",
                        hybrid_row$bias_pct, "\\% & ", hybrid_row$rmse_pct, "\\% \\\\\n")
}

latex_table <- paste0(latex_table, "\\bottomrule
\\end{tabular}
\\begin{tablenotes}
\\small
\\item Notes: Bias and RMSE reported as percentage of true treatment effect (0.5). Results based on 10,000 simulations per scenario with 5,000 observations each.
\\end{tablenotes}
\\end{table}")

writeLines(latex_table, file.path(OUTPUT_DIR, "simulation_table.tex"))

#===============================================================================
# PRINT SUMMARY
#===============================================================================

cat("\n")
cat("================================================================\n")
cat("  FIGURES GENERATED SUCCESSFULLY\n")
cat("================================================================\n")
cat("\nFiles saved to:", OUTPUT_DIR, "\n\n")

cat("Figures:\n")
cat("  - fig1_bias_comparison.pdf/png\n")
cat("  - fig2_rmse_comparison.pdf/png\n")
cat("  - fig3_density_baseline.pdf/png\n")
cat("  - fig4_density_confounding.pdf/png\n")
cat("  - fig5_bias_variance.pdf/png\n")
cat("\nTables:\n")
cat("  - summary_table.csv\n")
cat("  - simulation_table.tex\n")

cat("\n\nSummary Statistics:\n")
cat("================================================================\n")
print(summary_stats %>% 
        filter(estimator %in% c("RD (Optimal BW)", "CF Standard", "CF Hybrid")) %>%
        select(scenario, estimator, bias_pct, rmse_pct) %>%
        arrange(scenario, estimator),
      n = 20)
