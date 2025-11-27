################################################################################
# OZ Simulation: Causal Forest vs. Regression Discontinuity
# 
# Compares RD and Causal Forest estimators in an Opportunity Zone context
# Uses MVN data structure (following Gulen, Jens, & Page 2020)
# 
# Key modification from Gulen et al.: We include the running variable as a 
# covariate in the causal forest (hybrid identification strategy)
#
# Author: William McWilliams
# Virginia Tech, Department of Agricultural and Applied Economics
# Date: November 2025
#
# FEATURES:
#   - Progress tracking with timestamps and ETA
#   - Checkpointing: saves progress every N simulations
#   - Resume capability: if stopped, re-run to continue from last checkpoint
#   - Parallel processing support (optional)
#   - Adjustable number of simulations
#
# INSTRUCTIONS:
#   1. Set configuration in the USER CONFIGURATION section
#   2. Run the script
#   3. If you need to stop, just interrupt - progress is saved
#   4. Re-run to resume from last checkpoint
#   5. To start fresh, delete files in OUTPUT_DIR or change OUTPUT_DIR
################################################################################

#===============================================================================
# USER CONFIGURATION - MODIFY THESE AS NEEDED
#===============================================================================

# Number of simulations per scenario (start small, increase as needed)
# Recommendation: Start with 50-100 to estimate timing, then scale up
NUM_SIMULATIONS <- 10000

# Sample size per simulation (number of "census tracts")
NUM_OBS <- 5000

# How often to save checkpoint (every N simulations)
CHECKPOINT_INTERVAL <- 10

# Number of CPU cores for parallel processing
# Set to "auto" to use (total cores - 2), or specify a number
# "auto" leaves 2 cores free for system tasks
# Set to 1 for sequential (safer, easier to debug)
NUM_CORES <- "auto"  # Options: "auto", or a specific number like 4

# Output directory (created if doesn't exist)
OUTPUT_DIR <- "./oz_simulation_output"

# Which scenarios to run? Set to FALSE to skip
RUN_SCENARIO_1_BASELINE <- TRUE        # Homogeneous effects, no confounding
RUN_SCENARIO_2_HETEROGENEOUS <- TRUE   # Heterogeneous effects
RUN_SCENARIO_3_CONFOUNDING <- TRUE     # Unobserved confounders
RUN_SCENARIO_4_COMBINED <- TRUE        # Heterogeneity + confounding

# Random seed for reproducibility
MASTER_SEED <- 20251126

#===============================================================================
# PACKAGE INSTALLATION AND LOADING
#===============================================================================

cat("\n")
cat("================================================================\n")
cat("  OZ SIMULATION: Causal Forest vs Regression Discontinuity     \n")
cat("================================================================\n")
cat(paste("  Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n"))
cat(paste("  Simulations per scenario:", NUM_SIMULATIONS, "\n"))
cat(paste("  Observations per simulation:", NUM_OBS, "\n"))

# Required packages (always needed)
# Note: We use 'rdrobust' instead of 'rdd' (more modern, actively maintained)
required_packages <- c("MASS", "grf", "rdrobust", "dplyr", "parallel", "doParallel", "foreach")

# Install and load
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(paste("Installing", pkg, "...\n"))
    install.packages(pkg, repos = "https://cloud.r-project.org", quiet = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Determine number of cores to use
total_cores <- parallel::detectCores()
if (NUM_CORES == "auto") {
  NUM_CORES <- max(1, total_cores - 2)  # Leave 2 cores free, minimum 1
}
NUM_CORES <- as.integer(NUM_CORES)
NUM_CORES <- max(1, min(NUM_CORES, total_cores - 1))  # Safety bounds

cat(paste("  Total CPU cores available:", total_cores, "\n"))
cat(paste("  CPU cores to use:", NUM_CORES, "\n"))

# Set up parallel backend if using multiple cores
if (NUM_CORES > 1) {
  # Create cluster
  cl <- parallel::makeCluster(NUM_CORES)
  doParallel::registerDoParallel(cl)
  cat(paste("  Parallel backend: doParallel with", NUM_CORES, "workers\n"))
  
  # We'll need to clean up the cluster on exit
  # This ensures cleanup even if the script is interrupted
  reg.finalizer(environment(), function(e) {
    try(parallel::stopCluster(cl), silent = TRUE)
  }, onexit = TRUE)
} else {
  cat("  Running in sequential mode (NUM_CORES = 1)\n")
}

cat("================================================================\n\n")
cat("All required packages loaded.\n\n")

# Create output directory
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
  cat(paste("Created output directory:", OUTPUT_DIR, "\n\n"))
}

#===============================================================================
# HELPER FUNCTIONS
#===============================================================================

format_time <- function(seconds) {
  # Format seconds into human-readable string
  if (is.na(seconds) || is.infinite(seconds)) return("calculating...")
  if (seconds < 60) return(paste0(round(seconds), "s"))
  if (seconds < 3600) return(paste0(floor(seconds/60), "m ", round(seconds%%60), "s"))
  return(paste0(floor(seconds/3600), "h ", floor((seconds%%3600)/60), "m"))
}

print_progress <- function(i, total, start_time, scenario) {
  # Print progress bar with ETA
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  rate <- i / elapsed
  eta <- (total - i) / rate
  pct <- round(100 * i / total)
  
  # Create progress bar
  bar_width <- 30
  filled <- round(bar_width * i / total)
  bar <- paste0("[", paste(rep("=", filled), collapse = ""),
                paste(rep(" ", bar_width - filled), collapse = ""), "]")
  
  cat(sprintf("\r%s %s %3d%% (%d/%d) | Elapsed: %s | ETA: %s    ",
              scenario, bar, pct, i, total,
              format_time(elapsed), format_time(eta)))
  flush.console()
}

save_checkpoint <- function(results_list, scenario_name, current_index, seeds_used) {
  # Save checkpoint for resuming later
  checkpoint <- list(
    results = results_list,
    last_completed = current_index,
    seeds_used = seeds_used,
    timestamp = Sys.time(),
    config = list(
      num_simulations = NUM_SIMULATIONS,
      num_obs = NUM_OBS
    )
  )
  checkpoint_file <- file.path(OUTPUT_DIR, paste0(scenario_name, "_checkpoint.rds"))
  saveRDS(checkpoint, checkpoint_file)
}

load_checkpoint <- function(scenario_name) {
  # Load checkpoint if it exists
  checkpoint_file <- file.path(OUTPUT_DIR, paste0(scenario_name, "_checkpoint.rds"))
  if (file.exists(checkpoint_file)) {
    checkpoint <- readRDS(checkpoint_file)
    # Verify configuration matches
    if (checkpoint$config$num_simulations == NUM_SIMULATIONS &&
        checkpoint$config$num_obs == NUM_OBS) {
      cat(paste0("  Resuming from checkpoint (", checkpoint$last_completed, 
                 " completed, saved ", format(checkpoint$timestamp, "%H:%M:%S"), ")\n"))
      return(checkpoint)
    } else {
      cat("  Checkpoint found but config changed. Starting fresh.\n")
      return(NULL)
    }
  }
  return(NULL)
}

save_final <- function(results_df, scenario_name, elapsed_time) {
  # Save final results and clean up checkpoint
  
  # Save RDS
  rds_file <- file.path(OUTPUT_DIR, paste0(scenario_name, "_final.rds"))
  saveRDS(results_df, rds_file)
  
  # Save CSV
  csv_file <- file.path(OUTPUT_DIR, paste0(scenario_name, "_final.csv"))
  write.csv(results_df, csv_file, row.names = FALSE)
  
  # Calculate and save summary statistics
  summary_stats <- calculate_summary(results_df)
  summary_file <- file.path(OUTPUT_DIR, paste0(scenario_name, "_summary.csv"))
  write.csv(summary_stats, summary_file, row.names = FALSE)
  
  # Remove checkpoint
  checkpoint_file <- file.path(OUTPUT_DIR, paste0(scenario_name, "_checkpoint.rds"))
  if (file.exists(checkpoint_file)) file.remove(checkpoint_file)
  
  cat(paste0("\n  Completed in ", format_time(elapsed_time), "\n"))
  cat(paste0("  Saved: ", basename(rds_file), ", ", basename(csv_file), ", ", basename(summary_file), "\n"))
}

calculate_summary <- function(results_df) {
  # Calculate bias, RMSE, etc. for each estimator
  te <- results_df$true_te[1]  # True treatment effect
  
  summary <- data.frame(
    estimator = c("RD_optimal", "RD_half_bw", "RD_double_bw", 
                  "CF_ate", "CF_att", "CF_hybrid_ate", "CF_hybrid_att"),
    mean_estimate = NA,
    bias = NA,
    bias_pct = NA,
    rmse = NA,
    rmse_pct = NA,
    variance = NA,
    stringsAsFactors = FALSE
  )
  
  # RD estimators
  summary$mean_estimate[1] <- mean(results_df$rd_optimal, na.rm = TRUE)
  summary$mean_estimate[2] <- mean(results_df$rd_half, na.rm = TRUE)
  summary$mean_estimate[3] <- mean(results_df$rd_double, na.rm = TRUE)
  
  # CF estimators (standard - no running variable)
  summary$mean_estimate[4] <- mean(results_df$cf_ate, na.rm = TRUE)
  summary$mean_estimate[5] <- mean(results_df$cf_att, na.rm = TRUE)
  
  # CF hybrid estimators (with running variable)
  summary$mean_estimate[6] <- mean(results_df$cf_hybrid_ate, na.rm = TRUE)
  summary$mean_estimate[7] <- mean(results_df$cf_hybrid_att, na.rm = TRUE)
  
  # Calculate bias and RMSE for each
  est_cols <- c("rd_optimal", "rd_half", "rd_double", 
                "cf_ate", "cf_att", "cf_hybrid_ate", "cf_hybrid_att")
  
  for (j in 1:7) {
    estimates <- results_df[[est_cols[j]]]
    estimates <- estimates[!is.na(estimates)]
    
    summary$bias[j] <- mean(estimates) - te
    summary$bias_pct[j] <- 100 * (mean(estimates) - te) / te
    summary$rmse[j] <- sqrt(mean((estimates - te)^2))
    summary$rmse_pct[j] <- 100 * sqrt(mean((estimates - te)^2)) / te
    summary$variance[j] <- var(estimates)
  }
  
  return(summary)
}

#===============================================================================
# CORE SIMULATION FUNCTION
#===============================================================================

simulate_once <- function(seed, num_obs, het_slope = 0, latent_corr = 0, 
                          include_hybrid = TRUE) {
  #
  # Simulate one dataset and estimate treatment effects with RD and CF
  #
  # Args:
  #   seed: Random seed for reproducibility
  
  #   num_obs: Number of observations (census tracts)
  #   het_slope: Heterogeneity in treatment effects (0 = homogeneous)
  #   latent_corr: Correlation of latent variable with running variable
  #   include_hybrid: Whether to also run CF with running variable included
  #
  # Returns:
  #   Named list with all estimates
  #
  
  set.seed(seed)
  
  #-----------------------------------------------------------------------------
  # 1. DATA GENERATING PROCESS (MVN, following Gulen et al. structure)
  #-----------------------------------------------------------------------------
  
  # Variables:
  # x1, x2, x3 = observed covariates (interpret as tract characteristics)
  # r = running variable (distance to eligibility threshold, OZ-style)
  # x_latent = unobserved confounder
  
  # Means
  mu <- c(0, 0, 0, 0, 0)  # x1, x2, x3, r, x_latent
  
  # Standard deviations
  sig <- c(1.0, 1.0, 1.0, 1.0, 1.0)
  
  # Correlation matrix
  # Key: x_latent (col 5) correlation with r (col 4) introduces confounding
  rho_mat <- matrix(c(
    1.00,  0.30,  0.20,  0.10,  0.00,   # x1
    0.30,  1.00,  0.25,  0.15,  0.00,   # x2
    0.20,  0.25,  1.00,  0.20,  0.00,   # x3
    0.10,  0.15,  0.20,  1.00,  latent_corr,  # r (running variable)
    0.00,  0.00,  0.00,  latent_corr,  1.00   # x_latent (unobserved)
  ), nrow = 5, byrow = TRUE)
  
  # Covariance matrix
  sigma <- rho_mat * (sig %o% sig)
  
  # Generate data
  data <- as.data.frame(MASS::mvrnorm(num_obs, mu, sigma))
  names(data) <- c("x1", "x2", "x3", "r", "x_latent")
  
  #-----------------------------------------------------------------------------
  # 2. TREATMENT ASSIGNMENT (Fuzzy RD, OZ-style)
  #-----------------------------------------------------------------------------
  
  # Eligibility: r >= 0 means eligible
  data$eligible <- as.integer(data$r >= 0)
  
  # Selection probability among eligible (fuzzy RD)
  # In OZ: governors select ~25% of eligible tracts
  # Selection probability depends on observed characteristics (like in real OZ)
  selection_prob <- 0.25 + 0.05 * data$x1 + 0.03 * data$x2  # Base ~25%, varies with X
  selection_prob <- pmax(0.05, pmin(0.95, selection_prob))  # Bound away from 0,1
  
  data$selected <- data$eligible * (runif(num_obs) < selection_prob)
  data$W <- data$selected  # Treatment indicator
  
  #-----------------------------------------------------------------------------
  # 3. OUTCOME GENERATION
  #-----------------------------------------------------------------------------
  
  # True treatment effect (homogeneous or heterogeneous)
  base_te <- 0.5
  if (het_slope == 0) {
    data$true_te <- base_te
  } else {
    # Heterogeneous: TE varies with x2 (and implicitly with r through correlation)
    data$true_te <- base_te + het_slope * data$x2
  }
  
  # Outcome: Y = f(X) + tau*W + error
  # Note: x_latent affects Y but is NOT included in estimation
  data$Y <- 0.3 * data$x1 + 0.2 * data$x2 + 0.1 * data$x3 +
    0.4 * data$x_latent +  # Unobserved confounder
    data$true_te * data$W +
    rnorm(num_obs, 0, 1)
  
  # Average true treatment effect (for comparison)
  true_ate <- mean(data$true_te)
  true_att <- mean(data$true_te[data$W == 1])
  
  #-----------------------------------------------------------------------------
  # 4. RD ESTIMATION (using rdrobust package)
  #-----------------------------------------------------------------------------
  
  # Use rdrobust package (Calonico, Cattaneo, Titiunik)
  rd_result <- tryCatch({
    
    # Prepare covariates matrix
    covs_matrix <- as.matrix(data[, c("x1", "x2", "x3")])
    
    # Main RD estimate with optimal bandwidth (fuzzy RD)
    rd_fit <- rdrobust::rdrobust(y = data$Y, x = data$r, fuzzy = data$W,
                                 covs = covs_matrix, 
                                 kernel = "triangular", bwselect = "mserd")
    
    # Get the optimal bandwidth
    bw_optimal <- rd_fit$bws[1, 1]  # Bandwidth for point estimate
    
    # Estimate at half bandwidth
    rd_half <- rdrobust::rdrobust(y = data$Y, x = data$r, fuzzy = data$W, covs = covs_matrix,
                                  kernel = "triangular", h = bw_optimal / 2)
    
    # Estimate at double bandwidth  
    rd_double <- rdrobust::rdrobust(y = data$Y, x = data$r, fuzzy = data$W, covs = covs_matrix,
                                    kernel = "triangular", h = bw_optimal * 2)
    
    list(
      optimal = rd_fit$coef[1],      # Conventional RD estimate
      half = rd_half$coef[1],        # Estimate at half bandwidth
      double = rd_double$coef[1],    # Estimate at double bandwidth
      bandwidth = bw_optimal,
      optimal_se = rd_fit$se[1],
      half_se = rd_half$se[1],
      double_se = rd_double$se[1]
    )
  }, error = function(e) {
    list(optimal = NA, half = NA, double = NA, bandwidth = NA,
         optimal_se = NA, half_se = NA, double_se = NA)
  })
  
  #-----------------------------------------------------------------------------
  # 5. CAUSAL FOREST ESTIMATION (Standard - without running variable)
  #-----------------------------------------------------------------------------
  
  cf_result <- tryCatch({
    # Covariates for standard CF (NO running variable)
    X_standard <- as.matrix(data[, c("x1", "x2", "x3")])
    
    # Propensity forest
    w_hat <- grf::regression_forest(X_standard, data$W, num.trees = 500)$predictions
    w_hat <- pmax(0.01, pmin(0.99, w_hat))  # Bound away from 0,1
    
    # Outcome forest
    y_hat <- grf::regression_forest(X_standard, data$Y, num.trees = 500)$predictions
    
    # Causal forest
    cf_fit <- grf::causal_forest(X_standard, data$Y, data$W,
                                 Y.hat = y_hat, W.hat = w_hat,
                                 num.trees = 1000)
    
    ate <- grf::average_treatment_effect(cf_fit, target.sample = "all")
    att <- grf::average_treatment_effect(cf_fit, target.sample = "treated")
    
    list(ate = ate[1], ate_se = ate[2], att = att[1], att_se = att[2])
  }, error = function(e) {
    list(ate = NA, ate_se = NA, att = NA, att_se = NA)
  })
  
  #-----------------------------------------------------------------------------
  # 6. CAUSAL FOREST ESTIMATION (Hybrid - WITH running variable)
  #-----------------------------------------------------------------------------
  
  cf_hybrid_result <- list(ate = NA, ate_se = NA, att = NA, att_se = NA)
  
  if (include_hybrid) {
    cf_hybrid_result <- tryCatch({
      # Covariates for hybrid CF (INCLUDES running variable)
      X_hybrid <- as.matrix(data[, c("x1", "x2", "x3", "r")])
      
      # Propensity forest
      w_hat <- grf::regression_forest(X_hybrid, data$W, num.trees = 500)$predictions
      w_hat <- pmax(0.01, pmin(0.99, w_hat))
      
      # Outcome forest
      y_hat <- grf::regression_forest(X_hybrid, data$Y, num.trees = 500)$predictions
      
      # Causal forest
      cf_fit <- grf::causal_forest(X_hybrid, data$Y, data$W,
                                   Y.hat = y_hat, W.hat = w_hat,
                                   num.trees = 1000)
      
      ate <- grf::average_treatment_effect(cf_fit, target.sample = "all")
      att <- grf::average_treatment_effect(cf_fit, target.sample = "treated")
      
      list(ate = ate[1], ate_se = ate[2], att = att[1], att_se = att[2])
    }, error = function(e) {
      list(ate = NA, ate_se = NA, att = NA, att_se = NA)
    })
  }
  
  #-----------------------------------------------------------------------------
  # 7. RETURN RESULTS
  #-----------------------------------------------------------------------------
  
  return(list(
    # True effects
    true_te = true_ate,
    true_att = true_att,
    
    # RD estimates
    rd_optimal = rd_result$optimal,
    rd_half = rd_result$half,
    rd_double = rd_result$double,
    rd_bandwidth = rd_result$bandwidth,
    rd_optimal_se = rd_result$optimal_se,
    rd_half_se = rd_result$half_se,
    rd_double_se = rd_result$double_se,
    
    # CF standard estimates
    cf_ate = cf_result$ate,
    cf_ate_se = cf_result$ate_se,
    cf_att = cf_result$att,
    cf_att_se = cf_result$att_se,
    
    # CF hybrid estimates (with running variable)
    cf_hybrid_ate = cf_hybrid_result$ate,
    cf_hybrid_ate_se = cf_hybrid_result$ate_se,
    cf_hybrid_att = cf_hybrid_result$att,
    cf_hybrid_att_se = cf_hybrid_result$att_se,
    
    # Metadata
    n_treated = sum(data$W),
    n_control = sum(1 - data$W),
    het_slope = het_slope,
    latent_corr = latent_corr
  ))
}

#===============================================================================
# TEST SIMULATION (run one first to catch errors)
#===============================================================================

cat("Running test simulation to verify setup...\n")

test_result <- tryCatch({
  simulate_once(
    seed = 12345,
    num_obs = 1000,  # Smaller for quick test
    het_slope = 0,
    latent_corr = 0,
    include_hybrid = TRUE
  )
}, error = function(e) {
  cat("\n\n**********************************************\n")
  cat("*** ERROR IN TEST SIMULATION ***\n")
  cat("**********************************************\n")
  cat("Error message:", conditionMessage(e), "\n")
  cat("\nThis must be fixed before running full simulations.\n")
  cat("Common issues:\n")
  cat("  - Package not installed properly (try install.packages('rdrobust'))\n")
  cat("  - Insufficient data for RD estimation\n")
  cat("  - Memory issues\n\n")
  stop(e)
})

# Check if test worked
cat("\nTest simulation results:\n")
cat("  true_te:        ", test_result$true_te, "\n")
cat("  true_att:       ", test_result$true_att, "\n")
cat("  n_treated:      ", test_result$n_treated, "\n")
cat("  n_control:      ", test_result$n_control, "\n")
cat("  rd_optimal:     ", test_result$rd_optimal, "\n")
cat("  rd_bandwidth:   ", test_result$rd_bandwidth, "\n")
cat("  cf_ate:         ", test_result$cf_ate, "\n")
cat("  cf_hybrid_ate:  ", test_result$cf_hybrid_ate, "\n")

if (is.na(test_result$rd_optimal)) {
  cat("\n*** WARNING: RD estimation returned NA ***\n")
  cat("This might indicate an issue with rdrobust.\n")
  cat("Continuing anyway - will use NA for RD results.\n")
}

if (is.na(test_result$cf_ate)) {
  cat("\n*** WARNING: Causal Forest estimation returned NA ***\n")
  cat("This might indicate an issue with grf package.\n")
}

if (!is.na(test_result$rd_optimal) && !is.na(test_result$cf_ate)) {
  cat("\n*** Test simulation successful! ***\n")
}
cat("\n")

#===============================================================================
# SCENARIO RUNNER (with parallel processing support)
#===============================================================================

run_scenario <- function(scenario_name, het_slope, latent_corr) {
  #
  # Run all simulations for one scenario with checkpointing and parallel support
  #
  
  cat(paste0("\n", strrep("=", 60), "\n"))
  cat(paste0("  SCENARIO: ", scenario_name, "\n"))
  cat(paste0("  het_slope = ", het_slope, ", latent_corr = ", latent_corr, "\n"))
  cat(paste0("  Mode: ", ifelse(NUM_CORES > 1, paste("PARALLEL (", NUM_CORES, " cores)"), "SEQUENTIAL"), "\n"))
  cat(paste0(strrep("=", 60), "\n"))
  
  # Check for existing checkpoint
  checkpoint <- load_checkpoint(scenario_name)
  
  if (!is.null(checkpoint)) {
    results_list <- checkpoint$results
    start_index <- checkpoint$last_completed + 1
    seeds <- checkpoint$seeds_used
  } else {
    results_list <- vector("list", NUM_SIMULATIONS)
    start_index <- 1
    set.seed(MASTER_SEED)
    seeds <- sample(1:1e7, NUM_SIMULATIONS)
  }
  
  # Check if already complete
  if (start_index > NUM_SIMULATIONS) {
    cat("  Already complete. Loading existing results.\n")
    final_file <- file.path(OUTPUT_DIR, paste0(scenario_name, "_final.rds"))
    if (file.exists(final_file)) {
      return(readRDS(final_file))
    }
  }
  
  # Run simulations
  start_time <- Sys.time()
  
  if (NUM_CORES > 1) {
    #---------------------------------------------------------------------------
    # PARALLEL EXECUTION
    #---------------------------------------------------------------------------
    
    # Create local copies of parameters for parallel workers
    # (Global variables may not be visible to worker processes)
    num_obs_local <- NUM_OBS
    het_slope_local <- het_slope
    latent_corr_local <- latent_corr
    
    # Process in batches for checkpointing
    batch_size <- CHECKPOINT_INTERVAL
    
    i <- start_index
    while (i <= NUM_SIMULATIONS) {
      # Determine batch end
      batch_end <- min(i + batch_size - 1, NUM_SIMULATIONS)
      batch_indices <- i:batch_end
      batch_seeds <- seeds[batch_indices]
      
      # Print progress at batch start
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      completed <- i - start_index
      if (completed > 0) {
        rate <- completed / elapsed
        eta <- (NUM_SIMULATIONS - i + 1) / rate
      } else {
        eta <- NA
      }
      cat(sprintf("\r[%s] %s: Batch %d-%d of %d | Elapsed: %s | ETA: %s     ",
                  format(Sys.time(), "%H:%M:%S"),
                  substr(scenario_name, 1, 20),
                  i, batch_end, NUM_SIMULATIONS,
                  format_time(elapsed),
                  format_time(eta)))
      flush.console()
      
      # Run batch in parallel
      # Note: We must export all variables used in simulate_once
      batch_results <- foreach(
        j = 1:length(batch_indices),
        .packages = c("MASS", "grf", "rdrobust"),
        .export = c("simulate_once", "num_obs_local", "het_slope_local", "latent_corr_local"),
        .errorhandling = "pass"
      ) %dopar% {
        # Capture any errors with full traceback
        tryCatch({
          simulate_once(
            seed = batch_seeds[j],
            num_obs = num_obs_local,  # Use local copy
            het_slope = het_slope_local,
            latent_corr = latent_corr_local,
            include_hybrid = TRUE
          )
        }, error = function(e) {
          # Return error info instead of crashing
          list(error = TRUE, message = conditionMessage(e))
        })
      }
      
      # Store batch results
      for (j in 1:length(batch_indices)) {
        results_list[[batch_indices[j]]] <- batch_results[[j]]
      }
      
      # After first batch, check for errors and report
      if (i == start_index) {
        n_errors <- sum(sapply(batch_results, function(x) !is.null(x$error) && x$error == TRUE))
        if (n_errors > 0) {
          cat("\n\n*** ERRORS DETECTED IN FIRST BATCH ***\n")
          for (j in 1:length(batch_results)) {
            if (!is.null(batch_results[[j]]$error) && batch_results[[j]]$error == TRUE) {
              cat(paste("  Simulation", j, "error:", batch_results[[j]]$message, "\n"))
              break  # Just show first error
            }
          }
          cat("\n")
        }
      }
      
      # Save checkpoint after each batch
      save_checkpoint(results_list, scenario_name, batch_end, seeds)
      
      # Move to next batch
      i <- batch_end + 1
    }
    
  } else {
    #---------------------------------------------------------------------------
    # SEQUENTIAL EXECUTION
    #---------------------------------------------------------------------------
    
    for (i in start_index:NUM_SIMULATIONS) {
      
      # Run one simulation
      results_list[[i]] <- simulate_once(
        seed = seeds[i],
        num_obs = NUM_OBS,
        het_slope = het_slope,
        latent_corr = latent_corr,
        include_hybrid = TRUE
      )
      
      # Print progress
      print_progress(i, NUM_SIMULATIONS, start_time, substr(scenario_name, 1, 15))
      
      # Save checkpoint periodically
      if (i %% CHECKPOINT_INTERVAL == 0) {
        save_checkpoint(results_list, scenario_name, i, seeds)
      }
    }
  }
  
  # Convert to data frame
  # Handle any errors that might have occurred
  valid_results <- lapply(results_list, function(x) {
    # Check for explicit error flag
    if (!is.null(x$error) && x$error == TRUE) {
      return(list(
        true_te = NA, true_att = NA,
        rd_optimal = NA, rd_half = NA, rd_double = NA, rd_bandwidth = NA,
        rd_optimal_se = NA, rd_half_se = NA, rd_double_se = NA,
        cf_ate = NA, cf_ate_se = NA, cf_att = NA, cf_att_se = NA,
        cf_hybrid_ate = NA, cf_hybrid_ate_se = NA, cf_hybrid_att = NA, cf_hybrid_att_se = NA,
        n_treated = NA, n_control = NA, het_slope = het_slope, latent_corr = latent_corr
      ))
    }
    # Check for other types of failures
    if (inherits(x, "error") || is.null(x)) {
      return(list(
        true_te = NA, true_att = NA,
        rd_optimal = NA, rd_half = NA, rd_double = NA, rd_bandwidth = NA,
        rd_optimal_se = NA, rd_half_se = NA, rd_double_se = NA,
        cf_ate = NA, cf_ate_se = NA, cf_att = NA, cf_att_se = NA,
        cf_hybrid_ate = NA, cf_hybrid_ate_se = NA, cf_hybrid_att = NA, cf_hybrid_att_se = NA,
        n_treated = NA, n_control = NA, het_slope = het_slope, latent_corr = latent_corr
      ))
    }
    return(x)
  })
  
  results_df <- do.call(rbind, lapply(valid_results, as.data.frame))
  
  # Count any failures
  n_failed <- sum(is.na(results_df$rd_optimal) & is.na(results_df$cf_ate))
  if (n_failed > 0) {
    cat(paste0("\n  Warning: ", n_failed, " simulations failed\n"))
  }
  
  # Save final results
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  save_final(results_df, scenario_name, elapsed)
  
  return(results_df)
}

#===============================================================================
# MAIN EXECUTION
#===============================================================================

all_results <- list()

# Scenario 1: Baseline (homogeneous effects, no confounding)
if (RUN_SCENARIO_1_BASELINE) {
  all_results$baseline <- run_scenario(
    scenario_name = "scenario1_baseline",
    het_slope = 0,
    latent_corr = 0
  )
}

# Scenario 2: Heterogeneous effects
if (RUN_SCENARIO_2_HETEROGENEOUS) {
  all_results$heterogeneous <- run_scenario(
    scenario_name = "scenario2_heterogeneous",
    het_slope = 0.3,  # Treatment effect varies with x2
    latent_corr = 0
  )
}

# Scenario 3: Unobserved confounding
if (RUN_SCENARIO_3_CONFOUNDING) {
  all_results$confounding <- run_scenario(
    scenario_name = "scenario3_confounding",
    het_slope = 0,
    latent_corr = 0.3  # Latent variable correlated with running variable
  )
}

# Scenario 4: Combined (heterogeneity + confounding)
if (RUN_SCENARIO_4_COMBINED) {
  all_results$combined <- run_scenario(
    scenario_name = "scenario4_combined",
    het_slope = 0.3,
    latent_corr = 0.2  # Moderate confounding
  )
}

#===============================================================================
# FINAL SUMMARY
#===============================================================================

# Stop parallel cluster if it was started
if (NUM_CORES > 1 && exists("cl")) {
  cat("\nShutting down parallel cluster...\n")
  try(parallel::stopCluster(cl), silent = TRUE)
}

cat("\n")
cat(strrep("=", 60), "\n")
cat("  SIMULATION COMPLETE\n")
cat(strrep("=", 60), "\n")
cat(paste("  Finished:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n"))
cat(paste("  Results saved to:", OUTPUT_DIR, "\n"))
cat("\n")

# Print summary for each scenario
summary_files <- list.files(OUTPUT_DIR, pattern = "_summary.csv", full.names = TRUE)
for (f in summary_files) {
  cat(paste0("\n", basename(f), ":\n"))
  summ <- read.csv(f)
  print(summ[, c("estimator", "bias_pct", "rmse_pct")], row.names = FALSE)
}

cat("\n")
cat("To load results in R:\n")
cat(paste0('  results <- readRDS("', file.path(OUTPUT_DIR, "scenario1_baseline_final.rds"), '")\n'))
cat("\n")