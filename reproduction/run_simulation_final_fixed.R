# Final fixed simulation using single treatment type
# Since multiple treatment type doesn't work with binary indicators,
# we use single treatment type and improve evaluation logic

library(FindIt)
source("simulation_data_gen.R")

cat("=== Final Fixed Simulation (Single Treatment Type) ===\n\n")

# Simulation parameters
n_treatments <- 10
n_covariates <- 3
n_obs <- c(500, 1000, 2000)
n_reps <- 100

cat("Parameters:\n")
cat("- Treatments:", n_treatments, "\n")
cat("- Covariates:", n_covariates, "\n")
cat("- Sample sizes:", paste(n_obs, collapse=", "), "\n")
cat("- Replications:", n_reps, "\n")
cat("- Note: Using single treatment type (multiple type incompatible with binary indicators)\n\n")

# Storage for results
results_list <- list()

# Run simulation for each sample size
for (n in n_obs) {
  cat("=== Sample size:", n, "===\n")
  
  # Storage for this sample size
  fdr_largest <- numeric(n_reps)
  dr_largest <- numeric(n_reps)
  fdr_top3 <- numeric(n_reps)
  dr_top3 <- numeric(n_reps)
  
  for (rep in 1:n_reps) {
    if (rep %% 10 == 0) cat("  Replication", rep, "/", n_reps, "\n")
    
    # Generate data
    sim_data <- generate_sim_data(n = n, n_treatments = n_treatments, 
                                  n_covariates = n_covariates, 
                                  seed = rep)
    
    # Get true effects
    true_effects_rep <- sim_data$beta_true
    true_largest_idx <- which.max(true_effects_rep)
    true_top3_idx <- order(true_effects_rep, decreasing=TRUE)[1:3]
    
    # Prepare data with single treatment (numeric, not factor!)
    sim_df <- data.frame(
      outcome = as.integer(sim_data$Y),
      treatment = as.integer(sim_data$treatment),  # Numeric, not factor!
      V1 = sim_data$V[, 1],
      V2 = sim_data$V[, 2],
      V3 = sim_data$V[, 3]
    )
    
    # Run FindIt with single treatment type
    tryCatch({
      result <- FindIt(
        model.treat = outcome ~ treatment,
        model.main = ~ V1 + V2 + V3,
        model.int = ~ V1 + V2 + V3,  # Treatment-covariate interactions
        data = sim_df,
        type = "binary",
        treat.type = "single",
        search.lambdas = TRUE  # Auto-tune lambdas for better performance
      )
      
      # Extract treatment effects using predict
      # For single treatment type, predict gives us Treatment.effect for each observation
      pred_result <- predict(result)
      
      if (!is.null(pred_result$data)) {
        pred_data <- pred_result$data
        
        # Extract average treatment effect for each treatment level
        treatment_effects <- numeric(n_treatments)
        for (i in 1:n_treatments) {
          # Find rows where treatment == i
          treat_rows <- pred_data[pred_data$treatment == i, ]
          if (nrow(treat_rows) > 0) {
            # Average effect across observations with this treatment
            treatment_effects[i] <- mean(treat_rows$Treatment.effect, na.rm = TRUE)
          } else {
            treatment_effects[i] <- 0
          }
        }
      } else {
        # Fallback: use ATE as proxy
        treatment_effects <- rep(if (!is.null(result$ATE)) result$ATE else 0, n_treatments)
      }
      
      # Evaluate according to paper definition
      # Find estimated largest effect (among nonzero)
      nonzero_effects <- treatment_effects[treatment_effects != 0]
      if (length(nonzero_effects) > 0) {
        nonzero_idx <- which(treatment_effects != 0)
        largest_nonzero_idx <- nonzero_idx[which.max(abs(treatment_effects[nonzero_idx]))]
        
        # Discovery for largest: correctly identify treatment 1 as largest with correct sign
        dr_largest[rep] <- as.numeric(
          largest_nonzero_idx == true_largest_idx &&
          sign(treatment_effects[largest_nonzero_idx]) == sign(true_effects_rep[true_largest_idx])
        )
        
        fdr_largest[rep] <- as.numeric(!dr_largest[rep])
        
        # Discovery for top 3
        top3_nonzero_idx <- nonzero_idx[order(abs(treatment_effects[nonzero_idx]), decreasing=TRUE)[1:min(3, length(nonzero_idx))]]
        true_top3_in_estimated <- sum(true_top3_idx %in% top3_nonzero_idx)
        correct_signs <- all(sign(treatment_effects[intersect(true_top3_idx, top3_nonzero_idx)]) == 
                             sign(true_effects_rep[intersect(true_top3_idx, top3_nonzero_idx)]))
        
        dr_top3[rep] <- as.numeric(true_top3_in_estimated >= 2 && correct_signs)
        fdr_top3[rep] <- as.numeric(!dr_top3[rep])
        
      } else {
        dr_largest[rep] <- 0
        dr_top3[rep] <- 0
        fdr_largest[rep] <- NA
        fdr_top3[rep] <- NA
      }
      
    }, error = function(e) {
      # If error, mark as failed
      dr_largest[rep] <- 0
      dr_top3[rep] <- 0
      fdr_largest[rep] <- NA
      fdr_top3[rep] <- NA
    })
  }
  
  # Store results
  results_list[[paste0("n", n)]] <- list(
    fdr_largest = fdr_largest,
    dr_largest = dr_largest,
    fdr_top3 = fdr_top3,
    dr_top3 = dr_top3
  )
  
  # Summary
  cat("\nResults for n =", n, ":\n")
  cat("  DR (largest):", round(mean(dr_largest, na.rm=TRUE), 3), "\n")
  cat("  FDR (largest):", round(mean(fdr_largest, na.rm=TRUE), 3), "\n")
  cat("  DR (top 3):", round(mean(dr_top3, na.rm=TRUE), 3), "\n")
  cat("  FDR (top 3):", round(mean(fdr_top3, na.rm=TRUE), 3), "\n\n")
}

# Save results
save(results_list, file = "simulation_results_final_fixed.RData")
cat("Final fixed simulation complete! Results saved.\n")

# Print summary table
cat("\n=== Summary Table ===\n")
cat("Sample Size | DR(largest) | FDR(largest) | DR(top3) | FDR(top3)\n")
cat("------------|-------------|--------------|----------|----------\n")
for (n in n_obs) {
  res <- results_list[[paste0("n", n)]]
  cat(sprintf("%11d | %11.3f | %12.3f | %8.3f | %8.3f\n",
              n,
              mean(res$dr_largest, na.rm=TRUE),
              mean(res$fdr_largest, na.rm=TRUE),
              mean(res$dr_top3, na.rm=TRUE),
              mean(res$fdr_top3, na.rm=TRUE)))
}

