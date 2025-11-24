# Run FindIt method on LaLonde dataset
# Binary treatment, focus on treatment-covariate interactions

library(FindIt)

# Load prepared data
load("lalonde_prepared.RData")

cat("Running FindIt on LaLonde dataset...\n")
cat("Observations:", length(Y), "\n")
cat("Treatment: ", sum(T_var), "treated, ", sum(1-T_var), "control\n", sep="")
cat("Outcome: ", sum(Y), "positive, ", sum(1-Y), "negative\n", sep="")
cat("Covariates:", paste(V_vars, collapse=", "), "\n\n")

# Prepare data frame
lalonde_df <- LaLonde
lalonde_df$outcome <- Y
lalonde_df$treat <- T_var

# Run FindIt
# For LaLonde: single treatment, so treat.type="single"
# We want to find treatment-covariate interactions
cat("Fitting FindIt model...\n")
cat("This may take a few minutes...\n\n")

tryCatch({
  # First, let FindIt search for lambdas (this takes time)
  # Then we can use those lambdas for faster runs
  result <- FindIt(
    model.treat = outcome ~ treat,
    model.main = ~ age + educ + black,  # Main effects
    model.int = ~ age + educ + black,   # Covariates to interact with treatment
    data = lalonde_df,
    type = "binary",
    treat.type = "single",
    search.lambdas = TRUE
  )
  
  cat("\nFindIt model fitted successfully!\n")
  print(summary(result))
  
  # Extract ATE
  cat("\n=== Results ===\n")
  cat("Average Treatment Effect (ATE):", result$ATE, "\n")
  
  # Get nonzero coefficients
  if (!is.null(result$coefs)) {
    nonzero_idx <- which(result$coefs != 0)
    nonzero_coefs <- result$coefs[nonzero_idx]
    cat("\nNonzero coefficients:", length(nonzero_coefs), "\n")
    cat("\nNonzero coefficients:\n")
    print(nonzero_coefs)
    
    # Focus on treatment-covariate interactions
    treat_int_coefs <- nonzero_coefs[grepl("treat:", names(nonzero_coefs))]
    if (length(treat_int_coefs) > 0) {
      cat("\nTreatment-covariate interactions:\n")
      print(treat_int_coefs)
    }
  }
  
  # Get predicted treatment effects
  pred <- predict(result)
  cat("\nPredicted treatment effects summary:\n")
  print(summary(pred$data$Treatment.effect))
  
  # Save results
  save(result, pred, lalonde_df, file = "findit_lalonde_results.RData")
  cat("\nResults saved to findit_lalonde_results.RData\n")
  
}, error = function(e) {
  cat("\nError running FindIt:\n")
  print(e)
})

