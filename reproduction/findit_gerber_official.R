# Use official FindIt example for GerberGreen factorial design
# Based on FindIt package documentation Example 2

library(FindIt)
data(GerberGreen)

cat("=== Using Official FindIt Example for Factorial Design ===\n\n")
cat("Following Example 2 from FindIt documentation\n\n")

# Check data structure
cat("Data structure:\n")
cat("Observations:", nrow(GerberGreen), "\n")
cat("Variables:", paste(names(GerberGreen), collapse=", "), "\n\n")

# Use EXACT syntax from documentation
cat("Fitting factorial design model...\n")
cat("This matches the official example exactly.\n\n")

# First, try with provided lambdas (faster)
cat("Attempt 1: Using provided lambdas from documentation...\n")
tryCatch({
  result <- FindIt(
    model.treat = voted98 ~ persngrp + phnscrpt + mailings + appeal,
    nway = 4,
    model.main = ~ age + majorpty + vote96.1 + vote96.0,
    data = GerberGreen,
    type = "binary",
    treat.type = "multiple",
    search.lambdas = FALSE,
    lambdas = c(-15.000, -6.237)  # From documentation example
  )
  
  cat("\n✓ Factorial design model fitted successfully!\n")
  print(summary(result))
  
  # Check coefficients
  if (!is.null(result$coefs)) {
    nonzero_idx <- which(result$coefs != 0)
    nonzero_coefs <- result$coefs[nonzero_idx]
    cat("\n=== Nonzero Coefficients ===\n")
    cat("Total nonzero:", length(nonzero_coefs), "\n")
    
    # Look for interaction terms
    interaction_terms <- names(nonzero_coefs)[grepl(":", names(nonzero_coefs))]
    cat("\nTreatment interaction terms:", length(interaction_terms), "\n")
    if (length(interaction_terms) > 0) {
      cat("\nSample interaction terms:\n")
      print(head(interaction_terms, 15))
      cat("\nCorresponding coefficients:\n")
      print(head(nonzero_coefs[interaction_terms], 15))
    }
    
    # Check main effects
    main_effects <- names(nonzero_coefs)[!grepl(":", names(nonzero_coefs)) & 
                                         names(nonzero_coefs) %in% 
                                         c("persngrp", "phnscrpt", "mailings", "appeal")]
    cat("\nTreatment main effects found:", length(main_effects), "\n")
    if (length(main_effects) > 0) {
      print(nonzero_coefs[main_effects])
    }
  }
  
  # Get predicted treatment effects
  cat("\n=== Predicted Treatment Effects ===\n")
  pred <- predict(result, unique = TRUE)
  cat("Unique treatment combinations:", nrow(pred$data), "\n")
  cat("\nTop 10 treatment combinations:\n")
  print(head(pred$data, 10))
  cat("\nBottom 10 treatment combinations:\n")
  print(tail(pred$data, 10))
  
  # Save results
  save(result, pred, file = "findit_gerber_factorial_official.RData")
  cat("\n✓ Results saved to findit_gerber_factorial_official.RData\n")
  
}, error = function(e) {
  cat("\n✗ Error with provided lambdas:\n")
  print(e)
  
  cat("\nAttempt 2: Letting FindIt search for lambdas (will take longer)...\n")
  
  # Try with lambda search
  tryCatch({
    result2 <- FindIt(
      model.treat = voted98 ~ persngrp + phnscrpt + mailings + appeal,
      nway = 4,
      model.main = ~ age + majorpty + vote96.1 + vote96.0,
      data = GerberGreen,
      type = "binary",
      treat.type = "multiple",
      search.lambdas = TRUE  # Let it search
    )
    
    cat("\n✓ Factorial design with lambda search fitted!\n")
    print(summary(result2))
    
    cat("\nOptimal lambdas found:", result2$lambdas, "\n")
    
    save(result2, file = "findit_gerber_factorial_official.RData")
    cat("\n✓ Results saved.\n")
    
  }, error = function(e2) {
    cat("\n✗ Lambda search also failed:\n")
    print(e2)
  })
})

