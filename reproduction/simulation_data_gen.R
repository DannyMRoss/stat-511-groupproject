# Generate simulated data for treatment effect heterogeneity study
# Based on paper Section 4.1, simplified version

generate_sim_data <- function(n, n_treatments = 10, n_covariates = 3, seed = NULL) {
  # Generate data according to paper's DGP
  
  if (!is.null(seed)) set.seed(seed)
  
  # Treatment effects
  # 3 treatments with substantive effects: 7%, 5%, -3%
  # Remaining treatments: negligible effects (±1%)
  beta_true <- c(
    0.07,   # Treatment 1: largest positive effect
    0.05,   # Treatment 2: second largest positive
    -0.03,  # Treatment 3: largest negative
    runif(n_treatments - 3, -0.01, 0.01)  # Remaining: negligible
  )
  
  # Pre-treatment covariates: substantial predictive power
  # Generate from multivariate normal
  V <- matrix(rnorm(n * n_covariates, mean = 0, sd = 1), 
              nrow = n, ncol = n_covariates)
  colnames(V) <- paste0("V", 1:n_covariates)
  
  # Covariate effects (substantial predictive power)
  gamma_true <- c(0.5, -0.3, 0.3)  # Main effects for covariates
  
  # Treatment assignment: random, independent of covariates
  # Each unit assigned to one treatment (including control = 0)
  treatment <- sample(0:n_treatments, size = n, replace = TRUE)
  
  # Create Z matrix: treatment indicators
  Z <- matrix(0, nrow = n, ncol = n_treatments)
  for (i in 1:n_treatments) {
    Z[, i] <- as.numeric(treatment == i)
  }
  colnames(Z) <- paste0("Z", 1:n_treatments)
  
  # Generate outcome from linear probability model
  # Y* = intercept + Z*beta + V*gamma + error
  # Then transform to binary: Y = 1 if Y* > threshold
  
  # Intercept (baseline probability)
  intercept <- 0.4
  
  # Linear predictor
  linear_pred <- intercept + 
                 Z %*% beta_true + 
                 V %*% gamma_true
  
  # Add noise (for linear probability model, we can use logistic or probit)
  # Using probit-like transformation for binary outcome
  prob <- pnorm(linear_pred)  # Transform to [0,1] probability
  
  # Generate binary outcome
  Y <- rbinom(n, size = 1, prob = prob)
  
  # Return data
  return(list(
    Y = Y,
    Z = Z,
    V = V,
    treatment = treatment,
    beta_true = beta_true,
    gamma_true = gamma_true,
    prob = prob,
    n = n,
    n_treatments = n_treatments,
    n_covariates = n_covariates
  ))
}

# Test data generation
cat("Testing data generation...\n")
test_data <- generate_sim_data(n = 1000, seed = 123)

cat("\nGenerated data summary:\n")
cat("- Sample size:", test_data$n, "\n")
cat("- Treatments:", test_data$n_treatments, "\n")
cat("- Covariates:", test_data$n_covariates, "\n")
cat("- Outcome: ", sum(test_data$Y), " positive, ", 
    sum(1-test_data$Y), " negative\n", sep="")
cat("- Treatment distribution:\n")
print(table(test_data$treatment))
cat("\nTrue treatment effects:\n")
print(round(test_data$beta_true, 4))
cat("\nTrue covariate effects:\n")
print(round(test_data$gamma_true, 4))

# Save function
save(generate_sim_data, file = "simulation_data_gen.RData")
cat("\nData generation function saved.\n")

