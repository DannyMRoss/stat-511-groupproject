# Prepare LaLonde dataset for FindIt analysis
# LaLonde: binary treatment, focus on treatment-covariate interactions

library(FindIt)

# Load data
data(LaLonde)
cat("LaLonde dataset loaded:", nrow(LaLonde), "observations\n")

# Check data structure
cat("\nData structure:\n")
str(LaLonde)

# Check variable names
cat("\nVariable names:\n")
print(names(LaLonde))

# According to paper:
# - Binary treatment variable (K=1)
# - Outcome: earnings in 1978 > earnings in 1975
# - Pre-treatment covariates: age, education, race, earnings

# Check treatment variable
if ("treat" %in% names(LaLonde)) {
  cat("\nTreatment variable (treat):\n")
  print(table(LaLonde$treat))
} else {
  cat("\nWarning: 'treat' variable not found. Available variables:\n")
  print(names(LaLonde))
}

# Check outcome variable
# Paper says: binary outcome of whether earnings in 1978 > earnings in 1975
# Need to check if this is already computed or needs to be created
if ("outcome" %in% names(LaLonde)) {
  cat("\nOutcome variable found:\n")
  print(table(LaLonde$outcome))
} else if ("re78" %in% names(LaLonde) && "re75" %in% names(LaLonde)) {
  # Create outcome: re78 > re75
  LaLonde$outcome <- as.numeric(LaLonde$re78 > LaLonde$re75)
  cat("\nCreated outcome variable (re78 > re75):\n")
  print(table(LaLonde$outcome))
} else {
  cat("\nWarning: Outcome variable not found. Need to check data structure.\n")
}

# Prepare covariates for V matrix
# According to paper: main effects of pre-treatment covariates
# Variables: age, education, race, earnings (re75)

V_vars <- c()
if ("age" %in% names(LaLonde)) {
  V_vars <- c(V_vars, "age")
}
if ("educ" %in% names(LaLonde) || "education" %in% names(LaLonde)) {
  educ_var <- ifelse("educ" %in% names(LaLonde), "educ", "education")
  V_vars <- c(V_vars, educ_var)
}
if ("black" %in% names(LaLonde)) {
  V_vars <- c(V_vars, "black")
}
if ("hispan" %in% names(LaLonde) || "hispanic" %in% names(LaLonde)) {
  hispan_var <- ifelse("hispan" %in% names(LaLonde), "hispan", "hispanic")
  V_vars <- c(V_vars, hispan_var)
}
if ("re75" %in% names(LaLonde)) {
  V_vars <- c(V_vars, "re75")
}

cat("\nCovariates to include in V matrix:", paste(V_vars, collapse=", "), "\n")

# Create V matrix (main effects only for LaLonde)
V <- LaLonde[, V_vars, drop = FALSE]
V <- as.data.frame(lapply(V, as.numeric))

cat("\nV matrix created:", nrow(V), "x", ncol(V), "\n")
cat("V variables:", paste(colnames(V), collapse=", "), "\n")

# For LaLonde, Z matrix will be treatment-covariate interactions
# Z consists of: treatment indicator + 20 treatment-covariate interactions
# But we'll let FindIt handle this automatically

# Treatment variable
if ("treat" %in% names(LaLonde)) {
  T_var <- LaLonde$treat
} else {
  cat("\nError: Treatment variable not found\n")
  T_var <- NULL
}

# Outcome
Y <- LaLonde$outcome

# Save prepared data
save(LaLonde, V, Y, T_var, V_vars,
     file = "lalonde_prepared.RData")

cat("\nData preparation complete. Saved to lalonde_prepared.RData\n")
cat("\nSummary:\n")
cat("- Total observations:", nrow(LaLonde), "\n")
if (!is.null(T_var)) {
  cat("- Treatment: ", sum(T_var), "treated, ", sum(1-T_var), "control\n", sep="")
}
cat("- Outcome: ", sum(Y), "positive, ", sum(1-Y), "negative\n", sep="")
cat("- V matrix (covariates):", nrow(V), "x", ncol(V), "\n")

