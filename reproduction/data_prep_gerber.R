# Prepare GerberGreen dataset for FindIt analysis
# Create Z matrix (treatment indicators) and V matrix (covariates)

library(FindIt)
library(dplyr)

# Load data
data(GerberGreen)
cat("GerberGreen dataset loaded:", nrow(GerberGreen), "observations\n")

# Check data structure
cat("\nData structure:\n")
str(GerberGreen)

# Create treatment combination indicator
# According to paper: 192 binary indicators for treatment combinations
# But we have 72 unique combinations in the data (69 active + 3 control variations)
treatment_combo <- paste(GerberGreen$persngrp, 
                         GerberGreen$phnscrpt, 
                         GerberGreen$mailings, 
                         GerberGreen$appeal, 
                         sep="-")

unique_combos <- unique(treatment_combo)
cat("\nUnique treatment combinations:", length(unique_combos), "\n")

# Create Z matrix: binary indicators for each unique treatment combination
# Exclude control (0-0-0-X) as baseline
control_mask <- (GerberGreen$persngrp == "0" & 
                GerberGreen$phnscrpt == "0" & 
                GerberGreen$mailings == "0")

# Get active treatment combinations (excluding control)
active_combos <- unique(treatment_combo[!control_mask])
cat("Active treatment combinations (excluding control):", length(active_combos), "\n")

# Create Z matrix: one column per active treatment combination
Z <- matrix(0, nrow = nrow(GerberGreen), ncol = length(active_combos))
colnames(Z) <- paste0("Z_", active_combos)

for (i in 1:length(active_combos)) {
  Z[, i] <- as.numeric(treatment_combo == active_combos[i])
}

cat("Z matrix created:", nrow(Z), "x", ncol(Z), "\n")
cat("Sum of Z indicators (should equal non-control obs):", sum(Z), "\n")
cat("Non-control observations:", sum(!control_mask), "\n")

# Create V matrix: pre-treatment covariates
# According to paper: main effects + two-way interactions + age^2
# Variables: age, majorpty, voted96.1, voted96.0

# Main effects
# Note: variable names in data are vote96.1 and vote96.0 (with dot)
V_main <- data.frame(
  age = as.numeric(GerberGreen$age),
  majorpty = as.numeric(GerberGreen$majorpty == "1"),  # 1 = Democratic
  voted96_1 = as.numeric(GerberGreen$vote96.1 == "1"),
  voted96_0 = as.numeric(GerberGreen$vote96.0 == "1")
)

# Two-way interactions (6 pairs: C(4,2) = 6)
V_interactions <- data.frame(
  age_majorpty = V_main$age * V_main$majorpty,
  age_voted96_1 = V_main$age * V_main$voted96_1,
  age_voted96_0 = V_main$age * V_main$voted96_0,
  majorpty_voted96_1 = V_main$majorpty * V_main$voted96_1,
  majorpty_voted96_0 = V_main$majorpty * V_main$voted96_0,
  voted96_1_voted96_0 = V_main$voted96_1 * V_main$voted96_0
)

# Age squared
V_age_sq <- data.frame(age_sq = V_main$age^2)

# Combine V matrix: 4 main + 6 interactions + 1 age^2 = 11 variables
# But paper says KV = 10, so maybe intercept is included separately
V <- cbind(V_main, V_interactions, V_age_sq)
cat("\nV matrix created:", nrow(V), "x", ncol(V), "\n")
cat("V variables:", paste(colnames(V), collapse=", "), "\n")

# Outcome variable
Y <- as.numeric(GerberGreen$voted98)

# Save prepared data
save(GerberGreen, Z, V, Y, treatment_combo, active_combos, control_mask,
     file = "gerber_prepared.RData")

cat("\nData preparation complete. Saved to gerber_prepared.RData\n")
cat("\nSummary:\n")
cat("- Total observations:", nrow(GerberGreen), "\n")
cat("- Control observations:", sum(control_mask), "\n")
cat("- Treatment observations:", sum(!control_mask), "\n")
cat("- Z matrix (treatment indicators):", nrow(Z), "x", ncol(Z), "\n")
cat("- V matrix (covariates):", nrow(V), "x", ncol(V), "\n")
cat("- Outcome (voted98):", sum(Y), "voted,", sum(1-Y), "did not vote\n")

