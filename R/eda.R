# Data Description: Provide a detailed overview of the dataset used (from the main application), including information about the variables in the dataset. If the paper involves a regression model, identify the response variable and predictor variables. Include the first few rows of the datasets in the report as a table. This section should also include essential exploratory data analysis (EDA) which you perform, including univariate, bivariate, multivariate (e.g., visualizing potential interaction effects) analyses. The EDA should include visualizations (with informative titles and labels) and summary statistics with descriptions of the main results from the visualizations.

library(FindIt)
library(kableExtra)

# GerberGreen
k <- kbl(
  head(GerberGreen),
  format = "latex",
  booktabs = TRUE,
  align = "c",
  position = 'h',
  linesep = 0,
  caption = "1998 New Haven Get-Out-the-Vote",
  label = "GerberGreen"
)
save_kable(k, file = "Report/tbls/GerberGreentbl.tex")

# LaLonde