# Data Description: Provide a detailed overview of the dataset used (from the main application), including information about the variables in the dataset. If the paper involves a regression model, identify the response variable and predictor variables. Include the first few rows of the datasets in the report as a table. This section should also include essential exploratory data analysis (EDA) which you perform, including univariate, bivariate, multivariate (e.g., visualizing potential interaction effects) analyses. The EDA should include visualizations (with informative titles and labels) and summary statistics with descriptions of the main results from the visualizations.

library(FindIt)
library(kableExtra)
library(scales)

# GerberGreen
tbl.prev <- function(df, n = 3){
  tblh <- head(df, n)
  tblt <- tail(df, n)
  vdots_row <- as.data.frame(matrix("\\vdots", nrow = 1, ncol = ncol(tbl)))
  names(vdots_row) <- names(tbl)
  tbl <- rbind(tblh, vdots_row, tblt)
  row.names(tbl)[n+1] <- ""
  return(tbl)
}

tbl <- tbl.prev(GerberGreen)


k <- kbl(
  tbl,
  escape = FALSE,
  format = "latex",
  booktabs = TRUE,
  align = "c",
  position = 'h',
  linesep = '',
  caption = "1998 New Haven Get-Out-the-Vote",
  label = "GerberGreen",
)

save_kable(k, file = "Report/tbls/GerberGreentbl.tex")

# LaLonde