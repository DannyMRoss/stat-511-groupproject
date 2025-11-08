library(FindIt)
library(kableExtra)
library(scales)
library(data.table)

tbl.prev <- function(df, n = 3, digits = 2){
  tblh <- head(df, n)
  tblt <- tail(df, n)
  vdots_row <- as.data.frame(matrix("\\vdots", nrow = 1, ncol = ncol(tblh)))
  names(vdots_row) <- names(tblh)
  tblh[] <- lapply(tblh, function(x) if(is.numeric(x)) round(x, digits) else x)
  tblt[] <- lapply(tblt, function(x) if(is.numeric(x)) round(x, digits) else x)
  tbl <- as.data.frame(rbind(tblh, vdots_row, tblt))
  row.names(tbl) <- c(1:n,"",(nrow(df)-n+1):nrow(df))
  return(tbl)
}

two.way.bp <- function(
    df,
    x = "persngrp",
    y = "voted98",
    x.desc = "Visit",
    y.desc = "Voting Outcome",
    x.levels = c("0=No", "1=Yes"),
    y.levels = c("0=No", "1=Yes"),
    ylab = "Count",
    cols = c("lightgray", "black"),
    yadj = .025,
    cex.main = .75,
    cex.axis = 0.5,
    cex.text = 0.75,
    cex.legend = 0.75,
    legend = FALSE
){
  tab <- t(table(df[[x]], df[[y]]))
  prop <- prop.table(tab, margin = 2)
  label = paste0(round(as.vector(prop)*100, 1), "%")
  ymax <- max(as.vector(tab) * yadj)
  bp <- barplot(
    tab, 
    beside = TRUE,
    col = cols,
    main = x.desc,
    cex.main = cex.main,
    ylim = c(0,max(tab) * (1+yadj*2)),
    xaxt = 'n',
    yaxt = 'n'
  )
  axis(1, at = colMeans(bp), labels = x.levels, line = -1, tick = FALSE, cex.axis = cex.axis)
  if (legend){
    legend("topright", title = y, legend = y.levels, fill = cols,
           bty = 'n', cex = cex.legend)
  }
  text(as.vector(bp), as.vector(tab) + ymax, labels = label, cex = cex.text)
}

# GerberGreen ----
## tables ----
GerberGreen.tbl <- tbl.prev(GerberGreen)

k <- kbl(
  GerberGreen.tbl,
  row.names = TRUE,
  escape = FALSE,
  format = "latex",
  booktabs = TRUE,
  align = "c",
  linesep = '',
  caption = "Gerber and Green (1998) New Haven Get-Out-the-Vote",
  label = "GerberGreen") %>% 
  kable_styling(
    latex_options = c("scale_down", "hold_position")
  )

save_kable(k, file = "Report/tbls/GerberGreentbl.tex")

phnscrpt.labels <- c("None","Civic-Blood","Civic","Civic or Blood-Civic", "Neighbor","Neighbor or Civic-Neighbor","Close")
appeal.labels <- c("Civic Duty", "Neighborhood Solidarity","Close Election")
persngrp.labels <- c("No","Yes")
treatments <- c("persngrp","phnscrpt","mailings","appeal")
controls <- c("age_g", "majorpty", "vote96.1", "vote96.0")
dt <- copy(GerberGreen)
dt[, phnscrpt := factor(phnscrpt, levels = 0:6, labels = phnscrpt.labels)]
dt[, persngrp := factor(persngrp, levels = 0:1, labels = persngrp.labels)]
dt[, appeal := factor(appeal, levels = 1:3, labels = appeal.labels)]

dt[, age_g := cut(age, breaks = c(18,29,39,49,59,79,99), include.lowest = T, ordered_result = T)]

A <- dt[,.("Registered" = .N, "Voted" = sum(voted98)), keyby = treatments]
A[, `Proportion` := percent(Voted / `Registered`,.1)]
setnames(A, treatments, c("Visit","Phone","Mailings","Appeal"))
setorder(A, -Proportion)

B <- dt[,.("Registered" = .N, "Voted" = sum(voted98)), keyby = controls]
B[, `Proportion` := percent(Voted / `Registered`,.1)]
setnames(B, controls, c("Age","Major Party","Voted in '96","Abstained in '96"))

k <- kbl(
  A,
  row.names = TRUE,
  format = "latex",
  booktabs = TRUE,
  align = "c",
  linesep = '',
  caption = "Get-Out-the-Vote Treatment Interactions",
  label = "GerberGreenTreatmentInteraction") %>% 
  kable_styling(
    latex_options = c("scale_down", "hold_position"),
    font_size = 6
  )

save_kable(k, file = "Report/tbls/GerberGreenTreatmentInteraction.tex")

k <- kbl(
  B,
  row.names = TRUE,
  format = "latex",
  booktabs = TRUE,
  align = "c",
  linesep = '',
  caption = "Get-Out-the-Vote Control Interactions",
  label = "GerberGreenControlInteraction") %>% 
  kable_styling(
    latex_options = c("scale_down", "hold_position"),
    font_size = 6
  )

save_kable(k, file = "Report/tbls/GerberGreenControlInteraction.tex")

## figures ----

GerberGreenTreatments <- function(){
  graphics.off()
  png(
    "Report/figs/GerberGreenTreatments.png",
    width = 6.5,
    height = 6.5,
    units = "in",
    res = 1000
  )
  par(mfrow = c(2, 2), mar = c(1,1,1,1))
  two.way.bp(
    GerberGreen,
    x.desc = "Visit"
  )
  two.way.bp(
    GerberGreen,
    x = "phnscrpt",
    x.desc = "Phone Message", 
    x.levels = phnscrpt.labels,
    cex.text = .25,
    cex.axis = .25,
    legend = TRUE
  )
  two.way.bp(
    GerberGreen,
    x = "mailings",
    x.desc = "Mailings",
    x.levels = c(0,1,2,3),
    cex.text = .5
  )
  two.way.bp(
    GerberGreen,
    x = "appeal",
    x.desc = "Appeal", 
    x.levels = appeal.labels,
  )
  dev.off()
}

GerberGreenTreatments()

GerberGreenControls <- function(){
  graphics.off()
  png(
    "Report/figs/GerberGreenControls.png",
    width = 6.5,
    height = 6.5,
    units = "in",
    res = 1000
  )
  par(mfrow = c(2, 2), mar = c(1,1,1,1))
  two.way.bp(
    dt,
    x = "age_g",
    x.desc = "Age",
    x.levels = dt[,levels(age_g)],
    cex.text = 0.4,
  )
  two.way.bp(
    GerberGreen,
    x = "majorpty",
    x.desc = "Major Party", 
    x.levels = c("0=Not Democratic or Republican", "1=Democratic or Republican")
  )
  two.way.bp(
    GerberGreen,
    x = "vote96.1",
    x.desc = "Voted in 1996",
    x.levels = c("0=No", "1=Yes")
  )
  two.way.bp(
    GerberGreen,
    x = "vote96.0",
    x.desc = "Abstained in 1996",
    x.levels = c("0=No", "1=Yes"),
    legend = TRUE
  )
  dev.off()
}

GerberGreenControls()

# LaLonde ----

LaLonde.tbl <- tbl.prev(LaLonde)

k <- kbl(
  LaLonde.tbl,
  row.names = TRUE,
  escape = FALSE,
  format = "latex",
  booktabs = TRUE,
  align = "c",
  linesep = '',
  caption = "LaLonde (1986) National Supported Work Study",
  label = "LaLonde"
) %>% 
  kable_styling(
    latex_options = c("scale_down", "hold_position")
  )

save_kable(k, file = "Report/tbls/LaLondetbl.tex")
