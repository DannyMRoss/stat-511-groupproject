# packages ----
library(FindIt)
library(kableExtra)
library(scales)
library(data.table)

# helper functions ----
## fcn to format dataset for preview tables
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

## fcn for two-way barplots for outcome by treatment or control
two.way.bp <- function(
    df,
    x = "persngrp",
    y = "voted98",
    x.desc = "Visit",
    y.desc = "Voting Outcome",
    x.levels = c("0=No", "1=Yes"),
    y.levels = c("0=No", "1=Yes"),
    ylab = "Count",
    xlab = NULL,
    cols = c("lightgray", "black"),
    yadj = .04,
    cex.main = .75,
    cex.axis = 0.5,
    cex.text = 0.75,
    cex.legend = 0.75,
    yat = seq(0,7000,1000),
    legend = FALSE
){
  tab <- t(table(df[[x]], df[[y]]))
  prop <- prop.table(tab, margin = 2)
  label = paste0(round(as.vector(prop)*100, 1), "%")
  ym <- max(yat)*(1+3*yadj)
  yadd <- ym * yadj
  bp <- barplot(
    tab, 
    beside = TRUE,
    col = cols,
    main = x.desc,
    cex.main = cex.main,
    ylim = c(0,ym),
    yaxt = 'n',
    xaxt = 'n'
    )
  axis(
    1, 
    at = colMeans(bp), 
    labels = x.levels, 
    line = 0, 
    tick = FALSE, 
    cex.axis = cex.axis
    )
  axis(
    2,
    at = yat,
    line = 0,
    labels = yat,
    tick = F,
    cex.axis = 0.5
    )
  if (!is.null(xlab)){
    mtext(xlab, side = 1, line = 1)
  }
  if (legend){
    legend(
      "topright",
      title = y,
      legend = y.levels,
      fill = cols,
      bty = 'n',
      cex = cex.legend
      )
  }
  text(as.vector(bp), as.vector(tab) + yadd, labels = label, cex = cex.text)
}

# GerberGreen ----
## preview table ----
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

## ... prep for interaction tables ...
phnscrpt.labels <- c("None","Civic-Blood","Civic","Civic or Blood-Civic", "Neighbor","Neighbor or Civic-Neighbor","Close")
appeal.labels <- c("Civic Duty", "Neighborhood Solidarity","Close Election")
persngrp.labels <- c("No","Yes")
treatments <- c("persngrp","phnscrpt","mailings","appeal")
controls <- c("age_g", "majorpty", "vote96.1", "vote96.0")
dt <- copy(GerberGreen)
setDT(dt)
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

## treatment interaction table ----
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

## control interaction table ----
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
    font_size = 8
  )

save_kable(k, file = "Report/tbls/GerberGreenControlInteraction.tex")

## treatment barplot ----
GerberGreenTreatments <- function(
    file = "Report/figs/GerberGreenTreatments.png",
    width = 6.5,
    height = 6.5,
    units = "in",
    res = 1000
    ){
  graphics.off()
  png(
    file,
    width = width,
    height = height,
    units = units,
    res = res
    )
  par(
    mfrow = c(2, 2),
    mar = c(2, 2, 1, 1),
    mgp = c(3, 0, 0)
    )
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
    cex.axis = .25
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
    legend = TRUE
    )
  dev.off()
}

GerberGreenTreatments(height = 3.5)

## control barplot ----
GerberGreenControls <- function(
    file = "Report/figs/GerberGreenControls.png",
    width = 6.5,
    height = 6.5,
    units = "in",
    res = 1000
    ){
  graphics.off()
  png(
    file,
    width = width,
    height = height,
    units = units,
    res = res
  )
  par(
    mfrow = c(2, 2),
    mar = c(2, 2, 1, 1),
    mgp = c(3, 0, 0)
    )
  two.way.bp(
    dt,
    x = "age_g",
    x.desc = "Age",
    x.levels = dt[,levels(age_g)],
    cex.text = 0.4
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

GerberGreenControls(height = 3.5)

# LaLonde ----
## preview table ----
LaLonde$wts.extrap <- NULL

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

## in text stats ----
nrow(LaLonde)
table(LaLonde$outcome)
prop.table(table(LaLonde$outcome))

table(LaLonde$outcome, LaLonde$treat)
prop.table(table(LaLonde$outcome, LaLonde$treat))

## treatment barplot
LaLondeTreatments <- function(
    file = "Report/figs/LaLondeTreatments.png",
    width = 3.5,
    height = 2,
    units = "in",
    res = 1000
){
  graphics.off()
  png(
    file,
    width = width,
    height = height,
    units = units,
    res = res
  )
  par(
    mfrow = c(1, 1),
    mar = c(2, 2, 1, 1),
    mgp = c(3, 0, 0)
  )
  two.way.bp(
    LaLonde,
    x = "treat",
    y = "outcome",
    x.desc = "Job Training on Earnings",
    y.desc = "Earnings Outcome",
    x.levels = c("0=No", "1=Yes"),
    y.levels = c("0=1978 Earnings Not Larger", "1=1978 Earnings Larger"),
    ylab = "Count",
    xlab = "Treatment",
    cols = c("lightgray", "black"),
    yadj = .025,
    cex.main = .5,
    cex.axis = .5,
    cex.text = 0.5,
    cex.legend = 0.5,
    yat = seq(0,400,100),
    legend = TRUE
  )
  dev.off()
}

LaLondeTreatments()

## ... prep for control barplot ....
dt <- copy(LaLonde)
setDT(dt)
dt[, age_g := cut(age, breaks = c(17,22,27,32,37,42,47,52,57), include.lowest = T, ordered_result = T)]
dt[, educ_g := cut(educ, breaks = c(3,6,9,12,16), include.lowest = T, ordered_result = T)]
dt[, log.re75_g := cut(log.re75, breaks = c(0,5:11), include.lowest = T, ordered_result = T)]
levels(dt$log.re75_g)[1] <- "0"

## control barplot ----
LaLondeControls <- function(
    file = "Report/figs/LaLondeControls.png",
    width = 6.5,
    height = 4.5,
    units = "in",
    res = 1000
){
  graphics.off()
  png(
    file,
    width = width,
    height = height,
    units = units,
    res = res
  )
  par(
    mfrow = c(3, 3),
    mar = c(2, 2, 1, 1),
    mgp = c(3, 0, 0)
  )
  two.way.bp(
    dt,
    x = "age_g",
    y = "outcome",
    x.desc = "Age",
    y.desc = "Earnings Outcome",
    x.levels = levels(dt$age_g),
    y.levels = c("0=1978 Earnings Not Larger", "1=1978 Earnings Larger"),
    cex.axis = 0.75,
    cex.text = 0.25,
    yat = seq(0,400,100),
    legend = FALSE
  )
  two.way.bp(
    dt,
    x = "educ_g",
    y = "outcome",
    x.desc = "Education",
    y.desc = "Earnings Outcome",
    x.levels = levels(dt$educ_g),
    y.levels = c("0=1978 Earnings Not Larger", "1=1978 Earnings Larger"),
    cex.axis = 0.75,
    cex.text = 0.25,
    yat = seq(0,400,100),
    legend = FALSE
  )
  two.way.bp(
    LaLonde,
    x = "black",
    y = "outcome",
    x.desc = "Black",
    y.desc = "Earnings Outcome",
    x.levels = c("0=No", "1=Yes"),
    y.levels = c("0=1978 Earnings Not Larger", "1=1978 Earnings Larger"),
    cex.axis = 0.75,
    yat = seq(0,400,100),
    legend = FALSE
  )
  two.way.bp(
    LaLonde,
    x = "hisp",
    y = "outcome",
    x.desc = "Hispanic",
    y.desc = "Earnings Outcome",
    x.levels = c("0=No", "1=Yes"),
    y.levels = c("0=1978 Earnings Not Larger", "1=1978 Earnings Larger"),
    cex.axis = 0.75,
    yat = seq(0,400,100),
    legend = FALSE
  )
  two.way.bp(
    LaLonde,
    x = "white",
    y = "outcome",
    x.desc = "White",
    y.desc = "Earnings Outcome",
    x.levels = c("0=No", "1=Yes"),
    y.levels = c("0=1978 Earnings Not Larger", "1=1978 Earnings Larger"),
    cex.axis = 0.75,
    yat = seq(0,400,100),
    legend = FALSE
  )
  two.way.bp(
    LaLonde,
    x = "marr",
    y = "outcome",
    x.desc = "Married",
    y.desc = "Earnings Outcome",
    x.levels = c("0=No", "1=Yes"),
    y.levels = c("0=1978 Earnings Not Larger", "1=1978 Earnings Larger"),
    cex.axis = 0.75,
    yat = seq(0,400,100),
    legend = FALSE
  )
  two.way.bp(
    LaLonde,
    x = "nodegr",
    y = "outcome",
    x.desc = "No High School Degree",
    y.desc = "Earnings Outcome",
    x.levels = c("0=No", "1=Yes"),
    y.levels = c("0=1978 Earnings Not Larger", "1=1978 Earnings Larger"),
    cex.axis = 0.75,
    yat = seq(0,400,100),
    legend = FALSE
  )
  two.way.bp(
    dt,
    x = "log.re75_g",
    y = "outcome",
    x.desc = "1975 log earnings",
    y.desc = "Earnings Outcome",
    x.levels = levels(dt$log.re75_g),
    y.levels = c("0=1978 Earnings Not Larger", "1=1978 Earnings Larger"),
    cex.axis = 0.75,
    cex.text = 0.25,
    yat = seq(0,400,100),
    legend = FALSE
  )
  two.way.bp(
    LaLonde,
    x = "u75",
    y = "outcome",
    x.desc = "1975 Unemployment",
    y.desc = "Earnings Outcome",
    x.levels = c("0=No", "1=Yes"),
    y.levels = c("0=1978 Earnings Not Larger", "1=1978 Earnings Larger"),
    cex.axis = 0.75,
    cex.legend = .5,
    yat = seq(0,400,100),
    legend = TRUE
  )
  dev.off()
}

LaLondeControls()

