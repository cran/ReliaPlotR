## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ReliaPlotR)
library(WeibullR)
library(WeibullR.ALT)

## ----data---------------------------------------------------------------------
d1 <- alt.data(c(248, 456, 528, 731, 813, 537), stress = 300)
d2 <- alt.data(c(164, 176, 289), stress = 350)
d3 <- alt.data(c(88, 112, 152), stress = 400)

## ----parallel, results = "hide"-----------------------------------------------
obj <- alt.parallel(
  alt.make(list(d1, d2, d3), dist = "weibull", alt.model = "arrhenius", view_dist_fits = FALSE),
  view_parallel_fits = FALSE
)

## ----fit, results = "hide"----------------------------------------------------
obj <- alt.fit(obj)

## ----prob-plot----------------------------------------------------------------
plotly_alt(obj)

## ----prob-custom--------------------------------------------------------------
plotly_alt(
  obj,
  main    = "Reliability Test Results",
  xlab    = "Hours to Failure",
  cols    = c("#1f77b4", "#ff7f0e", "#2ca02c"),
  showGrid = FALSE
)

## ----rel-plot-----------------------------------------------------------------
plotly_rel(obj)

## ----rel-percentiles----------------------------------------------------------
plotly_rel(obj, percentiles = c(5, 50, 95))

## ----rel-no-perc--------------------------------------------------------------
plotly_rel(obj, showPerc = FALSE)

## ----rel-custom---------------------------------------------------------------
plotly_rel(
  obj,
  main    = "Arrhenius Life-Stress Relationship",
  fitCol  = "darkgreen",
  percCol = "steelblue",
  signif  = 4
)

