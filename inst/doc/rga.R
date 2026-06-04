## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, warning=FALSE-----------------------------------------------------
library(ReliaGrowR)
library(ReliaPlotR)

## -----------------------------------------------------------------------------
times <- c(100, 200, 300, 400, 500)
failures <- c(1, 2, 1, 3, 2)

## -----------------------------------------------------------------------------
result <- rga(times, failures)
plotly_rga(result)

## -----------------------------------------------------------------------------
times <- c(25, 55, 97, 146, 201, 268, 341, 423, 513, 609, 710, 820, 940, 1072, 1217)
failures <- c(1, 1, 2, 4, 4, 1, 1, 2, 1, 4, 1, 1, 3, 3, 4)
breaks <- 500

## -----------------------------------------------------------------------------
result <- rga(times, failures, model_type = "Piecewise NHPP", breaks = breaks)
plotly_rga(result, fitCol = "blue", confCol = "blue", breakCol = "red")

## -----------------------------------------------------------------------------
times <- c(100, 200, 300, 400, 500)
failures <- c(1, 2, 1, 3, 2)

## -----------------------------------------------------------------------------
fit <- duane(times, failures)
plotly_duane(fit, fitCol = "darkgreen", confCol = "darkgreen")

## -----------------------------------------------------------------------------
times1 <- c(100, 200, 300, 400, 500)
failures1 <- c(1, 2, 1, 3, 2)
result1 <- rga(times1, failures1)

times2 <- c(50, 150, 250, 350, 450)
failures2 <- c(2, 1, 3, 1, 2)
result2 <- rga(times2, failures2)

plotly_rga(list(result1, result2), cols = c("steelblue", "tomato"))

