## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, warning=FALSE-----------------------------------------------------
library(ReliaGrowR)
library(ReliaPlotR)

## ----echo=TRUE----------------------------------------------------------------
id    <- c("U1","U1","U1","U2","U2","U3","U3","U3","U4","U5","U5")
time  <- c( 120, 310, 560, 200, 480, 95, 280, 430, 370, 150, 490)

## ----echo=TRUE----------------------------------------------------------------
end_times <- c(U1 = 600, U2 = 600, U3 = 600, U4 = 600, U5 = 600)
fit_mcf   <- mcf(id = id, time = time, end_time = end_times)
plotly_mcf(fit_mcf)

## -----------------------------------------------------------------------------
fit_exp <- exposure(id = id, time = time)
plotly_exposure(fit_exp)

## -----------------------------------------------------------------------------
times  <- c(120, 200, 310, 370, 430, 480, 490, 560)
events <- c(  1,   1,   1,   1,   1,   1,   1,   1)
fit_nhpp <- nhpp(time = times, event = events, model_type = "Power Law")
plotly_nhpp(fit_nhpp)

## -----------------------------------------------------------------------------
# Dense failures before the design change (t < 500), sparse afterwards
times2  <- c(30, 65, 105, 150, 200, 255, 315, 380, 450, 800, 1300, 1950, 2800)
events2 <- rep(1, 13)
fit_nhpp2 <- nhpp(time = times2, event = events2, breaks = 500, method = "LS")
plotly_nhpp(fit_nhpp2, fitCol = "steelblue", confCol = "steelblue", breakCol = "red")

## -----------------------------------------------------------------------------
id2   <- c("A","A","B","B","B","C")
time2 <- c(180, 420, 90, 310, 530, 260)
end2  <- c(A = 600, B = 600, C = 600)
fit_mcf2 <- mcf(id = id2, time = time2, end_time = end2)

plotly_mcf(list(fit_mcf, fit_mcf2), cols = c("steelblue", "tomato"))

