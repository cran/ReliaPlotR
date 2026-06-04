## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(WeibullR)
library(ReliaPlotR)

## ----echo=TRUE----------------------------------------------------------------
failures <- c(30, 49, 82, 90, 96)

## ----echo=TRUE----------------------------------------------------------------
obj <- wblr.conf(wblr.fit(wblr(failures)))
plotly_wblr(obj)

## ----echo=TRUE----------------------------------------------------------------
suspensions <- c(100, 45, 10)
obj <- wblr.conf(wblr.fit(wblr(failures, suspensions)))
plotly_wblr(obj, suspensions, fitCol = "blue", confCol = "blue")

## ----echo=TRUE----------------------------------------------------------------
inspection_data <- data.frame(
  left = c(0, 6.12, 19.92, 29.64, 35.4, 39.72, 45.32, 52.32),
  right = c(6.12, 19.92, 29.64, 35.4, 39.72, 45.32, 52.32, 63.48),
  qty = c(5, 16, 12, 18, 18, 2, 6, 17)
)

## ----echo=TRUE----------------------------------------------------------------
suspensions <- data.frame(time = 63.48, event = 0, qty = 73)

## ----echo=TRUE, warning=FALSE-------------------------------------------------
obj <- wblr(suspensions, interval = inspection_data)
obj <- wblr.fit(obj, method.fit = "mle")
obj <- wblr.conf(obj, method.conf = "fm", lty = 2)
suspensions <- as.vector(suspensions$time)
plotly_wblr(obj,
  susp = suspensions, fitCol = "red", confCol = "red", intCol = "blue",
  main = "Parts Cracking Inspection Interval Analysis",
  ylab = "Cumulative % Cracked", xlab = "Inspection Time"
)

## ----echo=TRUE----------------------------------------------------------------
failures <- c(25, 30, 42, 49, 55, 67, 73, 82, 90, 96, 101, 110, 120, 132, 145)
fit <- wblr.conf(wblr.fit(wblr(failures), dist = "weibull3p"))
plotly_wblr(fit, fitCol = "darkgreen", confCol = "darkgreen")

## ----echo=TRUE----------------------------------------------------------------
failures <- c(30, 49, 82, 90, 96)
obj <- wblr.conf(wblr.fit(wblr(failures), method.fit = "mle"), method.conf = "lrb")
plotly_contour(obj, col = "blue")

## ----echo=TRUE----------------------------------------------------------------
failures1 <- c(30, 49, 82, 90, 96)
failures2 <- c(20, 40, 60, 80, 100)
obj1 <- wblr.conf(wblr.fit(wblr(failures1), method.fit = "mle"), method.conf = "lrb")
obj2 <- wblr.conf(wblr.fit(wblr(failures2), method.fit = "mle"), method.conf = "lrb")
plotly_wblr(list(obj1, obj2), cols = c("steelblue", "tomato"))

