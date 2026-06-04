## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
has_ellmer <- requireNamespace("ellmer", quietly = TRUE)

## ----setup--------------------------------------------------------------------
library(ReliaPlotR)

## ----install, eval = FALSE----------------------------------------------------
# pak::pak(c("mcptools", "ellmer"))

## ----wblr-example, eval = has_ellmer------------------------------------------
# Five machines that failed at these times (hours)
tool <- ReliaPlotR:::.make_wblr_tool()
result <- tool(failures = c(30, 49, 82, 90, 96))
str(result)

## ----wblr-susp, eval = has_ellmer---------------------------------------------
result_susp <- tool(
  failures    = c(30, 49, 82, 90, 96),
  suspensions = c(100, 120, 150)
)
cat("Beta:", result_susp$param1, "\n")
cat("Eta: ", result_susp$param2, "\n")

## ----alt-example, eval = has_ellmer, results = "hide"-------------------------
tool_alt <- ReliaPlotR:::.make_alt_tool()
result_alt <- tool_alt(
  stresses      = c(300, 350, 400),
  failures_json = "[[248,456,528,731,813,537],[164,176,289],[88,112,152]]",
  dist          = "weibull",
  alt_model     = "arrhenius"
)

## ----alt-show, eval = has_ellmer----------------------------------------------
# Per-stress-level parameter estimates
result_alt$parallel

# Global Arrhenius life-stress relationship coefficients
result_alt$relationship

## ----plot-wblr-example, eval = has_ellmer-------------------------------------
ptool <- ReliaPlotR:::.make_plot_wblr_tool()
json_str <- ptool(failures = c(30, 49, 82, 90, 96))
nchar(json_str)  # plotly JSON string length

## ----plot-alt-example, eval = has_ellmer, results = "hide"--------------------
ptool_alt <- ReliaPlotR:::.make_plot_alt_tool()
result_plots <- ptool_alt(
  stresses      = c(300, 350, 400),
  failures_json = "[[248,456,528,731,813,537],[164,176,289],[88,112,152]]",
  dist          = "weibull",
  alt_model     = "arrhenius"
)

## ----plot-alt-show, eval = has_ellmer-----------------------------------------
nchar(result_plots$probability_plot)
nchar(result_plots$life_stress_plot)

## ----plot-rga-example, eval = has_ellmer--------------------------------------
ptool_rga <- ReliaPlotR:::.make_plot_rga_tool()
json_rga <- ptool_rga(
  times    = c(100, 200, 300, 400, 500),
  failures = c(  2,   1,   3,   1,   2)
)
nchar(json_rga)

