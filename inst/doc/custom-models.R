## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(rumenGP)

## -----------------------------------------------------------------------------
manual_volume <- data.frame(

  Bottle = c(
    rep(1, 10),
    rep(2, 10)
  ),

  Treatment = c(
    rep("Control", 10),
    rep("Corn", 10)
  ),

  Time = rep(
    c(
      0, 2, 4, 6, 8,
      12, 16, 24, 36, 48
    ),
    2
  ),

  Gas = c(

    0, 5, 12, 20, 28,
    40, 55, 75, 90, 100,

    0, 8, 18, 30, 42,
    58, 72, 95, 110, 120

  )

)

## -----------------------------------------------------------------------------
gp <- as_rumen_gp(
  data = manual_volume,
  head_col = "Bottle",
  treatment_col = "Treatment",
  time_col = "Time",
  gas_col = "Gas"
)

## -----------------------------------------------------------------------------
exp_fit <- fit_custom(

  data = gp,

  formula =
    Gas_mL ~
      A *
      (
        1 -
          exp(
            -k * Time_h
          )
      ),

  start = list(
    A = 120,
    k = 0.05
  ),

  lower = c(
    A = 0,
    k = 0
  ),

  model_name =
    "Simple Exponential"

)

## -----------------------------------------------------------------------------
summary(exp_fit)

## -----------------------------------------------------------------------------
exp_fit$parameters

## -----------------------------------------------------------------------------
hyperbolic_fit <- fit_custom(

  data = gp,

  formula =
    Gas_mL ~
      A *
      (
        Time_h /
        (
          Time_h + K
        )
      ),

  start = list(
    A = 150,
    K = 10
  ),

  lower = c(
    A = 0,
    K = 0
  ),

  model_name =
    "Hyperbolic"

)

## -----------------------------------------------------------------------------
summary(hyperbolic_fit)

## -----------------------------------------------------------------------------
hyperbolic_fit$parameters

## -----------------------------------------------------------------------------
richards_fit <- fit_custom(

  data = gp,

  formula =
    Gas_mL ~
      VF *
      (
        1 -
          b *
          exp(
            -k * Time_h
          )
      )^m,

  start = list(
    VF = max(gp$Gas_mL) * 1.1,
    b = 0.9,
    k = 0.05,
    m = 1
  ),

  lower = c(
    VF = 0,
    b = 0,
    k = 0,
    m = 0
  ),

  upper = c(
    VF = Inf,
    b = 1,
    k = Inf,
    m = 10
  ),

  model_name =
    "Richards"

)

## -----------------------------------------------------------------------------
richards_fit$diagnostics

## -----------------------------------------------------------------------------
richards_fit$parameters

## -----------------------------------------------------------------------------
groot_fit <- fit_groot(gp)

brody_fit <- fit_brody(gp)

## -----------------------------------------------------------------------------
compare_models(

  Groot = groot_fit,

  Brody = brody_fit,

  Hyperbolic = hyperbolic_fit

)

## ----eval = FALSE-------------------------------------------------------------
# plot_fit(
#   hyperbolic_fit,
#   head = 1
# )

## ----eval = FALSE-------------------------------------------------------------
# plot_residuals(
#   hyperbolic_fit,
#   head = 1
# )

