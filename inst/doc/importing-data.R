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
    1,1,1,
    2,2,2
  ),

  Treatment = c(
    "Control",
    "Control",
    "Control",
    "Corn",
    "Corn",
    "Corn"
  ),

  Time = c(
    0,4,8,
    0,4,8
  ),

  Gas = c(
    0,20,40,
    0,35,60
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
gp

## -----------------------------------------------------------------------------
class(gp)

## -----------------------------------------------------------------------------
manual_pressure <- data.frame(

  Bottle = rep(
    1,
    10
  ),

  Time = c(
    0,
    2,
    4,
    6,
    8,
    12,
    16,
    24,
    36,
    48
  ),

  PSI = c(
    0,
    0.2,
    0.5,
    0.8,
    1.2,
    1.8,
    2.5,
    3.2,
    4.0,
    4.5
  )

)

## -----------------------------------------------------------------------------
gp_pressure <- as_rumen_gp(

  data = manual_pressure,

  head_col = "Bottle",

  time_col = "Time",

  pressure_col = "PSI",

  pressure_unit = "psi",

  headspace_volume = 60

)

## -----------------------------------------------------------------------------
head(gp_pressure)

## -----------------------------------------------------------------------------
negative_pressure <- data.frame(

  Bottle = c(
    1,1,1
  ),

  Time = c(
    0,4,8
  ),

  PSI = c(
    -0.5,
    0.2,
    1.0
  )

)

## -----------------------------------------------------------------------------
gp_negative <- as_rumen_gp(

  data = negative_pressure,

  head_col = "Bottle",

  time_col = "Time",

  pressure_col = "PSI",

  pressure_unit = "psi",

  headspace_volume = 60,

  zero_negative_pressure = TRUE

)

## -----------------------------------------------------------------------------
validate_ankom(
  gp
)

## -----------------------------------------------------------------------------
fit <- fit_groot(
  gp
)

## -----------------------------------------------------------------------------
summary(fit)

## -----------------------------------------------------------------------------
comparison <- compare_models(

  Groot = fit_groot(gp),

  Brody = fit_brody(gp),

  Gompertz = fit_gompertz(gp)

)

comparison

