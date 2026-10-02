## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(rumenGP)

## -----------------------------------------------------------------------------
files <- example_data()

files

## -----------------------------------------------------------------------------
raw_data <- read_ankom(
  files$ankom
)

metadata <- read_metadata(
  files$metadata
)

## -----------------------------------------------------------------------------
metadata <- validate_metadata(
  metadata
)

## -----------------------------------------------------------------------------
gp <- process_ankom(
  raw_data,
  metadata,
  headspace_ml = 210,
  temperature_c = 39,
  zero_negative_pressure = TRUE
)

## -----------------------------------------------------------------------------
gp <- validate_ankom(
  gp
)

class(gp)

## -----------------------------------------------------------------------------
head(gp)

## ----eval = FALSE-------------------------------------------------------------
# plot_gp(
#   gp,
#   head = "1"
# )

## -----------------------------------------------------------------------------
groot_fit <- fit_groot(gp)

gompertz_fit <- fit_gompertz(gp)

brody_fit <- fit_brody(gp)

## -----------------------------------------------------------------------------
summary(groot_fit)

## -----------------------------------------------------------------------------
flags <- flag_model(
  groot_fit
)

head(flags)

## ----eval = FALSE-------------------------------------------------------------
# plot_fit(
#   groot_fit,
#   head = "1"
# )

## ----eval = FALSE-------------------------------------------------------------
# plot_residuals(
#   groot_fit,
#   head = "1"
# )

## -----------------------------------------------------------------------------
comparison <- compare_models(

  Groot = groot_fit,

  Gompertz = gompertz_fit,

  Brody = brody_fit

)

comparison

## -----------------------------------------------------------------------------
rank_models(
  comparison
)

## -----------------------------------------------------------------------------
treatment_comparison <-
  compare_models_by_treatment(

    Groot = groot_fit,

    Gompertz = gompertz_fit,

    Brody = brody_fit

  )

treatment_comparison

## -----------------------------------------------------------------------------
ranked_treatments <-
  rank_models_by_treatment(
    treatment_comparison
  )

ranked_treatments

## -----------------------------------------------------------------------------
best_models <-
  best_model_by_treatment(
    ranked_treatments
  )

best_models

## -----------------------------------------------------------------------------
model_win_frequency(
  best_models
)

## ----eval = FALSE-------------------------------------------------------------
# gp_clean <- exclude_heads(
#   gp,
#   heads = c("10"),
#   reason = "Sensor malfunction"
# )

