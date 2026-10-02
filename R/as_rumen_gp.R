
#' Convert data to a rumen_gp object
#'
#' Converts gas production data from any source
#' into the internal rumenGP format.
#'
#' The function accepts either cumulative gas
#' volume or gas pressure measurements.
#'
#' When pressure is supplied, gas volume is
#' estimated using the same conversion used
#' by process_ankom().
#'
#' @param data A data frame.
#' @param head_col Column identifying bottles.
#' @param time_col Column containing incubation time.
#' @param gas_col Optional column containing cumulative
#' gas production (mL).
#' @param pressure_col Optional column containing
#' pressure measurements.
#' @param treatment_col Optional treatment column.
#' @param bottle_col Optional bottle column.
#' @param rep_col Optional replicate column.
#' @param pressure_unit Pressure unit.
#' Either "psi" or "kpa".
#' @param headspace_volume Headspace volume.
#' Required when pressure_col is supplied.
#' @param headspace_unit Headspace unit.
#' Either "mL" or "L".
#' @param temperature Incubation temperature
#' in degC.
#' @param zero_negative_pressure Logical.
#' If TRUE, negative pressure values are
#' converted to zero before gas-volume
#' calculations.
#'
#' @examples
#'
#'
#' # ----------------------------
#' # Example 1: Gas volume data
#' # ----------------------------
#'
#' manual_volume <- data.frame(
#'   Bottle = c(
#'     1, 1, 1,
#'     2, 2, 2
#'   ),
#'   Treatment = c(
#'     "Control", "Control", "Control",
#'     "Corn", "Corn", "Corn"
#'   ),
#'   Time = c(
#'     0, 4, 8,
#'     0, 4, 8
#'   ),
#'   Gas = c(
#'     0, 20, 40,
#'     0, 35, 60
#'   )
#' )
#'
#' gp <- as_rumen_gp(
#'   data = manual_volume,
#'   head_col = "Bottle",
#'   treatment_col = "Treatment",
#'   time_col = "Time",
#'   gas_col = "Gas"
#' )
#'
#' head(gp)
#'
#' # ----------------------------
#' # Example 2: Pressure data
#' # ----------------------------
#'
#' manual_pressure <- data.frame(
#'   Bottle = rep(
#'     1,
#'     10
#'   ),
#'   Time = c(
#'     0, 2, 4, 6, 8,
#'     12, 16, 24, 36, 48
#'   ),
#'   PSI = c(
#'     0,
#'     0.2,
#'     0.5,
#'     0.8,
#'     1.2,
#'     1.8,
#'     2.5,
#'     3.2,
#'     4.0,
#'     4.5
#'   )
#' )
#'
#' gp <- as_rumen_gp(
#'   data = manual_pressure,
#'   head_col = "Bottle",
#'   time_col = "Time",
#'   pressure_col = "PSI",
#'   pressure_unit = "psi",
#'   headspace_volume = 60
#' )
#'
#' head(gp)
#'
#' # Example model fit
#' fit <- fit_groot(gp)
#'
#' summary(fit)
#'
#'
#'
#' @return A rumen_gp object.
#'
#' @export
as_rumen_gp <- function(
    data,
    head_col,
    time_col,
    gas_col = NULL,
    pressure_col = NULL,
    treatment_col = NULL,
    bottle_col = NULL,
    rep_col = NULL,
    pressure_unit = c(
      "psi",
      "kpa"
    ),
    headspace_volume = NULL,
    headspace_unit = c(
      "mL",
      "L"
    ),
    temperature = 39,
    zero_negative_pressure = FALSE
) {

  # ----------------------------
  # Input validation
  # ----------------------------

  if (!is.data.frame(data)) {

    stop(
      "data must be a data.frame."
    )

  }

  if (
    is.null(gas_col) &&
    is.null(pressure_col)
  ) {

    stop(
      "Provide either gas_col or pressure_col."
    )

  }

  if (
    !is.null(gas_col) &&
    !is.null(pressure_col)
  ) {

    stop(
      "Provide only one of gas_col or pressure_col."
    )

  }

  pressure_unit <- match.arg(
    pressure_unit
  )

  headspace_unit <- match.arg(
    headspace_unit
  )

  # ----------------------------
  # Core structure
  # ----------------------------

  out <- data.frame(

    Head =
      data[[head_col]],

    Bottle =
      if (is.null(bottle_col)) {

        data[[head_col]]

      } else {

        data[[bottle_col]]

      },

    Rep =
      if (is.null(rep_col)) {

        1

      } else {

        data[[rep_col]]

      },

    Treatment =
      if (is.null(treatment_col)) {

        "Unknown"

      } else {

        data[[treatment_col]]

      },

    Time_h =
      data[[time_col]]

  )

  # ----------------------------
  # Direct gas volume
  # ----------------------------

  if (!is.null(gas_col)) {

    out$Gas_mL <- data[[gas_col]]

  }

  # ----------------------------
  # Pressure conversion
  # ----------------------------

  if (!is.null(pressure_col)) {

    if (is.null(headspace_volume)) {

      stop(
        paste(
          "headspace_volume must be supplied",
          "when pressure_col is used."
        )
      )

    }

    pressure_values <-
      data[[pressure_col]]

    # ----------------------------
    # Optional pressure correction
    # ----------------------------

    if (zero_negative_pressure) {

      pressure_values <-
        pmax(
          pressure_values,
          0
        )

    }

    out$Gas_mL <-
      pressure_to_volume(
        pressure =
          pressure_values,

        pressure_unit =
          pressure_unit,

        headspace_volume =
          headspace_volume,

        headspace_unit =
          headspace_unit,

        temperature =
          temperature
      )

  }

  # ----------------------------
  # Validation
  # ----------------------------

  if (
    any(
      is.na(
        out$Time_h
      )
    )
  ) {

    stop(
      "Time contains missing values."
    )

  }

  if (
    any(
      is.na(
        out$Gas_mL
      )
    )
  ) {

    stop(
      "Gas contains missing values."
    )

  }

  # ----------------------------
  # Sort
  # ----------------------------

  out <- out |>
    dplyr::arrange(
      Head,
      Time_h
    )

  # ----------------------------
  # Class
  # ----------------------------

  class(out) <- c(
    "rumen_gp",
    class(out)
  )

  out

}
