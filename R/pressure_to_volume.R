
#' Convert Pressure to Gas Volume
#'
#' Converts pressure measurements to estimated
#' gas volumes using the ideal gas law.
#'
#' The function supports pressure measurements
#' in PSI or kPa and headspace volumes in mL or L.
#'
#' ## Equation
#'
#' Gas volume is estimated using the ideal gas law:
#'
#' \deqn{
#' PV = nRT
#' }
#'
#' where:
#'
#' \itemize{
#'   \item \eqn{P} is pressure
#'   \item \eqn{V} is headspace volume
#'   \item \eqn{n} is gas moles
#'   \item \eqn{R} is the gas constant
#'   \item \eqn{T} is absolute temperature
#' }
#'
#' Estimated gas moles are converted to an
#' equivalent gas volume.
#'
#' @param pressure Numeric pressure values.
#'
#' @param pressure_unit Pressure unit.
#' One of:
#' \itemize{
#'   \item \code{"psi"}
#'   \item \code{"kpa"}
#' }
#'
#' @param headspace_volume Headspace volume.
#'
#' @param headspace_unit Headspace volume unit.
#' One of:
#' \itemize{
#'   \item \code{"mL"}
#'   \item \code{"L"}
#' }
#'
#' @param temperature Incubation temperature
#' in degrees Celsius.
#'
#' @examples
#' # Convert PSI measurements
#' pressure_to_volume(
#'   pressure = c(
#'     0.5,
#'     1.0,
#'     1.5
#'   ),
#'   pressure_unit = "psi",
#'   headspace_volume = 60,
#'   headspace_unit = "mL",
#'   temperature = 39
#' )
#'
#' # Convert kPa measurements
#' pressure_to_volume(
#'   pressure = c(
#'     5,
#'     10,
#'     15
#'   ),
#'   pressure_unit = "kpa",
#'   headspace_volume = 0.06,
#'   headspace_unit = "L",
#'   temperature = 39
#' )
#'
#' @return A numeric vector containing
#' estimated gas volumes in mL.
#'
#' @export
pressure_to_volume <- function(
    pressure,
    pressure_unit = c(
      "psi",
      "kpa"
    ),
    headspace_volume,
    headspace_unit = c(
      "mL",
      "L"
    ),
    temperature = 39
) {

  pressure_unit <- match.arg(
    pressure_unit
  )

  headspace_unit <- match.arg(
    headspace_unit
  )

  if (
    !is.numeric(
      headspace_volume
    ) ||
    length(
      headspace_volume
    ) != 1 ||
    headspace_volume <= 0
  ) {

    stop(
      "headspace_volume must be a positive number."
    )

  }

  if (
    !is.numeric(
      temperature
    ) ||
    length(
      temperature
    ) != 1
  ) {

    stop(
      "temperature must be numeric."
    )

  }

  psi_to_kpa <- 6.894757293

  R_constant <- 8.314472

  pressure_kPa <- pressure

  if (
    pressure_unit == "psi"
  ) {

    pressure_kPa <-
      pressure * psi_to_kpa

  }

  headspace_L <- headspace_volume

  if (
    headspace_unit == "mL"
  ) {

    headspace_L <-
      headspace_volume / 1000

  }

  temperature_K <-
    temperature + 273.15

  gas_moles <-
    pressure_kPa *
    (
      headspace_L /
        (
          R_constant *
            temperature_K
        )
    )

  gas_mL <-
    gas_moles *
    22.4 *
    1000

  gas_mL

}
