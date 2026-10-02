
#' Process ANKOM RF Data
#'
#' Converts raw ANKOM RF output into a standardized
#' dataset suitable for rumenGP analyses.
#'
#' The function:
#'
#' \itemize{
#'   \item Merges ANKOM measurements with metadata
#'   \item Removes Head 0 (the ANKOM receiver/base station)
#'   \item Converts pressure measurements to gas volumes
#'   \item Applies headspace and temperature corrections
#'   \item Produces a standardized \code{rumen_gp} object
#' }
#'
#' Head 0 is reserved by the ANKOM RF system as the
#' receiver/base station and is automatically removed
#' during processing.
#'
#' @param raw_data Raw ANKOM data table.
#'
#' @param metadata Metadata table.
#'
#' @param headspace_ml Bottle headspace volume (mL).
#'
#' @param temperature_c Incubation temperature (°C).
#'
#' @param zero_negative_pressure Logical. If \code{TRUE},
#' negative pressure values are converted to zero before
#' gas-volume calculations.
#'
#' @examples
#'
#' files <- example_data()
#'
#' raw_data <- read_ankom(
#'   files$ankom
#' )
#'
#' metadata <- read_metadata(
#'   files$metadata
#' )
#'
#' gp <- process_ankom(
#'   raw_data,
#'   metadata,
#'   headspace_ml = 210,
#'   temperature_c = 39
#' )
#'
#' head(gp)
#'
#' # Alternative behavior:
#' # convert negative pressures to zero
#' gp_zeroed <- process_ankom(
#'   raw_data,
#'   metadata,
#'   headspace_ml = 210,
#'   temperature_c = 39,
#'   zero_negative_pressure = TRUE
#' )
#'
#' head(gp_zeroed)
#'
#' @return A \code{rumen_gp} object containing:
#' \itemize{
#'   \item Time points
#'   \item Gas production values
#'   \item Bottle identifiers
#'   \item Treatment assignments
#'   \item Additional metadata
#' }
#'
#' @seealso
#' \code{\link{read_ankom}},
#' \code{\link{read_metadata}},
#' \code{\link{as_rumen_gp}},
#' \code{\link{plot_gp}}
#'
#' @export
process_ankom <- function(
    raw_data,
    metadata = NULL,
    headspace_ml = 210,
    temperature_c = 39,
    zero_negative_pressure = FALSE
) {

  # ----------------------------
  # Input validation
  # ----------------------------

  if (!is.data.frame(raw_data)) {
    stop("raw_data must be a data.frame.")
  }

  if (!is.numeric(headspace_ml) ||
      length(headspace_ml) != 1 ||
      headspace_ml <= 0) {
    stop("headspace_ml must be a positive number.")
  }

  if (!is.numeric(temperature_c) ||
      length(temperature_c) != 1) {
    stop("temperature_c must be numeric.")
  }

  if (!is.logical(zero_negative_pressure) ||
      length(zero_negative_pressure) != 1) {
    stop(
      "zero_negative_pressure must be TRUE or FALSE."
    )
  }

  # ----------------------------
  # Experimental constants
  # ----------------------------

  headspace_L <- headspace_ml / 1000

  temp_K <- temperature_c + 273.15

  R_constant <- 8.314472

  psi_to_kpa <- 6.894757293

  # ----------------------------
  # Time processing
  # ----------------------------

  names(raw_data)[1] <- "time_raw"

  raw_data$Time_h <- parse_ankom_time(
    raw_data$time_raw
  )

  # ----------------------------
  # Long format conversion
  # ----------------------------

  df <- raw_data |>
    tidyr::pivot_longer(
      cols = -c(
        time_raw,
        Time_h
      ),
      names_to = "Head",
      values_to = "Gas_PSI"
    ) |>
    dplyr::mutate(
      Head = as.character(Head)
    ) |>

    # Remove ANKOM receiver/head 0
    dplyr::filter(
      Head != "0"
    ) |>

    # Remove empty channels
    dplyr::filter(
      !is.na(Gas_PSI)
    )

  # ----------------------------
  # Optional pressure correction
  # ----------------------------

  if (zero_negative_pressure) {

    df <- df |>
      dplyr::mutate(
        Gas_PSI = pmax(
          Gas_PSI,
          0
        )
      )

  }

  # ----------------------------
  # Gas calculations
  # ----------------------------

  df <- df |>
    dplyr::mutate(

      # PSI to kPa
      Gas_kPa =
        Gas_PSI * psi_to_kpa,

      # Ideal gas law
      Gas_moles =
        Gas_kPa *
        (
          headspace_L /
            (
              R_constant *
                temp_K
            )
        ),

      # Avogadro conversion
      Gas_mL =
        Gas_moles *
        22.4 *
        1000
    )

  # ----------------------------
  # Metadata join
  # ----------------------------

  if (!is.null(metadata)) {

    required_cols <- c(
      "Head",
      "Treatment",
      "Rep"
    )

    missing_cols <- setdiff(
      required_cols,
      names(metadata)
    )

    if (length(missing_cols) > 0) {

      stop(
        paste(
          "Metadata is missing required column(s):",
          paste(
            missing_cols,
            collapse = ", "
          )
        )
      )

    }

    metadata$Head <- as.character(
      metadata$Head
    )

    df <- dplyr::left_join(
      df,
      metadata,
      by = "Head"
    )

    # Keep only valid bottles
    df <- df |>
      dplyr::filter(
        !is.na(Treatment)
      )

  }

  # ----------------------------
  # Store processing settings
  # ----------------------------

  attr(df, "settings") <- list(
    headspace_ml = headspace_ml,
    temperature_c = temperature_c,
    psi_to_kpa = psi_to_kpa,
    gas_constant = R_constant,
    zero_negative_pressure = zero_negative_pressure
  )

  # ----------------------------
  # Assign class
  # ----------------------------

  class(df) <- c(
    "rumen_gp",
    class(df)
  )

  df
}
