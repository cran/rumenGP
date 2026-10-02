
#' Validate Processed Rumen Gas Production Data
#'
#' Performs quality-control checks on a
#' \code{rumen_gp} object.
#'
#' Supported data sources include datasets
#' created by:
#'
#' \itemize{
#'   \item \code{process_ankom()}
#'   \item \code{as_rumen_gp()}
#' }
#'
#' Validation checks may include:
#'
#' \itemize{
#'   \item Required columns
#'   \item Missing values
#'   \item Duplicate observations
#'   \item Time ordering
#'   \item Gas production values
#'   \item ANKOM-specific pressure checks
#' }
#'
#' ANKOM-specific checks are performed only when
#' \code{Gas_PSI} is available.
#'
#' This function is useful for confirming that a
#' dataset is suitable for downstream modeling,
#' visualization, and model comparison workflows.
#'
#' @param data A \code{rumen_gp} object.
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
#' validate_ankom(
#'   gp
#' )
#'
#' # Validation also supports datasets
#' # created using as_rumen_gp()
#'
#' @return The validated \code{rumen_gp} object.
#'
#' @seealso
#' \code{\link{process_ankom}},
#' \code{\link{as_rumen_gp}},
#' \code{\link{validate_metadata}}
#'
#' @export
validate_ankom <- function(data) {

  # ----------------------------
  # Check object class
  # ----------------------------

  if (!inherits(data, "rumen_gp")) {

    stop(
      "Input must be a rumen_gp object."
    )

  }

  # ----------------------------
  # Required columns
  # ----------------------------

  required_cols <- c(
    "Head",
    "Bottle",
    "Rep",
    "Treatment",
    "Time_h",
    "Gas_mL"
  )

  missing_cols <- setdiff(
    required_cols,
    names(data)
  )

  if (length(missing_cols) > 0) {

    stop(
      paste(
        "Missing required column(s):",
        paste(
          missing_cols,
          collapse = ", "
        )
      )
    )

  }

  # ----------------------------
  # Missing values
  # ----------------------------

  if (any(is.na(data$Time_h))) {

    stop(
      "Missing Time_h values detected."
    )

  }

  if (any(is.na(data$Gas_mL))) {

    stop(
      "Missing Gas_mL values detected."
    )

  }

  # ----------------------------
  # Duplicate observations
  # ----------------------------

  duplicates <- data |>
    dplyr::count(
      Head,
      Time_h
    ) |>
    dplyr::filter(
      n > 1
    )

  if (nrow(duplicates) > 0) {

    warning(
      paste(
        "Duplicate Head-Time observations detected:",
        nrow(duplicates),
        "duplicate combinations found."
      )
    )

  }

  # ----------------------------
  # Bottle observation counts
  # ----------------------------

  bottle_counts <- data |>
    dplyr::count(
      Head
    )

  if (any(bottle_counts$n < 2)) {

    warning(
      "One or more bottles contain fewer than 2 observations."
    )

  }

  # ----------------------------
  # ANKOM-specific checks
  # ----------------------------

  if ("Gas_PSI" %in% names(data)) {

    min_pressure <- min(
      data$Gas_PSI,
      na.rm = TRUE
    )

    if (is.finite(min_pressure)) {

      if (min_pressure < -1) {

        warning(
          paste(
            "Large negative pressure values detected.",
            "Minimum PSI =",
            round(
              min_pressure,
              3
            ),
            ". Please inspect the affected bottles."
          )
        )

      } else if (min_pressure < 0) {

        message(
          paste(
            "Minor negative pressure values detected.",
            "Minimum PSI =",
            round(
              min_pressure,
              3
            ),
            ". These may reflect normal sensor variation."
          )
        )

      }

    }

  }

  # ----------------------------
  # Time ordering check
  # ----------------------------

  time_issue <- data |>
    dplyr::group_by(
      Head
    ) |>
    dplyr::summarise(
      Ordered =
        all(
          diff(Time_h) >= 0
        ),
      .groups = "drop"
    ) |>
    dplyr::filter(
      !Ordered
    )

  if (nrow(time_issue) > 0) {

    warning(
      paste(
        "Time_h is not monotonically increasing in",
        nrow(time_issue),
        "bottle(s)."
      )
    )

  }

  # ----------------------------
  # Validation summary
  # ----------------------------

  message(
    "rumenGP data validation passed.",
    "\nObservations: ", nrow(data),
    "\nHeads: ", dplyr::n_distinct(data$Head),
    "\nTreatments: ", dplyr::n_distinct(data$Treatment)
  )

  data

}
