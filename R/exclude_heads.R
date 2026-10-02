
#' Exclude Problematic ANKOM Heads
#'
#' Removes one or more bottles from a
#' \code{rumen_gp} object while recording
#' exclusion information.
#'
#' This function is useful for excluding:
#'
#' \itemize{
#'   \item Leaking bottles
#'   \item Sensor failures
#'   \item Damaged bottles
#'   \item Biologically implausible observations
#'   \item Other quality-control issues
#' }
#'
#' Exclusion information is retained to support
#' transparent reporting and reproducible analyses.
#'
#' @param data A \code{rumen_gp} object.
#'
#' @param heads Character vector of bottle
#' identifiers to remove.
#'
#' @param reason Character vector containing the
#' reason for each exclusion.
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
#' gp_filtered <- exclude_heads(
#'   data = gp,
#'   heads = c(
#'     "12",
#'     "18"
#'   ),
#'   reason = c(
#'     "Bottle leak",
#'     "Sensor malfunction"
#'   )
#' )
#'
#' gp_filtered
#'
#' @return A filtered \code{rumen_gp} object.
#'
#' @seealso
#' \code{\link{validate_ankom}},
#' \code{\link{flag_model}},
#' \code{\link{process_ankom}}
#'
#' @export
exclude_heads <- function(
    data,
    heads,
    reason = NULL
) {

  if (!inherits(data, "rumen_gp")) {
    stop(
      "Input must be a rumen_gp object."
    )
  }

  excluded <- data.frame(
    Head = heads,
    Reason =
      if (is.null(reason))
        NA_character_
    else
      reason
  )

  out <- data |>
    dplyr::filter(
      !Head %in% heads
    )

  attr(
    out,
    "excluded_heads"
  ) <- excluded

  class(out) <- class(data)

  out

}
