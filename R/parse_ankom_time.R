
#' Parse ANKOM Timestamps
#'
#' Converts ANKOM RF timestamps into elapsed
#' incubation time expressed in hours.
#'
#' ANKOM RF systems record measurements using
#' timestamps. This function converts those
#' timestamps into elapsed incubation time,
#' measured relative to the first observation.
#'
#' The resulting values are used throughout
#' rumenGP for:
#'
#' \itemize{
#'   \item Data processing
#'   \item Model fitting
#'   \item Visualization
#'   \item Model comparison
#' }
#'
#' In most workflows, this function is called
#' automatically by \code{process_ankom()} and
#' does not need to be used directly.
#'
#' @param time_raw Character vector containing
#' ANKOM timestamps.
#'
#' @examples
#'
#' timestamps <- c(
#'   "2024-01-01 08:00:00",
#'   "2024-01-01 12:00:00",
#'   "2024-01-01 20:00:00"
#' )
#'
#' parse_ankom_time(
#'   timestamps
#' )
#'
#' # Typical workflow
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
#' head(
#'   gp$Time_h
#' )
#'
#' @return A numeric vector containing elapsed
#' incubation time in hours.
#'
#' @seealso
#' \code{\link{read_ankom}},
#' \code{\link{process_ankom}},
#' \code{\link{example_data}}
#'
#' @export
parse_ankom_time <- function(time_raw) {

  # ANKOM omits day 0, so add it back
  time_std <- ifelse(
    grepl("^[0-9]+\\.", time_raw),
    time_raw,
    paste0("0.", time_raw)
  )

  parsed <- strsplit(time_std, "\\.")

  total_seconds <- vapply(
    parsed,
    function(x) {

      day <- as.numeric(x[1])

      time_of_day <- lubridate::hms(x[2])

      day * 86400 +
        as.numeric(time_of_day)

    },
    numeric(1)
  )

  elapsed_h <- (
    total_seconds -
      min(total_seconds, na.rm = TRUE)
  ) / 3600

  elapsed_h

}
