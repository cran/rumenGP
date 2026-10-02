
#' Import ANKOM RF Output
#'
#' Reads a raw ANKOM RF export file and returns
#' the contents as a data frame.
#'
#' This function is typically the first step in
#' the ANKOM workflow:
#'
#' \itemize{
#'   \item Import ANKOM data
#'   \item Import metadata
#'   \item Process data
#'   \item Fit kinetic models
#' }
#'
#' The imported data can subsequently be processed
#' using \code{process_ankom()}.
#'
#' @param file Path to an ANKOM Excel file.
#'
#' @examples
#'
#' files <- example_data()
#'
#' raw_data <- read_ankom(
#'   files$ankom
#' )
#'
#' head(
#'   raw_data
#' )
#'
#' # Typical workflow
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
#'   gp
#' )
#'
#' @return A data frame containing raw ANKOM RF data.
#'
#' @seealso
#' \code{\link{read_metadata}},
#' \code{\link{process_ankom}},
#' \code{\link{example_data}}
#'
#' @export
read_ankom <- function(file) {

  if (!file.exists(file)) {
    stop("File does not exist.")
  }

  df <- readxl::read_excel(file)

  names(df)[1] <- "time_raw"

  df

}
