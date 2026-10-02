
#' Import Metadata
#'
#' Reads metadata associated with an ANKOM RF
#' experiment.
#'
#' Metadata are used to identify bottles,
#' treatments, replicates, and other experimental
#' information required for downstream analyses.
#'
#' This function is typically used together with:
#'
#' \itemize{
#'   \item \code{read_ankom()}
#'   \item \code{process_ankom()}
#' }
#'
#' as part of the standard ANKOM workflow.
#'
#' @param file Path to a metadata file.
#'
#' @param sheet Sheet name containing metadata.
#'
#' @examples
#'
#' files <- example_data()
#'
#' metadata <- read_metadata(
#'   files$metadata
#' )
#'
#' head(
#'   metadata
#' )
#'
#' # Typical workflow
#' raw_data <- read_ankom(
#'   files$ankom
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
#' @return A data frame containing experimental
#' metadata.
#'
#' @seealso
#' \code{\link{read_ankom}},
#' \code{\link{process_ankom}},
#' \code{\link{validate_metadata}},
#' \code{\link{example_data}}
#'
#' @export
read_metadata <- function(
    file,
    sheet = "metadata"
) {

  metadata <- readxl::read_excel(
    path = file,
    sheet = sheet
  )

  metadata

}
