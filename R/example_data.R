
#' Example ANKOM Data
#'
#' Returns paths to example files included with
#' the package.
#'
#' The example dataset can be used to explore
#' package functionality, reproduce examples,
#' and learn rumenGP workflows without requiring
#' external files.
#'
#' The returned object includes:
#'
#' \itemize{
#'   \item Example ANKOM RF data
#'   \item Example metadata
#' }
#'
#' These files are used throughout the package
#' documentation, examples, and vignettes.
#'
#' @examples
#'
#' files <- example_data()
#'
#' files
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
#'   gp
#' )
#'
#' fit <- fit_groot(
#'   gp
#' )
#'
#' summary(
#'   fit
#' )
#'
#' @return A named list containing file paths to
#' package example data.
#'
#' @seealso
#' \code{\link{read_ankom}},
#' \code{\link{read_metadata}},
#' \code{\link{process_ankom}},
#' \code{\link{fit_groot}}
#'
#' @export
example_data <- function() {

  list(

    ankom = system.file(
      "extdata",
      "example_ankom.xlsx",
      package = "rumenGP"
    ),

    metadata = system.file(
      "extdata",
      "example_metadata.xlsx",
      package = "rumenGP"
    )

  )

}
