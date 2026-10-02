
#' Validate Metadata
#'
#' Validates experimental metadata prior to analysis.
#'
#' Metadata are required for linking bottles to
#' treatments and biological replicates during
#' data processing and model fitting.
#'
#' Required columns:
#'
#' \itemize{
#'   \item \code{Head}
#'   \item \code{Treatment}
#'   \item \code{Rep}
#' }
#'
#' Validation checks may include:
#'
#' \itemize{
#'   \item Presence of required columns
#'   \item Missing values
#'   \item Duplicate bottle identifiers
#'   \item Invalid treatment assignments
#' }
#'
#' This function is typically used before
#' \code{process_ankom()} to ensure metadata
#' are suitable for downstream analyses.
#'
#' @param metadata Metadata table.
#'
#' @examples
#'
#' files <- example_data()
#'
#' metadata <- read_metadata(
#'   files$metadata
#' )
#'
#' validate_metadata(
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
#' @return The validated metadata table.
#'
#' @seealso
#' \code{\link{read_metadata}},
#' \code{\link{validate_ankom}},
#' \code{\link{process_ankom}},
#' \code{\link{example_data}}
#'
#' @export
validate_metadata <- function(metadata) {

  # ----------------------------
  # Basic checks
  # ----------------------------

  if (!is.data.frame(metadata)) {
    stop("metadata must be a data.frame.")
  }

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
        "Missing required column(s):",
        paste(
          missing_cols,
          collapse = ", "
        )
      )
    )

  }

  # ----------------------------
  # Head validation
  # ----------------------------

  if (any(is.na(metadata$Head))) {

    stop(
      "Metadata contains missing Head values."
    )

  }

  if (anyDuplicated(metadata$Head)) {

    duplicated_heads <- unique(
      metadata$Head[
        duplicated(metadata$Head)
      ]
    )

    stop(
      paste(
        "Duplicate Head values detected:",
        paste(
          duplicated_heads,
          collapse = ", "
        )
      )
    )

  }

  # ----------------------------
  # Treatment validation
  # ----------------------------

  if (any(is.na(metadata$Treatment))) {

    stop(
      "Metadata contains missing Treatment values."
    )

  }

  # ----------------------------
  # Rep validation
  # ----------------------------

  if (any(is.na(metadata$Rep))) {

    stop(
      "Metadata contains missing Rep values."
    )

  }

  # ----------------------------
  # Character conversion
  # ----------------------------

  metadata$Head <- as.character(
    metadata$Head
  )

  metadata$Treatment <- as.character(
    metadata$Treatment
  )

  # ----------------------------
  # Success message
  # ----------------------------

  message(
    "Metadata validation passed.",
    "\nHeads: ", nrow(metadata),
    "\nTreatment: ", length(unique(metadata$Treatment))
  )

  metadata

}
