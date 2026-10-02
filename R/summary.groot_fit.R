
#' Summary of Groot Fits
#'
#' Summarizes parameter estimates and goodness-of-fit
#' statistics for a fitted Groot model.
#'
#' The summary typically includes:
#'
#' \itemize{
#'   \item Asymptotic gas production (\code{VF})
#'   \item Half-time parameter (\code{b})
#'   \item Shape parameter (\code{k})
#'   \item Residual Sum of Squares (RSS)
#'   \item Root Mean Squared Error (RMSE)
#'   \item R-squared (R²)
#'   \item Akaike Information Criterion (AIC)
#'   \item Bayesian Information Criterion (BIC)
#' }
#'
#' The Groot model is a flexible sigmoidal model
#' commonly used in rumen gas production studies.
#'
#' The parameter \code{b} represents the time
#' required to reach approximately half of the
#' asymptotic gas production, while \code{k}
#' controls curve shape and steepness.
#'
#' ## Notes
#'
#' The Groot model is mathematically equivalent
#' to the generalized Michaelis-Menten model
#' implemented in \code{fit_mm()}.
#'
#' Parameter correspondence:
#'
#' \itemize{
#'   \item \code{VF = A}
#'   \item \code{b = K}
#'   \item \code{k = c}
#' }
#'
#' @param object A \code{groot_fit} object.
#'
#' @param ... Additional arguments passed to methods.
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
#' fit <- fit_groot(
#'   gp
#' )
#'
#' summary(
#'   fit
#' )
#'
#' @return A data frame containing parameter estimates
#' and model diagnostics for each fitted bottle.
#'
#' @seealso
#' \code{\link{fit_groot}},
#' \code{\link{fit_mm}},
#' \code{\link{plot_fit}},
#' \code{\link{plot_residuals}},
#' \code{\link{compare_models}}
#'
#' @export
summary.groot_fit <- function(
    object,
    ...
) {

  diagnostics <- object$diagnostics

  cat(
    "\nGroot model summary\n",
    "-------------------\n",
    "Total bottles: ",
    nrow(diagnostics),
    "\n",
    "Successful fits: ",
    sum(diagnostics$Converged),
    "\n",
    "Failed fits: ",
    sum(!diagnostics$Converged),
    "\n",
    "Low R-squared (< 0.90): ",
    sum(
      diagnostics$R2 < 0.90,
      na.rm = TRUE
    ),
    "\n\n",
    sep = ""
  )

  invisible(object)

}
