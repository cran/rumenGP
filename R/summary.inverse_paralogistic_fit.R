
#' Summary of Inverse Paralogistic Fits
#'
#' Summarizes parameter estimates and goodness-of-fit
#' statistics for a fitted Inverse Paralogistic model.
#'
#' The summary typically includes:
#'
#' \itemize{
#'   \item Asymptotic gas production (\code{VF})
#'   \item Rate parameter (\code{r})
#'   \item Shape parameter (\code{a})
#'   \item Residual Sum of Squares (RSS)
#'   \item Root Mean Squared Error (RMSE)
#'   \item R-squared (R²)
#'   \item Akaike Information Criterion (AIC)
#'   \item Bayesian Information Criterion (BIC)
#' }
#'
#' The Inverse Paralogistic model is a flexible
#' sigmoidal model capable of describing a broad
#' range of cumulative gas production profiles.
#'
#' The parameter \code{r} controls the speed of
#' gas production, while \code{a} controls curve
#' shape, steepness, and inflection behavior.
#'
#' @param object An \code{inverse_paralogistic_fit} object.
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
#' fit <- fit_inverse_paralogistic(
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
#' \code{\link{fit_inverse_paralogistic}},
#' \code{\link{plot_fit}},
#' \code{\link{plot_residuals}},
#' \code{\link{compare_models}}
#'
#' @export
summary.inverse_paralogistic_fit <- function(
    object,
    ...
) {

  diagnostics <- object$diagnostics

  cat(
    "\nInverse Paralogistic model summary\n",
    "----------------------------------\n",
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
