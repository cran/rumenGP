
#' Summary of Log-logistic Fits
#'
#' Summarizes parameter estimates and goodness-of-fit
#' statistics for a fitted Log-logistic model.
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
#' The Log-logistic model is a flexible sigmoidal
#' model capable of describing a broad range of
#' gas production profiles.
#'
#' The parameter \code{r} controls the speed of
#' gas production, while \code{a} controls curve
#' shape, steepness, and inflection behavior.
#'
#' ## Notes
#'
#' The Log-logistic model is mathematically
#' equivalent to both the Groot model
#' implemented in \code{fit_groot()} and the
#' generalized Michaelis-Menten model
#' implemented in \code{fit_mm()}.
#'
#' Parameter correspondence:
#'
#' \itemize{
#'   \item \code{VF = A}
#'   \item \code{a = c = k}
#'   \item \code{1/r = K = b}
#' }
#'
#' All three formulations produce identical
#' fitted values, residuals, diagnostics,
#' AIC, BIC, RMSE, and R-squared when
#' corresponding parameter values are used.
#'
#' @param object A \code{loglogistic_fit} object.
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
#' fit <- fit_loglogistic(
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
#' \code{\link{fit_loglogistic}},
#' \code{\link{fit_groot}},
#' \code{\link{fit_mm}},
#' \code{\link{plot_fit}},
#' \code{\link{plot_residuals}},
#' \code{\link{compare_models}}
#'
#' @export
summary.loglogistic_fit <- function(
    object,
    ...
) {

  diagnostics <- object$diagnostics

  cat(
    "\nLog-logistic model summary\n",
    "--------------------------\n",
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
