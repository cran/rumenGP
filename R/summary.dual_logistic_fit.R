
#' Summary of Dual Logistic Fits
#'
#' Summarizes parameter estimates and goodness-of-fit
#' statistics for a fitted dual-pool logistic model.
#'
#' The summary typically includes:
#'
#' \itemize{
#'   \item Rapid pool gas volume (\code{V1F})
#'   \item Slow pool gas volume (\code{V2F})
#'   \item Rapid pool rate constant (\code{k1})
#'   \item Slow pool rate constant (\code{k2})
#'   \item Lag time (\code{lambda})
#'   \item Residual Sum of Squares (RSS)
#'   \item Root Mean Squared Error (RMSE)
#'   \item R-squared (R²)
#'   \item Akaike Information Criterion (AIC)
#'   \item Bayesian Information Criterion (BIC)
#' }
#'
#' The Dual Logistic model partitions fermentation
#' into rapidly and slowly degradable fractions,
#' providing a biologically informative description
#' of fermentation dynamics.
#'
#' @param object A \code{dual_logistic_fit} object.
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
#' fit <- fit_dual_logistic(
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
#' \code{\link{fit_dual_logistic}},
#' \code{\link{plot_dual_pools}},
#' \code{\link{plot_fit}},
#' \code{\link{compare_models}}
#'
#' @export
summary.dual_logistic_fit <- function(
    object,
    ...
) {

  diagnostics <- object$diagnostics

  cat(
    "\nDual-pool Logistic model summary\n",
    "--------------------------------\n",
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
    sum(diagnostics$R2 < 0.90,
        na.rm = TRUE),
    "\n",
    "Lambda at boundary: ",
    sum(
      diagnostics$Lambda_Boundary,
      na.rm = TRUE
    ),
    "\n\n",
    sep = ""
  )

  invisible(object)

}
