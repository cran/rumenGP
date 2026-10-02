
#' Summary of EXP0 Fits
#'
#' Summarizes parameter estimates and goodness-of-fit
#' statistics for a fitted EXP0 model.
#'
#' The summary typically includes:
#'
#' \itemize{
#'   \item Asymptotic gas production (\code{Vf})
#'   \item Fractional rate constant (\code{k})
#'   \item Residual Sum of Squares (RSS)
#'   \item Root Mean Squared Error (RMSE)
#'   \item R-squared (R²)
#'   \item Akaike Information Criterion (AIC)
#'   \item Bayesian Information Criterion (BIC)
#' }
#'
#' The EXP0 model describes gas production as an
#' exponential approach to an asymptotic gas volume
#' without an explicit lag phase.
#'
#' @param object An \code{exp0_fit} object.
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
#' fit <- fit_exp0(
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
#' \code{\link{fit_exp0}},
#' \code{\link{plot_fit}},
#' \code{\link{plot_residuals}},
#' \code{\link{compare_models}}
#'
#' @export
summary.exp0_fit <- function(
    object,
    ...
) {

  diagnostics <- object$diagnostics

  cat(
    "\nExponential model (EXP0) summary\n",
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
    sum(
      diagnostics$R2 < 0.90,
      na.rm = TRUE
    ),
    "\n\n",
    sep = ""
  )

  invisible(object)

}
