
#' Summary of Burr XII Fits
#'
#' Summarizes parameter estimates and goodness-of-fit
#' statistics for a fitted Burr XII model.
#'
#' The summary typically includes:
#'
#' \itemize{
#'   \item Asymptotic gas production (\code{VF})
#'   \item Rate parameter (\code{r})
#'   \item Shape parameter (\code{a})
#'   \item Shape parameter (\code{p})
#'   \item Residual Sum of Squares (RSS)
#'   \item Root Mean Squared Error (RMSE)
#'   \item R-squared (R²)
#'   \item Akaike Information Criterion (AIC)
#'   \item Bayesian Information Criterion (BIC)
#' }
#'
#' The Burr XII model is a highly flexible
#' sigmoidal model capable of describing
#' diverse gas production profiles.
#'
#' The parameter \code{r} controls the speed
#' of gas production, while \code{a} and
#' \code{p} jointly control curve shape,
#' asymmetry, and inflection behavior.
#'
#' @param object A \code{burr_xii_fit} object.
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
#' fit <- fit_burr_xii(
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
#' \code{\link{fit_burr_xii}},
#' \code{\link{plot_fit}},
#' \code{\link{plot_residuals}},
#' \code{\link{compare_models}}
#'
#' @export
summary.burr_xii_fit <- function(
    object,
    ...
) {

  diagnostics <- object$diagnostics

  cat(
    "\nBurr XII model summary\n",
    "----------------------\n",
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
