
#' Summary of Michaelis-Menten Fits
#'
#' Summarizes parameter estimates and goodness-of-fit
#' statistics for a fitted Michaelis-Menten model.
#'
#' The summary typically includes:
#'
#' \itemize{
#'   \item Asymptotic gas production (\code{A})
#'   \item Half-time parameter (\code{K})
#'   \item Shape parameter (\code{c})
#'   \item Residual Sum of Squares (RSS)
#'   \item Root Mean Squared Error (RMSE)
#'   \item R-squared (R²)
#'   \item Akaike Information Criterion (AIC)
#'   \item Bayesian Information Criterion (BIC)
#' }
#'
#' The generalized Michaelis-Menten model is a
#' flexible sigmoidal model commonly used to describe
#' cumulative gas production.
#'
#' The parameter \code{K} represents the time
#' required to reach approximately half of the
#' asymptotic gas production, while \code{c}
#' controls curve shape and steepness.
#'
#' ## Notes
#'
#' The generalized Michaelis-Menten model is
#' mathematically equivalent to the Groot model
#' implemented in \code{fit_groot()}.
#'
#' Parameter correspondence:
#'
#' \itemize{
#'   \item \code{A = VF}
#'   \item \code{K = b}
#'   \item \code{c = k}
#' }
#'
#' Both formulations produce identical fitted values
#' and model diagnostics when convergence is achieved.
#'
#' @param object A \code{mm_fit} object.
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
#' fit <- fit_mm(
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
#' \code{\link{fit_mm}},
#' \code{\link{fit_groot}},
#' \code{\link{plot_fit}},
#' \code{\link{plot_residuals}},
#' \code{\link{compare_models}}
#'
#' @export
summary.mm_fit <- function(
    object,
    ...
) {

  diagnostics <- object$diagnostics

  n_total <- nrow(diagnostics)

  n_success <- sum(
    diagnostics$Converged,
    na.rm = TRUE
  )

  n_failed <- sum(
    !diagnostics$Converged,
    na.rm = TRUE
  )

  n_low_r2 <- sum(
    diagnostics$R2 < 0.90,
    na.rm = TRUE
  )

  cat(
    "\nMichaelis-Menten model summary\n",
    "------------------------------\n",
    "Total bottles: ", n_total, "\n",
    "Successful fits: ", n_success, "\n",
    "Failed fits: ", n_failed, "\n",
    "Low R-squared (< 0.90): ", n_low_r2, "\n\n",
    sep = ""
  )

  invisible(object)

}
