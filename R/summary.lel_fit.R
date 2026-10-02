
#' Summary of LEL Fits
#'
#' Summarizes parameter estimates and goodness-of-fit
#' statistics for a fitted LEL model.
#'
#' The summary typically includes:
#'
#' \itemize{
#'   \item Asymptotic gas production (\code{A})
#'   \item Fractional rate constant (\code{k})
#'   \item Shape parameter (\code{d})
#'   \item Lag time (\code{lambda})
#'   \item Residual Sum of Squares (RSS)
#'   \item Root Mean Squared Error (RMSE)
#'   \item R-squared (R²)
#'   \item Akaike Information Criterion (AIC)
#'   \item Bayesian Information Criterion (BIC)
#' }
#'
#' The LEL (Logistic-Exponential with Lag) model
#' combines exponential fermentation kinetics with
#' a logistic component and an explicit lag phase.
#'
#' The lag parameter (\code{lambda}) represents the
#' delay before substantial fermentation begins,
#' while the shape parameter (\code{d}) controls
#' curve flexibility.
#'
#' This combination makes the LEL model suitable
#' for describing complex sigmoidal fermentation
#' profiles with delayed onset.
#'
#' @param object A \code{lel_fit} object.
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
#' fit <- fit_lel(
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
#' \code{\link{fit_lel}},
#' \code{\link{fit_le0}},
#' \code{\link{plot_fit}},
#' \code{\link{plot_residuals}},
#' \code{\link{compare_models}}
#'
#' @export
summary.lel_fit <- function(
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

  n_lambda_boundary <- sum(
    diagnostics$Lambda_Boundary,
    na.rm = TRUE
  )

  cat(
    "\nLogistic-Exponential (LEL) model summary\n",
    "----------------------------------------\n",
    "Total bottles: ", n_total, "\n",
    "Successful fits: ", n_success, "\n",
    "Failed fits: ", n_failed, "\n",
    "Low R-squared (< 0.90): ", n_low_r2, "\n",
    "Lambda at boundary: ", n_lambda_boundary, "\n\n",
    sep = ""
  )

  invisible(object)

}
