
#' Summary of Orskov and McDonald Fits
#'
#' Summarizes parameter estimates and goodness-of-fit
#' statistics for a fitted Orskov and McDonald model.
#'
#' The summary typically includes:
#'
#' \itemize{
#'   \item Initial gas volume (\code{VF})
#'   \item Fermentable fraction (\code{b})
#'   \item Fractional rate constant (\code{k})
#'   \item Residual Sum of Squares (RSS)
#'   \item Root Mean Squared Error (RMSE)
#'   \item R-squared (R²)
#'   \item Akaike Information Criterion (AIC)
#'   \item Bayesian Information Criterion (BIC)
#' }
#'
#' The Orskov and McDonald model partitions gas
#' production into:
#'
#' \itemize{
#'   \item An intercept term (\code{VF})
#'   \item A fermentable fraction (\code{b})
#' }
#'
#' The asymptotic gas production is:
#'
#' \deqn{
#' VF + b
#' }
#'
#' The parameter \code{k} controls the rate at
#' which the asymptote is approached.
#'
#' This model is widely used in ruminant nutrition
#' research because the parameters have straightforward
#' biological interpretation.
#'
#' @param object An \code{orskov_fit} object.
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
#' fit <- fit_orskov(
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
#' \code{\link{fit_orskov}},
#' \code{\link{plot_fit}},
#' \code{\link{plot_residuals}},
#' \code{\link{compare_models}}
#'
#' @export
summary.orskov_fit <- function(
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
    "\nOrskov and McDonald model summary\n",
    "---------------------------------\n",
    "Total bottles: ", n_total, "\n",
    "Successful fits: ", n_success, "\n",
    "Failed fits: ", n_failed, "\n",
    "Low R-squared (< 0.90): ", n_low_r2, "\n\n",
    sep = ""
  )

  invisible(object)

}
