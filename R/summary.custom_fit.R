
#' Summary of Custom Model Fits
#'
#' Summarizes a fitted custom nonlinear model.
#'
#' The summary typically reports:
#'
#' \itemize{
#'   \item Model name
#'   \item Model formula
#'   \item Parameter estimates
#'   \item Residual Sum of Squares (RSS)
#'   \item Root Mean Squared Error (RMSE)
#'   \item R-squared (R²)
#'   \item Akaike Information Criterion (AIC)
#'   \item Bayesian Information Criterion (BIC)
#' }
#'
#' This method provides a concise overview of
#' parameter estimates and model performance for
#' user-defined nonlinear equations fitted with
#' \code{fit_custom()}.
#'
#' @param object A \code{custom_fit} object.
#'
#' @param ... Not used.
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
#' custom_fit <- fit_custom(
#'   data = gp,
#'   formula =
#'     Gas_mL ~
#'       A *
#'       (
#'         Time_h /
#'         (
#'           Time_h + K
#'         )
#'       ),
#'   start = list(
#'     A = 150,
#'     K = 10
#'   ),
#'   lower = c(
#'     A = 0,
#'     K = 0
#'   ),
#'   model_name = "Hyperbolic"
#' )
#'
#' summary(
#'   custom_fit
#' )
#'
#' @return Invisibly returns the input
#' \code{custom_fit} object.
#'
#' @seealso
#' \code{\link{fit_custom}},
#' \code{\link{plot_fit}},
#' \code{\link{plot_residuals}},
#' \code{\link{compare_models}}
#'
#' @export
summary.custom_fit <- function(
    object,
    ...
) {

  diagnostics <- object$diagnostics

  n_bottles <- nrow(
    diagnostics
  )

  n_success <- sum(
    diagnostics$Converged,
    na.rm = TRUE
  )

  n_failed <- sum(
    !diagnostics$Converged,
    na.rm = TRUE
  )

  mean_r2 <- mean(
    diagnostics$R2,
    na.rm = TRUE
  )

  mean_rmse <- mean(
    diagnostics$RMSE,
    na.rm = TRUE
  )

  mean_aic <- mean(
    diagnostics$AIC,
    na.rm = TRUE
  )

  mean_bic <- mean(
    diagnostics$BIC,
    na.rm = TRUE
  )

  cat(

    "\nCustom model summary\n",
    "--------------------\n",

    "Model name: ",
    object$model_name,

    "\n\nFormula:\n",

    paste(
      deparse(
        object$formula
      ),
      collapse = "\n"
    ),

    "\n\n",

    "Total bottles: ",
    n_bottles,

    "\n",

    "Successful fits: ",
    n_success,

    "\n",

    "Failed fits: ",
    n_failed,

    "\n",

    "Mean R-squared: ",
    round(
      mean_r2,
      4
    ),

    "\n",

    "Mean RMSE: ",
    round(
      mean_rmse,
      4
    ),

    "\n",

    "Mean AIC: ",
    round(
      mean_aic,
      4
    ),

    "\n",

    "Mean BIC: ",
    round(
      mean_bic,
      4
    ),

    "\n\n",

    sep = ""

  )

  invisible(object)

}
