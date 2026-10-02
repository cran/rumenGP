
#' Compare Fitted Kinetic Models
#'
#' Compares performance metrics across multiple
#' fitted kinetic models.
#'
#' Model comparison metrics typically include:
#'
#' \itemize{
#'   \item R-squared (R²)
#'   \item Root Mean Squared Error (RMSE)
#'   \item Residual Sum of Squares (RSS)
#'   \item Akaike Information Criterion (AIC)
#'   \item Bayesian Information Criterion (BIC)
#' }
#'
#' This function helps researchers identify models
#' that provide the best balance between goodness
#' of fit and model complexity.
#'
#' The resulting comparison table can be used with:
#'
#' \itemize{
#'   \item \code{rank_models()}
#'   \item \code{plot_model_performance()}
#'   \item \code{plot_model_rankings()}
#' }
#'
#' @param ... Named fitted model objects.
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
#' groot_fit <- fit_groot(
#'   gp
#' )
#'
#' gompertz_fit <- fit_gompertz(
#'   gp
#' )
#'
#' comparison <- compare_models(
#'   Groot = groot_fit,
#'   Gompertz = gompertz_fit
#' )
#'
#' comparison
#'
#' @return A data frame summarizing model
#' performance metrics for each fitted model.
#'
#' @seealso
#' \code{\link{rank_models}},
#' \code{\link{compare_models_by_treatment}},
#' \code{\link{plot_model_performance}},
#' \code{\link{plot_model_rankings}}
#'
#' @export
compare_models <- function(...) {

  models <- list(...)

  if (length(models) < 2) {
    stop(
      "Please provide at least two fitted models."
    )
  }

  results <- purrr::imap_dfr(
    models,
    function(model, model_name) {

      diagnostics <- model$diagnostics

      data.frame(

        Model = model_name,

        Bottles =
          nrow(diagnostics),

        Successful_Fits =
          sum(
            diagnostics$Converged,
            na.rm = TRUE
          ),

        Failed_Fits =
          sum(
            !diagnostics$Converged,
            na.rm = TRUE
          ),

        Mean_R2 =
          mean(
            diagnostics$R2,
            na.rm = TRUE
          ),

        Mean_RMSE =
          mean(
            diagnostics$RMSE,
            na.rm = TRUE
          ),

        Mean_RSS =
          mean(
            diagnostics$RSS,
            na.rm = TRUE
          ),

        Mean_AIC =
          mean(
            diagnostics$AIC,
            na.rm = TRUE
          ),

        Mean_BIC =
          mean(
            diagnostics$BIC,
            na.rm = TRUE
          ),

        Lambda_Boundary =
          sum(
            diagnostics$Lambda_Boundary,
            na.rm = TRUE
          )

      )

    }
  )

  rownames(results) <- NULL

  results

}
