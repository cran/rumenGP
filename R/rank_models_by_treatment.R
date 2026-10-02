
#' Rank Models Within Each Treatment
#'
#' Ranks fitted models within each treatment
#' using performance metrics produced by
#' \code{compare_models_by_treatment()}.
#'
#' Rankings can be based on metrics such as:
#'
#' \itemize{
#'   \item R-squared (R²)
#'   \item Root Mean Squared Error (RMSE)
#'   \item Residual Sum of Squares (RSS)
#'   \item Akaike Information Criterion (AIC)
#'   \item Bayesian Information Criterion (BIC)
#' }
#'
#' This function is useful for identifying
#' the best-performing model within each treatment
#' and for evaluating whether model performance
#' varies among treatments.
#'
#' @param comparison Output from
#' \code{compare_models_by_treatment()}.
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
#' comparison <- compare_models_by_treatment(
#'   Groot = groot_fit,
#'   Gompertz = gompertz_fit
#' )
#'
#' rank_models_by_treatment(
#'   comparison
#' )
#'
#' @return A data frame containing model rankings
#' for each treatment and performance metric.
#'
#' @seealso
#' \code{\link{compare_models_by_treatment}},
#' \code{\link{best_model_by_treatment}},
#' \code{\link{model_win_frequency}},
#' \code{\link{rank_models}}
#'
#' @export
rank_models_by_treatment <- function(comparison) {

  if (!is.data.frame(comparison)) {
    stop(
      "comparison must be a data.frame."
    )
  }

  required_cols <- c(
    "Treatment",
    "Model",
    "Mean_R2",
    "Mean_RMSE",
    "Mean_AIC",
    "Mean_BIC"
  )

  missing_cols <- setdiff(
    required_cols,
    names(comparison)
  )

  if (length(missing_cols) > 0) {

    stop(
      paste(
        "Missing required column(s):",
        paste(
          missing_cols,
          collapse = ", "
        )
      )
    )

  }

  comparison |>

    dplyr::group_by(
      Treatment
    ) |>

    dplyr::mutate(

      Rank_R2 =
        rank(
          -Mean_R2,
          ties.method = "min"
        ),

      Rank_RMSE =
        rank(
          Mean_RMSE,
          ties.method = "min"
        ),

      Rank_AIC =
        rank(
          Mean_AIC,
          ties.method = "min"
        ),

      Rank_BIC =
        rank(
          Mean_BIC,
          ties.method = "min"
        )

    ) |>

    dplyr::ungroup()

}
