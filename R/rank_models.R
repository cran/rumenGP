
#' Rank Models
#'
#' Ranks fitted models using multiple model
#' performance criteria.
#'
#' Rankings are based on metrics produced by
#' \code{compare_models()} and may include:
#'
#' \itemize{
#'   \item R-squared (R²)
#'   \item Root Mean Squared Error (RMSE)
#'   \item Residual Sum of Squares (RSS)
#'   \item Akaike Information Criterion (AIC)
#'   \item Bayesian Information Criterion (BIC)
#' }
#'
#' Models that perform consistently well across
#' multiple metrics typically receive better
#' overall rankings.
#'
#' This function is useful when comparing several
#' competing kinetic models and identifying those
#' that provide the best balance between fit quality
#' and model complexity.
#'
#' @param comparison Output of
#' \code{compare_models()}.
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
#' rank_models(
#'   comparison
#' )
#'
#' @return A data frame containing model rankings
#' across performance metrics.
#'
#' @seealso
#' \code{\link{compare_models}},
#' \code{\link{rank_models_by_treatment}},
#' \code{\link{plot_model_performance}},
#' \code{\link{plot_model_rankings}}
#'
#' @export
rank_models <- function(comparison) {

  comparison |>
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

    )

}
