
#' Identify the Best Model for Each Treatment
#'
#' Returns the top-ranked model within each treatment.
#'
#' Rankings are obtained from
#' \code{rank_models_by_treatment()} and are based on
#' model performance metrics such as:
#'
#' \itemize{
#'   \item R-squared (R²)
#'   \item Root Mean Squared Error (RMSE)
#'   \item Residual Sum of Squares (RSS)
#'   \item Akaike Information Criterion (AIC)
#'   \item Bayesian Information Criterion (BIC)
#' }
#'
#' This function provides a concise summary of the
#' best-performing model for each treatment and is
#' useful for identifying whether different treatments
#' are best described by different kinetic models.
#'
#' @param ranked_comparison Output from
#' \code{rank_models_by_treatment()}.
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
#' ranking <- rank_models_by_treatment(
#'   comparison
#' )
#'
#' best_model_by_treatment(
#'   ranking
#' )
#'
#' @return A data frame containing the
#' highest-ranked model for each treatment.
#'
#' @seealso
#' \code{\link{compare_models_by_treatment}},
#' \code{\link{rank_models_by_treatment}},
#' \code{\link{model_win_frequency}},
#' \code{\link{compare_models}}
#'
#' @export
best_model_by_treatment <- function(
    ranked_comparison
) {

  ranked_comparison |>

    dplyr::mutate(

      Total_Rank =
        Rank_R2 +
        Rank_RMSE +
        Rank_AIC +
        Rank_BIC

    ) |>

    dplyr::group_by(
      Treatment
    ) |>

    dplyr::mutate(
      Overall_Rank =
        rank(
          Total_Rank,
          ties.method = "min"
        )
    ) |>

    dplyr::ungroup() |>

    dplyr::filter(
      Overall_Rank == 1
    ) |>

    dplyr::arrange(
      Treatment
    )

}
