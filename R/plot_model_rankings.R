
#' Plot Model Rankings
#'
#' Visualizes model rankings across multiple
#' performance metrics.
#'
#' Rankings are typically based on metrics such as:
#'
#' \itemize{
#'   \item R-squared (R²)
#'   \item Root Mean Squared Error (RMSE)
#'   \item Residual Sum of Squares (RSS)
#'   \item Akaike Information Criterion (AIC)
#'   \item Bayesian Information Criterion (BIC)
#' }
#'
#' This visualization helps identify models that
#' consistently perform well across several
#' evaluation criteria.
#'
#' @param ranking Output from
#' \code{rank_models()}.
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
#' ranking <- rank_models(
#'   comparison
#' )
#'
#' plot_model_rankings(
#'   ranking
#' )
#'
#' @return A \code{ggplot2} object.
#'
#' @seealso
#' \code{\link{rank_models}},
#' \code{\link{compare_models}},
#' \code{\link{plot_model_performance}}
#'
#' @export
plot_model_rankings <- function(
    ranking
) {

  ranks <- ranking |>

    dplyr::select(
      Model,
      Rank_R2,
      Rank_RMSE,
      Rank_AIC,
      Rank_BIC
    ) |>

    tidyr::pivot_longer(
      cols = -Model,
      names_to = "Metric",
      values_to = "Rank"
    )

  ggplot2::ggplot(
    ranks,
    ggplot2::aes(
      x = Model,
      y = Rank,
      fill = Model
    )
  ) +

    ggplot2::geom_col() +

    ggplot2::facet_wrap(
      ~ Metric,
      ncol = 2
    ) +

    ggplot2::coord_flip() +

    ggplot2::scale_y_reverse(
      breaks = seq(
        1,
        max(ranks$Rank),
        1
      )
    ) +

    ggplot2::labs(
      title = "Model ranking comparison",
      subtitle =
        "Rank 1 = best model",
      x = NULL,
      y = "Rank"
    ) +

    ggplot2::theme_minimal() +

    ggplot2::theme(
      legend.position = "none",

      strip.text =
        ggplot2::element_text(
          face = "bold"
        ),

      plot.title =
        ggplot2::element_text(
          face = "bold"
        )
    )

}
