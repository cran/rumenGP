
#' Plot Model Performance
#'
#' Visualizes model performance metrics produced by
#' \code{compare_models()}.
#'
#' This plot provides a graphical comparison of
#' competing models using goodness-of-fit statistics.
#'
#' Typical metrics include:
#'
#' \itemize{
#'   \item R-squared (R²)
#'   \item Root Mean Squared Error (RMSE)
#'   \item Residual Sum of Squares (RSS)
#'   \item Akaike Information Criterion (AIC)
#'   \item Bayesian Information Criterion (BIC)
#' }
#'
#' The visualization helps identify models that
#' balance goodness of fit and model complexity.
#'
#' @param comparison Output from
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
#' plot_model_performance(
#'   comparison
#' )
#'
#' @return A \code{ggplot2} object.
#'
#' @seealso
#' \code{\link{compare_models}},
#' \code{\link{rank_models}},
#' \code{\link{plot_model_rankings}}
#'
#' @export
plot_model_performance <- function(
    comparison
) {

  performance <- comparison |>

    dplyr::select(
      Model,
      Mean_R2,
      Mean_RMSE,
      Mean_AIC,
      Mean_BIC
    ) |>

    tidyr::pivot_longer(
      cols = -Model,
      names_to = "Metric",
      values_to = "Value"
    ) |>

    dplyr::mutate(

      Value = dplyr::if_else(
        Metric == "Mean_R2",
        Value * 100,
        Value
      ),

      Metric = dplyr::recode(
        Metric,
        Mean_R2   = "Mean R-squared (%)",
        Mean_RMSE = "Mean RMSE",
        Mean_AIC  = "Mean AIC",
        Mean_BIC  = "Mean BIC"
      )

    )

  ggplot2::ggplot(
    performance,
    ggplot2::aes(
      x = Model,
      y = Value,
      fill = Model
    )
  ) +

    ggplot2::geom_col() +

    ggplot2::facet_wrap(
      ~ Metric,
      scales = "free",
      ncol = 2
    ) +

    ggplot2::coord_flip() +

    ggplot2::labs(
      title = "Model performance comparison",
      subtitle =
        paste(
          "Higher R-squared is better;",
          "lower RMSE, AIC and BIC are better"
        ),
      x = NULL,
      y = NULL
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
        ),

      plot.subtitle =
        ggplot2::element_text(
          size = 10
        )
    )

}
