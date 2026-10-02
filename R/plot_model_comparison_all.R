
#' Compare Models for All Bottles
#'
#' Displays observed and predicted gas production
#' values for multiple fitted models across all bottles.
#'
#' Observed values are shown alongside model
#' predictions, allowing visual comparison of
#' competing kinetic models across the entire dataset.
#'
#' This visualization is useful for:
#'
#' \itemize{
#'   \item Comparing model performance
#'   \item Evaluating agreement between models
#'   \item Identifying systematic deviations
#'   \item Exploring treatment responses
#' }
#'
#' @param ... Fitted model objects.
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
#' plot_model_comparison_all(
#'   Groot = groot_fit,
#'   Gompertz = gompertz_fit
#' )
#'
#' @return A \code{ggplot2} object.
#'
#' @seealso
#' \code{\link{compare_models}},
#' \code{\link{plot_model_comparison}},
#' \code{\link{plot_model_comparison_treatment}},
#' \code{\link{fit_groot}},
#' \code{\link{fit_gompertz}}
#'
#' @export
plot_model_comparison_all <- function(...) {

  fits <- list(...)

  prediction_list <- purrr::imap_dfr(
    fits,
    function(fit, model_name) {

      fit$predictions |>
        dplyr::mutate(
          Model = model_name
        )

    }
  )

  ggplot2::ggplot() +

    ggplot2::geom_point(
      data =
        prediction_list |>
        dplyr::distinct(
          Head,
          Time_h,
          Observed
        ),
      ggplot2::aes(
        x = Time_h,
        y = Observed
      ),
      size = 0.8
    ) +

    ggplot2::geom_line(
      data = prediction_list,
      ggplot2::aes(
        x = Time_h,
        y = Predicted,
        colour = Model
      ),
      linewidth = 0.8
    ) +

    ggplot2::facet_wrap(
      ~ Head,
      scales = "free_y"
    ) +

    ggplot2::theme_minimal() +

    ggplot2::labs(
      x = "Time (h)",
      y = "Gas production (mL)",
      title = "Model comparison across bottles"
    )

}
