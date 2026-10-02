
#' Compare Model Fits
#'
#' Displays observed gas production values together
#' with predictions from multiple fitted models for
#' a single bottle.
#'
#' This visualization is useful for:
#'
#' \itemize{
#'   \item Comparing competing kinetic models
#'   \item Evaluating model performance
#'   \item Identifying differences among fitted curves
#'   \item Assessing model agreement with observations
#' }
#'
#' Observed measurements are displayed alongside
#' predictions from each supplied model, allowing
#' direct visual comparison.
#'
#' @param ... Fitted model objects.
#'
#' @param head Head identifier.
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
#' plot_model_comparison(
#'   Groot = groot_fit,
#'   Gompertz = gompertz_fit,
#'   head = 1
#' )
#'
#' @return A \code{ggplot2} object.
#'
#' @seealso
#' \code{\link{compare_models}},
#' \code{\link{plot_model_comparison_all}},
#' \code{\link{plot_model_comparison_treatment}},
#' \code{\link{fit_groot}},
#' \code{\link{fit_gompertz}}
#'
#' @export
plot_model_comparison <- function(
    ...,
    head
) {

  fits <- list(...)

  prediction_list <- purrr::imap_dfr(
    fits,
    function(fit, model_name) {

      fit$predictions |>
        dplyr::filter(
          Head == as.character(head)
        ) |>
        dplyr::mutate(
          Model = model_name
        )

    }
  )

  if (nrow(prediction_list) == 0) {

    stop(
      paste(
        "No predictions available for Head",
        head
      )
    )

  }

  ggplot2::ggplot() +

    ggplot2::geom_point(
      data =
        prediction_list |>
        dplyr::distinct(
          Time_h,
          Observed
        ),
      ggplot2::aes(
        x = Time_h,
        y = Observed
      ),
      size = 2
    ) +

    ggplot2::geom_line(
      data = prediction_list,
      ggplot2::aes(
        x = Time_h,
        y = Predicted,
        colour = Model
      ),
      linewidth = 1
    ) +

    ggplot2::labs(
      title = paste(
        "Model comparison - Head",
        head
      ),
      x = "Time (h)",
      y = "Gas production (mL)"
    ) +

    ggplot2::theme_minimal()

}
