
#' Compare Models for a Treatment
#'
#' Displays observed and predicted gas production
#' values for multiple fitted models across all
#' replicates of a selected treatment.
#'
#' Observed measurements are displayed alongside
#' model predictions, allowing visual comparison
#' of competing kinetic models within a treatment.
#'
#' This visualization is useful for:
#'
#' \itemize{
#'   \item Comparing competing models
#'   \item Evaluating model performance by treatment
#'   \item Assessing agreement among biological replicates
#'   \item Identifying systematic prediction errors
#' }
#'
#' @param ... Fitted model objects.
#'
#' @param treatment Treatment name.
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
#' plot_model_comparison_treatment(
#'   Groot = groot_fit,
#'   Gompertz = gompertz_fit,
#'   treatment = unique(
#'     gp$Treatment
#'   )[1]
#' )
#'
#' @return A \code{ggplot2} object.
#'
#' @seealso
#' \code{\link{compare_models_by_treatment}},
#' \code{\link{plot_model_comparison}},
#' \code{\link{plot_model_comparison_all}},
#' \code{\link{fit_groot}},
#' \code{\link{fit_gompertz}}
#'
#' @export
plot_model_comparison_treatment <- function(
    ...,
    treatment
) {

  fits <- list(...)

  prediction_list <- purrr::imap_dfr(
    fits,
    function(fit, model_name) {

      fit$predictions |>
        dplyr::filter(
          Treatment == treatment
        ) |>
        dplyr::mutate(
          Model = model_name
        )

    }
  )

  if (nrow(prediction_list) == 0) {

    stop(
      paste(
        "Treatment",
        treatment,
        "not found."
      )
    )

  }

  ggplot2::ggplot() +

    ggplot2::geom_point(
      data =
        prediction_list |>
        dplyr::distinct(
          Head,
          Rep,
          Time_h,
          Observed
        ),
      ggplot2::aes(
        x = Time_h,
        y = Observed
      ),
      size = 1.5
    ) +

    ggplot2::geom_line(
      data = prediction_list,
      ggplot2::aes(
        x = Time_h,
        y = Predicted,
        colour = Model,
        group = interaction(Model, Head)
      ),
      linewidth = 1
    ) +

    ggplot2::facet_wrap(
      ~ Rep,
      scales = "free_y"
    ) +

    ggplot2::labs(
      title = paste(
        "Model comparison -",
        treatment
      ),
      x = "Time (h)",
      y = "Gas production (mL)"
    ) +

    ggplot2::theme_minimal()

}
