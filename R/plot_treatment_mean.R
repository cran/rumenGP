
#' Plot Treatment Means Across Models
#'
#' Compares observed and predicted treatment means
#' across multiple fitted models for a selected
#' treatment.
#'
#' The observed treatment mean is displayed as a
#' black line with optional standard-error bands.
#' Predicted treatment means from each fitted model
#' are overlaid for visual comparison.
#'
#' This visualization is useful for:
#'
#' \itemize{
#'   \item Comparing competing kinetic models
#'   \item Evaluating treatment-level model performance
#'   \item Assessing agreement between observations
#'         and predictions
#'   \item Comparing fermentation dynamics among models
#' }
#'
#' @param ... Fitted model objects.
#'
#' @param treatment Treatment name.
#'
#' @param show_se Logical. If \code{TRUE}, displays a
#' standard-error ribbon around the observed treatment mean.
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
#' plot_treatment_mean(
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
#' \code{\link{plot_all_treatment_means}},
#' \code{\link{compare_models_by_treatment}},
#' \code{\link{fit_groot}},
#' \code{\link{fit_gompertz}}
#'
#' @export
plot_treatment_mean <- function(
    ...,
    treatment,
    show_se = TRUE
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

  # ----------------------------
  # Observed means
  # ----------------------------

  observed_mean <- prediction_list |>
    dplyr::group_by(
      Time_h
    ) |>
    dplyr::summarise(
      Mean_Observed = mean(Observed),
      SD_Observed = sd(Observed),
      N = dplyr::n(),
      SE_Observed = SD_Observed / sqrt(N),
      .groups = "drop"
    )

  # ----------------------------
  # Predicted means
  # ----------------------------

  predicted_mean <- prediction_list |>
    dplyr::group_by(
      Model,
      Time_h
    ) |>
    dplyr::summarise(
      Mean_Predicted = mean(Predicted),
      .groups = "drop"
    )

  p <- ggplot2::ggplot()

  # ----------------------------
  # Observed SE ribbon
  # ----------------------------

  if (show_se) {

    p <- p +

      ggplot2::geom_ribbon(
        data = observed_mean,
        ggplot2::aes(
          x = Time_h,
          ymin = Mean_Observed - SE_Observed,
          ymax = Mean_Observed + SE_Observed
        ),
        alpha = 0.2,
        fill = "grey70"
      )

  }

  p +

    ggplot2::geom_point(
      data = observed_mean,
      ggplot2::aes(
        x = Time_h,
        y = Mean_Observed
      ),
      colour = "black",
      size = 2
    ) +

    ggplot2::geom_line(
      data = predicted_mean,
      ggplot2::aes(
        x = Time_h,
        y = Mean_Predicted,
        colour = Model
      ),
      linewidth = 1
    ) +

    ggplot2::labs(
      title = paste(
        "Treatment mean comparison -",
        treatment
      ),
      x = "Time (h)",
      y = "Gas production (mL)"
    ) +

    ggplot2::theme_minimal()

}
