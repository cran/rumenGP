
#' Plot All Fitted Curves
#'
#' Displays observed and predicted gas production
#' values for all bottles in a fitted model.
#'
#' Each panel corresponds to a single bottle and
#' shows:
#'
#' \itemize{
#'   \item Observed gas production values
#'   \item Model predictions
#' }
#'
#' This plot is useful for quickly evaluating
#' model performance across all bottles in a dataset.
#'
#' @param fit A fitted model object produced by one
#' of the rumenGP model-fitting functions.
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
#' fit <- fit_groot(
#'   gp
#' )
#'
#' plot_all_fits(
#'   fit
#' )
#'
#' @return A \code{ggplot2} object.
#'
#' @seealso
#' \code{\link{plot_fit}},
#' \code{\link{plot_residuals}},
#' \code{\link{fit_groot}}
#'
#' @export
plot_all_fits <- function(fit) {

  if (!"predictions" %in% names(fit)) {
    stop(
      "Fit object does not contain predictions."
    )
  }

  ggplot2::ggplot(
    fit$predictions,
    ggplot2::aes(
      x = Time_h
    )
  ) +

    ggplot2::geom_point(
      ggplot2::aes(
        y = Observed
      ),
      size = 1
    ) +

    ggplot2::geom_line(
      ggplot2::aes(
        y = Predicted
      ),
      colour = "blue"
    ) +

    ggplot2::facet_wrap(
      ~ Head,
      scales = "free_y"
    ) +

    ggplot2::theme_minimal() +

    ggplot2::labs(
      x = "Time (h)",
      y = "Gas production (mL)"
    )

}
