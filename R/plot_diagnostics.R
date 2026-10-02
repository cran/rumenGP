
#' Plot Diagnostic Summaries
#'
#' Creates diagnostic histograms for a fitted model.
#'
#' Diagnostic plots can be used to assess:
#'
#' \itemize{
#'   \item Residual distributions
#'   \item Parameter estimates
#'   \item Model fit quality
#'   \item Potential outliers
#' }
#'
#' These plots are useful for evaluating whether
#' model assumptions appear reasonable and for
#' identifying problematic fits.
#'
#' @param fit A fitted model object.
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
#' plot_diagnostics(
#'   fit
#' )
#'
#' @return A named list of \code{ggplot2} objects.
#'
#' @seealso
#' \code{\link{plot_fit}},
#' \code{\link{plot_residuals}},
#' \code{\link{flag_model}},
#' \code{\link{fit_groot}}
#'
#' @export
plot_diagnostics <- function(
    fit
) {

  diagnostics <- fit$diagnostics

  p1 <- ggplot2::ggplot(
    diagnostics,
    ggplot2::aes(
      x = R2
    )
  ) +
    ggplot2::geom_histogram(
      bins = 10
    ) +
    ggplot2::theme_minimal()

  p2 <- ggplot2::ggplot(
    diagnostics,
    ggplot2::aes(
      x = RMSE
    )
  ) +
    ggplot2::geom_histogram(
      bins = 10
    ) +
    ggplot2::theme_minimal()

  p3 <- ggplot2::ggplot(
    diagnostics,
    ggplot2::aes(
      x = AIC
    )
  ) +
    ggplot2::geom_histogram(
      bins = 10
    ) +
    ggplot2::theme_minimal()

  list(
    R2 = p1,
    RMSE = p2,
    AIC = p3
  )

}
