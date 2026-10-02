
#' Compare Residuals Across Models
#'
#' Displays residuals from multiple fitted models
#' for a selected bottle.
#'
#' Residuals are calculated as:
#'
#' \deqn{
#' Observed - Predicted
#' }
#'
#' and can be used to evaluate:
#'
#' \itemize{
#'   \item Model bias
#'   \item Systematic prediction errors
#'   \item Heteroscedasticity
#'   \item Relative model performance
#' }
#'
#' Models with residuals that are randomly
#' distributed around zero are generally
#' preferred over models showing systematic
#' patterns.
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
#' plot_residual_comparison(
#'   Groot = groot_fit,
#'   Gompertz = gompertz_fit,
#'   head = 1
#' )
#'
#' @return A \code{ggplot2} object.
#'
#' @seealso
#' \code{\link{plot_residuals}},
#' \code{\link{plot_model_comparison}},
#' \code{\link{compare_models}},
#' \code{\link{fit_groot}},
#' \code{\link{fit_gompertz}}
#'
#' @export
plot_residual_comparison <- function(
    ...,
    head
) {

  fits <- list(...)

  residuals_df <- purrr::imap_dfr(
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

  ggplot2::ggplot(
    residuals_df,
    ggplot2::aes(
      x = Time_h,
      y = Residual,
      colour = Model
    )
  ) +

    ggplot2::geom_hline(
      yintercept = 0,
      linetype = 2
    ) +

    ggplot2::geom_line() +

    ggplot2::geom_smooth(
      method = "loess",
      se = FALSE
    ) +

    ggplot2::theme_minimal()

}
