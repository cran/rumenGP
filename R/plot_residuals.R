
#' Plot Model Residuals
#'
#' Plots residuals for an individual bottle.
#'
#' Residuals are calculated as:
#'
#' \deqn{
#' Residual = Observed - Predicted
#' }
#'
#' Residual plots are useful for:
#'
#' \itemize{
#'   \item Identifying systematic model bias
#'   \item Detecting outliers
#'   \item Evaluating model assumptions
#'   \item Assessing goodness of fit
#' }
#'
#' Ideally, residuals should be randomly distributed
#' around zero with no obvious trend through time.
#'
#' @param fit A fitted model object containing a
#' predictions element.
#'
#' @param head Optional Head identifier.
#' If omitted and only one bottle is present,
#' that bottle is plotted automatically.
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
#' plot_residuals(
#'   fit,
#'   head = 1
#' )
#'
#' @return A \code{ggplot2} object.
#'
#' @seealso
#' \code{\link{plot_fit}},
#' \code{\link{plot_residual_comparison}},
#' \code{\link{plot_diagnostics}},
#' \code{\link{fit_groot}}
#'
#' @export
plot_residuals <- function(
    fit,
    head = NULL
) {

  if (!"predictions" %in% names(fit)) {

    stop(
      "Fit object does not contain predictions."
    )

  }

  if (nrow(fit$predictions) == 0) {

    stop(
      "Fit object contains no residuals."
    )

  }

  model_name <- class(fit)[1]

  available_heads <- unique(
    fit$predictions$Head
  )

  # ----------------------------
  # Automatic Head selection
  # ----------------------------

  if (is.null(head)) {

    if (length(available_heads) == 1) {

      head <- available_heads

    } else {

      stop(
        paste(
          "Multiple bottles detected.",
          "Please supply head =",
          paste(
            available_heads,
            collapse = ", "
          )
        )
      )

    }

  }

  # ----------------------------
  # Filter selected bottle
  # ----------------------------

  df <- fit$predictions |>
    dplyr::filter(
      Head == as.character(head)
    )

  # ----------------------------
  # Missing bottle handling
  # ----------------------------

  if (nrow(df) == 0) {

    stop(
      paste(
        "No residuals available for Head",
        head
      )
    )

  }

  # ----------------------------
  # Plot
  # ----------------------------

  ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = Time_h,
      y = Residual
    )
  ) +

    ggplot2::geom_hline(
      yintercept = 0,
      linetype = 2,
      colour = "black"
    ) +

    ggplot2::geom_point(
      size = 2
    ) +

    ggplot2::geom_line(
      linewidth = 0.5
    ) +

    ggplot2::geom_smooth(
      method = "loess",
      se = FALSE,
      colour = "red",
      linewidth = 1
    ) +

    ggplot2::labs(
      title = paste(
        "Residual plot -",
        model_name,
        "- Head",
        head
      ),
      x = "Time (h)",
      y = "Residual (Observed - Predicted)"
    ) +

    ggplot2::theme_minimal()

}
