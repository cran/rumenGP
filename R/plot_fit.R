
#' Plot Fitted Model
#'
#' Plots observed and predicted gas production
#' values for an individual bottle.
#'
#' Observed measurements are displayed alongside
#' the fitted model curve, allowing visual
#' assessment of model performance.
#'
#' This visualization is useful for:
#'
#' \itemize{
#'   \item Evaluating model fit
#'   \item Identifying systematic deviations
#'   \item Inspecting individual fermentation profiles
#'   \item Comparing observed and predicted values
#' }
#'
#' @param fit A fitted model object.
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
#' # Plot a specific bottle
#' plot_fit(
#'   fit,
#'   head = 1
#' )
#'
#' @return A \code{ggplot2} object.
#'
#' @seealso
#' \code{\link{plot_all_fits}},
#' \code{\link{plot_residuals}},
#' \code{\link{fit_groot}}
#'
#' @export
plot_fit <- function(
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
      "Fit object contains no predictions."
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

    diagnostic <- fit$diagnostics |>
      dplyr::filter(
        Head == as.character(head)
      )

    if (nrow(diagnostic) > 0) {

      stop(
        paste(
          "Head",
          head,
          "has no predictions because fit status is:",
          diagnostic$Status
        )
      )

    }

    stop(
      paste(
        "Head",
        head,
        "was not found."
      )
    )

  }

  # ----------------------------
  # Plot
  # ----------------------------

  ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = Time_h
    )
  ) +

    ggplot2::geom_point(
      ggplot2::aes(
        y = Observed
      ),
      size = 2
    ) +

    ggplot2::geom_line(
      ggplot2::aes(
        y = Predicted
      ),
      colour = "blue",
      linewidth = 1
    ) +

    ggplot2::labs(
      title = paste(
        "Model fit -",
        model_name,
        "- Head",
        head
      ),
      x = "Time (h)",
      y = "Gas production (mL)"
    ) +

    ggplot2::theme_minimal()

}
