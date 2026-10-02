
#' Plot Dual-Pool Logistic Decomposition
#'
#' Visualizes the rapid pool, slow pool,
#' total predicted gas production, and
#' observed gas production for a fitted
#' dual-pool logistic model.
#'
#' The plot helps interpret the relative
#' contributions of rapidly and slowly
#' fermentable fractions through time.
#'
#' Components displayed include:
#'
#' \itemize{
#'   \item Observed gas production
#'   \item Predicted total gas production
#'   \item Rapid fermentation pool
#'   \item Slow fermentation pool
#' }
#'
#' This visualization is useful for
#' understanding substrate heterogeneity
#' and fermentation dynamics.
#'
#' @param fit A \code{dual_logistic_fit} object.
#'
#' @param head Optional bottle identifier.
#' If supplied, only that bottle will
#' be plotted.
#'
#' @param treatment Optional treatment name.
#' If supplied, a representative bottle
#' from that treatment will be plotted.
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
#' fit <- fit_dual_logistic(
#'   gp
#' )
#'
#' plot_dual_pools(
#'   fit,
#'   head = 1
#' )
#'
#' @return A \code{ggplot2} object.
#'
#' @seealso
#' \code{\link{fit_dual_logistic}},
#' \code{\link{plot_fit}},
#' \code{\link{plot_residuals}}
#'
#' @export
plot_dual_pools <- function(
    fit,
    head = NULL,
    treatment = NULL
) {

  if (!inherits(
    fit,
    "dual_logistic_fit"
  )) {

    stop(
      "fit must be a dual_logistic_fit object."
    )

  }

  if (!is.null(head) &&
      !is.null(treatment)) {

    stop(
      "Provide either head or treatment, not both."
    )

  }

  preds <- fit$predictions

  pars <- fit$parameters

  # ----------------------------------
  # Single head
  # ----------------------------------

  if (!is.null(head)) {

    pars <- pars |>
      dplyr::filter(
        Head == head
      )

    preds <- preds |>
      dplyr::filter(
        Head == head
      )

    if (nrow(pars) == 0) {

      stop(
        "Head not found."
      )

    }

    V1F <- pars$V1F
    V2F <- pars$V2F
    k1 <- pars$k1
    k2 <- pars$k2
    lambda <- pars$lambda

    plot_data <- preds |>

      dplyr::mutate(

        Rapid_Pool =

          V1F /
          (
            1 +
              exp(
                2 -
                  4 *
                  k1 *
                  (Time_h - lambda)
              )
          ),

        Slow_Pool =

          V2F /
          (
            1 +
              exp(
                2 -
                  4 *
                  k2 *
                  (Time_h - lambda)
              )
          )

      )

    return(

      ggplot2::ggplot() +

        ggplot2::geom_point(
          data = plot_data,
          ggplot2::aes(
            Time_h,
            Observed
          ),
          size = 2
        ) +

        ggplot2::geom_line(
          data = plot_data,
          ggplot2::aes(
            Time_h,
            Predicted,
            color = "Total"
          ),
          linewidth = 1.2
        ) +

        ggplot2::geom_line(
          data = plot_data,
          ggplot2::aes(
            Time_h,
            Rapid_Pool,
            color = "Rapid pool"
          ),
          linewidth = 1
        ) +

        ggplot2::geom_line(
          data = plot_data,
          ggplot2::aes(
            Time_h,
            Slow_Pool,
            color = "Slow pool"
          ),
          linewidth = 1
        ) +

        ggplot2::labs(
          title = paste(
            "Dual-pool Logistic:",
            "Head",
            head
          ),
          x = "Time (h)",
          y = "Gas production (mL)",
          color = NULL
        ) +

        ggplot2::theme_minimal()

    )

  }

  # ----------------------------------
  # One treatment
  # ----------------------------------

  if (!is.null(treatment)) {

    pars <- pars |>
      dplyr::filter(
        Treatment == treatment
      )

    preds <- preds |>
      dplyr::filter(
        Treatment == treatment
      )

    if (nrow(pars) == 0) {

      stop(
        "Treatment not found."
      )

    }

  }

  # ----------------------------------
  # Treatment means
  # ----------------------------------

  pool_data <- preds |>

    dplyr::left_join(
      pars |>
        dplyr::select(
          Head,
          V1F,
          V2F,
          k1,
          k2,
          lambda
        ),
      by = "Head"
    ) |>

    dplyr::mutate(

      Rapid_Pool =

        V1F /
        (
          1 +
            exp(
              2 -
                4 *
                k1 *
                (Time_h - lambda)
            )
        ),

      Slow_Pool =

        V2F /
        (
          1 +
            exp(
              2 -
                4 *
                k2 *
                (Time_h - lambda)
            )
        )

    )

  treatment_means <- pool_data |>

    dplyr::group_by(
      Treatment,
      Time_h
    ) |>

    dplyr::summarise(

      Observed =
        mean(
          Observed,
          na.rm = TRUE
        ),

      Predicted =
        mean(
          Predicted,
          na.rm = TRUE
        ),

      Rapid_Pool =
        mean(
          Rapid_Pool,
          na.rm = TRUE
        ),

      Slow_Pool =
        mean(
          Slow_Pool,
          na.rm = TRUE
        ),

      .groups = "drop"
    )

  p <- ggplot2::ggplot() +

    ggplot2::geom_point(
      data = treatment_means,
      ggplot2::aes(
        Time_h,
        Observed
      ),
      size = 1.5
    ) +

    ggplot2::geom_line(
      data = treatment_means,
      ggplot2::aes(
        Time_h,
        Predicted,
        color = "Total"
      ),
      linewidth = 1.2
    ) +

    ggplot2::geom_line(
      data = treatment_means,
      ggplot2::aes(
        Time_h,
        Rapid_Pool,
        color = "Rapid pool"
      ),
      linewidth = 1
    ) +

    ggplot2::geom_line(
      data = treatment_means,
      ggplot2::aes(
        Time_h,
        Slow_Pool,
        color = "Slow pool"
      ),
      linewidth = 1
    ) +

    ggplot2::labs(
      x = "Time (h)",
      y = "Gas production (mL)",
      color = NULL
    ) +

    ggplot2::theme_minimal()

  if (!is.null(treatment)) {

    p <- p +

      ggplot2::labs(
        title = paste(
          "Dual-pool Logistic:",
          treatment
        )
      )

  } else {

    p <- p +

      ggplot2::labs(
        title =
          "Dual-pool Logistic decomposition by treatment"
      ) +

      ggplot2::facet_wrap(
        ~ Treatment
      )

  }

  p

}
