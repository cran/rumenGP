
#' Fit Custom Nonlinear Model
#'
#' Fits a user-defined nonlinear model
#' to each bottle in a rumen_gp dataset.
#'
#' ## Overview
#'
#' This function allows researchers to fit
#' custom nonlinear kinetic equations using
#' \code{minpack.lm::nlsLM()}.
#'
#' Custom models integrate directly with:
#'
#' \itemize{
#'   \item \code{summary()}
#'   \item \code{plot_fit()}
#'   \item \code{plot_residuals()}
#'   \item \code{compare_models()}
#' }
#'
#' making them fully compatible with the
#' rumenGP modeling framework.
#'
#' ## Formula Requirements
#'
#' The model formula must:
#'
#' \itemize{
#'   \item Use \code{Gas_mL} as the response variable
#'   \item Use \code{Time_h} as the time variable
#'   \item Include all parameters listed in
#'         \code{start}
#' }
#'
#' Example:
#'
#' \preformatted{
#' Gas_mL ~
#'   A *
#'   (
#'     Time_h /
#'     (
#'       Time_h + K
#'     )
#'   )
#' }
#'
#' ## Starting Values
#'
#' Starting values are supplied through
#' \code{start}.
#'
#' Example:
#'
#' \preformatted{
#' start = list(
#'   A = 150,
#'   K = 10
#' )
#' }
#'
#' Good starting values often improve
#' convergence and reduce fitting failures.
#'
#' ## Parameter Bounds
#'
#' Optional lower and upper bounds may be
#' supplied.
#'
#' Example:
#'
#' \preformatted{
#' lower = c(
#'   A = 0,
#'   K = 0
#' )
#'
#' upper = c(
#'   A = 500,
#'   K = 100
#' )
#' }
#'
#' Bounds can improve stability and prevent
#' biologically unrealistic parameter estimates.
#'
#' ## Best Practices
#'
#' \itemize{
#'   \item Start with biologically meaningful equations
#'   \item Use reasonable starting values
#'   \item Apply parameter bounds when appropriate
#'   \item Compare custom models against built-in models
#'   \item Evaluate both fit quality and parameter interpretability
#' }
#'
#' @param data A rumen_gp object.
#'
#' @param formula A nonlinear model formula.
#'
#' @param start Named list of starting values.
#'
#' @param lower Optional named numeric vector
#' of lower parameter bounds.
#'
#' @param upper Optional named numeric vector
#' of upper parameter bounds.
#'
#' @param model_name Character string used
#' to label the fitted model.
#'
#' @examples
#'
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
#' # Hyperbolic model
#' custom_fit <- fit_custom(
#'
#'   data = gp,
#'
#'   formula =
#'     Gas_mL ~
#'       A *
#'       (
#'         Time_h /
#'         (
#'           Time_h + K
#'         )
#'       ),
#'
#'   start = list(
#'     A = 150,
#'     K = 10
#'   ),
#'
#'   lower = c(
#'     A = 0,
#'     K = 0
#'   ),
#'
#'   model_name = "Hyperbolic"
#'
#' )
#'
#' summary(custom_fit)
#'
#' plot_fit(
#'   custom_fit,
#'   head = 1
#' )
#'
#' plot_residuals(
#'   custom_fit,
#'   head = 1
#' )
#'
#' # Compare with built-in models
#' compare_models(
#'   Groot = fit_groot(gp),
#'   Hyperbolic = custom_fit
#' )
#'
#'
#'
#' @return A \code{custom_fit} object containing:
#' \itemize{
#'   \item Model name
#'   \item Formula
#'   \item Starting values
#'   \item Parameter bounds
#'   \item Parameter estimates
#'   \item Model diagnostics
#'   \item Predicted values
#'   \item Residuals
#' }
#'
#' @seealso
#' \code{\link{fit_groot}},
#' \code{\link{fit_mm}},
#' \code{\link{compare_models}},
#' \code{\link{plot_fit}},
#' \code{\link{plot_residuals}}
#'
#' @export
fit_custom <- function(
    data,
    formula,
    start,
    lower = NULL,
    upper = NULL,
    model_name = "Custom"
) {

  # ----------------------------
  # Validation
  # ----------------------------

  if (!inherits(data, "rumen_gp")) {

    stop(
      "Input must be a rumen_gp object."
    )

  }

  validate_ankom(data)

  if (missing(formula)) {

    stop(
      "formula must be supplied."
    )

  }

  if (missing(start)) {

    stop(
      "start must be supplied."
    )

  }

  # ----------------------------
  # Fit one bottle
  # ----------------------------

  fit_one_bottle <- function(df) {

    fit <- tryCatch({

      if (
        is.null(lower) &&
        is.null(upper)
      ) {

        minpack.lm::nlsLM(

          formula = formula,

          data = df,

          start = start,

          control =
            minpack.lm::nls.lm.control(
              maxiter = 500
            )

        )

      } else if (
        !is.null(lower) &&
        is.null(upper)
      ) {

        minpack.lm::nlsLM(

          formula = formula,

          data = df,

          start = start,

          lower = lower,

          control =
            minpack.lm::nls.lm.control(
              maxiter = 500
            )

        )

      } else {

        minpack.lm::nlsLM(

          formula = formula,

          data = df,

          start = start,

          lower = lower,

          upper = upper,

          control =
            minpack.lm::nls.lm.control(
              maxiter = 500
            )

        )

      }

    }, error = function(e) NULL)

    if (is.null(fit)) {

      return(
        list(
          model = NULL,
          converged = FALSE,
          status = "FIT_FAILED"
        )
      )

    }

    preds <- predict(
      fit,
      newdata = df
    )

    residuals <- df$Gas_mL - preds

    rss <- sum(
      residuals^2,
      na.rm = TRUE
    )

    tss <- sum(
      (
        df$Gas_mL -
          mean(
            df$Gas_mL,
            na.rm = TRUE
          )
      )^2,
      na.rm = TRUE
    )

    r2 <- if (tss > 0) {

      1 - rss / tss

    } else {

      NA_real_

    }

    rmse <- sqrt(
      mean(
        residuals^2,
        na.rm = TRUE
      )
    )

    list(
      model = fit,
      converged = TRUE,
      status = "OK",
      predictions = preds,
      residuals = residuals,
      rss = rss,
      r2 = r2,
      rmse = rmse,
      aic = AIC(fit),
      bic = BIC(fit)
    )

  }

  # ----------------------------
  # Split bottles
  # ----------------------------

  split_data <- data |>
    dplyr::group_split(
      Head
    )

  fits <- purrr::map(
    split_data,
    fit_one_bottle
  )

  # ----------------------------
  # Parameters
  # ----------------------------

  parameters <- purrr::map2_dfr(
    split_data,
    fits,
    function(df, fit) {

      if (!fit$converged) {

        out <- data.frame(
          Head = unique(df$Head),
          Bottle = unique(df$Bottle),
          Rep = unique(df$Rep),
          Treatment = unique(df$Treatment)
        )

        for (p in names(start)) {

          out[[p]] <- NA_real_

        }

        return(out)

      }

      coef_fit <- coef(
        fit$model
      )

      out <- data.frame(
        Head = unique(df$Head),
        Bottle = unique(df$Bottle),
        Rep = unique(df$Rep),
        Treatment = unique(df$Treatment)
      )

      for (p in names(coef_fit)) {

        out[[p]] <- coef_fit[[p]]

      }

      out

    }
  )

  # ----------------------------
  # Diagnostics
  # ----------------------------

  diagnostics <- purrr::map2_dfr(
    split_data,
    fits,
    function(df, fit) {

      data.frame(
        Head = unique(df$Head),
        Bottle = unique(df$Bottle),
        Rep = unique(df$Rep),
        Treatment = unique(df$Treatment),

        Converged = fit$converged,

        Status =
          ifelse(
            fit$converged,
            fit$status,
            "FIT_FAILED"
          ),

        RSS =
          ifelse(
            fit$converged,
            fit$rss,
            NA
          ),

        R2 =
          ifelse(
            fit$converged,
            fit$r2,
            NA
          ),

        RMSE =
          ifelse(
            fit$converged,
            fit$rmse,
            NA
          ),

        AIC =
          ifelse(
            fit$converged,
            fit$aic,
            NA
          ),

        BIC =
          ifelse(
            fit$converged,
            fit$bic,
            NA
          )

      )

    }
  )

  # ----------------------------
  # Predictions
  # ----------------------------

  predictions <- purrr::map2_dfr(
    split_data,
    fits,
    function(df, fit) {

      if (!fit$converged) {

        return(NULL)

      }

      data.frame(
        Head = df$Head,
        Bottle = df$Bottle,
        Rep = df$Rep,
        Treatment = df$Treatment,

        Time_h = df$Time_h,

        Observed = df$Gas_mL,

        Predicted = fit$predictions,

        Residual = fit$residuals
      )

    }
  )

  rownames(parameters) <- NULL
  rownames(diagnostics) <- NULL
  rownames(predictions) <- NULL

  # ----------------------------
  # Output
  # ----------------------------

  out <- list(

    model_name = model_name,

    formula = formula,

    start = start,

    lower = lower,

    upper = upper,

    parameters = parameters,

    diagnostics = diagnostics,

    predictions = predictions

  )

  class(out) <- c(
    "custom_fit",
    class(out)
  )

  out

}
