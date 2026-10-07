
#' Fit Log-logistic model
#'
#' Fits the Log-logistic gas production model
#' to each bottle in a rumen_gp dataset.
#'
#' ## Equation
#'
#' \deqn{
#' V(t)
#' =
#' \frac{
#' VF(rt)^a
#' }
#' {
#' 1 + (rt)^a
#' }
#' }
#'
#' where:
#'
#' \itemize{
#'   \item \eqn{V(t)} is cumulative gas production at time \eqn{t}
#'   \item \eqn{VF} is asymptotic gas production
#'   \item \eqn{r} is the rate parameter
#'   \item \eqn{a} is the shape parameter
#' }
#'
#' ## Interpretation
#'
#' The Log-logistic model is a flexible sigmoidal
#' model capable of describing a wide range of gas
#' production profiles.
#'
#' The parameter \eqn{r} controls the speed of gas
#' production, while \eqn{a} controls curve shape,
#' steepness, and inflection behavior.
#'
#' ## Advantages
#'
#' \itemize{
#'   \item Excellent flexibility
#'   \item Biologically interpretable parameters
#'   \item Often produces excellent fits
#'   \item Common in nonlinear growth and
#'         fermentation modeling
#' }
#'
#' ## Limitations
#'
#' \itemize{
#'   \item Requires positive incubation times
#'   \item Shape parameter may be less intuitive
#'         than simple exponential models
#' }
#'
#' ## Notes
#'
#' The Log-logistic model is mathematically
#' equivalent to both the Groot model
#' implemented in \code{fit_groot()} and the
#' generalized Michaelis-Menten model
#' implemented in \code{fit_mm()}.
#'
#' Parameter correspondence:
#'
#' \itemize{
#'   \item \code{VF = A}
#'   \item \code{a = c = k}
#'   \item \code{1/r = K = b}
#' }
#'
#' Both formulations produce identical fitted
#' values, residuals, diagnostics, AIC, BIC,
#' RMSE, and R-squared when convergence is
#' achieved.
#'
#' Researchers may choose either formulation
#' according to the terminology commonly used
#' in their field.
#'
#' @param data A rumen_gp object.
#'
#' @param start Optional list of starting values.
#' May contain any of:
#' \itemize{
#'   \item \code{VF}
#'   \item \code{r}
#'   \item \code{a}
#' }
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
#' # Fit using package default starting values
#' fit_default <- fit_loglogistic(
#'   gp
#' )
#'
#' summary(fit_default)
#'
#' # Fit using custom starting values
#' fit_custom_start <- fit_loglogistic(
#'   gp,
#'   start = list(
#'     VF = 120,
#'     r = 0.10,
#'     a = 2
#'   )
#' )
#'
#' summary(fit_custom_start)
#'
#' @return A \code{loglogistic_fit} object containing:
#' \itemize{
#'   \item Parameter estimates
#'   \item Model diagnostics
#'   \item Predicted values
#'   \item Residuals
#' }
#'
#' @export
fit_loglogistic <- function(
    data,
    start = NULL
) {

  if (!inherits(data, "rumen_gp")) {

    stop(
      "Input must be a rumen_gp object."
    )

  }

  validate_ankom(data)

  fit_one_bottle <- function(df) {

    df_fit <- df |>
      dplyr::filter(
        Time_h > 0
      )

    # ----------------------------------
    # Default starting values
    # ----------------------------------

    default_start <- list(

      VF = max(
        df_fit$Gas_mL,
        na.rm = TRUE
      ) * 1.05,

      r = 0.10,

      a = 2

    )

    fit_start <- default_start

    # ----------------------------------
    # User-defined overrides
    # ----------------------------------

    if (!is.null(start)) {

      valid_names <- c(
        "VF",
        "r",
        "a"
      )

      invalid_names <- setdiff(
        names(start),
        valid_names
      )

      if (length(invalid_names) > 0) {

        stop(
          paste(
            "Invalid start parameter(s):",
            paste(
              invalid_names,
              collapse = ", "
            )
          )
        )

      }

      fit_start[
        names(start)
      ] <- start

    }

    fit <- tryCatch({

      minpack.lm::nlsLM(

        Gas_mL ~

          VF *
          (
            (r * Time_h)^a
          ) /
          (
            1 +
              (r * Time_h)^a
          ),

        data = df_fit,

        start = fit_start,

        lower = c(
          VF = 0,
          r = 1e-6,
          a = 1e-6
        ),

        control =
          minpack.lm::nls.lm.control(
            maxiter = 1000
          )

      )

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
      newdata = df_fit
    )

    residuals <- df_fit$Gas_mL - preds

    rss <- sum(
      residuals^2,
      na.rm = TRUE
    )

    tss <- sum(
      (
        df_fit$Gas_mL -
          mean(df_fit$Gas_mL)
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

        return(
          data.frame(
            Head = unique(df$Head),
            Bottle = unique(df$Bottle),
            Rep = unique(df$Rep),
            Treatment = unique(df$Treatment),

            VF = NA_real_,
            r = NA_real_,
            a = NA_real_
          )
        )

      }

      coef_fit <- coef(
        fit$model
      )

      data.frame(
        Head = unique(df$Head),
        Bottle = unique(df$Bottle),
        Rep = unique(df$Rep),
        Treatment = unique(df$Treatment),

        VF = coef_fit["VF"],
        r = coef_fit["r"],
        a = coef_fit["a"]
      )

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

      df_pred <- df |>
        dplyr::filter(
          Time_h > 0
        )

      data.frame(
        Head = df_pred$Head,
        Bottle = df_pred$Bottle,
        Rep = df_pred$Rep,
        Treatment = df_pred$Treatment,

        Time_h = df_pred$Time_h,

        Observed = df_pred$Gas_mL,

        Predicted = fit$predictions,

        Residual = fit$residuals
      )

    }
  )

  rownames(parameters) <- NULL
  rownames(diagnostics) <- NULL
  rownames(predictions) <- NULL

  out <- list(
    parameters = parameters,
    diagnostics = diagnostics,
    predictions = predictions
  )

  class(out) <- c(
    "loglogistic_fit",
    class(out)
  )

  out

}
