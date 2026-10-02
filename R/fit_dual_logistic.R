
#' Fit dual-pool logistic model
#'
#' Fits a dual-pool logistic model representing
#' rapidly and slowly degradable fractions.
#'
#' ## Equation
#'
#' \deqn{
#' V(t)=
#' \frac{V1F}
#' {
#' 1+\exp\left[2-4k1(t-\lambda)\right]
#' }
#' +
#' \frac{V2F}
#' {
#' 1+\exp\left[2-4k2(t-\lambda)\right]
#' }
#' }
#'
#' where:
#'
#' \itemize{
#'   \item \eqn{V(t)} is cumulative gas production at time \eqn{t}
#'   \item \eqn{V1F} is the final gas volume from the rapidly fermentable fraction
#'   \item \eqn{V2F} is the final gas volume from the slowly fermentable fraction
#'   \item \eqn{k1} is the fractional rate constant of the rapid fraction
#'   \item \eqn{k2} is the fractional rate constant of the slow fraction
#'   \item \eqn{\lambda} is lag time
#' }
#'
#' ## Interpretation
#'
#' The Dual Logistic model assumes that gas production
#' originates from two independent fermentation pools:
#'
#' \itemize{
#'   \item A rapidly degradable fraction (\eqn{V1F})
#'   \item A slowly degradable fraction (\eqn{V2F})
#' }
#'
#' Each fraction follows a logistic fermentation pattern
#' with its own rate constant.
#'
#' ## Advantages
#'
#' \itemize{
#'   \item Represents complex fermentation dynamics
#'   \item Separates rapid and slow fermentation pools
#'   \item Biologically meaningful parameterization
#'   \item Useful for heterogeneous substrates
#' }
#'
#' ## Limitations
#'
#' \itemize{
#'   \item Requires estimation of five parameters
#'   \item More computationally demanding
#'   \item Greater risk of parameter correlation
#'   \item May require careful starting values
#' }
#'
#' @param data A rumen_gp object.
#'
#' @param start Optional list of starting values.
#' May contain any of:
#' \itemize{
#'   \item \code{V1F}
#'   \item \code{V2F}
#'   \item \code{k1}
#'   \item \code{k2}
#'   \item \code{lambda}
#' }
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
#' # Fit using package default starting values
#' fit_default <- fit_dual_logistic(
#'   gp
#' )
#'
#' summary(fit_default)
#'
#' # Fit using custom starting values
#' fit_custom_start <- fit_dual_logistic(
#'   gp,
#'   start = list(
#'     V1F = 30,
#'     V2F = 70,
#'     k1 = 0.20,
#'     k2 = 0.05,
#'     lambda = 0.50
#'   )
#' )
#'
#' summary(fit_custom_start)
#'
#'
#'
#' @return A \code{dual_logistic_fit} object containing:
#' \itemize{
#'   \item Parameter estimates
#'   \item Model diagnostics
#'   \item Predicted values
#'   \item Residuals
#'   \item Rapid and slow pool estimates
#' }
#'
#' @export
fit_dual_logistic <- function(
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

    vmax <- max(
      df$Gas_mL,
      na.rm = TRUE
    )

    # ----------------------------------
    # Default starting values
    # ----------------------------------

    default_start <- list(

      V1F = vmax * 0.30,

      V2F = vmax * 0.70,

      k1 = 0.20,

      k2 = 0.05,

      lambda = 0.50

    )

    fit_start <- default_start

    # ----------------------------------
    # User-defined overrides
    # ----------------------------------

    if (!is.null(start)) {

      valid_names <- c(
        "V1F",
        "V2F",
        "k1",
        "k2",
        "lambda"
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

          V1F /
          (
            1 +
              exp(
                2 -
                  4 *
                  k1 *
                  (Time_h - lambda)
              )
          ) +

          V2F /
          (
            1 +
              exp(
                2 -
                  4 *
                  k2 *
                  (Time_h - lambda)
              )
          ),

        data = df,

        start = fit_start,

        lower = c(
          V1F = 0,
          V2F = 0,
          k1 = 0,
          k2 = 0,
          lambda = 0
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
          mean(df$Gas_mL)
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

    coef_fit <- coef(fit)

    lambda_boundary <-
      coef_fit["lambda"] <= 1e-6

    status <- if (lambda_boundary) {

      "LAMBDA_AT_BOUNDARY"

    } else {

      "OK"

    }

    list(
      model = fit,
      converged = TRUE,
      status = status,
      lambda_boundary = lambda_boundary,
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

            V1F = NA_real_,
            V2F = NA_real_,
            VF = NA_real_,
            k1 = NA_real_,
            k2 = NA_real_,
            lambda = NA_real_
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

        V1F = coef_fit["V1F"],
        V2F = coef_fit["V2F"],

        VF =
          coef_fit["V1F"] +
          coef_fit["V2F"],

        k1 = coef_fit["k1"],
        k2 = coef_fit["k2"],
        lambda = coef_fit["lambda"]
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

        Lambda_Boundary =
          ifelse(
            fit$converged,
            fit$lambda_boundary,
            NA
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

  out <- list(
    parameters = parameters,
    diagnostics = diagnostics,
    predictions = predictions
  )

  class(out) <- c(
    "dual_logistic_fit",
    class(out)
  )

  out

}
