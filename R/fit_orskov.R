
#' Fit Orskov and McDonald model
#'
#' Fits the Orskov and McDonald gas-production
#' model to each bottle in a rumen_gp dataset.
#'
#' ## Equation
#'
#' \deqn{
#' V(t)
#' =
#' VF
#' +
#' b
#' \left(
#' 1-e^{-kt}
#' \right)
#' }
#'
#' where:
#'
#' \itemize{
#'   \item \eqn{V(t)} is cumulative gas production at time \eqn{t}
#'   \item \eqn{VF} is the intercept (initial gas volume)
#'   \item \eqn{b} is the fermentable fraction
#'   \item \eqn{k} is the fractional rate constant
#' }
#'
#' ## Interpretation
#'
#' The Orskov and McDonald model partitions gas
#' production into:
#'
#' \itemize{
#'   \item An intercept term (\eqn{VF})
#'   \item A fermentable fraction (\eqn{b})
#' }
#'
#' The asymptotic gas production is:
#'
#' \deqn{
#' VF + b
#' }
#'
#' The parameter \eqn{k} controls the rate at which
#' the asymptote is approached.
#'
#' ## Advantages
#'
#' \itemize{
#'   \item Widely used in ruminant nutrition research
#'   \item Parameters have straightforward biological interpretation
#'   \item Stable convergence
#'   \item Useful benchmark model for comparison
#' }
#'
#' ## Limitations
#'
#' \itemize{
#'   \item No explicit lag parameter
#'   \item Limited flexibility for highly sigmoidal
#'         fermentation profiles
#'   \item Less adaptable than Gompertz, Groot,
#'         or Dual Logistic models
#' }
#'
#' @param data A rumen_gp object.
#'
#' @param start Optional list of starting values.
#' May contain any of:
#' \itemize{
#'   \item \code{VF}
#'   \item \code{b}
#'   \item \code{k}
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
#' fit_default <- fit_orskov(
#'   gp
#' )
#'
#' summary(fit_default)
#'
#' # Fit using custom starting values
#' fit_custom_start <- fit_orskov(
#'   gp,
#'   start = list(
#'     VF = 5,
#'     b = 120,
#'     k = 0.05
#'   )
#' )
#'
#' summary(fit_custom_start)
#'
#'
#'
#' @return An \code{orskov_fit} object containing:
#' \itemize{
#'   \item Parameter estimates
#'   \item Model diagnostics
#'   \item Predicted values
#'   \item Residuals
#' }
#'
#' @export
fit_orskov <- function(
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

    # ----------------------------------
    # Default starting values
    # ----------------------------------

    default_start <- list(

      VF = min(
        df$Gas_mL,
        na.rm = TRUE
      ),

      b =
        max(
          df$Gas_mL,
          na.rm = TRUE
        ) -
        min(
          df$Gas_mL,
          na.rm = TRUE
        ),

      k = 0.05

    )

    fit_start <- default_start

    # ----------------------------------
    # User-defined overrides
    # ----------------------------------

    if (!is.null(start)) {

      valid_names <- c(
        "VF",
        "b",
        "k"
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

          VF +

          b *
          (
            1 -
              exp(
                -k * Time_h
              )
          ),

        data = df,

        start = fit_start,

        lower = c(
          VF = 0,
          b = 0,
          k = 0
        ),

        control =
          minpack.lm::nls.lm.control(
            maxiter = 500
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
            b = NA_real_,
            k = NA_real_
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
        b = coef_fit["b"],
        k = coef_fit["k"]
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

  # ----------------------------
  # Clean row names
  # ----------------------------

  rownames(parameters) <- NULL
  rownames(diagnostics) <- NULL
  rownames(predictions) <- NULL

  # ----------------------------
  # Output
  # ----------------------------

  out <- list(
    parameters = parameters,
    diagnostics = diagnostics,
    predictions = predictions
  )

  class(out) <- c(
    "orskov_fit",
    class(out)
  )

  out

}
