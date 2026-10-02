
#' Fit Logistic model
#'
#' Fits a Logistic gas-production model
#' to each bottle in a rumen_gp dataset.
#'
#' ## Equation
#'
#' \deqn{
#' V(t)
#' =
#' \frac{A}
#' {
#' 1+\exp
#' \left[
#' 2+
#' 4k(\lambda-t)
#' \right]
#' }
#' }
#'
#' where:
#'
#' \itemize{
#'   \item \eqn{V(t)} is cumulative gas production at time \eqn{t}
#'   \item \eqn{A} is asymptotic gas production
#'   \item \eqn{k} is the fractional rate constant
#'   \item \eqn{\lambda} is lag time
#' }
#'
#' ## Interpretation
#'
#' The Logistic model describes gas production using a
#' sigmoidal curve with an initial lag phase,
#' a period of rapid fermentation, and a plateau
#' approaching the asymptotic gas production.
#'
#' The parameter \eqn{k} controls the steepness of the
#' curve, while \eqn{\lambda} determines the position
#' of the sigmoid along the time axis.
#'
#' ## Advantages
#'
#' \itemize{
#'   \item Explicit lag parameter
#'   \item Smooth sigmoidal behavior
#'   \item Stable convergence
#'   \item Widely used in biological growth and
#'         fermentation studies
#' }
#'
#' ## Limitations
#'
#' \itemize{
#'   \item Assumes a symmetric sigmoidal curve
#'   \item May not adequately fit highly asymmetric
#'         fermentation profiles
#'   \item Less flexible than Gompertz or Dual Logistic
#'         models
#' }
#'
#' @param data A rumen_gp object.
#'
#' @param start Optional list of starting values.
#' May contain any of:
#' \itemize{
#'   \item \code{A}
#'   \item \code{k}
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
#' fit_default <- fit_logistic(
#'   gp
#' )
#'
#' summary(fit_default)
#'
#' # Fit using custom starting values
#' fit_custom_start <- fit_logistic(
#'   gp,
#'   start = list(
#'     A = 120,
#'     k = 0.05,
#'     lambda = 1
#'   )
#' )
#'
#' summary(fit_custom_start)
#'
#'
#'
#' @return A \code{logistic_fit} object containing:
#' \itemize{
#'   \item Parameter estimates
#'   \item Model diagnostics
#'   \item Predicted values
#'   \item Residuals
#' }
#'
#' @export
fit_logistic <- function(
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

    t <- df$Time_h
    y <- df$Gas_mL

    # ----------------------------------
    # Default starting values
    # ----------------------------------

    default_start <- list(

      A = max(
        max(y, na.rm = TRUE),
        1
      ),

      k = max(
        max(
          diff(y) / diff(t),
          na.rm = TRUE
        ) /
          max(
            max(y, na.rm = TRUE),
            1
          ),
        0.001
      ),

      lambda = 1

    )

    fit_start <- default_start

    # ----------------------------------
    # User-defined overrides
    # ----------------------------------

    if (!is.null(start)) {

      valid_names <- c(
        "A",
        "k",
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

          A /
          (
            1 +
              exp(
                2 +
                  4 *
                  k *
                  (lambda - Time_h)
              )
          ),

        data = df,

        start = fit_start,

        lower = c(
          A = 0,
          k = 0,
          lambda = 0
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

    residuals <- y - preds

    rss <- sum(
      residuals^2,
      na.rm = TRUE
    )

    tss <- sum(
      (
        y -
          mean(
            y,
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

            A = NA_real_,
            k = NA_real_,
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

        A = coef_fit["A"],
        k = coef_fit["k"],
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
    "logistic_fit",
    class(out)
  )

  out

}
