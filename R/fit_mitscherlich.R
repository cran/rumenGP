
#' Fit Mitscherlich model
#'
#' Fits the Mitscherlich gas-production model
#' to each bottle in a rumen_gp dataset.
#'
#' ## Equation
#'
#' \deqn{
#' V(t)
#' =
#' A
#' \left[
#' 1
#' -
#' \exp
#' \left(
#' -k(t-\lambda)
#' -
#' d
#' \left(
#' \sqrt{t+0.001}
#' -
#' \sqrt{\lambda+0.001}
#' \right)
#' \right)
#' \right]
#' }
#'
#' where:
#'
#' \itemize{
#'   \item \eqn{V(t)} is cumulative gas production at time \eqn{t}
#'   \item \eqn{A} is asymptotic gas production
#'   \item \eqn{k} is the fractional rate constant
#'   \item \eqn{d} is a diffusion or shape parameter
#'   \item \eqn{\lambda} is lag time
#' }
#'
#' ## Interpretation
#'
#' The Mitscherlich model combines an exponential
#' fermentation component with a diffusion-like term.
#'
#' The parameter \eqn{k} describes the primary
#' fermentation rate, while \eqn{d} provides
#' additional flexibility for representing changes
#' in fermentation dynamics over time.
#'
#' ## Advantages
#'
#' \itemize{
#'   \item Explicit lag parameter
#'   \item Flexible curve shape
#'   \item Can describe complex fermentation dynamics
#'   \item Often performs well when simple exponential
#'         models are inadequate
#' }
#'
#' ## Limitations
#'
#' \itemize{
#'   \item More complex than EXP0 or EXPL
#'   \item Increased parameter correlation
#'   \item Diffusion parameter may be less intuitive
#'         biologically
#'   \item May require careful starting values
#' }
#'
#' @param data A rumen_gp object.
#'
#' @param start Optional list of starting values.
#' May contain any of:
#' \itemize{
#'   \item \code{A}
#'   \item \code{k}
#'   \item \code{d}
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
#' fit_default <- fit_mitscherlich(
#'   gp
#' )
#'
#' summary(fit_default)
#'
#' # Fit using custom starting values
#' fit_custom_start <- fit_mitscherlich(
#'   gp,
#'   start = list(
#'     A = 120,
#'     k = 0.05,
#'     d = 0.05,
#'     lambda = 0.50
#'   )
#' )
#'
#' summary(fit_custom_start)
#'
#'
#'
#' @return A \code{mitscherlich_fit} object containing:
#' \itemize{
#'   \item Parameter estimates
#'   \item Model diagnostics
#'   \item Predicted values
#'   \item Residuals
#' }
#'
#' @export
fit_mitscherlich <- function(
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

      k = 0.05,

      d = 0.05,

      lambda = 0.5

    )

    fit_start <- default_start

    # ----------------------------------
    # User-defined overrides
    # ----------------------------------

    if (!is.null(start)) {

      valid_names <- c(
        "A",
        "k",
        "d",
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

          A *

          (
            1 -
              exp(
                -k *
                  (Time_h - lambda)

                -

                  d *
                  (
                    sqrt(Time_h + 0.001) -
                      sqrt(lambda + 0.001)
                  )
              )
          ),

        data = df,

        start = fit_start,

        lower = c(
          A = 0,
          k = 0,
          d = 0,
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
            d = NA_real_,
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
        d = coef_fit["d"],
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
    "mitscherlich_fit",
    class(out)
  )

  out

}
