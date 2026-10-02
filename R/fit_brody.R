
#' Fit Brody model
#'
#' Fits the Brody gas production model
#' to each bottle in a rumen_gp dataset.
#'
#' ## Equation
#'
#' \deqn{
#' V(t) = A \left(1 - b e^{-kt}\right)
#' }
#'
#' where:
#'
#' \itemize{
#'   \item \eqn{V(t)} is cumulative gas production at time \eqn{t}
#'   \item \eqn{A} is asymptotic gas production
#'   \item \eqn{b} is an integration constant
#'   \item \eqn{k} is the fractional rate constant
#' }
#'
#' ## Interpretation
#'
#' The Brody model describes gas production as a
#' monotonic increase toward an asymptotic value.
#' The parameter \eqn{k} controls the speed of
#' fermentation, while \eqn{b} controls the initial
#' position of the curve.
#'
#' ## Advantages
#'
#' \itemize{
#'   \item Simple and robust
#'   \item Stable convergence
#'   \item Easy biological interpretation
#'   \item Useful as a baseline model
#' }
#'
#' ## Limitations
#'
#' \itemize{
#'   \item No explicit lag parameter
#'   \item Less flexible than sigmoidal models
#'   \item May not adequately represent strongly
#'         sigmoidal fermentation profiles
#' }
#'
#' @param data A rumen_gp object.
#'
#' @param start Optional list of starting values.
#' May contain any of:
#' \itemize{
#'   \item \code{A}
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
#' fit_default <- fit_brody(
#'   gp
#' )
#'
#' summary(fit_default)
#'
#' # Fit using custom starting values
#' fit_custom_start <- fit_brody(
#'   gp,
#'   start = list(
#'     A = 120,
#'     b = 0.9,
#'     k = 0.05
#'   )
#' )
#'
#' summary(fit_custom_start)
#'
#'
#'
#' @return A \code{brody_fit} object containing:
#' \itemize{
#'   \item Parameter estimates
#'   \item Model diagnostics
#'   \item Predicted values
#'   \item Residuals
#' }
#'
#' @export
fit_brody <- function(
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

      A = max(
        df$Gas_mL,
        na.rm = TRUE
      ),

      b = 0.9,

      k = 0.05

    )

    fit_start <- default_start

    # ----------------------------------
    # User-defined overrides
    # ----------------------------------

    if (!is.null(start)) {

      valid_names <- c(
        "A",
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

          A *
          (
            1 -
              b *
              exp(
                -k * Time_h
              )
          ),

        data = df,

        start = fit_start,

        lower = c(
          A = 0,
          b = 0,
          k = 0
        ),

        upper = c(
          A = Inf,
          b = 1,
          k = Inf
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

    b_boundary <-
      coef_fit["b"] >= 0.999

    status <- if (b_boundary) {
      "B_AT_BOUNDARY"
    } else {
      "OK"
    }

    list(
      model = fit,
      converged = TRUE,
      status = status,
      b_boundary = b_boundary,
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

        A = coef_fit["A"],
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

        B_Boundary =
          ifelse(
            fit$converged,
            fit$b_boundary,
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
    "brody_fit",
    class(out)
  )

  out

}
