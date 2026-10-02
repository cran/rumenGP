
#' Fit exponential model without lag (EXP0)
#'
#' Fits the exponential gas production model
#' without an explicit lag phase.
#'
#' ## Equation
#'
#' \deqn{
#' V(t) = Vf \left( 1 - e^{-kt} \right)
#' }
#'
#' where:
#'
#' \itemize{
#'   \item \eqn{V(t)} is cumulative gas production at time \eqn{t}
#'   \item \eqn{Vf} is asymptotic gas production
#'   \item \eqn{k} is the fractional rate constant
#' }
#'
#' ## Interpretation
#'
#' The EXP0 model assumes that gas production
#' increases exponentially toward an asymptotic
#' value without an explicit lag phase.
#'
#' ## Advantages
#'
#' \itemize{
#'   \item Simple and computationally efficient
#'   \item Stable convergence
#'   \item Easy biological interpretation
#' }
#'
#' ## Limitations
#'
#' \itemize{
#'   \item Does not model lag time
#'   \item Limited flexibility for sigmoidal
#'         fermentation profiles
#' }
#'
#' @param data A rumen_gp object.
#'
#' @param start Optional list of starting values.
#' May contain any of:
#' \itemize{
#'   \item \code{Vf}
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
#' # Fit using package defaults
#' fit_default <- fit_exp0(
#'   gp
#' )
#'
#' summary(fit_default)
#'
#' # Fit using user-defined starting values
#' fit_custom_start <- fit_exp0(
#'   gp,
#'   start = list(
#'     Vf = 120,
#'     k = 0.05
#'   )
#' )
#'
#' summary(fit_custom_start)
#'
#'
#'
#' @return An \code{exp0_fit} object containing:
#' \itemize{
#'   \item Parameter estimates
#'   \item Model diagnostics
#'   \item Predicted values
#'   \item Residuals
#' }
#'
#' @export
fit_exp0 <- function(
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

      Vf = max(
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
        "Vf",
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

          Vf *
          (
            1 -
              exp(
                -k * Time_h
              )
          ),

        data = df,

        start = fit_start,

        lower = c(
          Vf = 0,
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

            Vf = NA_real_,
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

        Vf = coef_fit["Vf"],
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

  rownames(parameters) <- NULL
  rownames(diagnostics) <- NULL
  rownames(predictions) <- NULL

  out <- list(
    parameters = parameters,
    diagnostics = diagnostics,
    predictions = predictions
  )

  class(out) <- c(
    "exp0_fit",
    class(out)
  )

  out

}
