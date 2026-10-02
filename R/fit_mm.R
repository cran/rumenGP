
#' Fit Michaelis-Menten model
#'
#' Fits the generalized Michaelis-Menten model
#' to each bottle in a rumen_gp dataset.
#'
#' ## Equation
#'
#' \deqn{
#' V(t)
#' =
#' A
#' \frac{t^{c}}
#' {
#' t^{c}+K^{c}
#' }
#' }
#'
#' where:
#'
#' \itemize{
#'   \item \eqn{V(t)} is cumulative gas production at time \eqn{t}
#'   \item \eqn{A} is asymptotic gas production
#'   \item \eqn{K} is the half-time parameter
#'   \item \eqn{c} is the shape parameter
#' }
#'
#' ## Interpretation
#'
#' The generalized Michaelis-Menten model describes
#' cumulative gas production using a flexible sigmoidal
#' function.
#'
#' The parameter \eqn{K} represents the time required
#' to reach approximately half of the asymptotic gas
#' production, while \eqn{c} controls curve shape and
#' steepness.
#'
#' ## Advantages
#'
#' \itemize{
#'   \item Flexible sigmoidal behavior
#'   \item Biologically meaningful half-time parameter
#'   \item Usually converges reliably
#'   \item Well suited for rumen gas production data
#' }
#'
#' ## Limitations
#'
#' \itemize{
#'   \item Shape parameter may be difficult to interpret
#'         biologically
#'   \item More complex than simple exponential models
#' }
#'
#' ## Notes
#'
#' The generalized Michaelis-Menten model is
#' mathematically equivalent to the Groot model
#' implemented in \code{fit_groot()}.
#'
#' Parameter correspondence:
#'
#' \itemize{
#'   \item \code{A = VF}
#'   \item \code{K = b}
#'   \item \code{c = k}
#' }
#'
#' Both formulations produce identical fitted values
#' and model diagnostics when convergence is achieved.
#'
#' Researchers may choose either formulation according
#' to the terminology commonly used in their field.
#'
#' @param data A rumen_gp object.
#'
#' @param start Optional list of starting values.
#' May contain any of:
#' \itemize{
#'   \item \code{A}
#'   \item \code{K}
#'   \item \code{c}
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
#' fit_default <- fit_mm(
#'   gp
#' )
#'
#' summary(fit_default)
#'
#' # Fit using custom starting values
#' fit_custom_start <- fit_mm(
#'   gp,
#'   start = list(
#'     A = 120,
#'     K = 10,
#'     c = 2
#'   )
#' )
#'
#' summary(fit_custom_start)
#'
#'
#'
#' @return A \code{mm_fit} object containing:
#' \itemize{
#'   \item Parameter estimates
#'   \item Model diagnostics
#'   \item Predicted values
#'   \item Residuals
#' }
#'
#' @export
fit_mm <- function(
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

      K = median(
        t,
        na.rm = TRUE
      ),

      c = 1

    )

    fit_start <- default_start

    # ----------------------------------
    # User-defined overrides
    # ----------------------------------

    if (!is.null(start)) {

      valid_names <- c(
        "A",
        "K",
        "c"
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

          (Time_h^c) /

          (
            Time_h^c +
              K^c
          ),

        data = df,

        start = fit_start,

        lower = c(
          A = 0,
          K = 0,
          c = 0
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

            A = NA_real_,
            K = NA_real_,
            c = NA_real_
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
        K = coef_fit["K"],
        c = coef_fit["c"]
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
    "mm_fit",
    class(out)
  )

  out

}
