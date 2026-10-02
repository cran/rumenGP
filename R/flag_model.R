
#' Flag potentially problematic model fits
#'
#' Flags bottles with poor convergence,
#' low R-squared values and parameter-boundary issues.
#'
#' @param fit A fitted model object.
#' @param r2_threshold Minimum acceptable R-squared.
#'
#' @return Diagnostic table with QC flags.
#'
#' @export
flag_model <- function(
    fit,
    r2_threshold = 0.90
) {

  # ----------------------------
  # Validation
  # ----------------------------

  if (!"diagnostics" %in% names(fit)) {

    stop(
      "Fit object does not contain diagnostics."
    )

  }

  diagnostics <- fit$diagnostics

  # ----------------------------
  # Lambda boundary support
  # ----------------------------

  if (
    !"Lambda_Boundary" %in%
    names(diagnostics)
  ) {

    diagnostics$Lambda_Boundary <- FALSE

  }

  # ----------------------------
  # QC flags
  # ----------------------------

  flags <- diagnostics |>

    dplyr::mutate(

      Flag_FitFailed =
        !Converged,

      Flag_LowR2 =
        dplyr::if_else(
          !is.na(R2) &
            R2 < r2_threshold,
          TRUE,
          FALSE
        ),

      Flag_LambdaBoundary =
        dplyr::if_else(
          !is.na(Lambda_Boundary) &
            Lambda_Boundary,
          TRUE,
          FALSE
        )

    ) |>

    dplyr::mutate(

      Overall_Flag =
        dplyr::case_when(

          Flag_FitFailed ~
            "FIT_FAILED",

          Flag_LowR2 &
            Flag_LambdaBoundary ~
            "LOW_R2 + LAMBDA_AT_BOUNDARY",

          Flag_LowR2 ~
            "LOW_R2",

          Flag_LambdaBoundary ~
            "LAMBDA_AT_BOUNDARY",

          TRUE ~
            "OK"

        )

    )

  flags

}
