
#' Model Win Frequency
#'
#' Summarizes how often each model is the
#' best-performing model across treatments.
#'
#' Win frequency is calculated from the output of
#' \code{best_model_by_treatment()} and reports the
#' number of treatments for which each model achieved
#' the highest overall ranking.
#'
#' This summary is useful for identifying models
#' that consistently perform well across multiple
#' treatments.
#'
#' Models with higher win frequencies generally
#' demonstrate greater robustness across a dataset,
#' although treatment-specific performance should
#' also be considered.
#'
#' @param best_models Output from
#' \code{best_model_by_treatment()}.
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
#' groot_fit <- fit_groot(
#'   gp
#' )
#'
#' gompertz_fit <- fit_gompertz(
#'   gp
#' )
#'
#' comparison <- compare_models_by_treatment(
#'   Groot = groot_fit,
#'   Gompertz = gompertz_fit
#' )
#'
#' ranking <- rank_models_by_treatment(
#'   comparison
#' )
#'
#' best_models <- best_model_by_treatment(
#'   ranking
#' )
#'
#' model_win_frequency(
#'   best_models
#' )
#'
#' @return A data frame summarizing the number and
#' proportion of treatment-level wins for each model.
#'
#' @seealso
#' \code{\link{compare_models_by_treatment}},
#' \code{\link{rank_models_by_treatment}},
#' \code{\link{best_model_by_treatment}},
#' \code{\link{compare_models}}
#'
#' @export
model_win_frequency <- function(
    best_models
) {

  best_models |>

    dplyr::count(
      Model,
      name = "Treatments_Won"
    ) |>

    dplyr::arrange(
      dplyr::desc(
        Treatments_Won
      )
    )

}
