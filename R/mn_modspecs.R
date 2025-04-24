#' Multinomial Model Comparison Table
#'
#' Compare multiple multinomial models and generate a customizable performance summary table.
#'
#' This function creates a tidy tibble or a formatted \code{gt} table displaying key performance metrics
#' and model structure for one or more multinomial regression models. It extracts model formulas,
#' sample sizes, deviance, effective degrees of freedom (EDF), and model fit statistics including
#' AIC, BIC, R-squared, RMSE, sigma, and a rank-based performance score. Optionally, the table can
#' display the full model formula or just the predictor structure, and highlight the best model
#' based on a selected performance criterion.
#'
#' This function is currently limited to models fitted using \code{nnet::multinom()} and is not
#' guaranteed to work with other model classes. Models must have a formula interface and be compatible
#' with both \code{broom::glance()} and \code{performance::compare_performance()}.
#'
#' @param ... One or more multinomial model objects (fitted with \code{nnet::multinom()}).
#' @param modspec Optional. Character string indicating model structure to display. Use \code{"Predictors"} to
#' display only the right-hand side of the formula (i.e., predictor terms), \code{"Formula"} to show the full formula,
#' or \code{NULL} (default) to omit both.
#' @param highlight Optional. A performance metric to use for row highlighting (e.g., \code{"AIC"}, \code{"R2"}, or \code{"Performance_Score"}).
#' @param gt Logical. If \code{TRUE}, returns a formatted \code{gt} table. If \code{FALSE}, returns a tibble (default: \code{FALSE}).
#' @param color A fill color used to highlight the best model row when \code{highlight} is specified and \code{gt = TRUE}.
#'
#' @return A tibble or \code{gt} table containing model performance metrics. Includes:
#' \itemize{
#'   \item \code{Model}: the original object name
#'   \item \code{Class}: model class (always \code{"multinom"})
#'   \item \code{N}: number of observations
#'   \item \code{EDF}: effective degrees of freedom
#'   \item \code{AIC}, \code{BIC}: model fit statistics
#'   \item \code{R2}, \code{R2_adj}: pseudo-R-squared and adjusted variant
#'   \item \code{RMSE}, \code{Sigma}: residual error metrics
#'   \item \code{Deviance}: deviance of the model
#'   \item \code{Performance_Score}: normalized model ranking
#'   \item \code{Predictors} or \code{Formula}: optional structural information
#' }
#'
#' @examples
#' library(nnet)
#' data(iris)
#'
#' mn1 <- multinom(Species ~ Sepal.Length, data = iris)
#' mn2 <- multinom(Species ~ Sepal.Length + Sepal.Width, data = iris)
#'
#' # Basic usage
#' mn_modspecs(mn1, mn2)
#'
#' # Show predictor terms only
#' mn_modspecs(mn1, mn2, modspec = "Predictors")
#'
#' # Show full formulas and highlight best AIC
#' mn_modspecs(mn1, mn2, modspec = "Formula", highlight = "AIC", gt = TRUE)
#'
#' @export

mn_modspecs <- function(..., modspec = NULL, highlight = NULL, gt = FALSE, color = "lightgreen") {
  models_quos <- rlang::enquos(...)

  model_names <- purrr::map_chr(models_quos, ~ rlang::as_label(.x))
  models <- purrr::map(models_quos, rlang::eval_tidy)

  formula_text <- purrr::map_chr(models, ~ {
    f <- .x$call$formula
    paste(deparse(f), collapse = " ")
  })

  glance_table <- purrr::map_dfr(models, glance) |>
    mutate(Formula = formula_text) |>
    select(Formula, nobs, everything())

  glance_table <- glance_table |> select(-AIC)

  perf_table <- as_tibble(compare_performance(models)) |>
    mutate(Class = Model) |> select(-Model)

  rank_table <- as_tibble(compare_performance(models, rank = TRUE)) |>
    select(Performance_Score) |>
    mutate(Performance_Score = Performance_Score * 100)

  comb_tbl <- bind_cols(
    tibble(Model = model_names),
    perf_table,
    as_tibble(glance_table),
    rank_table
  ) |>
    mutate(
      R2 = R2,  # force consistent caps
      R2_adj = R2_adjusted,
      N = nobs,
      Deviance = deviance,
      EDF = edf,
      Formula = str_trim(Formula),
      Predictors = str_remove(Formula, ".*~\\s*"),
      across(c("R2", "R2_adj", "RMSE", "Sigma"), ~ round(.x, 3)),
      across(c("N", "EDF", "Deviance"), ~ round(.x, 0)),
      across(c("AIC", "BIC", "Performance_Score"), ~ round(.x, 1))
    )

  if (!is.null(modspec) && !modspec %in% c("Predictors", "Formula")) {
    stop('modspec must be one of "Predictors", "Formula", or NULL')
  }

  if (is.null(modspec)) {
    modspecs_tbl <- comb_tbl |>
      select(Model, Class, N, EDF, AIC, BIC, R2, R2_adj, RMSE, Sigma, Deviance, Performance_Score) |>
      arrange(AIC, BIC)
  } else {
    modspecs_tbl <- comb_tbl |>
      select(Model, all_of(modspec), N, EDF, AIC, BIC, R2, R2_adj, RMSE, Sigma, Deviance) |>
      arrange(Model)
  }

  if (!gt) return(modspecs_tbl)

  # ✨ Highlight logic
  library(gt)

  gt_table <- modspecs_tbl |> gt()

  if (!is.null(highlight)) {
    if (!highlight %in% names(modspecs_tbl)) {
      stop(paste0('highlight must be one of the column names in the table. Options include: ',
                  paste(names(modspecs_tbl), collapse = ", ")))
    }

    # Metrics where larger is better
    larger_is_better <- c("R2", "R2_adj", "Performance_Score")

    best_row <- if (highlight %in% larger_is_better) {
      which.max(modspecs_tbl[[highlight]])
    } else {
      which.min(modspecs_tbl[[highlight]])
    }

    gt_table <- gt_table |>
      tab_style(
        style = list(
          cell_fill(color = color),
          cell_text(weight = "bold")
        ),
        locations = cells_body(
          rows = best_row
        )
      )
  }

  return(gt_table)
}
