#' Calibration Slopes for Multinomial Models
#'
#' Estimates calibration slopes and intercepts for each outcome class predicted by a multinomial model.
#' This method evaluates how well the predicted probabilities for each class align with the observed class labels,
#' using simple linear regression: \eqn{(Observed ~ Predicted)} for each class separately.
#'
#' A slope close to 1.0 indicates good calibration (predicted probabilities match observed frequencies).
#' A slope > 1 suggests overconfidence (model probabilities are too extreme), while a slope < 1 suggests underconfidence.
#'
#' @param model A fitted multinomial model (e.g., created with \code{nnet::multinom()}).
#' @param data A data frame containing the variables used to fit the model.
#' @param group_name A character string labeling the model (used in output table).
#'
#' @return A tibble with one row per outcome class, and columns for:
#' \describe{
#'   \item{model}{The name/label of the model (group_name).}
#'   \item{outcome}{The class label for which calibration is assessed.}
#'   \item{intercept}{The intercept of the calibration regression.}
#'   \item{slope}{The slope of the calibration regression, measuring calibration.}
#' }
#'
#' @details
#' This function is intended for use with multiclass classification models where the predicted outcome
#' is a factor variable with more than two levels. It calculates, for each class, a binary indicator
#' of whether the observed label equals the class, and regresses this indicator on the predicted probability
#' for that class. This is a simple and interpretable approach to assessing calibration for multiclass models.
#'
#' @examples
#' library(nnet)
#' model <- multinom(Species ~ Sepal.Length + Sepal.Width, data = iris)
#' calibration_slopes(model, iris, group_name = "iris_model")
#'
#' @export

mn_calibration_slopes <- function(model, data, group_name = "Model") {
  # Step 1: Validate model class
  if (!inherits(model, "multinom")) {
    stop("The model must be a fitted object of class 'multinom' (from nnet::multinom).")
  }

  # Step 2: Try loading predict method (ensure nnet is loaded)
  if (!"nnet" %in% loadedNamespaces()) {
    warning("Package 'nnet' is not loaded. Attempting to load it now...")
    suppressPackageStartupMessages(library(nnet))
  }

  # Step 3: Get outcome variable name
  outcome_var <- as.character(model$call$formula[[2]])

  # Step 4: Get predicted probabilities
  probs <- predict(model, newdata = data, type = "probs")

  # Handle binary model (1-class vector case)
  if (is.vector(probs)) {
    probs <- matrix(probs, ncol = 1, dimnames = list(NULL, levels(data[[outcome_var]])[2]))
  }

  # Combine probs with data
  calib_data <- cbind(data, probs)

  # Step 5: Loop over class names and compute calibration slopes
  results <- purrr::map_dfr(colnames(probs), function(class_name) {
    calib_data <- calib_data |>
      mutate(y_bin = as.numeric(.data[[outcome_var]] == class_name))

    # Use as.formula to safely reference class column
    fml <- as.formula(paste0("y_bin ~ `", class_name, "`"))
    lm_fit <- lm(fml, data = calib_data)

    tibble(
      Model = group_name,
      Outcome = class_name,
      Intercept = coef(lm_fit)[1],
      Slope = coef(lm_fit)[2]
    )
  })

  return(results)
}
