#' ezefa: Item-level loadings, communalities, and diagnostics from factor analysis
#'
#' This function summarizes item-level factor loadings from a `psych::fa()` object
#' and appends item-level communalities (h²), and optionally, uniqueness (u²) and item-level KMO.
#'
#' @param fa_object A fitted factor analysis object from `psych::fa()`.
#' @param data The original data frame used for factor analysis (only needed if `kmo = TRUE`).
#' @param u Logical. If TRUE, includes item-level uniqueness (u²). Default is TRUE.
#' @param kmo Logical. If TRUE, includes item-level KMO from `psych::KMO()`. Default is TRUE.
#' @param digits Number of decimal places to round numeric values. Default is 3.
#'
#' @return A tibble with rows as items and columns for each factor loading, h², and optionally u² and KMO.
#' @examples
#' \dontrun{
#' fa_res <- psych::fa(mtcars[, 1:6], nfactors = 2)
#' ezefa(fa_res, data = mtcars[, 1:6])
#' ezefa(fa_res, data = mtcars[, 1:6], u = FALSE, kmo = FALSE)
#' }
#'
#' @export
ezefa <- function(fa_object, data = NULL, u = TRUE, kmo = TRUE, digits = 3) {
  # Validate input
  if (!inherits(fa_object, "fa")) {
    stop("Input must be a psych::fa object.")
  }

  loadings <- as.data.frame(unclass(fa_object$loadings))
  colnames(loadings) <- paste0("Loading_F", seq_len(ncol(loadings)))
  loadings <- round(loadings, digits)

  df <- tibble::tibble(
    Item = rownames(loadings),
    !!!loadings,
    h2 = round(fa_object$communality, digits)
  )

  if (u) {
    df$u2 <- round(fa_object$uniquenesses, digits)
  }

  if (kmo) {
    if (is.null(data)) stop("You must provide the original data if kmo = TRUE.")
    item_kmo <- psych::KMO(data)$MSAi
    df$KMO <- round(item_kmo[match(df$Item, names(item_kmo))], digits)
  }

  df
}
