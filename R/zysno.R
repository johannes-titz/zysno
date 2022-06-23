#' Calculate errors for one item pair (old version) 
#'
#' Uses Kronecker product for generating combinations
#' is relatively slow
#' @param item_pair 2-column df or matrix
#' @return number of errors
get_errors_for_one_item_pair <- function(item_pair) {
  item_pair <- as.matrix(item_pair)
  n_cols <- ncol(item_pair)
  n_rows <- nrow(item_pair)
  diffs <- item_pair %x% rep(1, n_rows) - rep(1, n_rows) %x% item_pair
  errors <- diffs[,1] * diffs[,2] < 0
  return(errors)
}

#' Calculate errors for one item pair
#'
#' @param item_pair 2-column df or matrix
#' @return number of errors
#' @export
errors_item_pair <- function(item_pair) {
  crosstab <- table(item_pair[, 1], item_pair[, 2])
  # must be at least a 2 x 2 table
  if (any(dim(crosstab) < 2)) return(0)
  errors <- 0
  # can we replace this with a Map?
  # skip first column and last row
  for (i in 1:(nrow(crosstab) - 1)) {
    for (j in 2:ncol(crosstab)) {
      errors <- errors + crosstab[i, j] * sum(crosstab[1:nrow(crosstab) > i, 1:ncol(crosstab) < j])
    }
  }
  # we only make half of all possible comparisons
  return(errors * 2)
}

# hier passt was nicht, leichte unterschätzung der werte
bt_item_pair <- function(item_pair) {
  item_pair <- as.matrix(item_pair)
  rows <- sample.int(nrow(item_pair), replace = T)
  rows2 <- sample.int(nrow(item_pair), replace = T)
  new_item_pair <- cbind(item_pair[rows, 1], item_pair[rows2, 2])
  sum(errors_item_pair(new_item_pair))
}

get_expected_errors_bt <- function(item_pair, bt_samples) {
  median(sapply(1:bt_samples, function(x) bt_item_pair(item_pair)))
}

#' Calculate number of expected errors for one item pair
#'
#' @return number of expected errors 
#' @export
get_expected_errors <- function(item_pair) {
  n_rows <- nrow(item_pair)
  fkj <- table(item_pair[, 1])
  fki <- table(item_pair[, 2])
  qj <- 1 - sum(fkj^2) / n_rows^2
  qi <- 1 - sum(fki^2) / n_rows^2
  n_rows^2 * qj * qi / 2
}

#' Main function of package zysno
#'
#' Performs analysis of unidimensionality based on the paper by Zysno
#' 
#' @param d dataset, rows are participants, columns are items
#' @return list of different outputs
#' @importFrom DescTools CombSet
#' @export
zysnotize <- function(d, cl = 1) {
  comps <- DescTools::CombSet(1:ncol(d), 2, repl = T, ord = T)
  dfs <- Map(function(x, y) d[, c(x, y)], comps[, 1], comps[, 2])
  errors <- unlist(pbapply::pblapply(dfs, errors_item_pair, cl = cl))
  expected_errors <- pbapply::pbsapply(dfs, get_expected_errors, cl = cl)
  error_matrix <- matrix(errors, nrow = sqrt(length(errors))) / 2
  expected_error_matrix <- matrix(expected_errors,
                                  nrow = sqrt(length(expected_errors))) / 2
  sum_errors <- sum(error_matrix)
  # expected errors in the diagonale are actually 0,
  # use upper tri instead
  sum_expected_errors <- sum(expected_error_matrix[upper.tri(expected_error_matrix)]) * 2
  scalability <- 1 - sum_errors / sum_expected_errors
  scalability_matrix <- 1 - error_matrix / expected_error_matrix
  return(tibble::lst(error_matrix,
                     expected_error_matrix,
                     scalability_matrix,
                     scalability,
                     sum_errors,
                     sum_expected_errors))
}


#' find the border between items/categories
#' 
scale_items <- function(d) {
  d <- as.data.frame(d)
  colnames(d) <- 1:ncol(d)
  d2 <- tidyr::pivot_longer(d, cols = 1:ncol(d))
  # verbose, but probably not the fastest
  d3 <- d2 %>%
    dplyr::group_by(name, value) %>%
    dplyr::summarize(sum = length(value)) %>%
    dplyr::mutate(cumsum = cumsum(sum)) %>%
    # highest value is not a border
    dplyr::filter(cumsum < nrow(d)) %>%
    dplyr::arrange(cumsum) %>%
    dplyr::mutate(label = paste(name, value + 1, sep = "_"))
  d3$rank <- seq(from = 1, length.out = nrow(d3), by = 2)
  return(d3)
}
