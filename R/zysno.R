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
  errors <- diffs[, 1] * diffs[,2] < 0
  return(errors)
}

#' Calculate errors for one item pair
#'
#' @param item_pair 2-column df or matrix
#' @return number of errors
#' @export
errors_item_pair <- function(item_pair) {
  item_pair <- na.omit(item_pair)
  crosstab <- table(item_pair[, 1], item_pair[, 2])
  # must be at least a 2 x 2 table
  if (any(dim(crosstab) < 2)) return(0)
  errors <- 0
  # can we replace this with a Map?
  # skip first column and last row
  for (i in 1:(nrow(crosstab) - 1)) {
    for (j in 2:ncol(crosstab)) {
      errors <- errors + as.numeric(crosstab[i, j]) * as.numeric(sum(crosstab[1:nrow(crosstab) > i, 1:ncol(crosstab) < j]))
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
  item_pair <- na.omit(item_pair)
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

#' of course only for item pair!
find_error_cells <- function(item_scale) {
  order <- as.numeric(substr(item_scale, 1, 1))
  row <- cumsum(ifelse(order == 1, 1, 0))
  col <- cumsum(ifelse(order == 2, 1, 0))
  # always start at 0, 0
  # but indices start at 1, 1
  row <- c(0, row) + 1
  col <- c(0, col) + 1
  # all possible indices
  indices <- expand.grid(min(row):max(row), min(col):max(col))
  indices$label <- paste(indices[,1], indices[,2])
  # filter out non-error cells
  filter <- indices$label %in% paste(row, col)
  indices$value <- 1
  indices$value[filter] <- 0
  return(as.matrix(xtabs(value~Var1+Var2, data=indices)))
}

lv_errors_item_pair <- function(item_pair) {
  item_pair <- na.omit(item_pair)
  error_cells <- find_error_cells(scale_items(item_pair)$label)
  tbl <- table(item_pair[, 1], item_pair[, 2])
  if (ncol(tbl) < 2 || nrow(tbl) < 2) {
    return(data.frame(act_sum = NA, exp_sum = NA, h = NA))
  }
  chi <- chisq.test(tbl)
  exp_sum <- sum(chi$expected * error_cells)
  act_sum <- sum(tbl * error_cells)
  h <- 1 - act_sum / exp_sum
  return(data.frame(act_sum, exp_sum, h))
}

#' tests are needed!
#' @export
loevenize <- function(d) {
  # do not compare with itself
  comps <- DescTools::CombSet(1:ncol(d), 2, repl = FALSE, ord = TRUE)
  colnames(comps) <- c("row", "col")
  dfs <- Map(function(x, y) d[, c(x, y)], comps[, 1], comps[, 2])
  res <- pbapply::pblapply(dfs, lv_errors_item_pair)
  res <- Reduce(rbind, res)
  res <- cbind(comps, res)
  error_matrix <- as.matrix(xtabs(act_sum ~ row + col, data = res))
  expected_error_matrix <- as.matrix(xtabs(exp_sum ~ row + col, data = res))
  h_matrix <- as.matrix(xtabs(h ~ row + col, data = res))
  sum_errors <- sum(error_matrix) / 2
  sum_expected_errors <- sum(expected_error_matrix) / 2
  h <- 1 - sum_errors / sum_expected_errors
  return(tibble::lst(error_matrix,
                     expected_error_matrix,
                     h_matrix,
                     h,
                     sum_errors,
                     sum_expected_errors))
}

#' Generate data from cross table
#'
#' Useful when you do not have raw data but calculations must be performed
#' on raw data.
#'
#' @param tbl Cross table of two variables
#' @return data as matrix
tbl2data <- function(tbl) {
  d <- as.data.frame.table(tbl)
  d$Var1 <- as.numeric(d$Var1)
  d$Var2 <- as.numeric(d$Var2)
  x <- rep(d$Var1, d$Freq)
  y <- rep(d$Var2, d$Freq)
  res <- cbind(x, y)
  return(res)
}
