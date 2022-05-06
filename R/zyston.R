#'
#' @importFrom DescTools CombSet
#' @param item_pair 2-column df or matrix
#' @return number of errors
get_errors_for_one_item_pair <- function(item_pair) {
  rows <- DescTools::CombSet(1:nrow(item_pair), 2, repl = F,
                             ord = F)
  diffs <- item_pair[rows[, 1], ] - item_pair[rows[, 2], ]
  diffs2 <- diffs[, 1] * diffs[, 2]
  return(sum(diffs2 < 0))
}

#' @param d dataset, rows are participants, columns are items
#' @return matrix of pair-wise errors for each item combination
#' @importFrom DescTools CombSet
create_item_error_mtrx <- function(d) {
  comps <- DescTools::CombSet(1:ncol(d), 2, repl = T, ord = T)
  dfs <- Map(function(x, y) d[, c(x, y)], comps[,1], comps[,2])
  res <- sapply(dfs, get_errors_for_one_item_pair)
  matrix(res, nrow = sqrt(length(res)))
}

#' @param d
get_errors_for_all_items <- function(d) {
  error_mtrx <- create_item_error_mtrx(d)
  colSums(error_mtrx)
}

#' @param person_pair 2-row df or matrix representing answers of two participants to at least two items
#' @return number of violations for one person pair
zyston3 <- function(person_pair) {
  cols <- DescTools::CombSet(1:ncol(person_pair), 2, repl = F, ord = F)
  diffs2 <- diff(person_pair[,cols[,1]]) * diff(person_pair[,cols[,2]])
  return(sum(diffs2 < 0))
}

#' @param d
#' @param person_row
#' @return number of violations for one person
get_errors_for_one_person <- function(d, person_row) {
  diffs <- sapply(1:nrow(d), function(x) zyston3(d[c(person_row, x),]))
  return(sum(diffs))
}

#' @param d
#' @return number of violations for all persons
get_errors_for_all_persons <- function(d) {
  sapply(1:nrow(d), function(x) get_errors_for_one_person(d, x))
}

#' @export
zyston <- function(data) {
  d <- data
  item_errors <- get_errors_for_all_items(d)
  person_errors <- get_errors_for_all_persons(d)
  item_mtrx <- create_item_error_mtrx(d)
  combn_item_comps <- DescTools::CombN(ncol(d), 2)
  N <- nrow(d)^2 * DescTools::CombN(ncol(d), 2)
  N2 <- nrow(d) * (nrow(d) - 1) * combn_item_comps
  n_comp_per_item <- N2 / ncol(d)
  n_comp_per_person <- N2 / nrow(d)
  n_comp_per_mtrx_cell <- nrow(d) * (nrow(d) - 1) / 2
  abs <- list(item_mtrx, item_errors, person_errors)
  rel <- list(item_mtrx / n_comp_per_mtrx_cell,
              item_errors / n_comp_per_item,
              person_errors / n_comp_per_person)
  return(list(abs, rel))
}

#' @export
zyston_summary <- function(zyston) {
  signif(zyston[[2]][[1]], 3)
}
