person_errors <- c(
  1, 26, 3, 24, 4, 13, 7, 7, 11, 11, 9, 9, 9, 16, 16, 4, 4, 22, 0, 0
)

# error cells

item_scale_schuur <- paste(c(1, 2, 1, 2), c(1, 1, 2, 2), sep = "_")
error_cells <- matrix(c(0, 0, 1, 1, 0, 0, 1, 1, 0), nrow = 3)
calculated_error_cells <- find_error_cells(item_scale_schuur)

test_that("error cells are correct", {
  expect_equal(error_cells, calculated_error_cells, ignore_attr = TRUE)
})

# example van schuur page 78-79, testing homogenity, observed and expected
# errors

tbl <- matrix(c(149, 99, 33, 101, 395, 101, 18, 74, 140), nrow = 3)
df <- df_from_tbl(tbl)
schuur <- unlist(loevenize(df)[c("sum_errors", "sum_expected_errors", "h")])
correct_values <- c(226, 388.24, 0.42)
test_that("van schuur page 78-79", {
 expect_equal(round(schuur, 2), correct_values, ignore_attr = TRUE)
})

# todo: example p. 38
tbl <- matrix(c(157, 185, 134, 724), nrow = 2)
loev <- loevenize(df_from_tbl(tbl))
expected <- c(208.1, 0.36)
# loev$h == 0.36
# loev$sum_expected_errors = 208.1

# scale items
correct_scale_labels <- paste(c(3, 1, 3, 2, 3, 2, 1, 3),
                              c(1, 1, 2, 1, 3, 2, 2, 4), sep = "_")

item_scale <- scale_items(zysno47[, 1:3])

test_that("scaling items works", {
  expect_identical(correct_scale_labels, item_scale$label)
})

em <- zysnotize(zysno47)

test_that("expected errors are correct", {
           expect_equal(em$sum_expected_errors, 393.165)
  }
)

test_that("scalability is correct", {
           expect_equal(round(em$scalability, 1), 0.5)
  }
)

test_that("errors item pair can handle less than 2x2 categories!
", {
          expect_equal(errors_item_pair(laura_post[,1:2]), 0)
          expect_equal(errors_item_pair(laura_post[,9:10]), 6)
}
)
#test_that("errors for one person is correct", {
#  expect_equal(get_errors_for_one_person(zyston47, 1), person_errors[1])
#})

#test_that("errors for all persons are correct", {
#  expect_equal(, person_errors)
#})

#test_that("errors for all persons are correct for new version", {
#  expect_equal(get_errors_for_all_persons(zyston47), person_errors)
#})

item_errors <- c(24, 36, 38, 98)

test_that("errors for all items are correct", {
  expect_equal(colSums(em$error_matrix), item_errors)
})

#test_that("selections for matrices are correct", {
#  expect_equal(create_person_comparison_matrices(zyston47)[1],
#               matrix(rep(c(0, 0, 1, 0), 20), nrow = 4, byrow=T)),
#  expect_equal(create_person_comparison_matrices(zyston47)[3],
#               matrix(rep(c(1, 0, 1, 0), 20), nrow = 4, byrow=T))
#}
# get_errors_for_one_person(zys, 1)
# get_errors_for_all_persons(zys) # use this as test
#

# tests for number of possible errors for item pairs, 0 to n*(n-1)
# get_errors_for_one_item_pair(data.frame(1:20, 20:1)), 190
#get_errors_for_one_item_pair(data.frame(1:20, 1:20)), 0
