person_errors <- c(1, 26, 3, 24, 4, 13, 7, 7, 11, 11, 9, 9, 9, 16, 16, 4, 4, 22, 0, 0)

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