person_errors <- c(1, 26, 3, 24, 4, 13, 7, 7, 11, 11, 9, 9, 9, 16, 16, 4, 4, 22, 0, 0)

test_that("errors for one person is correct", {
  expect_equal(get_errors_for_one_person(zyston47, 1), person_errors[1])
})

test_that("errors for all persons is correct", {
  expect_equal(get_errors_for_all_persons(zyston47), person_errors)
})

item_errors <- c(24, 36, 38, 98)

test_that("errors for one item is correct", {
  expect_equal(get_errors_for_one_item_pair(zyston47[, c(1,4)]), item_errors[1])
})

test_that("errors for all items are correct", {
  expect_equal(get_errors_for_all_items(zyston47), item_errors)
})

# get_errors_for_one_person(zys, 1)
# get_errors_for_all_persons(zys) # use this as test
#

# tests for number of possible errors for item pairs, 0 to n*(n-1)
# get_errors_for_one_item_pair(data.frame(1:20, 20:1)), 190
#get_errors_for_one_item_pair(data.frame(1:20, 1:20)), 0
