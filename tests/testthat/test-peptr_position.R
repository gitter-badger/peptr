# Instancing ----

test_that("new_position works", {
  expect_equal(length(new_position()), 0)
  expect_equal(class(new_position()), c("peptr_position", "vctrs_vctr"))
  expect_equal(length(new_position(1:3)), 3)
})

test_that("peptr_position works", {
  # Prototype
  expect_equal(length(peptr_position()), 0)
  expect_equal(class(peptr_position()), c("peptr_position", "vctrs_vctr"))
  # Basics
  expect_equal(class(peptr_position(1:3)),
               c("peptr_position", "vctrs_vctr"))
  expect_true(peptr_is_position(peptr_position(1:3)))
  expect_false(peptr_is_position(3L))
  # NA
  expect_true(is.na(peptr_position(NA)))
  expect_equal(is.na(peptr_position(c(1, 3, NA))), c(FALSE, FALSE, TRUE))
  # Data is correct

})

# Casting ----

test_that("casting double and peptr_position", {
  expect_is(vctrs::vec_cast(1, peptr_position()), "peptr_position")
})

test_that("casting peptr_position and double", {
  expect_is(vctrs::vec_cast(peptr_position(5), double()), "numeric")
})
