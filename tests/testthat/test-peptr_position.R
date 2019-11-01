x <- c(1L, 2L, 3L)
y <- peptr_position(x)

# Instancing ----

test_that("new_position works", {
  expect_equal(length(new_position()), 0)
  expect_equal(class(new_position()), c("peptr_position", "vctrs_vctr"))
  expect_equal(length(new_position(x)), 3)
})

test_that("peptr_position works", {
  expect_equal(length(peptr_position()), 0)
  expect_equal(class(peptr_position()), c("peptr_position", "vctrs_vctr"))
  expect_equal(
    class(y),
    c("peptr_position", "vctrs_vctr")
  )
  expect_equal(
    class(peptr_position(as.numeric(x))),
    c("peptr_position", "vctrs_vctr")
  )
  expect_true(peptr_is_position(y))
  expect_false(peptr_is_position(x))
  expect_true(is.na(peptr_position(NA)))
  expect_equal(is.na(peptr_position(c(1, 3, NA))), c(FALSE, FALSE, TRUE))
  expect_equal(as.integer(y), x)
})

# Print ----

test_that("peptr_position prints", {
  expect_that(print(y), prints_text())
  expect_that(print(peptr_position(NA)), prints_text())
  expect_equal(vctrs::vec_ptype_abbr(y), "pos")
})

# Casting ----

test_that("casting integer and peptr_position", {
  expect_is(vctrs::vec_cast(1L, peptr_position()), "peptr_position")
})

test_that("casting peptr_position and integer", {
  expect_is(vctrs::vec_cast(peptr_position(5L), integer()), "integer")
})


test_that("casting double and peptr_position", {
  expect_is(vctrs::vec_cast(1, peptr_position()), "peptr_position")
})

test_that("casting peptr_position and double", {
  expect_is(vctrs::vec_cast(peptr_position(5), double()), "numeric")
})
