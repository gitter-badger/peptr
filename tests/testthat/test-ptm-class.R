x <- c(1L, 2L, 3L)
y <- peptr_ptm(x, "ptm_test")

test_that("new_ptm works", {
  expect_equal(length(new_ptm()), 0)
  expect_equal(class(new_ptm()), c("peptr_ptm", "vctrs_rcrd", "vctrs_vctr"))
  expect_equal(length(new_ptm(peptr_position(x))), 3)
})

test_that("peptr_ptm works", {
  expect_equal(length(peptr_ptm()), 0)
  expect_equal(class(peptr_ptm()), c("peptr_ptm", "vctrs_rcrd", "vctrs_vctr"))
  expect_equal(
    class(y),
    c("peptr_ptm", "vctrs_rcrd", "vctrs_vctr")
  )
  expect_equal(
    class(peptr_ptm(as.numeric(x))),
    c("peptr_ptm", "vctrs_rcrd", "vctrs_vctr")
  )
  expect_true(peptr_is_ptm(y))
  expect_false(peptr_is_ptm(x))
  expect_true(is.na(peptr_ptm(NA)))
  expect_equal(is.na(peptr_ptm(c(1, 3, NA))), c(FALSE, FALSE, TRUE))
  expect_is(as.integer(y), "integer")
  expect_is(as.numeric(y), "numeric")
  expect_equal(as.integer(y), x)
  expect_equal(vctrs::field(y, "position"), peptr_position(x))
  expect_equal(peptr_get_ptm_name(y), "ptm_test")
  expect_equal(peptr_get_ptm_name(peptr_ptm(x)), "unknown")
})
