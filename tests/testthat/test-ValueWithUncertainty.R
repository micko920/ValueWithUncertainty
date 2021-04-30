context("ValueWithUncertainty")

testthat::test_that("errors", {
  testthat::expect_error(
    ValueWithUncertainty(-1),
    "p cannot be less than 0"
  )

  testthat::expect_error(
    ValueWithUncertainty(2),
    "p cannot be greater than 1"
  )
})

testthat::test_that("errors", {
  testthat::expect_equal(
    ValueWithUncertainty(p = 0.0222),
    "p = .022"
  )
})
