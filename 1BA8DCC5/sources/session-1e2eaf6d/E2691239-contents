test_that("This function returns correct structure", {
  dat <- sim.scen.shortsfe.comint(4)
  expect_true(is.data.frame(dat))
  expect_equal(ncol(dat), 5)
  expect_true("chamber" %in% names(dat))
})


