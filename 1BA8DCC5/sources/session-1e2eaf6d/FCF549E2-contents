test_that("sim.scen.shortsfe.comint returns correct structure", {
  dat <- sim.scen.shortsfe.comint()
  expect_true(is.data.frame(dat))
  expect_equal(ncol(dat), 5)
  expect_true("chamber" %in% names(dat))
})
