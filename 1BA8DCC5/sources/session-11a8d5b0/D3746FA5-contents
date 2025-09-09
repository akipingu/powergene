test_that("This function returns correct structure", {
  dat <- sim.scen.longsfe.sinint(n.ch.per.trt = 4, exp.length = 90, sampl.freq = "weekly")
  expect_s3_class(dat, "data.frame")
  expect_true(all(c("replicates", "intvn", "timef", "time", "chamber", "intvn.time") %in% names(dat)))
})
