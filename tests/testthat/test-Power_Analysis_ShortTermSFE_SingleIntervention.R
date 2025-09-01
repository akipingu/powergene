test_that("power estimate returns expected structure", {
  result <- sim.power.shortsfe.sinint(4, 50, 0.8, 0.1807, nsim = 10, n.cores = 1)
  expect_named(result, c("power", "ci.lower", "ci.upper"))
  expect_true(all(result >= 0 & result <= 1))
})
