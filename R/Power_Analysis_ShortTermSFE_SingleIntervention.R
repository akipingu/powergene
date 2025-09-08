#' Simulate Experimental Design for Short-Term Semi-Field Intervention
#'
#' Constructs a template dataset representing a short-term semi-field experiment
#' with two ITN treatment levels and a specified number of chambers per treatment.
#' Each chamber is uniquely identified by its treatment and replicate combination.
#'
#' @details
#' This case study considers various design parameters including:
#' - Number of chambers per treatment (`n.ch.per.trt`), e.g., 4
#' - Expected mosquitoes to be recaptured in control chamber (`lambda`), e.g., 50
#' - Proportion reduction due to ITN (`interv.effect`), e.g., 0.8
#' - Inter-chamber variance (`chamber.var`), e.g., 0.1807
#' - Number of simulations (`nsim`), e.g., 100
#'
#' @param n.ch.per.trt Integer. Number of chambers per treatment level.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{replicates}{Replicate number within each treatment group}
#'   \item{treatment}{Treatment level (0 = no ITN, 1 = ITN)}
#'   \item{chamber}{Unique chamber identifier as a factor}
#' }
#'
#' @examples
#' sim.scen.shortsfe.sinint(4)
#'
#' @export
sim.scen.shortsfe.sinint <- function(n.ch.per.trt) {

  # Simulate brief progress bar
  pb <- utils::txtProgressBar(min = 0, max = 100, style = 3)
  for (i in seq(0, 100, by = 10)) {
    Sys.sleep(0.05)  # brief pause to simulate progress
    utils::setTxtProgressBar(pb, i)
  }
  close(pb)

  treatment.lev <- 0:1
  dat <- expand.grid(replicates = 1:n.ch.per.trt, treatment = treatment.lev)
  dat$chamber <- factor(paste(dat$treatment, dat$replicates, sep = "-"))
  dat <- dat[, c("replicates", "treatment", "chamber")]
  dat
}

#' Simulate Mosquito Count Data for Short-Term Semi-Field Intervention
#'
#' Generates simulated mosquito count data under a short-term semi-field experimental design
#' with fixed effects for treatment, random effects for chamber variability, and Poisson-distributed outcomes.
#'
#' @param n.ch.per.trt Integer. Number of chambers per treatment level.
#' @param lambda Numeric. Mean mosquito count in control chambers.
#' @param interv.effect Numeric. Proportion reduction due to ITN.
#' @param chamber.var Numeric. Variance of random chamber effects.
#'
#' @return A data frame with the following columns:
#' \describe{
#'   \item{replicates}{Replicate number within each treatment group}
#'   \item{treatment}{Treatment level (0 = no ITN, 1 = ITN)}
#'   \item{chamber}{Unique chamber identifier}
#'   \item{lin.pred.fixed}{Linear predictor with fixed effects only}
#'   \item{mosquito.count.fixed}{Simulated counts based on fixed effects}
#'   \item{lin.pred.random}{Linear predictor including random chamber effects}
#'   \item{mosquito.count.rondom}{Simulated counts including random effects}
#' }
#'
#' @examples
#' sim.mosq.shortsfe.sinint(4, 50, 0.8, 0.1807)
#'
#' @export
sim.mosq.shortsfe.sinint <- function(n.ch.per.trt, lambda, interv.effect, chamber.var) {

  # Simulate brief progress bar
  pb <- utils::txtProgressBar(min = 0, max = 100, style = 3)
  for (i in seq(0, 100, by = 10)) {
    Sys.sleep(0.05)  # brief pause to simulate progress
    utils::setTxtProgressBar(pb, i)
  }
  close(pb)

  dat <- sim.scen.shortsfe.sinint(n.ch.per.trt)
  prop.remain <- 1 - interv.effect
  b.0 <- log(lambda)
  b.i <- log(prop.remain)
  chamber.re <- rnorm(nlevels(dat$chamber), sd = sqrt(chamber.var))
  names(chamber.re) <- levels(dat$chamber)
  dat$lin.pred.fixed <- round(b.0 + b.i * dat$treatment, 2)
  dat$mosquito.count.fixed <- rpois(nrow(dat), exp(dat$lin.pred.fixed))
  dat$lin.pred.random <- round(dat$lin.pred.fixed + chamber.re[as.character(dat$chamber)], 2)
  dat$mosquito.count.rondom <- rpois(nrow(dat), exp(dat$lin.pred.random))
  dat
}

#' Extract p-value from Simulated GLMM for Short-Term Semi-Field Intervention
#'
#' Simulates mosquito count data and fits a Poisson GLMM to estimate the effect
#' of ITN treatment. Returns the p-value associated with the treatment effect.
#'
#' @param n.ch.per.trt Integer. Number of chambers per treatment level.
#' @param lambda Numeric. Mean mosquito count in control chambers.
#' @param interv.effect Numeric. Proportion reduction due to ITN.
#' @param chamber.var Numeric. Variance of random chamber effects.
#'
#' @return A named numeric vector:
#' \describe{
#'   \item{pvalue}{P-value for the treatment effect from the GLMM}
#' }
#'
#' @examples
#' sim.pval.shortsfe.sinint(4, 50, 0.8, 0.1807)
#'
#'
#' @export
sim.pval.shortsfe.sinint <- function(n.ch.per.trt, lambda, interv.effect, chamber.var) {

  # Simulate brief progress bar
  pb <- utils::txtProgressBar(min = 0, max = 100, style = 3)
  for (i in seq(0, 100, by = 10)) {
    Sys.sleep(0.05)  # brief pause to simulate progress
    utils::setTxtProgressBar(pb, i)
  }
  close(pb)

  sim.mosq <- sim.mosq.shortsfe.sinint(n.ch.per.trt, lambda, interv.effect, chamber.var)
  model <- suppressMessages(suppressWarnings(
    lme4::glmer(mosquito.count.rondom ~ treatment + (1 | chamber),
                family = "poisson", data = sim.mosq)
  ))
  pvalue <- coef(summary(model))[2, "Pr(>|z|)"]
  c(pvalue = pvalue)
}

#' Estimate Empirical Power for Short-Term Semi-Field Intervention
#'
#' Runs repeated simulations and GLMM fits to estimate empirical power
#' as the proportion of simulations with p-values below 0.05.
#'
#' @param nsim Integer. Number of simulation replicates.
#' @param n.ch.per.trt Integer. Number of chambers per treatment level.
#' @param lambda Numeric. Mean mosquito count in control chambers.
#' @param interv.effect Numeric. Proportion reduction due to ITN.
#' @param chamber.var Numeric. Variance of random chamber effects.
#' @param n.cores Integer. Number of cores to use for parallel processing.
#'
#' @return A named numeric vector:
#' \describe{
#'   \item{power}{Estimated empirical power (rounded to two decimal places)}
#' }
#'
#' @note Parallel execution is supported via `ncores`, but examples default to `ncores = 1` for reproducibility and package checks.
#'
#' @examples
#' sim.power.shortsfe.sinint(
#'   nsim = 100,
#'   n.ch.per.trt = 4,
#'   lambda = 50,
#'   interv.effect = 0.8,
#'   chamber.var = 0.1807,
#'   n.cores = 1  # Prevent parallel execution during R CMD check
#' )
#'
#' @importFrom parallel makeCluster parLapply stopCluster clusterExport detectCores
#' @importFrom stats binom.test coef rnorm rpois
#'
#' @export
sim.power.shortsfe.sinint <- function(n.ch.per.trt, lambda, interv.effect, chamber.var, nsim,
                                      n.cores = 1) {

  # Simulate brief progress bar
  pb <- utils::txtProgressBar(min = 0, max = 100, style = 3)
  for (i in seq(0, 100, by = 10)) {
    Sys.sleep(0.05)  # brief pause to simulate progress
    utils::setTxtProgressBar(pb, i)
  }
  close(pb)

  # Define simulation wrapper
  sim_wrapper <- function(i) {
    result <- tryCatch(
      sim.pval.shortsfe.sinint(n.ch.per.trt, lambda, interv.effect, chamber.var),
      error = function(e) NA
    )
    result["pvalue"]
  }

  # Run simulations
  if (n.cores > 1) {
    cl <- parallel::makeCluster(n.cores)
    on.exit(parallel::stopCluster(cl))
    parallel::clusterExport(cl, varlist = c("sim.pval.shortsfe.sinint", "n.ch.per.trt",
                                            "lambda", "interv.effect", "chamber.var"),
                            envir = environment())
    pvals <- parallel::parLapply(cl, 1:nsim, sim_wrapper)
  } else {
    pvals <- lapply(1:nsim, sim_wrapper)
  }

  # Flatten p-values and count significant results
  pvals <- unlist(pvals)
  n.sig <- sum(pvals < 0.05, na.rm = TRUE)

  # Estimate power and confidence interval
  power.estimate <- c(
    power = round(n.sig / nsim, 2),
    binom.test(x = n.sig, n = nsim)$conf.int
  )
  names(power.estimate)[2:3] <- c("ci.lower", "ci.upper")
  power.estimate
}
