#' Simulate Experimental Design for Short-Term Semi-Field Intervention
#'
#' @details
#' This case study considers various design parameters including:
#' - Number of chambers per treatment (`n.ch.per.trt`), e.g., 4
#' - Mean mosquito count (`lambda`), e.g., 50
#' - Proportion remaining due to ITN (`interv.effect`), e.g., 0.2
#' - Inter-chamber variance (`chamber.var`), e.g., 0.1807
#' - Number of simulations (`nsim`), e.g., 100
#'
#' Constructs a template data set representing a short-term semi-field experiment
#' with two ITN treatment levels and a specified number of chambers per treatment.
#' Each chamber is uniquely identified by its treatment and replicate combination.
#'
#' @param n.ch.per.trt Integer. Number of chambers per treatment level.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{replicates}{Replicate number within each treatment group}
#'   \item{treatment}{Treatment level, e.g., for ITN, (0 = no ITN, 1 = ITN)}
#'   \item{chamber}{Unique chamber identifier as a factor}
#' }
#'
#' @examples
#' sim.scen.shortsfe.sinint(4)
#'
#' @export
sim.scen.shortsfe.sinint <- function(n.ch.per.trt){
  # design choices
  n.ch.per.trt <- n.ch.per.trt #4 # number of chambers per treatment
  treatment.lev <- 0:1 # ITN levels: 0 means no ITN and 1 means there is ITN
  # make template data set representing design
  dat <-expand.grid(replicates = 1:n.ch.per.trt, treatment = treatment.lev)
  # create chambers/replicates id
  dat$chamber <- factor(paste(dat$treatment, dat$replicates, sep="-"))
  # rearranging data set in a useful order
  dat <- dat[, c("replicates", "treatment", "chamber")]
  # output dat table
  dat
}

#' Simulate Mosquito Count Data for Short-Term Semi-Field Intervention
#'
#' Generates simulated mosquito count data under a short-term semi-field experimental design
#' with two ITN treatment levels. The function incorporates fixed effects for treatment,
#' random effects for chamber variability, and Poisson-distributed outcomes.
#'
#' @param n.ch.per.trt Integer. Number of chambers per treatment level.
#' @param lambda Numeric. Mean mosquito count in control chambers.
#' @param interv.effect Numeric. Proportion reduction due to ITN (e.g., 0.8 for 80% mortality).
#' @param chamber.var Numeric. Variance of random chamber effects.
#'
#' @return A data frame with columns:
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
#' sim.mosq.shortsfe.sinint(n.ch.per.trt = 4, lambda = 50, interv.effect = 0.8, chamber.var = 0.1807)
#'
#' @export
sim.mosq.shortsfe.sinint <- function(n.ch.per.trt,lambda,interv.effect,chamber.var){

  #reference previous dat table
  dat <- sim.scen.shortsfe.sinint(n.ch.per.trt)

  # parameter choices - fixed effects
  # mean recaptured mosquito count in the control group
  lambda <- lambda #50
  # proportion remaining in ITN chambers relative to control chambers
  prop.remain.after.interv.effect <- 1- interv.effect #0.2 # This corresponds to 80% mortality from ITN
  # assign chamber variance
  chamber.var <- chamber.var #0.1807

  # coefficients (parameter values) for the linear predictor terms
  b.0 <- log(lambda)
  b.i <- log(prop.remain.after.interv.effect)

  # simulate random effects for chambers
  chamber.re <- rnorm(nlevels(dat$chamber), sd = sqrt(chamber.var))
  names(chamber.re) <- levels(dat$chamber)
  # simulate linear predictor (with fixed effects only)
  # add the simulated data to dataset "dat" as a column named lin.pred
  dat$lin.pred.fixed <- round(b.0 +  b.i * dat$treatment, 2)
  # generate mosquito counts as fixed data using Poisson
  dat$mosquito.count.fixed <-
    rpois(nrow(dat),exp(dat$lin.pred.fixed))
  # add random effect (chamber.re) to fixed linear predictors above
  dat$lin.pred.random <-
    round(dat$lin.pred.fixed + chamber.re[as.character(dat$chamber)], 2)
  # generate mosquitoes counts as random data using Poisson
  dat$mosquito.count.rondom <-
    rpois(nrow(dat),exp(dat$lin.pred.random))
  # output the new data table “dat”
  dat
} # end of data simulation function "sim.dat.fun"

#' Extract p-value from Simulated GLMM for Short-Term Semi-Field Intervention
#' output parameters the p-values from the simulation function sim.pval.shortsfe.sinint
#' Simulates mosquito count data under a short-term semi-field experimental design
#' and fits a Poisson generalized linear mixed model (GLMM) to estimate the effect
#' of ITN treatment. Returns the p-value associated with the treatment effect.
#'
#' @param n.ch.per.trt Integer. Number of chambers per treatment level.
#' @param lambda Numeric. Mean mosquito count in control chambers.
#' @param interv.effect Numeric. Proportion reduction due to ITN (e.g., 0.8 for 80% mortality).
#' @param chamber.var Numeric. Variance of random chamber effects.
#'
#' @return A named numeric vector with:
#' \describe{
#'   \item{p}{P-value for the treatment effect from the GLMM}
#' }
#'
#' @examples
#' sim.pval.shortsfe.sinint(n.ch.per.trt = 4, lambda = 50, interv.effect = 0.8, chamber.var = 0.1807)
#'
#' @importFrom lme4 glmer
#'
#' @export
sim.pval.shortsfe.sinint <- function(n.ch.per.trt,lambda,interv.effect,chamber.var){
  sim.mosq <- sim.mosq.shortsfe.sinint(n.ch.per.trt,lambda,interv.effect,chamber.var)
  model.intervention <- suppressMessages(suppressWarnings( lme4::glmer(mosquito.count.rondom ~ treatment+(1|chamber),family="poisson",data=sim.mosq)))
  pvalue <- coef(summary(model.intervention))[2, "Pr(>|z|)"]
  c(pvalue = pvalue)
}

#' Estimate Empirical Power for Short-Term Semi-Field Intervention
#'
#' Runs repeated simulations of mosquito count data under a short-term semi-field design,
#' fits a GLMM to each dataset, and estimates empirical power as the proportion of
#' simulations with p-values below 0.05.
#'
#' @param nsim Integer. Number of simulation replicates.
#' @param n.ch.per.trt Integer. Number of chambers per treatment level.
#' @param lambda Numeric. Mean mosquito count in control chambers.
#' @param interv.effect Numeric. (e.g., 0.8 for 80% mortality).
#' @param chamber.var Numeric. Variance of random chamber effects.
#'
#' @return Numeric. Estimated empirical power (rounded to two decimal places).
#'
#' @examples
#' sim.power.shortsfe.sinint(nsim = 100, n.ch.per.trt = 4, lambda = 50, interv.effect = 0.8, chamber.var = 0.1807)
#'
#' @export
sim.power.shortsfe.sinint <- function(nsim, n.ch.per.trt, lambda, interv.effect, chamber.var,
                                      n.cores = parallel::detectCores() - 1) {
  cl <- parallel::makeCluster(n.cores)
  on.exit(parallel::stopCluster(cl))

  parallel::clusterExport(cl, varlist = c("sim.pval.shortsfe.sinint", "n.ch.per.trt",
                                          "lambda", "interv.effect", "chamber.var"), envir = environment())

  pvals <- parallel::parLapply(cl, 1:nsim, function(i) {
    result <- tryCatch(
      sim.pval.shortsfe.sinint(n.ch.per.trt, lambda, interv.effect, chamber.var),
      error = function(e) NA
    )
    result["pvalue"]
  })

  pvals <- unlist(pvals)
  n.sig <- sum(pvals < 0.05, na.rm = TRUE)
  power.estimate <- round(n.sig / nsim, 2)
  return(c(power = power.estimate))
}





