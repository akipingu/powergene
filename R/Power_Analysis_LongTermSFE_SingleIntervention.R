#' Simulate Design Scenarios for Long-Term Semi-Field Experiment Testing Single Intervention
#'
#' Generates a template data set for simulation-based power analysis under different experimental design scenarios.
#'
#' @param n.ch.per.trt Integer. Number of chambers per treatment group (e.g., 4 compartments per group).
#' @param exp.length Integer. Total duration of the experiment in days.
#' @param sampl.freq Character. Sampling frequency label: one of `"daily"`, `"weekly"`, `"biweekly"`, or `"monthly"`.
#'
#' @return A data frame representing the experimental design, including chamber identity, time points, and intvn-time interaction.
#' @export
#'
#' @examples
#' sim.scen.longsfe.sinint(
#' n.ch.per.trt = 4,
#' exp.length = 90,
#' sampl.freq = "weekly"
#' )
sim.scen.longsfe.sinint <- function(n.ch.per.trt, exp.length, sampl.freq = "weekly") {
  # Convert frequency label to interval in days
  sampling.interval <- switch(tolower(sampl.freq),
                              "daily" = 1,
                              "weekly" = 7,
                              "biweekly" = 14,
                              "monthly" = 30,
                              stop("Invalid sampl.freq. Choose 'daily', 'weekly', 'biweekly', or 'monthly'."))

  # Calculate number of sampling points
  n.time.points <- floor(exp.length / sampling.interval)

  # Generate time sequence scaled between 0 and 1
  time.seq <- (0:n.time.points) / n.time.points

  # Expand design grid
  dat <- expand.grid(
    replicates = 1:n.ch.per.trt,
    intvn = 0:1,
    time = time.seq
  )

  # Add derived variables
  dat$intvn.time <- dat$intvn * dat$time
  dat$chamber <- factor(paste(dat$intvn, dat$replicates, sep = "-"))
  dat$timef <- factor(dat$time)

  # Reorder columns
  dat <- dat[, c("replicates", "intvn", "timef", "time", "chamber", "intvn.time")]

  return(dat)
}

#' Simulate Mosquito Count Data for Long-Term Semi-Field Experiment (Single Intervention)
#'
#' Automates the simulation of mosquito count data under a semi-field experimental design testing a single intervention.
#'
#' @param n.ch.per.trt Integer. Number of chambers per treatment group.
#' @param exp.length Integer. Total duration of the experiment in days.
#' @param sampl.freq Character. Sampling frequency label: one of `"daily"`, `"weekly"`, `"biweekly"`, or `"monthly"`.
#' @param lambda Numeric. Expected number of mosquitoes sampled per chamber per time point in the absence of intervention.
#' @param intvn.effect Numeric. Proportion reduction in mosquito counts over time due to intervention (e.g., 0.8 for 80\% reduction).
#' @param chamber.var Numeric. Variance between chambers (random effect).
#' @param time.var Numeric. Variance between time points (random effect).
#' @param theta Numeric. Dispersion parameter for the negative binomial distribution.
#'
#' @return A data frame containing the original design plus simulated mosquito counts and linear predictors.
#'         Includes both fixed-effect and random-effect mosquito counts.
#' @examples
#' sim.mosq.longsfe.sinint(
#'   n.ch.per.trt = 4,
#'   exp.length = 90,
#'   sampl.freq = "weekly",
#'   lambda = 10,
#'   intvn.effect = 0.8,
#'   chamber.var = 0.1807,
#'   time.var = 0.2266,
#'   theta = 10
#' )
#' @importFrom stats rnbinom rnorm
#' @export
sim.mosq.longsfe.sinint <- function(n.ch.per.trt, exp.length, sampl.freq = "weekly",
                                    lambda, intvn.effect, chamber.var, time.var, theta) {

  # Generate design data
  dat <- sim.scen.longsfe.sinint(n.ch.per.trt, exp.length, sampl.freq)

  # Fixed effect coefficients
  b.0 <- log(lambda)
  b.intvn.const <- log(1)
  b.intvn.time <- log(1 - intvn.effect)

  # Random effects
  chamber.re <- rnorm(nlevels(dat$chamber), sd = sqrt(chamber.var))
  names(chamber.re) <- levels(dat$chamber)

  time.re <- rnorm(nlevels(dat$timef), sd = sqrt(time.var))
  names(time.re) <- levels(dat$timef)

  # Linear predictors
  dat$lin.pred.fixed <- b.0 + b.intvn.const * dat$intvn + b.intvn.time * dat$intvn.time
  dat$lin.pred.random <- dat$lin.pred.fixed +
    chamber.re[as.character(dat$chamber)] +
    time.re[as.character(dat$timef)]

  # Simulate mosquito counts
  # dat$mosquito.count.fixed <- exp(dat$lin.pred.fixed) #simulate from exp function rather than rnbinom
  dat$mosquito.count.fixed <- rnbinom(nrow(dat), mu = exp(dat$lin.pred.fixed), size = theta)
  dat$mosquito.count.random <- rnbinom(nrow(dat), mu = exp(dat$lin.pred.random), size = theta)

  return(dat)
}

#' Plot Mosquito Counts for Long-Term Semi-Field Experiment (Single Intervention)
#'
#' Generates a time series plot of mosquito counts based on either fixed effects only
#' or the full model including random effects, using data simulated by `sim.mosq.longsfe.sinint`.
#'
#' @param n.ch.per.trt Integer. Number of chambers per treatment group.
#' @param exp.length Integer. Total duration of the experiment in days.
#' @param sampl.freq Character. Sampling frequency label: one of `"daily"`, `"weekly"`, `"biweekly"`, or `"monthly"`.
#' @param lambda Numeric. Baseline mosquito count rate.
#' @param intvn.effect Numeric. Effect size of the intervention.
#' @param chamber.var Numeric. Variance component for chamber-level random effects.
#' @param time.var Numeric. Variance component for time-level random effects.
#' @param theta Numeric. Dispersion parameter for the negative binomial distribution.
#' @param use.random Logical. If `TRUE`, plots mosquito counts simulated with random effects;
#'        if `FALSE`, plots counts based on fixed effects only.
#'
#' @return A `ggplot` object showing mosquito counts over time by treatment group.
#'
#' @examples
#' sim.plot.longsfe.sinint(
#'   n.ch.per.trt = 4,
#'   exp.length = 90,
#'   sampl.freq = "weekly",
#'   lambda = 10,
#'   intvn.effect = 0.8,
#'   chamber.var = 0.1807,
#'   time.var = 0.2266,
#'   theta = 10,
#'   use.random = FALSE
#' )
#' @import dplyr
#' @import ggplot2
#' @export
sim.plot.longsfe.sinint <- function(n.ch.per.trt, exp.length, sampl.freq = "weekly",
                                    lambda, intvn.effect, chamber.var, time.var, theta,
                                    use.random = TRUE) {
  # Simulate data
  simdat <- sim.mosq.longsfe.sinint(n.ch.per.trt, exp.length, sampl.freq,
                                    lambda, intvn.effect, chamber.var, time.var, theta)

  # Add treatment labels
  simdat.treatment <- simdat %>%
    dplyr::mutate(Treatments = dplyr::case_when(
      intvn == 0 ~ "Control",
      intvn == 1 ~ "Intervention"
    ))

  # Choose count column
  count.col <- if (use.random) "mosquito.count.random" else "mosquito.count.fixed"

  # Map sampling frequency to interval and label
  freq.map <- list(
    daily = list(interval = 1, label = "Days"),
    weekly = list(interval = 7, label = "Weeks"),
    biweekly = list(interval = 14, label = "Biweekly"),
    monthly = list(interval = 30, label = "Months")
  )

  freq.key <- tolower(sampl.freq)
  if (!freq.key %in% names(freq.map)) {
    stop("Invalid sampl.freq. Choose 'daily', 'weekly', 'biweekly', or 'monthly'.")
  }

  sampling.interval <- freq.map[[freq.key]]$interval
  time.unit.label   <- freq.map[[freq.key]]$label
  time.values       <- simdat.treatment$time * (exp.length / sampling.interval)

  # Plot
  ggplot2::ggplot(simdat.treatment,
                  ggplot2::aes(x = time.values,
                               y = .data[[count.col]],
                               group = chamber,
                               col = Treatments)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(x = paste("Time (", time.unit.label, ")", sep = ""),
                  y = "Mosquito counts") +
    ggplot2::scale_color_manual(values = c("black", "red")) +
    ggplot2::theme_bw()
}

#' Extract p-value from GLMM for Simulated Mosquito Count Data
#'
#' Automates simulation and model comparison for long-term semi-field experiments testing a single intervention.
#' Fits two GLMMs (with and without interaction term) using either fixed-effect or random-effect mosquito counts.
#'
#' @param n.ch.per.trt Integer. Number of chambers per treatment group.
#' @param exp.length Integer. Total duration of the experiment in days.
#' @param sampl.freq Character. Sampling frequency label: one of `"daily"`, `"weekly"`, `"biweekly"`, or `"monthly"`.
#' @param lambda Numeric. Baseline mosquito count rate.
#' @param intvn.effect Numeric. Effect size of the intervention.
#' @param chamber.var Numeric. Variance component for chamber-level random effects.
#' @param time.var Numeric. Variance component for time-level random effects.
#' @param theta Numeric. Overdispersion parameter for the negative binomial model.
#' @param use.random Logical. If `TRUE`, uses mosquito counts simulated with random effects;
#'        if `FALSE`, uses counts based on fixed effects only.
#'
#' @return Named numeric vector containing the p-value from the likelihood ratio test.
#' @importFrom lme4 glmer.nb
#' @importFrom stats anova
#' @importFrom stats update
#' @export
#'
#' @examples
#' sim.pval.longsfe.sinint(n.ch.per.trt = 4, exp.length = 90, sampl.freq = "weekly",
#'                         lambda = 10, intvn.effect = 0.8, chamber.var = 0.2,
#'                         time.var = 0.1, theta = 10, use.random = TRUE)
sim.pval.longsfe.sinint <- function(n.ch.per.trt, exp.length, sampl.freq = "weekly",
                                    lambda, intvn.effect, chamber.var, time.var, theta,
                                    use.random = TRUE) {
  # Simulate data
  simdat <- sim.mosq.longsfe.sinint(n.ch.per.trt, exp.length, sampl.freq,
                                    lambda, intvn.effect, chamber.var, time.var, theta)

  # Choose response variable
  response.var <- if (use.random) "mosquito.count.random" else "mosquito.count.fixed"

  # Rename selected count column to 'mosquito.count' for modeling
  simdat$mosquito.count <- simdat[[response.var]]

  # Fit GLMMs
  model.intvn.time <- suppressMessages(suppressWarnings(lme4::glmer.nb(mosquito.count ~ intvn * time + (1 | chamber) + (1 | timef),
                                   data = simdat)))
  model.intvn <- suppressMessages(suppressWarnings(update(model.intvn.time, ~ . - intvn:time)))

  # Extract p-value from likelihood ratio test
  pvalue <- stats::anova(model.intvn.time, model.intvn)[2, "Pr(>Chisq)"]

  return(c(pvalue = pvalue))
}

#' Estimate Empirical Power for Long-Term Semi-Field Experiment Testing Single Intervention
#'
#' Runs repeated simulations and GLMM fits to estimate empirical power
#' as the proportion of simulations with p-values below 0.05.
#'
#' @param n.ch.per.trt Integer. Number of chambers per treatment group.
#' @param exp.length Integer. Total duration of the experiment in days.
#' @param sampl.freq Character. Sampling frequency label: one of `"daily"`, `"weekly"`, `"biweekly"`, or `"monthly"`.
#' @param lambda Numeric. Baseline mosquito count rate.
#' @param intvn.effect Numeric. Effect size of the intervention.
#' @param chamber.var Numeric. Variance component for chamber-level random effects.
#' @param time.var Numeric. Variance component for time-level random effects.
#' @param theta Numeric. Dispersion parameter for the negative binomial distribution.
#' @param use.random Logical. If `TRUE`, uses mosquito counts simulated with random effects;
#'        if `FALSE`, uses counts based on fixed effects only.
#' @param nsim Integer. Number of simulation replicates.
#' @param n.cores Integer. Number of cores to use for parallel processing.
#'
#' @return A named numeric vector:
#' \describe{
#'   \item{power}{Estimated empirical power (rounded to two decimal places)}
#'   \item{ci.lower}{Lower bound of 95\% confidence interval}
#'   \item{ci.upper}{Upper bound of 95\% confidence interval}
#' }
#'
#' @note Parallel execution is supported via `n.cores`, but examples default to `n.cores = 1` for reproducibility and package checks.
#'
#' @examples
#' sim.power.longsfe.sinint(
#'   n.ch.per.trt = 4,
#'   exp.length = 90,
#'   sampl.freq = "weekly",
#'   lambda = 10,
#'   intvn.effect = 0.8,
#'   chamber.var = 0.2,
#'   time.var = 0.1,
#'   theta = 10,
#'   nsim = 100,
#'   n.cores = 1,
#'   use.random = TRUE
#' )
#'
#' @importFrom parallel makeCluster parLapply stopCluster clusterExport detectCores
#' @importFrom stats binom.test
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export
sim.power.longsfe.sinint <- function(n.ch.per.trt, exp.length, sampl.freq,
                                     lambda, intvn.effect, chamber.var, time.var, theta,
                                     nsim = 100, n.cores = 1, use.random = TRUE) {

  # Simulation wrapper
  sim_wrapper <- function(i) {
    result <- tryCatch(
      sim.pval.longsfe.sinint(n.ch.per.trt, exp.length, sampl.freq,
                              lambda, intvn.effect, chamber.var, time.var, theta,
                              use.random = use.random),
      error = function(e) NA
    )
    result["pvalue"]
  }

  # Progress bar
  pb <- utils::txtProgressBar(min = 0, max = nsim, style = 3)

  # Run simulations
  if (n.cores > 1) {
    cl <- parallel::makeCluster(n.cores)
    on.exit(parallel::stopCluster(cl))
    parallel::clusterExport(cl, varlist = c("sim.pval.longsfe.sinint", "n.ch.per.trt", "exp.length",
                                            "sampl.freq", "lambda", "intvn.effect", "chamber.var",
                                            "time.var", "theta", "use.random"),
                            envir = environment())

    pvals <- vector("list", nsim)
    for (i in seq_len(nsim)) {
      pvals[[i]] <- parallel::parLapply(cl, i, sim_wrapper)[[1]]
      utils::setTxtProgressBar(pb, i)
    }
  } else {
    pvals <- vector("list", nsim)
    for (i in seq_len(nsim)) {
      pvals[[i]] <- sim_wrapper(i)
      utils::setTxtProgressBar(pb, i)
    }
  }

  close(pb)

  # Flatten and summarize
  pvals <- unlist(pvals)
  n.sig <- sum(pvals < 0.05, na.rm = TRUE)

  power.estimate <- c(
    power = round(n.sig / nsim, 2),
    stats::binom.test(x = n.sig, n = nsim)$conf.int
  )
  names(power.estimate)[2:3] <- c("ci.lower", "ci.upper")

  return(power.estimate)
}

