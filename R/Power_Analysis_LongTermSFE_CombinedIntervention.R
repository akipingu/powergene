#' Simulate Experimental Design Template with Dual Interventions (ITN and PPFa)
#'
#' Generates a structured dataset representing experimental design scenarios
#' for mosquito intervention studies involving two binary treatments: ITN and PPFa.
#' Sampling frequency is converted to a time interval, and time is scaled between 0 and 1.
#'
#' @param n.ch.per.trt Integer. Number of chambers per treatment group.
#' @param exp.length Integer. Total experiment duration in days.
#' @param sampl.freq Character. Sampling frequency label: one of `"daily"`, `"weekly"`, `"biweekly"`, or `"monthly"`.
#'
#' @return A \code{data.frame} with columns for replicate ID, intervention levels, time (numeric and factor),
#' chamber ID, and interaction terms.
#'
#' @examples
#' sim.scen.longsfe.comint(
#'   n.ch.per.trt = 4,
#'   exp.length = 90,
#'   sampl.freq = "weekly"
#' )
#'
#' @export
sim.scen.longsfe.comint <- function(n.ch.per.trt, exp.length, sampl.freq = "weekly") {
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

  # Fixed intervention levels
  intvn1.lev <- 0:1  # intvn1: 0 = absent, 1 = present
  intvn2.lev <- 0:1  # intvn2: 0 = absent, 1 = present

  # make a template data set representing the trial design
  dat <- expand.grid(
    replicates = 1:n.ch.per.trt,
    intvn1 = intvn1.lev,
    intvn2 = intvn2.lev,
    time = time.seq
  )

  # Add derived interaction terms
  dat$ixn.intvn1.intvn2.time <- dat$intvn1 * dat$intvn2 * dat$time # interaction between intvn1, intvn2 and time
  dat$ixn.intvn1.time <- dat$intvn1 * dat$time # interaction between intvn1 and time
  dat$ixn.intvn2.time <- dat$intvn2 * dat$time # interaction between intvn2 and time

  # create chambers/replicates ID e.g., 1-1-2 means chamber number 2 with both intvn1 and intvn2
  dat$chamber <- factor(paste(dat$intvn1, dat$intvn2, dat$replicates, sep = "-"))

  # Time as factor
  dat$timef <- factor(dat$time)

  # Reorder columns
  dat <- dat[, c("replicates", "intvn1", "intvn2", "timef", "time", "chamber",
                 "ixn.intvn1.intvn2.time", "ixn.intvn1.time", "ixn.intvn2.time")]

  return(dat)
}

#' Simulate Mosquito Counts with Time-Varying Effects of Two Interventions
#'
#' Simulates mosquito count data under a factorial design with two interventions—
#' \code{intvn1} (e.g., PPFa) and \code{intvn2} (e.g., ITN)—whose effects vary over time.
#' The simulation incorporates fixed effects (derived from user-defined parameters), random effects,
#' and interaction terms using a negative binomial distribution with a log-link function.
#'
#' Intervention effects should be specified as proportions of reduction (e.g., 0.8 means 80% reduction),
#' and are internally transformed using \code{log(1 - intvn.effect)} to ensure positivity of the mean.
#'
#' @param n.ch.per.trt Integer. Number of chambers per treatment group.
#' @param exp.length Integer. Total experiment duration in days.
#' @param sampl.freq Character. Sampling frequency label: one of `"daily"`, `"weekly"`, `"biweekly"`, or `"monthly"`.
#' @param lambda Numeric. Mean mosquito count in the control group.
#' @param intvn1.effect Numeric. Proportion reduction due to intvn1 over time.
#' @param intvn2.effect Numeric. Proportion reduction due to intvn2 over time.
#' @param ixn.effect Numeric. Proportion reduction due to intvn1 × intvn2 interaction over time.
#' @param chamber.var Numeric. Variance between chambers (random effect).
#' @param time.var Numeric. Variance across time points (random effect).
#' @param theta Numeric. Dispersion parameter for negative binomial distribution.
#' @param use.random Logical, NULL, or "ALL".
#' If \code{TRUE}, returns mosquito counts with random effects.
#' If \code{FALSE}, returns counts with fixed effects only.
#' If \code{NULL}, returns expected counts from fixed effects (no sampling).
#' If \code{"ALL"}, returns the full dataset with all mosquito count columns.
#'
#' @return A data frame containing the original design plus simulated mosquito counts and linear predictors.
#'         Output depends on the value of \code{use.random}.
#'
#' @examples
#' sim.mosq.longsfe.comint(
#'   n.ch.per.trt = 4,
#'   exp.length = 90,
#'   sampl.freq = "weekly",
#'   lambda = 10,
#'   intvn1.effect = 0.7,
#'   intvn2.effect = 0.8,
#'   ixn.effect = 0.8,
#'   chamber.var = 0.1807,
#'   time.var = 0.2266,
#'   theta = 10,
#'   use.random = "ALL"
#' )
#'
#' @export
sim.mosq.longsfe.comint <- function(n.ch.per.trt,
                                    exp.length,
                                    sampl.freq = "weekly",
                                    lambda,
                                    intvn1.effect,
                                    intvn2.effect,
                                    ixn.effect,
                                    chamber.var,
                                    time.var,
                                    theta,
                                    use.random = "ALL") {

  # Define log-link coefficients
  b.0 <- log(lambda)
  b.intvn1.time <- log(1 - intvn1.effect)
  b.intvn2.time <- log(1 - intvn2.effect)
  b.intvn1.intvn2.time <- log(1 - ixn.effect)

  # Generate design table
  dat <- sim.scen.longsfe.comint(n.ch.per.trt, exp.length, sampl.freq)

  # Simulate random effects
  chamber.re <- rnorm(nlevels(dat$chamber), sd = sqrt(chamber.var))
  names(chamber.re) <- levels(dat$chamber)

  time.re <- rnorm(nlevels(dat$timef), sd = sqrt(time.var))
  names(time.re) <- levels(dat$timef)

  # Fixed effects linear predictor
  dat$lin.pred.fixed <- b.0 +
    b.intvn1.time * dat$ixn.intvn1.time +
    b.intvn2.time * dat$ixn.intvn2.time +
    b.intvn1.intvn2.time * dat$ixn.intvn1.intvn2.time

  # Add random effects
  dat$lin.pred.random <- dat$lin.pred.fixed +
    chamber.re[as.character(dat$chamber)] +
    time.re[as.character(dat$timef)]

  # Simulate mosquito counts
  dat$mosquito.count.fixed.exp <- exp(dat$lin.pred.fixed)
  dat$mosquito.count.fixed <- rnbinom(nrow(dat), mu = exp(dat$lin.pred.fixed), size = theta)
  dat$mosquito.count.random <- rnbinom(nrow(dat), mu = exp(dat$lin.pred.random), size = theta)

  # Return based on use.random
  if (isTRUE(use.random)) {
    return(dat[, c(names(dat)[1:which(names(dat) == "lin.pred.random")], "mosquito.count.random"), drop = FALSE])
  } else if (isFALSE(use.random)) {
    return(dat[, c(names(dat)[1:which(names(dat) == "lin.pred.random")], "mosquito.count.fixed"), drop = FALSE])
  } else if (is.null(use.random)) {
    return(dat[, c(names(dat)[1:which(names(dat) == "lin.pred.random")], "mosquito.count.fixed.exp"), drop = FALSE])
  } else if (identical(use.random, "ALL")) {
    return(dat)
  } else {
    warning("Invalid use.random value. Returning full dataset.")
    return(dat)
  }
}

#' Plot Mosquito Counts for Long-Term Semi-Field Experiment (Two Interventions)
#'
#' Generates a time series plot of mosquito counts based on either fixed effects only,
#' random effects, or expected counts, using data simulated by `sim.mosq.longsfe.comint`.
#'
#' @param n.ch.per.trt Integer. Number of chambers per treatment group.
#' @param exp.length Integer. Total duration of the experiment in days.
#' @param sampl.freq Character. Sampling frequency label: one of `"daily"`, `"weekly"`, `"biweekly"`, or `"monthly"`.
#' @param lambda Numeric. Baseline mosquito count rate.
#' @param intvn1.effect Numeric. Proportion reduction due to intervention 1.
#' @param intvn2.effect Numeric. Proportion reduction due to intervention 2.
#' @param ixn.effect Numeric. Proportion reduction due to interaction between interventions.
#' @param chamber.var Numeric. Variance component for chamber-level random effects.
#' @param time.var Numeric. Variance component for time-level random effects.
#' @param theta Numeric. Dispersion parameter for the negative binomial distribution.
#' @param use.random Logical or NULL.
#' If \code{TRUE}, plots mosquito counts simulated with random effects.
#' If \code{FALSE}, plots counts based on fixed effects only.
#' If \code{NULL}, plots expected counts from fixed effects (no sampling).
#'
#' @return A `ggplot` object showing mosquito counts over time by treatment group.
#'
#' @examples
#' sim.plot.longsfe.comint(
#'   n.ch.per.trt = 4,
#'   exp.length = 90,
#'   sampl.freq = "weekly",
#'   lambda = 10,
#'   intvn1.effect = 0.7,
#'   intvn2.effect = 0.8,
#'   ixn.effect = 0.8,
#'   chamber.var = 0.1807,
#'   time.var = 0.2266,
#'   theta = 10,
#'   use.random = TRUE
#' )
#'
#' @import dplyr
#' @import ggplot2
#' @export
sim.plot.longsfe.comint <- function(n.ch.per.trt, exp.length, sampl.freq = "weekly", lambda, intvn1.effect,
                                    intvn2.effect, ixn.effect, chamber.var, time.var, theta, use.random = TRUE) {
  # Simulate full data set
  simdat <- sim.mosq.longsfe.comint(n.ch.per.trt, exp.length, sampl.freq, lambda, intvn1.effect,
                                    intvn2.effect, ixn.effect, chamber.var, time.var, theta, use.random)

  # Validate use.random for plotting
  if (!isTRUE(use.random) && !isFALSE(use.random) && !is.null(use.random)) {
    stop("Invalid use.random value for plotting. Please use TRUE, FALSE, or NULL.")
  }

  # Add treatment labels
  simdat.treatment <- simdat %>%
    dplyr::mutate(Treatments = dplyr::case_when(
      intvn1 == 0 & intvn2 == 0 ~ "Control",
      intvn1 == 1 & intvn2 == 0 ~ "Intervention 1",
      intvn1 == 0 & intvn2 == 1 ~ "Intervention 2",
      intvn1 == 1 & intvn2 == 1 ~ "Interaction"
    ))

  # Arrange legend order
  simdat.treatment$Treatments <- factor(simdat.treatment$Treatments,
                                        levels = c("Control", "Intervention 1", "Intervention 2", "Interaction"))

  # Choose count column
  count.col <- if (isTRUE(use.random)) {
    "mosquito.count.random"
  } else if (isFALSE(use.random)) {
    "mosquito.count.fixed"
  } else {
    "mosquito.count.fixed.exp"
  }

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
                  y = "Mosquito counts",
                  color = "Treatment") +
    ggplot2::scale_color_manual(values = c("black", "blue", "purple", "red")) +
    ggplot2::theme_bw()
}

#' Extract p-value from GLMM for Simulated Mosquito Count Data (Two Interventions)
#'
#' Automates simulation and model comparison for long-term semi-field experiments testing two interventions.
#' Fits two GLMMs (with and without the three-way interaction term) using either fixed-effect or random-effect mosquito counts.
#'
#' @param n.ch.per.trt Integer. Number of chambers per treatment group.
#' @param exp.length Integer. Total duration of the experiment in days.
#' @param sampl.freq Character. Sampling frequency label: one of `"daily"`, `"weekly"`, `"biweekly"`, or `"monthly"`.
#' @param lambda Numeric. Baseline mosquito count rate.
#' @param intvn1.effect Numeric. Proportion reduction due to intervention 1.
#' @param intvn2.effect Numeric. Proportion reduction due to intervention 2.
#' @param ixn.effect Numeric. Proportion reduction due to interaction between interventions.
#' @param chamber.var Numeric. Variance component for chamber-level random effects.
#' @param time.var Numeric. Variance component for time-level random effects.
#' @param theta Numeric. Overdispersion parameter for the negative binomial model.
#' @param use.random Logical.
#' If \code{TRUE}, uses mosquito counts simulated with random effects;
#' If \code{FALSE}, uses counts based on fixed effects only.
#'
#' @return Named numeric vector containing the p-value from the likelihood ratio test.
#' @importFrom lme4 glmer.nb
#' @importFrom stats anova
#' @importFrom stats update
#' @export
#'
#' @examples
#' sim.pval.longsfe.comint(
#'   n.ch.per.trt = 4,
#'   exp.length = 90,
#'   sampl.freq = "weekly",
#'   lambda = 10,
#'   intvn1.effect = 0.7,
#'   intvn2.effect = 0.8,
#'   ixn.effect = 0.8,
#'   chamber.var = 0.1807,
#'   time.var = 0.2266,
#'   theta = 10,
#'   use.random = TRUE
#' )
sim.pval.longsfe.comint <- function(n.ch.per.trt, exp.length, sampl.freq = "weekly",
                                    lambda, intvn1.effect, intvn2.effect, ixn.effect,
                                    chamber.var, time.var, theta, use.random = TRUE) {

  # Validate use.random
  if (!isTRUE(use.random) && !isFALSE(use.random)) {
    stop("Invalid use.random value. Please use TRUE or FALSE.")
  }

  # Simulate full dataset
  simdat <- sim.mosq.longsfe.comint(n.ch.per.trt, exp.length, sampl.freq,
                                    lambda, intvn1.effect, intvn2.effect, ixn.effect,
                                    chamber.var, time.var, theta, use.random = "ALL")

  # Choose response variable
  response.var <- if (isTRUE(use.random)) {
    "mosquito.count.random"
  } else {
    "mosquito.count.fixed"
  }

  # Rename selected count column to 'mosquito.count' for modeling
  simdat$mosquito.count <- simdat[[response.var]]

  # Fit GLMMs
  model.full <- suppressMessages(suppressWarnings(
    lme4::glmer.nb(mosquito.count ~ intvn1 * intvn2 * time + (1 | chamber) + (1 | timef),
                   data = simdat))
  )

  model.reduced <- suppressMessages(suppressWarnings(
    update(model.full, ~ . - intvn1:intvn2:time))
  )

  # Extract p-value from likelihood ratio test
  pvalue <- stats::anova(model.full, model.reduced)[2, "Pr(>Chisq)"]

  return(c(pvalue = pvalue))
}

#' Estimate Empirical Power for Long-Term Semi-Field Experiment Testing Two Interventions
#'
#' Runs repeated simulations and GLMM fits to estimate empirical power
#' as the proportion of simulations with p-values below 0.05.
#'
#' @param n.ch.per.trt Integer. Number of chambers per treatment group.
#' @param exp.length Integer. Total duration of the experiment in days.
#' @param sampl.freq Character. Sampling frequency label: one of `"daily"`, `"weekly"`, `"biweekly"`, or `"monthly"`.
#' @param lambda Numeric. Baseline mosquito count rate.
#' @param intvn1.effect Numeric. Proportion reduction due to intervention 1.
#' @param intvn2.effect Numeric. Proportion reduction due to intervention 2.
#' @param ixn.effect Numeric. Proportion reduction due to interaction between interventions.
#' @param chamber.var Numeric. Variance component for chamber-level random effects.
#' @param time.var Numeric. Variance component for time-level random effects.
#' @param theta Numeric. Dispersion parameter for the negative binomial distribution.
#' @param nsim Integer. Number of simulation replicates.
#' @param n.cores Integer. Number of cores to use for parallel processing.
#' @param use.random Logical.
#' If \code{TRUE}, uses mosquito counts simulated with random effects;
#' If \code{FALSE}, uses counts based on fixed effects only.
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
#' sim.power.longsfe.comint(
#'   n.ch.per.trt = 4,
#'   exp.length = 90,
#'   sampl.freq = "weekly",
#'   lambda = 10,
#'   intvn1.effect = 0.7,
#'   intvn2.effect = 0.8,
#'   ixn.effect = 0.8,
#'   chamber.var = 0.1807,
#'   time.var = 0.2266,
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
sim.power.longsfe.comint <- function(n.ch.per.trt, exp.length, sampl.freq,
                                     lambda, intvn1.effect, intvn2.effect, ixn.effect,
                                     chamber.var, time.var, theta,
                                     nsim = 100, n.cores = 1, use.random = TRUE) {

  # Validate use.random
  if (!isTRUE(use.random) && !isFALSE(use.random)) {
    stop("Invalid use.random value. Please use TRUE or FALSE.")
  }

  # Simulation wrapper
  sim_wrapper <- function(i) {
    result <- tryCatch(
      sim.pval.longsfe.comint(n.ch.per.trt, exp.length, sampl.freq,
                              lambda, intvn1.effect, intvn2.effect, ixn.effect,
                              chamber.var, time.var, theta,
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
    parallel::clusterExport(cl, varlist = c("sim.pval.longsfe.comint", "n.ch.per.trt", "exp.length",
                                            "sampl.freq", "lambda", "intvn1.effect", "intvn2.effect",
                                            "ixn.effect", "chamber.var", "time.var", "theta", "use.random"),
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
