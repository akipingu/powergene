#' Simulate Design Scenarios for Long-Term Semi-Filed Experiment Testing Combined Interventions
#'
#' Constructs a data frame representing the factorial design of semi-field experiments
#' testing combined interventions. Each chamber is uniquely identified and assigned
#' treatments combinations, including interaction terms. The intervention here are shortly named as `intvn1` and `intvn2`.
#'
#' @param n.ch.per.trt An integer specifying the number of chambers allocated per treatment group.
#' @param exp.length An integer indicating total duration of the experiment in days, e.g., 90 days.
#' @param sampl.freq A character for a sampling frequency label. Example, for 90 days experiment; `"daily"` means 90 sampling points, `"weekly"` means 12 sampling points, `"biweekly"` means 6 sampling points, or `"monthly"` means 3 sampling points.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{replicates}{Replicate number within each treatment group (e.g., for a total of 2 chambers per treatment means 1, 2 replicates per treatment)}
#'   \item{intvn1}{Intervention status (e.g., 0 = control or no intervention 1 and 1 = there is intervention 1)}
#'   \item{intvn2}{Intervention status (e.g., 0 = control or no intervention 2 and 1 = there is intervention 2)}
#'   \item{chamber}{Unique chamber identifier as a factor (e.g., 0-0-1, 0-0-2 for the control chambers and 0-1-1, 1-0-1, 1-1-1 for intervention chambers)}
#'   \item{time}{Time sequence scaled between 0 and 1, it is calculate by the ratio of sampling points by their total number}
#'   \item{timef}{This is just the same as "time" but as a factor}
#'   \item{ixn.intvn1.time}{This is the interaction between an intvn1 and time, i.e., intvn1 x time}
#'   \item{ixn.intvn2.time}{This is the interaction between an intvn2 and time, i.e., intvn2 x time}
#'   \item{ixn.intvn1.intvn2.time}{This is the interaction between an intvn1, intvn2 and time, i.e., intvn1 x intvn2 x time}
#' }
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

  # Create chambers/replicates ID e.g., 1-1-2 means chamber number 2 with both intvn1 and intvn2
  dat$chamber <- factor(paste(dat$intvn1, dat$intvn2, dat$replicates, sep = "-"))

  # Time as factor
  dat$timef <- factor(dat$time)

  # Reorder columns
  dat <- dat[, c("replicates", "intvn1", "intvn2", "chamber", "time", "timef",
                  "ixn.intvn1.time", "ixn.intvn2.time", "ixn.intvn1.intvn2.time")]

  return(dat)
}

#' Simulate Mosquito Count Data for Long-Term Semi-Field Experiment Testing Single Intervention
#'
#' Generates simulated mosquito count data under a short-term semi-field experimental design
#' with fixed effects for interventions `intvn1` and `intvn2`, random effects for chambers and sampling time points variability, and a negative binomial distributed outcomes.
#' Uses output from `sim.scen.longsfe.comint()` to incorporate the table of experimental design scenarios.
#'
#' @param n.ch.per.trt An integer specifying the number of chambers allocated per treatment group.
#' @param exp.length An integer indicating total duration of the experiment in days, e.g., 90 days.
#' @param sampl.freq A character for a sampling frequency label. Example, for 90 days experiment; `"daily"` means 90 sampling points, `"weekly"` means 12 sampling points, `"biweekly"` means 6 sampling points, or `"monthly"` means 3 sampling points.
#' @param lambda A numeric value indicating the expected mean mosquito count in control chambers.
#' @param intvn1.effect A numeric value representing the proportional reduction in mosquito count due to the intervention 1 (e.g., ITN).
#' @param intvn2.effect A numeric value representing the proportional reduction in mosquito count due to the intervention 2 (PPFa).
#' @param ixn.effect A numeric value representing the proportional reduction in mosquito count due to the interaction between interventions (e.g., ITN x PPFa).
#' @param chamber.var A numeric value specifying the variance of random chamber-level effects.
#' @param time.var A numeric value indicating the variance between sampling time points (random effect).
#' @param theta A numeric value quantifying overdispersion parameter for the negative binomial distribution.
#' @param use.random A logical value indicating whether to return mosquito counts simulated through a sampling distribution (with random or fixed chamber effects) or all.
#' If \code{TRUE}, returns expected mosquito counts simulated through sampling distribution with random effects;
#' If \code{FALSE}, returns expected mosquito counts simulated through sampling distribution based on fixed effects only;
#' If \code{NULL}, returns expected mosquito counts simulated through exponential function based on fixed effects (no sampling distribution);
#' If \code{"ALL"}, returns the full data set with all mosquito count columns based in all three options described above.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{replicates}{Replicate number within each treatment group (e.g., for a total of 2 chambers per treatment means 1, 2 replicates per treatment)}
#'   \item{intvn1}{Intervention status (e.g., 0 = control or no intervention 1 and 1 = there is intervention 1)}
#'   \item{intvn2}{Intervention status (e.g., 0 = control or no intervention 2 and 1 = there is intervention 2)}
#'   \item{chamber}{Unique chamber identifier as a factor (e.g., 0-0-1, 0-0-2 for the control chambers and 0-1-1, 1-0-1, 1-1-1 for intervention chambers)}
#'   \item{time}{Time sequence scaled between 0 and 1, it is calculate by the ratio of sampling points by their total number}
#'   \item{timef}{This is just the same as "time" but as a factor}
#'   \item{ixn.intvn1.time}{This is the interaction between an intvn1 and time, i.e., intvn1 x time}
#'   \item{ixn.intvn2.time}{This is the interaction between an intvn2 and time, i.e., intvn2 x time}
#'   \item{ixn.intvn1.intvn2.time}{This is the interaction between an intvn1, intvn2 and time, i.e., intvn1 x intvn2 x time}
#'   \item{lin.pred.fixed}{Linear predictor with fixed effects only}
#'   \item{lin.pred.random}{Linear predictor with random chamber effects}
#'   \item{mosquito.count.fixed.exp}{Simulted mosquito counts through exponetial function from fixed effects only (no sampling)}
#'   \item{mosquito.count.fixed}{Simulated mosquito counts through sampling distribution based on fixed effects only}
#'   \item{mosquito.count.random}{Simulated mosquito counts through sampling distribution accounting for random effects}
#' }

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

#' Plot Mosquito Counts for Long-Term Semi-Field Experiment Testing Combined Interventions
#'
#' Generates a time series plot of mosquito counts based on either fixed effects only,
#' random effects, or fixed effects (no sampling distribution), using data simulated by `sim.mosq.longsfe.comint`.
#'
#' @param n.ch.per.trt An integer specifying the number of chambers allocated per treatment group.
#' @param exp.length An integer indicating total duration of the experiment in days, e.g., 90 days.
#' @param sampl.freq A character for a sampling frequency label. Example, for 90 days experiment; `"daily"` means 90 sampling points, `"weekly"` means 12 sampling points, `"biweekly"` means 6 sampling points, or `"monthly"` means 3 sampling points.
#' @param lambda A numeric value indicating the expected mean mosquito count in control chambers.
#' @param intvn1.effect A numeric value representing the proportional reduction in mosquito count due to the intervention 1 (e.g., ITN).
#' @param intvn2.effect A numeric value representing the proportional reduction in mosquito count due to the intervention 2 (PPFa).
#' @param ixn.effect A numeric value representing the proportional reduction in mosquito count due to the interaction between interventions (e.g., ITN x PPFa).
#' @param chamber.var A numeric value specifying the variance of random chamber-level effects.
#' @param time.var A numeric value indicating the variance between sampling time points (random effect).
#' @param theta A numeric value quantifying overdispersion parameter for the negative binomial distribution.
#' @param use.random A logical value indicating whether to return mosquito counts simulated through a sampling distribution (with random or fixed chamber effects) or all.
#' If \code{TRUE}, returns expected mosquito counts simulated through sampling distribution with random effects;
#' If \code{FALSE}, returns expected mosquito counts simulated through sampling distribution based on fixed effects only;
#' If \code{NULL}, returns expected mosquito counts simulated through exponential function based on fixed effects (no sampling distribution);
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

#' Extract p-value from Simulated GLMM for Long-Term Semi-Field Experiment Testing Combined Interventions
#'
#' Returns the p-value by fitting a negative binomial GLMM to simulated mosquito count data.
#' Uses simulated mosquito counts data from `sim.mosq.longsfe.comint()` and fits a negative binomial GLMM to extract the p-value associated with the intvn effect.
#'
#' @param n.ch.per.trt An integer specifying the number of chambers allocated per treatment group.
#' @param exp.length An integer indicating total duration of the experiment in days, e.g., 90 days.
#' @param sampl.freq A character for a sampling frequency label. Example, for 90 days experiment; `"daily"` means 90 sampling points, `"weekly"` means 12 sampling points, `"biweekly"` means 6 sampling points, or `"monthly"` means 3 sampling points.
#' @param lambda A numeric value indicating the expected mean mosquito count in control chambers.
#' @param intvn1.effect A numeric value representing the proportional reduction in mosquito count due to the intervention 1 (e.g., ITN).
#' @param intvn2.effect A numeric value representing the proportional reduction in mosquito count due to the intervention 2 (PPFa).
#' @param ixn.effect A numeric value representing the proportional reduction in mosquito count due to the interaction between interventions (e.g., ITN x PPFa).
#' @param chamber.var A numeric value specifying the variance of random chamber-level effects.
#' @param time.var A numeric value indicating the variance between sampling time points (random effect).
#' @param theta A numeric value quantifying overdispersion parameter for the negative binomial distribution.
#' @param use.random A logical value indicating whether to return mosquito counts simulated through a sampling distribution (with random or fixed chamber effects) or all.
#' If \code{TRUE}, returns expected mosquito counts simulated through sampling distribution with random effects;
#' If \code{FALSE}, returns expected mosquito counts simulated through sampling distribution based on fixed effects only;
#'
#' @return A named numeric vector:
#' \describe{
#'   \item{pvalue}{A p-value from likelihood ratio test comparing models with and without the interaction term}
#' }
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

#' Estimate Empirical Power for Long-Term Semi-Field Experiment Testing Combined Interventions
#'
#' Runs repeated simulations and negative binomial GLMM fits to estimate empirical power
#' as the proportion of simulations with p-values below 0.05. These p-values are generated using the function called `sim.pval.longsfe.comint()`.
#'
#' @param n.ch.per.trt An integer specifying the number of chambers allocated per treatment group.
#' @param exp.length An integer indicating total duration of the experiment in days, e.g., 90 days.
#' @param sampl.freq A character for a sampling frequency label. Example, for 90 days experiment; `"daily"` means 90 sampling points, `"weekly"` means 12 sampling points, `"biweekly"` means 6 sampling points, or `"monthly"` means 3 sampling points.
#' @param lambda A numeric value indicating the expected mean mosquito count in control chambers.
#' @param intvn1.effect A numeric value representing the proportional reduction in mosquito count due to the intervention 1 (e.g., ITN).
#' @param intvn2.effect A numeric value representing the proportional reduction in mosquito count due to the intervention 2 (PPFa).
#' @param ixn.effect A numeric value representing the proportional reduction in mosquito count due to the interaction between interventions (e.g., ITN x PPFa).
#' @param chamber.var A numeric value specifying the variance of random chamber-level effects.
#' @param time.var A numeric value indicating the variance between sampling time points (random effect).
#' @param theta A numeric value quantifying overdispersion parameter for the negative binomial distribution.
#' @param nsim An integer indicating the total number of simulations.
#' @param n.cores An integer the number of cores to use for parallel processing.
#' @param use.random A logical value indicating whether to plot mosquito counts simulated using a sampling distribution (with random or fixed effects).
#' If \code{TRUE}, returns power for expected mosquito counts simulated using sampling distribution with random effects;
#' If \code{FALSE}, returns power for expected counts using sampling distribution based on fixed effects only.
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
