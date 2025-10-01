#' Simulate Design Scenarios for Short-Term Semi-Field Experiment Testing Combined Interventions
#'
#' Constructs a data frame representing the factorial design of semi-field experiments
#' testing combined interventions. Each chamber is uniquely identified and assigned
#' treatments combinations, including interaction terms. The intervention here are shortly named as `intvn1` and `intvn2`.
#'
#' @param n.ch.per.trt An integer specifying the number of chambers allocated per treatment group.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{replicates}{Replicate number within each treatment group (e.g., for a total of 2 chambers per treatment means 1, 2 replicates per treatment)}
#'   \item{intvn1}{Intervention status (e.g., 0 = control or no intervention 1 and 1 = there is intervention 1)}
#'   \item{intvn2}{Intervention status (e.g., 0 = control or no intervention 2 and 1 = there is intervention 2)}
#'   \item{ixn}{Interventions interactions status (e.g., 0 = control or no intervention and 1 = there is interaction)}
#'   \item{chamber}{Unique chamber identifier as a factor (e.g., 0-0-1, 0-0-2 for the control chambers and 0-1-1, 1-0-1, 1-1-1 for intervention chambers)}
#' }
#' @examples
#' sim.scen.shortsfe.comint(n.ch.per.trt = 4)
#'
#' @export
sim.scen.shortsfe.comint <- function(n.ch.per.trt) {
# Expand factorial design
  dat <- expand.grid(replicates = 1:n.ch.per.trt,
                     intvn1 = 0:1,
                     intvn2 = 0:1)

  # Create unique chamber ID
  dat$chamber <- factor(paste(dat$intvn1, dat$intvn2, dat$replicates, sep = "-"))

  # Define interaction term
  dat$ixn <- dat$intvn1 * dat$intvn2

  # Reorder columns for clarity
  dat <- dat[, c("replicates", "intvn1", "intvn2", "ixn", "chamber")]

  return(dat)
}

#' Simulate Mosquito Count Data for Short-Term Semi-Field Experiment Testing Combined Interventions
#'
#' Generates simulated mosquito count data under a short-term semi-field experimental design
#' with fixed effects for interventions, `intvn1` and `intvn2`, random effects for chamber variability, and Poisson-distributed outcomes.
#' Uses output from `sim.scen.shortsfe.comint()` to incorporate the table of experimental design scenarios.
#'
#' @param n.ch.per.trt An integer specifying the number of chambers allocated per treatment group.
#' @param lambda A numeric value indicating the expected mean mosquito count in control chambers.
#' @param intvn1.effect A numeric value representing the proportional reduction in mosquito count due to the intervention 1 (e.g., ITN).
#' @param intvn2.effect A numeric value representing the proportional reduction in mosquito count due to the intervention 2 (e.g., PPFa).
#' @param ixn.effect A numeric value representing the proportional reduction in mosquito count due to the interaction (e.g., ITN x PPFa).
#' @param chamber.var A numeric value specifying the variance of random chamber-level effects.
#' @param use.random A logical value indicating whether to return mosquito counts simulated through a sampling distribution (with random or fixed chamber effects) or all.
#' If \code{TRUE}, returns expected mosquito counts simulated through sampling distribution with random effects;
#' If \code{FALSE}, returns expected mosquito counts simulated through sampling distribution based on fixed effects only;
#' If \code{NULL}, returns expected mosquito counts simulated through exponential function based on fixed effects (no sampling distribution);
#' If \code{"ALL"}, returns the full data set with all mosquito count columns based in all three options described above.
#'
#' @return A data frame with the following columns, depending on the \code{use.random} option:
#' \describe{
#'   \item{replicates}{Replicate number within each treatment group (e.g., for a total of 2 chambers per treatment means 1, 2 replicates per treatment)}
#'   \item{intvn1}{Intervention status (e.g., 0 = control or no intervention 1 and 1 = there is intervention 1)}
#'   \item{intvn2}{Intervention status (e.g., 0 = control or no intervention 2 and 1 = there is intervention 2)}
#'   \item{ixn}{Interventions interactions status (e.g., 0 = control or no intervention and 1 = there is interaction)}
#'   \item{chamber}{Unique chamber identifier as a factor (e.g., 0-0-1, 0-0-2 for the control chambers and 0-1-1, 1-0-1, 1-1-1 for intervention chambers)}
#'   \item{lin.pred.fixed}{Linear predictor with fixed effects only}
#'   \item{lin.pred.random}{Linear predictor with random chamber effects}
#'   \item{mosquito.count.fixed.exp}{Simulted mosquito counts through exponetial function from fixed effects only (no sampling)}
#'   \item{mosquito.count.fixed}{Simulated mosquito counts through sampling distribution based on fixed effects only}
#'   \item{mosquito.count.random}{Simulated mosquito counts through sampling distribution accounting for random effects}
#' }
#'
#' @examples
#' sim.mosq.shortsfe.comint(
#'   n.ch.per.trt = 4,
#'   lambda = 50,
#'   intvn1.effect = 0.7,
#'   intvn2.effect = 0.8,
#'   ixn.effect = 0.5,
#'   chamber.var = 0.1807,
#'   use.random = "ALL"
#' )
#'
#' @importFrom stats rnorm rpois
#' @export
sim.mosq.shortsfe.comint <- function(n.ch.per.trt, lambda, intvn1.effect, intvn2.effect,
                                     ixn.effect, chamber.var, use.random = "ALL") {

  # Generate design data
  dat <- sim.scen.shortsfe.comint(n.ch.per.trt)

  # Convert intervention effects to log-scale coefficients
  b.0 <- log(lambda)
  b.intvn1 <- log(1 - intvn1.effect)
  b.intvn2 <- log(1 - intvn2.effect)
  b.ixn <- log(1 - ixn.effect)

  # Simulate random chamber effects
  chamber.re <- rnorm(nlevels(dat$chamber), sd = sqrt(chamber.var))
  names(chamber.re) <- levels(dat$chamber)

  # Compute linear predictors
  dat$lin.pred.fixed <- b.0 + b.intvn1 * dat$intvn1 + b.intvn2 * dat$intvn2 + b.ixn * dat$ixn
  dat$lin.pred.random <- dat$lin.pred.fixed + chamber.re[as.character(dat$chamber)]

  # Simulate mosquito counts
  dat$mosquito.count.fixed.exp <- exp(dat$lin.pred.fixed)
  dat$mosquito.count.fixed <- rpois(nrow(dat), exp(dat$lin.pred.fixed))
  dat$mosquito.count.random <- rpois(nrow(dat), exp(dat$lin.pred.random))

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

#' Plot Mosquito Counts for Short-Term Semi-Field Experiment Testing Combined Interventions
#'
#' Generates a boxplot of simulated mosquito counts grouped by intervention combinations.
#' Uses output from `sim.mosq.shortsfe.comint()` and overlays jittered points to show chamber-level variation.
#'
#' @param n.ch.per.trt An integer specifying the number of chambers allocated per treatment group.
#' @param lambda A numeric value indicating the expected mean mosquito count in control chambers.
#' @param intvn1.effect A numeric value representing the proportional reduction in mosquito count due to the intervention 1 (e.g., ITN).
#' @param intvn2.effect A numeric value representing the proportional reduction in mosquito count due to the intervention 2 (e.g., PPFa).
#' @param ixn.effect A numeric value representing the proportional reduction in mosquito count due to the interaction (e.g., ITN x PPFa).
#' @param chamber.var A numeric value specifying the variance of random chamber-level effects.
#' @param use.random A logical value indicating whether to return mosquito counts simulated through a sampling distribution (with random or fixed chamber effects) or all.
#' If \code{TRUE}, returns expected mosquito counts simulated through sampling distribution with random effects;
#' If \code{FALSE}, returns expected mosquito counts simulated through sampling distribution based on fixed effects only;
#' If \code{NULL}, returns expected mosquito counts simulated through exponential function based on fixed effects (no sampling distribution);
#' @param jitter A logical value indicating whether to overlay individual chamber-level points
#' If \code{TRUE}, overlays individual chamber-level points.
#' If \code{FALSE}, do not overlay individual chamber-level points.
#'
#' @return A `ggplot` object showing expected mosquito counts by treatment group.
#'
#' @examples
#' sim.plot.shortsfe.comint(
#'   n.ch.per.trt = 4,
#'   lambda = 50,
#'   intvn1.effect = 0.7,
#'   intvn2.effect = 0.8,
#'   ixn.effect = 0.5,
#'   chamber.var = 0.1807,
#'   use.random = TRUE,
#'   jitter = TRUE
#' )
#'
#' @import ggplot2
#' @importFrom dplyr mutate case_when
#' @export
sim.plot.shortsfe.comint <- function(n.ch.per.trt, lambda, intvn1.effect, intvn2.effect,
                                     ixn.effect, chamber.var, use.random = TRUE, jitter = TRUE) {

  # Validate use.random
  if (!isTRUE(use.random) && !isFALSE(use.random) && !is.null(use.random)) {
    stop("Invalid use.random value. Please use TRUE, FALSE, or NULL.")
  }

  # Simulate full dataset
  dat <- sim.mosq.shortsfe.comint(n.ch.per.trt, lambda, intvn1.effect, intvn2.effect,
                                  ixn.effect, chamber.var, use.random = "ALL")

  # Assign readable Treatments labels and enforce order
  dat <- dplyr::mutate(dat,
                       Treatments = dplyr::case_when(
                         intvn1 == 0 & intvn2 == 0 ~ "Control",
                         intvn1 == 1 & intvn2 == 0 ~ "Intervention 1",
                         intvn1 == 0 & intvn2 == 1 ~ "Intervention 2",
                         intvn1 == 1 & intvn2 == 1 ~ "Interaction"
                       ))

  dat$Treatments <- factor(dat$Treatments,
                          levels = c("Control", "Intervention 1", "Intervention 2", "Interaction"))

  # Select mosquito count column
  count.col <- if (isTRUE(use.random)) {
    "mosquito.count.random"
  } else if (isFALSE(use.random)) {
    "mosquito.count.fixed"
  } else {
    "mosquito.count.fixed.exp"
  }

  # Create boxplot
  p <- ggplot2::ggplot(dat, ggplot2::aes(x = Treatments, y = .data[[count.col]])) +
    ggplot2::geom_boxplot(outlier.shape = NA, fill = "lightblue") +
    ggplot2::labs(x = "Treatments",
                  y = "Mosquito counts",
                  title = "") +
    ggplot2::theme_bw()

  # Add jittered points if requested
  if (jitter) {
    p <- p + ggplot2::geom_jitter(width = 0.2, alpha = 0.6, color = "grey")
  }

  return(p)
}

#' Extract p-value from Simulated GLMM for Short-Term Semi-Field Experiment Testing Combined Interventions
#'
#' Returns the p-value by fitting a Poisson GLMM to simulated mosquito count data.
#' Uses simulated mosquito counts data from `sim.mosq.shortsfe.comint()` and fits a Poisson GLMM to extract the p-value associated with the intvn effect.
#'
#' @param n.ch.per.trt An integer specifying the number of chambers allocated per treatment group.
#' @param lambda A numeric value indicating the expected mean mosquito count in control chambers.
#' @param intvn1.effect A numeric value representing the proportional reduction in mosquito count due to the intervention 1 (e.g., ITN).
#' @param intvn2.effect A numeric value representing the proportional reduction in mosquito count due to the intervention 2 (e.g., PPFa).
#' @param ixn.effect A numeric value representing the proportional reduction in mosquito count due to the interaction (e.g., ITN x PPFa).
#' @param chamber.var A numeric value specifying the variance of random chamber-level effects.
#' @param use.random A logical value indicating whether to return mosquito counts simulated through a sampling distribution (with random or fixed chamber effects) or all.
#' If \code{TRUE}, returns expected mosquito counts simulated through sampling distribution with random effects;
#' If \code{FALSE}, returns expected mosquito counts simulated through sampling distribution based on fixed effects only;
#'
#' @return A named numeric vector:
#' \describe{
#'   \item{pvalue}{A p-value from likelihood ratio test comparing models with and without the interaction term}
#' }
#'
#' @examples
#' sim.pval.shortsfe.comint(
#'   n.ch.per.trt = 4,
#'   lambda = 50,
#'   intvn1.effect = 0.7,
#'   intvn2.effect = 0.8,
#'   ixn.effect = 0.5,
#'   chamber.var = 0.1807,
#'   use.random = TRUE
#' )
#'
#' @importFrom lme4 glmer
#' @importFrom stats anova
#' @export
sim.pval.shortsfe.comint <- function(n.ch.per.trt, lambda, intvn1.effect, intvn2.effect,
                                     ixn.effect, chamber.var, use.random = TRUE) {

  # Validate use.random
  if (!isTRUE(use.random) && !isFALSE(use.random)) {
    stop("Invalid use.random value. Please use TRUE or FALSE.")
  }

  # Simulate full dataset
  simdat2 <- sim.mosq.shortsfe.comint(n.ch.per.trt, lambda, intvn1.effect, intvn2.effect,
                                      ixn.effect, chamber.var, use.random = "ALL")

  # Choose response variable
  response.var <- if (isTRUE(use.random)) {
    "mosquito.count.random"
  } else {
    "mosquito.count.fixed"
  }

  # Rename selected count column to 'mosquito.count' for modeling
  simdat2$mosquito.count <- simdat2[[response.var]]

  # Fit GLMMs
  fit.ixn <- suppressMessages(suppressWarnings(
    lme4::glmer(mosquito.count ~ intvn1 + intvn2 + ixn + (1 | chamber),
                family = "poisson", data = simdat2))
  )

  fit.noixn <- suppressMessages(suppressWarnings(
    lme4::glmer(mosquito.count ~ intvn1 + intvn2 + (1 | chamber),
                family = "poisson", data = simdat2))
  )

  # Extract p-value from likelihood ratio test
  pvalue <- stats::anova(fit.ixn, fit.noixn)[2, "Pr(>Chisq)"]

  return(c(pvalue = pvalue))
}

#' Estimate Empirical Power for Short-Term Semi-Field Experiment Testing Combined Intervention
#'
#' Runs repeated simulations and Poisson GLMM fits to estimate empirical power
#' as the proportion of simulations with p-values below 0.05. These p-values are generated using the function called `sim.pval.shortsfe.comint()`.
#'
#' @param n.ch.per.trt An integer specifying the number of chambers allocated per treatment group.
#' @param lambda A numeric value indicating the expected mean mosquito count in control chambers.
#' @param intvn1.effect A numeric value representing the proportional reduction in mosquito count due to the intervention 1 (e.g., ITN).
#' @param intvn2.effect A numeric value representing the proportional reduction in mosquito count due to the intervention 2 (e.g., PPFa).
#' @param ixn.effect A numeric value representing the proportional reduction in mosquito count due to the interaction (e.g., ITN x PPFa).
#' @param chamber.var A numeric value specifying the variance of random chamber-level effects.
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
#' sim.power.shortsfe.comint(
#'   n.ch.per.trt = 4,
#'   lambda = 50,
#'   intvn1.effect = 0.7,
#'   intvn2.effect = 0.8,
#'   ixn.effect = 0.5,
#'   chamber.var = 0.1807,
#'   nsim = 100,
#'   n.cores = 1,
#'   use.random = TRUE
#' )
#'
#' @importFrom parallel makeCluster parLapply stopCluster clusterExport detectCores
#' @importFrom stats binom.test
#' @export
sim.power.shortsfe.comint <- function(n.ch.per.trt, lambda, intvn1.effect, intvn2.effect,
                                      ixn.effect, chamber.var, nsim, n.cores = 1, use.random = TRUE) {

  # Validate use.random
  if (!isTRUE(use.random) && !isFALSE(use.random)) {
    stop("Invalid use.random value. Please use TRUE or FALSE.")
  }

  # Define simulation wrapper
  sim_wrapper <- function(i) {
    result <- tryCatch(
      sim.pval.shortsfe.comint(n.ch.per.trt, lambda, intvn1.effect, intvn2.effect,
                               ixn.effect, chamber.var, use.random = use.random),
      error = function(e) NA
    )
    result["pvalue"]
  }

  # Initialize progress bar
  pb <- utils::txtProgressBar(min = 0, max = nsim, style = 3)

  # Run simulations with progress
  if (n.cores > 1) {
    cl <- parallel::makeCluster(n.cores)
    on.exit(parallel::stopCluster(cl))
    parallel::clusterExport(cl, varlist = c("sim.pval.shortsfe.comint", "n.ch.per.trt",
                                            "lambda", "intvn1.effect", "intvn2.effect",
                                            "ixn.effect", "chamber.var", "use.random"),
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

  # Flatten p-values and count significant results
  pvals <- unlist(pvals)
  n.sig <- sum(pvals < 0.05, na.rm = TRUE)

  # Estimate power and confidence interval
  power.estimate <- c(
    power = round(n.sig / nsim, 2),
    stats::binom.test(x = n.sig, n = nsim)$conf.int
  )
  names(power.estimate)[2:3] <- c("ci.lower", "ci.upper")

  return(power.estimate)
}
