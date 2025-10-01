#' Simulate Design Scenarios for Short-Term Semi-Field Experiment Testing Single Intervention
#'
#' Constructs a design scenario table representing a short-term semi-field experiment
#' with two treatment levels (or intervention status), i.e., control (no intervention) and intervention, and a specified number of chambers or compartment per treatment.
#' Each chamber is uniquely identified by a treatment level and a replicate combination. An intervention here is shortly named as `intvn`.
#'
#' @details
#' This function considers a single design parameter, which is
#' the total number of chambers or compartments per treatment (`n.ch.per.trt`), e.g., 4.
#' The `n.ch.per.trt` indicates how many chambers known as `replicates` are present per treatment.
#'
#' @param n.ch.per.trt An integer specifying the number of chambers allocated per treatment group.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{replicates}{Replicate number within each treatment group (e.g., for a total of 2 chambers per treatment means 1, 2 replicates per treatment)}
#'   \item{intvn}{Intervention status (e.g., 0 = control or no intervention and 1 = there is intervention)}
#'   \item{chamber}{Unique chamber identifier as a factor (e.g., 0-1, 0-2 for the control chambers and 1-1, 1-2 for intervention chambers)}
#' }
#'
#' @examples
#' sim.scen.shortsfe.sinint(n.ch.per.trt = 4)
#'
#' @export
sim.scen.shortsfe.sinint <- function(n.ch.per.trt) {
  intvn.lev <- 0:1
  dat <- expand.grid(replicates = 1:n.ch.per.trt, intvn = intvn.lev)
  dat$chamber <- factor(paste(dat$intvn, dat$replicates, sep = "-"))
  dat <- dat[, c("replicates", "intvn", "chamber")]
  dat
}

#' Simulate Mosquito Count Data for Short-Term Semi-Field Experiment Testing Single Intervention
#'
#' Generates simulated mosquito count data under a short-term semi-field experimental design
#' with fixed effects for intervention ('intvn'), random effects for chamber variability, and Poisson-distributed outcomes.
#' Uses output from `sim.scen.shortsfe.sinint()` to incorporate the table of experimental design scenarios.
#'
#' @param n.ch.per.trt An integer specifying the number of chambers allocated per treatment group.
#' @param lambda A numeric value indicating the expected mean mosquito count in control chambers.
#' @param intvn.effect A numeric value representing the proportional reduction in mosquito count due to the intervention (e.g., ITN).
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
#'   \item{intvn}{Intervention status (e.g., 0 = control or no intervention and 1 = there is intervention)}
#'   \item{chamber}{Unique chamber identifier as a factor (e.g., 0-1, 0-2 for the control chambers and 1-1, 1-2 for intervention chambers)}
#'   \item{lin.pred.fixed}{Linear predictor with fixed effects only}
#'   \item{lin.pred.random}{Linear predictor with random chamber effects}
#'   \item{mosquito.count.fixed.exp}{Simulted mosquito counts through exponetial function from fixed effects only (no sampling)}
#'   \item{mosquito.count.fixed}{Simulated mosquito counts through sampling distribution based on fixed effects only}
#'   \item{mosquito.count.random}{Simulated mosquito counts through sampling distribution accounting for random effects}
#' }
#'
#' @examples
#' sim.mosq.shortsfe.sinint(
#'   n.ch.per.trt = 4,
#'   lambda = 50,
#'   intvn.effect = 0.8,
#'   chamber.var = 0.1807,
#'   use.random = "ALL"
#' )
#'
#' @importFrom stats rnorm rpois
#' @export
sim.mosq.shortsfe.sinint <- function(n.ch.per.trt, lambda, intvn.effect, chamber.var, use.random = "ALL") {

  # Validate use.random
  if (!isTRUE(use.random) && !isFALSE(use.random) && !is.null(use.random) && !identical(use.random, "ALL")) {
    warning("Invalid use.random value. Returning full dataset.")
    use.random <- "ALL"
  }

  # Generate table of design scenarios
  dat <- sim.scen.shortsfe.sinint(n.ch.per.trt)

  # Fixed effect coefficients
  b.0 <- log(lambda)
  b.i <- log(1 - intvn.effect)

  # Random chamber effects
  chamber.re <- rnorm(nlevels(dat$chamber), sd = sqrt(chamber.var))
  names(chamber.re) <- levels(dat$chamber)

  # Linear predictors
  dat$lin.pred.fixed <- b.0 + b.i * dat$intvn
  dat$lin.pred.random <- dat$lin.pred.fixed + chamber.re[as.character(dat$chamber)]

  # Simulated counts
  dat$mosquito.count.fixed.exp <- exp(dat$lin.pred.fixed)
  dat$mosquito.count.fixed <- rpois(nrow(dat), dat$mosquito.count.fixed.exp)
  dat$mosquito.count.random <- rpois(nrow(dat), exp(dat$lin.pred.random))

  # Return based on use.random
  if (isTRUE(use.random)) {
    return(dat[, c(names(dat)[1:which(names(dat) == "lin.pred.random")], "mosquito.count.random"), drop = FALSE])
  } else if (isFALSE(use.random)) {
    return(dat[, c(names(dat)[1:which(names(dat) == "lin.pred.random")], "mosquito.count.fixed"), drop = FALSE])
  } else if (is.null(use.random)) {
    return(dat[, c(names(dat)[1:which(names(dat) == "lin.pred.random")], "mosquito.count.fixed.exp"), drop = FALSE])
  } else {
    return(dat)
  }
}

#' Plot Mosquito Counts from Short-Term Semi-Field Experiment Testing Single Intervention
#'
#' Generates a boxplot of simulated mosquito counts grouped by intervention status (Control vs Intervention).
#' Uses output from `sim.mosq.shortsfe.sinint()` and can overlay jittered points to show chamber-level variation.
#'
#' @param n.ch.per.trt An integer specifying the number of chambers allocated per treatment group.
#' @param lambda A numeric value indicating the expected mean mosquito count in control chambers.
#' @param intvn.effect A numeric value representing the proportional reduction in mosquito count due to the intervention (e.g., ITN).
#' @param chamber.var A numeric value specifying the variance of random chamber-level effects.
#' @param use.random A logical value indicating whether to plot mosquito counts simulated using a sampling distribution (with random or fixed effects) or exponential function (fixed effect)
#' If \code{TRUE}, plots expected mosquito counts simulated using sampling distribution with random effects;
#' If \code{FALSE}, plots expected counts using sampling distribution based on fixed effects only;
#' If \code{NULL}, plots expected counts simulated using exponential function with fixed effects only (no sampling).
#' @param jitter A logical value indicating whether to overlay individual chamber-level points
#' If \code{TRUE}, overlays individual chamber-level points.
#' If \code{FALSE}, do not overlay individual chamber-level points.
#'
#' @return A `ggplot` object showing expected mosquito counts by treatment group.
#'
#' @examples
#' sim.plot.shortsfe.sinint(
#'   n.ch.per.trt = 4,
#'   lambda = 50,
#'   intvn.effect = 0.8,
#'   chamber.var = 0.1807,
#'   use.random = TRUE,
#'   jitter = TRUE
#' )
#'
#' @import ggplot2
#' @importFrom dplyr mutate
#' @export
sim.plot.shortsfe.sinint <- function(n.ch.per.trt, lambda, intvn.effect, chamber.var,
                                     use.random = TRUE, jitter = TRUE) {

  # Validate use.random
  if (!isTRUE(use.random) && !isFALSE(use.random) && !is.null(use.random)) {
    stop("Invalid use.random value. Please use TRUE, FALSE, or NULL.")
  }

  # Simulate full dataset
  dat <- sim.mosq.shortsfe.sinint(n.ch.per.trt, lambda, intvn.effect, chamber.var, use.random)

  # Assign readable intvn labels
  dat <- dplyr::mutate(dat,
                       Treatments = dplyr::case_when(
                         intvn == 0 ~ "Control",
                         intvn == 1 ~ "Intervention"))

  dat$Treatments <- factor(dat$Treatments, levels = c("Control", "Intervention"))

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
    p <- p + ggplot2::geom_jitter(width = 0.2, alpha = 0.6, color = "darkred")
  }

  return(p)
}

#' Extract p-values from Simulated GLMM for Short-Term Semi-Field Experiment Testing Single Intervention
#'
#' Returns the p-value by fitting a Poisson GLMM to simulated mosquito count data.
#' Uses simulated mosquito counts data from `sim.mosq.shortsfe.sinint()` and fits a Poisson GLMM to extract the p-value associated with the intvn effect.
#'
#' @param n.ch.per.trt An integer specifying the number of chambers allocated per treatment group.
#' @param lambda A numeric value indicating the expected mean mosquito count in control chambers.
#' @param intvn.effect A numeric value representing the proportional reduction in mosquito count due to the intervention (e.g., ITN).
#' @param chamber.var A numeric value specifying the variance of random chamber-level effects.
#' @param use.random A logical value indicating whether to plot mosquito counts simulated using a sampling distribution (with random or fixed effects) or exponential function (fixed effect)
#' If \code{TRUE}, returns p-value for GLMM fitted to expected mosquito counts simulated using sampling distribution with random effects;
#' If \code{FALSE}, returns p-value for GLMM fitted to mosquito counts simulated using sampling distribution based on fixed effects only.
#'
#' @return A named numeric vector:
#' \describe{
#'   \item{pvalue}{A p-value for the intervention effect from the GLMM}
#' }
#'
#' @examples
#' sim.pval.shortsfe.sinint(
#'   n.ch.per.trt = 4,
#'   lambda = 50,
#'   intvn.effect = 0.8,
#'   chamber.var = 0.1807,
#'   use.random = TRUE
#' )
#'
#' @importFrom lme4 glmer
#' @importFrom stats coef
#' @export
sim.pval.shortsfe.sinint <- function(n.ch.per.trt, lambda, intvn.effect, chamber.var, use.random = TRUE) {

  # Validate use.random
  if (!isTRUE(use.random) && !isFALSE(use.random)) {
    stop("Invalid use.random value. Please use TRUE or FALSE.")
  }

  # Simulate full dataset
  simdat <- sim.mosq.shortsfe.sinint(n.ch.per.trt, lambda, intvn.effect, chamber.var, use.random)

  # Choose response variable
  response.var <- if (isTRUE(use.random)) {
    "mosquito.count.random"
  } else {
    "mosquito.count.fixed"
  }

  # Rename selected count column to 'mosquito.count' for modeling
  simdat$mosquito.count <- simdat[[response.var]]

  # Fit GLMM
  model <- suppressMessages(suppressWarnings(
    lme4::glmer(mosquito.count ~ intvn + (1 | chamber),
                family = "poisson", data = simdat)
  ))

  # Extract p-value
  pvalue <- coef(summary(model))[2, "Pr(>|z|)"]

  return(c(pvalue = pvalue))
}

#' Estimate Empirical Power for Short-Term Semi-Field Experiment Testing Single Intervention
#'
#' Runs repeated simulations and Poisson GLMM fits to estimate empirical power
#' as the proportion of simulations with p-values below 0.05. These p-values are generated using the function called `sim.pval.shortsfe.sinint()`.
#'
#' @param n.ch.per.trt An integer specifying the number of chambers allocated per treatment group.
#' @param lambda A numeric value indicating the expected mean mosquito count in control chambers.
#' @param intvn.effect A numeric value representing the proportional reduction in mosquito count due to the intervention (e.g., ITN).
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
#' sim.power.shortsfe.sinint(
#'   n.ch.per.trt = 4,
#'   lambda = 50,
#'   intvn.effect = 0.8,
#'   chamber.var = 0.1807,
#'   nsim = 100,
#'   n.cores = 1,
#'   use.random = TRUE
#' )
#'
#' @importFrom parallel makeCluster parLapply stopCluster clusterExport detectCores
#' @importFrom stats binom.test
#' @export
sim.power.shortsfe.sinint <- function(n.ch.per.trt, lambda, intvn.effect, chamber.var, nsim,
                                      n.cores = 1, use.random = TRUE) {

  # Validate use.random
  if (!isTRUE(use.random) && !isFALSE(use.random)) {
    stop("Invalid use.random value. Please use TRUE or FALSE.")
  }

  # Define simulation wrapper
  sim_wrapper <- function(i) {
    result <- tryCatch(
      sim.pval.shortsfe.sinint(n.ch.per.trt, lambda, intvn.effect, chamber.var, use.random),
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
    parallel::clusterExport(cl, varlist = c("sim.pval.shortsfe.sinint", "n.ch.per.trt",
                                            "lambda", "intvn.effect", "chamber.var", "use.random"),
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
