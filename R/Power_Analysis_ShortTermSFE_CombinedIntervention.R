#' Simulate Design Scenarios for Short-Term Semi-Field Experiment Testing Combined Interventions
#'
#' Constructs a data frame representing the factorial design of semi-field experiments
#' testing intvn1 and intvn2 interventions. Each chamber is uniquely identified and assigned
#' treatment combinations, including interaction terms.
#'
#' @param n.ch.per.trt Integer. Number of chambers per treatment combination (default = 4).
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{replicates}{Replicate number within each treatment combination}
#'   \item{intvn1}{intvn1a intervention level}
#'   \item{intvn2}{intvn2 intervention level}
#'   \item{ixn}{Interaction term: intvn1 × intvn2}
#'   \item{chamber}{Unique chamber identifier}
#' }
#' @examples
#' sim.scen.shortsfe.comint(n.ch.per.trt = 4)
#'
#' @export
sim.scen.shortsfe.comint <- function(n.ch.per.trt = 4) {

  # Simulate brief progress bar
  pb <- utils::txtProgressBar(min = 0, max = 100, style = 3)
  for (i in seq(0, 100, by = 10)) {
    Sys.sleep(0.05)  # brief pause to simulate progress
    utils::setTxtProgressBar(pb, i)
  }
  close(pb)

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


