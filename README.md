
<!-- README.md is generated from README.Rmd. Please edit that file -->

# powergene: A simulation-based power analysis R Package to aid the design of robust semi-field vector control experiments

<!-- badges: start -->

<!-- badges: end -->

This R package simulates mosquito count data under short-term semi-field
experimental designs, incorporating fixed treatment effects and random
chamber variability. It’s built for pragmatic trial planning, MSc-level
teaching, and reproducible research workflows.

## Installation

You can install the development version of powergene directly from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools") if not already installed
install.packages("devtools")
devtools::install_github("akipingu/powergene")
```

``` r
#load library
library(powergene)
```

## Example 1: Estimating power of a short-term SFE testing single intervention

This is a basic example which shows you how to apply the powergene
package to estimate power of a short-term SFE

1)  Simulate a table of scenarios based on number of chamber per
    treatment provided, e.g., n.ch.per.trt=4. After specifying number of
    chamber per each treatment (control vs. intervention), the function
    sim.scen.shortsfe.sinint will output three columns including number
    of chambers per each treatment (replicates), treatment status (0 for
    control and 1 for intervention) and id for each individual
    chamber/replicate.

``` r
sce.table <- sim.scen.shortsfe.sinint(n.ch.per.trt = 4)

#print the scenario table
sce.table
#>   replicates treatment chamber
#> 1          1         0     0-1
#> 2          2         0     0-2
#> 3          3         0     0-3
#> 4          4         0     0-4
#> 5          1         1     1-1
#> 6          2         1     1-2
#> 7          3         1     1-3
#> 8          4         1     1-4
```

Simulate mosquitoes counts/abundance by specifying the number of
chambers per treatment, e.g., n.ch.per.trt = 4, expected mosquitoes to
be recaptured from a control chamber, e.g., lambda=50, intervention
effect, e.g., interv.effect = 0.8 for 80% effect, and chamber-level
variance, e.g., chamber.var = 0.1807. The resultant number of mosquitoes
will be either with random effect included or without including the
random effect.

``` r
sim.mosquitoes <- sim.mosq.shortsfe.sinint(
  n.ch.per.trt = 4,
  lambda = 50,
  interv.effect = 0.8,
  chamber.var = 0.1807
)

#output mosquito counts per each chamber. I drop columns 4 and 6 because they are optional but you can print them if you want.
sim.mosquitoes <- sim.mosquitoes[, c(-4,-6)]
sim.mosquitoes
#>   replicates treatment chamber mosquito.count.fixed mosquito.count.rondom
#> 1          1         0     0-1                   58                    28
#> 2          2         0     0-2                   46                    66
#> 3          3         0     0-3                   55                    65
#> 4          4         0     0-4                   54                   100
#> 5          1         1     1-1                   10                    16
#> 6          2         1     1-2                   17                    11
#> 7          3         1     1-3                   14                     5
#> 8          4         1     1-4                    6                     5
```

3)  Estimate p-value using sim.pval.shortsfe.sinint function by
    specifying the number of chambers per treatment, e.g., n.ch.per.trt
    = 4, expected mosquitoes to be recaptured from a control chamber,
    e.g., lambda=50, intervention effect, e.g., interv.effect = 0.8 for
    80% effect, and chamber-level variance, e.g., chamber.var = 0.1807.

``` r
pvalue <- sim.pval.shortsfe.sinint(
    n.ch.per.trt = 4,
  lambda = 50,
  interv.effect = 0.8,
  chamber.var = 0.1807
)

#output the p-values
pvalue
#>       pvalue 
#> 0.0001555556
```

4)  Now you can estimate power by specifying the number of chambers per
    treatment, e.g., n.ch.per.trt = 4, expected mosquitoes to be
    recaptured from a control chamber, e.g., lambda=50, intervention
    effect, e.g., interv.effect = 0.8 for 80% effect, chamber-level
    variance, e.g., chamber.var = 0.1807, and the total simulations,
    e.g., nsim=100. A 100 simulations are just for practise, but it is
    recommended to run at least 1000 simulations if you are estimating
    power for a real experiment.

``` r
power.estimate <- sim.power.shortsfe.sinint(n.ch.per.trt = 4,
  lambda = 50,
  interv.effect = 0.8,
  chamber.var = 0.1807, nsim = 100
  )

#print estimated power
power.estimate
#> power 
#>     1
```
