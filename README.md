
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gofcat

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being
activelydeveloped](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Codecov test
coverage](https://codecov.io/gh/ejikeugba/gofcat/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ejikeugba/gofcat?branch=main)
[![Total
Downloads](http://cranlogs.r-pkg.org/badges/grand-total/gofcat)](https://CRAN.R-project.org/package=gofcat)
[![CRAN
status](https://www.r-pkg.org/badges/version/gofcat)](https://CRAN.R-project.org/package=gofcat)
[![license](https://img.shields.io/badge/license-GPL--2-blue.svg)](https://www.gnu.org/licenses/gpl-2.0.en.html)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/ejikeugba/gofcat?branch=main&svg=true)](https://ci.appveyor.com/project/ejikeugba/gofcat)
[![status](https://joss.theoj.org/papers/8079988579172de184353774929f28c1/status.svg)](https://joss.theoj.org/papers/8079988579172de184353774929f28c1)
<!-- badges: end -->
<!-- [![R buildstatus](https://github.com/ejikeugba/gofcat/workflows/R-CMD-check/badge.svg)](https://github.com/ejikeugba/gofcat/actions) -->
### Overview

Post-estimation (goodness-of-fit) tests for some widely used categorical response models (CRM). Package currently supports inputs from objects of class serp(), clm(), polr(), multinom(), mlogit(), vglm() and glm(). Available tests include the Hosmer-Lemeshow, the Lipsitz and the Pulkstenis-Robinson tests for the binary, multinomial and ordinal logistic regression models. The proportional odds, adjacent category, and constrained continuation-ratio models are particularly supported at the ordinal level. Tests for the proportional odds assumptions in ordinal models are also possible with the Brant and the Likelihood-Ratio tests. Moreover, several summary measures of predictive strength (Pseudo R-squared) and error measurement metrics (brier score, misclassification rate and logloss) are also available for the binary, multinomial and ordinal models.

### Example

``` r
require(serp)
set.seed(1)
n <- 200
dt <- data.frame(y = ordered(rbinom(n,2,0.5)), x1 = factor(rbinom(n,2,0.7)), x2 = runif(n))
sp <- serp(y ~ x1 + x2, slope="parallel", link = "logit", reverse= TRUE, data = dt)
```

``` r
## Goodness-of-fit
# Hosmer-Lemeshow test
hosmerlem(sp, tables = TRUE)
hosmerlem(sp, tables = TRUE, customFreq = rep(20,10))

# Lipsitz test
lipsitz(sp)
lipsitz(sp, customFreq = rep(20, 10))

# Pulkstenis-Robinson test
pulkroben(sp, test = "chisq", tables = TRUE)
pulkroben(sp, test = "deviance", tables = TRUE)
```

``` r
## Proportional odds test
brant.test(sp)
brant.test(sp, global = TRUE, call = TRUE)
LR.test(sp, call = TRUE)
```

``` r
## Error metrics
erroR(sp, type = "brier")
erroR(sp, type = "logloss")
erroR(sp, type = "misclass")

# with dataframe and custom threshold
df <- data.frame(y, sp$fitted.values)
erroR(df, type = "misclass", thresh = 0.7)
```

``` r
## Summary metrics
Rsquared(sp, measure = "ugba")
Rsquared(sp, measure = "mcfadden")
```

### Installation and Use

Before installing `gofcat`, it is encouraged to have a recent version of
[R](https://cran.r-project.org/bin/windows/base/) installed. The
released version of `gofcat` can be installed from
[CRAN](https://cran.r-project.org/package=gofcat) with:

``` r
install.packages("gofcat")
```

or the development version from
[GitHub](https://github.com/ejikeugba/gofcat) with:

``` r
if (!require("devtools")) install.packages("devtools")
devtools::install_github("ejikeugba/gofcat")
```

Load `gofcat` into R environment with:

``` r
library(gofcat)
```

### Community Guidelines

Pull requests are welcomed! Please submit your contributions to `gofcat`
through the list of `Pull Requests`, following the [contributing
guidelines](https://github.com/ejikeugba/gofcat/blob/main/CONTRIBUTING.md).
To report issues and/or seek support, please file a new ticket in the
[issue](https://github.com/ejikeugba/gofcat/issues) tracker, and expect
a feedback ASAP!

### Code of Conduct

Please note that `gofcat` is released with a [Contributor Code of
Conduct](https://github.com/ejikeugba/gofcat/blob/main/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.

### References

Ugba, E. R. and Gertheiss, J. (2023). A Modification of McFadden's $R^2$ for Binary and Ordinal Response Models. *Commun. Stat. Appl. Methods*, 
30, 49--63. https://doi.org/10.29220/CSAM.2023.30.1.049

Ugba, E. R. (2022). gofcat: An R package for goodness-of-fit of categorical response models. Journal of Open Source Software, 7(76), 4382, https://doi.org/10.21105/joss.04382

Fagerland, M. W. and Hosmer, D. W. (2017). How to test for goodness of fit in ordinal logistic regression models. *Stata Journal*, 17, 668-686.
