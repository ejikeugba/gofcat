#' Lipsitz Test for Categorical Response Models
#' @description This provides a post estimation goodness-of-fit test for
#' the ordinal response models. Supported models include the proportional odds,
#' adjacent-category, and constrained continuation-ratio models.
#' @usage lipsitz(model, group = 10, customFreq = NULL)
#' @param model a model object or data.frame of observed and estimated
#' values. The following class of objects can be directly passed to the
#' \code{lipsitz} function: vglm(), serp(), polr(), and clm(). Other class of
#' objects require providing a data.frame of observed and predicted values.
#' @param group Default to 10. The number of groups to be formed from the
#' original observations.
#' @param customFreq a vector of custom group frequencies to be used instead of
#' the default equal group frequencies. The vector should, however, sum up to
#' the total number of original observations.
#' @importFrom stats update
#' @importFrom stats pchisq
#' @importFrom stats fitted
#' @importFrom stats deviance
#' @details
#' Similar to the ordinal Hosmer-Lemeshow test (\code{\link{hosmerlem}}),
#' the Lipsitz test also group the observations into k separate groups using
#' the ordinal scores of the estimated values. According to Lipsitz, Fitzmaurice,
#' and Molenberghs (1996), the number of groups should be such that 6 <= k < n/5r,
#' with r the number of response category. An indicator variable is used to
#' denote the observations belonging to each group, producing additional pseudo
#' variables with which the original model is updated. Supposing the original
#' model fits correctly, then the coefficients of the pseudo variables all equal
#' zero. The likelihood ratio statistic calculated from the log likelihoods of
#' the original and the refitted models is subsequently compared with the
#' chi-squared distribution with k - 1 degrees of freedom.
#'
#' The Lipsitz test compliments the ordinal Hosmer-Lemeshow and the
#' Pulkstenis-Robinson tests. Fagerland and Hosmer (2013, 2016, 2017)
#' recommend comparing the three test.
#'
#' @return \item{LR}{the realized value of the likelihood ratio statistic.}
#' @return \item{df}{the degrees of freedom.}
#' @return \item{p.value}{the p-value of the test.}
#' @return \item{newVar}{a numeric vector of the newly generated variable.}
#'
#' @references
#' Fagerland, M. W. and Hosmer, D. W. (2013). A goodness-of-fit test for the
#'     proportional odds regression model. \emph{Statistics in Medicine},
#'     32, 2235-2249.
#'
#' Fagerland, M. W. and Hosmer, D. W. (2016). Tests for goodness of fit in
#'     ordinal logistic regression models. \emph{Journal of Statistical
#'     Computation and Simulation}, 86, 3398-3418.
#'
#' Fagerland, M. W. and Hosmer, D. W. (2017). How to test for goodness of fit
#'     in ordinal logistic regression models. \emph{Stata Journal}, 17, 668-686.
#'
#' @seealso
#' \code{\link{hosmerlem}}, \code{\link{pulkroben}}, \code{\link{brant.test}},
#' \code{\link{LR.test}}
#'
#' @examples
#'
#' require(VGAM)
#'
#' set.seed(1)
#' n <- 200
#' y <- ordered(rbinom(n, 2, 0.5))
#' x1 <- factor(rbinom(n, 1, 0.7))
#' x2 <- runif(n)
#'
#' ## proportional odds model
#' vg <- vglm(y ~ x1 + x2, model = TRUE,
#'            family = cumulative(link = "logitlink", parallel = TRUE))
#' lipsitz(vg, group=6)
#'
#' ## adjacent category model
#' ac <- update(vg, family = acat(parallel = TRUE))
#' lipsitz(ac)
#'
#' ## continuation ratio model
#' cr <- update(vg, family = cratio(parallel = TRUE))
#' lipsitz(cr)
#'
#' @export
#'
lipsitz <- function(model, group = 10, customFreq = NULL)
{
  function.name <- "lipsitz"
  cntr <- contr.fn(model, group, customFreq, measure=NULL,
                   thresh=NULL, call.fn=function.name)
  model <- cntr$model
  ypred <- cntr$ypred
  group <- cntr$group
  y <- cntr$y
  nlev <- nlevels(y)
  dt <- data.frame(id=seq.int(nrow(ypred)), ypred, y)
  dt$os <- rowSums(t(seq.int(ncol(ypred)) * t(ypred)))
  dt <- dt[order(dt$os, dt$y), ]
  fq <- splitter(vlen=nrow(dt), grp=group)
  if (!is.null(cntr$freq)) fq <- cntr$freq
  ct <- rep(seq.int(group), fq)
  dt <- data.frame(dt, group=factor(ct))
  dt <- dt[order(dt$id), ]
  origMod <- model
  if (cntr$mt=="vglm") {
    origMod@model$grp <- dt$group
    origMod@model <- ncleaner(origMod@model)
    newMod <- suppressWarnings(try(update(origMod, . ~ . + grp, data = origMod@model),
                  silent = TRUE))
  } else {
    origMod$model$grp <- dt$group
    origMod$model <- ncleaner(origMod$model)
    newMod <- suppressWarnings(try(update(origMod, . ~ . + grp, data = origMod$model),
                  silent = TRUE))
  }
  if (inherits(newMod, "try-error")) stop(cntr$fail)
  if (cntr$mt == "serp" || cntr$mt == "polr")
    LR <- origMod$deviance - newMod$deviance
  else if (cntr$mt == "vglm")
    LR <- deviance(origMod) - deviance(newMod)
  else if (cntr$mt == "clm")
    LR <- abs(-2 * (newMod$logLik - origMod$logLik))
  df <- group - 1
  pv <- 1 - pchisq(LR, df)
  lz <- list(LRT=LR, df=df, p.value=pv, newVar=dt$group, nlev=nlev)
  class(lz) <- function.name
  lz
}
