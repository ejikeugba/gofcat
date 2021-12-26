#' Pulkstenis-Robinson Test for Categorical Response Models
#' @description This provides a post estimation goodness-of-fit test for
#' the ordinal response models. Supported models include the proportional odds,
#' adjacent-category, and constrained continuation-ratio models.
#' @usage pulkroben(model, test = c("chisq", "deviance"), tables = FALSE)
#' @param model a model object or data.frame of observed and estimated
#' values. The following class of objects can be directly passed to the
#' \code{pulkroben} function: vglm(), serp(), polr(), and clm(). Other class of
#' objects require providing a data.frame of observed and predicted values.
#' @param test chooses between the chi-squared and the deviance test statistic.
#' @param tables Default to FALSE. When TRUE, both the observed and the
#' expected frequency tables are printed alongside test results.
#' @importFrom epiR epi.cp
#' @importFrom stats aggregate
#' @importFrom stats median
#' @importFrom stats model.frame
#' @details
#' The Pulkstenis-Robinson test groups the observations using the covariate
#' patterns obtained from the categorical covariates. Each covariate pattern
#' is subsequently split in two based on the median ordinal scores. The test
#' statistic (chi-sq or deviance) is obtaned using the tabulated observed and
#' estimated frequencies. Assuming c is the number of covariate patterns, r the
#' number of response categories and k the number of categorical variables in
#' the model, the test statistic approximates the chi-sq distribution with
#' (2c - 1)(r - 1) - k - 1 degrees of freedom (Pulkstenis and Robinson (2004)).
#' As recommended in Fagerland and Hosmer (2016, 2017), this test should
#' be compared with the Hosmer-Lemeshow and the Lipsitz tests.
#' @return \item{stat}{realized value of the chi-square or deviance statistic.}
#' @return \item{df}{the degrees of freedom.}
#' @return \item{p.value}{the p-value of the test statistic.}
#' @return \item{observed}{a table of the observed frequencies.}
#' @return \item{expected}{a table of the estimated frequencies.}
#' @return \item{rho}{percentage of estimated frequencies greater than one.}
#' @return \item{test}{a character vector of the type of test statistic used.}
#' @return \item{tables}{a TRUE or FALSE logical vector.}
#'
#' @references
#' Pulkstenis, E. and Robinson, T. J. (2004). Goodness-of-fit tests for ordinal
#'     response regression models. \emph{Statistics in Medicine} 23, 999-1014.
#'
#' Fagerland, M. W. and Hosmer, D. W. (2016). Tests for goodness of fit in
#'     ordinal logistic regression models. \emph{Journal of Statistical
#'     Computation and Simulation}, 86, 3398-3418.
#'
#' Fagerland, M. W. and Hosmer, D. W. (2017). How to test for goodness of fit
#'     in ordinal logistic regression models. \emph{Stata Journal}, 17, 668-686.
#'
#'
#' @seealso
#' \code{\link{hosmerlem}}, \code{\link{lipsitz}}, \code{\link{brant.test}},
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
#' pulkroben(vg, tables = TRUE)
#'
#' ## adjacent category model
#' ac <- update(vg, family = acat(parallel = TRUE))
#' pulkroben(ac, tables = TRUE)
#'
#' ## continuation ratio model
#' cr <- update(vg, family = cratio(parallel = TRUE))
#' pulkroben(cr, tables = TRUE)
#'
#' @export
#'
pulkroben <- function (model, test = c("chisq", "deviance"),
                       tables = FALSE)
{
  function.name <- "pulkroben"
  test <- match.arg(test)
  cntr <- contr.fn(model, group=NULL, customFreq=NULL, measure=NULL,
                   thresh=NULL, call.fn=function.name)
  ypred <- cntr$ypred
  y <- cntr$y
  cn <- colnames(ypred)
  if (cntr$mt=="vglm") {
    mf <- ncleaner(model.frame(model@model))
    fr <- sapply(mf, is.factor)
    fr[1L] <- TRUE
    if (!sum(fr) > 1L) stop(cntr$catpred)
  } else {
    mf <- ncleaner(model.frame(model))
    fr <- sapply(mf, is.factor)
    if (!sum(fr) > 1L) stop(cntr$catpred)
  }
  hh <- mf[,fr]
  fvars <- if (!is.null(dim(hh))) hh[,-1L, drop=FALSE] else data.frame(hh)
  cp <- epiR::epi.cp(fvars)
  os <- rowSums(t(seq.int(ncol(ypred)) * t(ypred)))
  dt <- data.frame(y, id=cp$id, os, ypred)
  gm <- aggregate(os ~ id, data=dt, median)
  obs <- tabx(gm, dt, cn)$obs
  exp <- tabx(gm, dt, cn)$exp
  obs.tab <- data.frame(rnames(nrow(gm)), rowSums(obs), obs)
  colnames(obs.tab) <- c("cov.pat", "total",colnames(ypred))
  exp.tab <- data.frame(rnames(nrow(gm)), round(exp,3L))
  npat <- nrow(exp.tab)
  ncat <- ncol(obs)
  colnames(exp.tab) <- colnames(obs.tab)[-2L]
  rho <- 1 - (sum(exp < 1)/(nrow(exp)*ncol(exp)))
  rr <- noquote(paste0(round(rho*100,2L), "%"))
  df <- (npat-1)*(ncat-1) - ncol(fvars) - 1
  eps = .Machine$double.eps
  if (test == "chisq"){
    chi <- (obs - exp)^{2}/exp
    chi <- replace(chi, is.na(chi), .Machine$double.eps)
    rho <- sum(chi)
  } else {
    dev <- obs * log(obs/exp)
    dev <- replace(dev, is.na(dev), .Machine$double.eps)
    rho <- 2 * sum(dev)
  }
  pv <- 1 - pchisq(rho, df)
  pr <- list(stat=rho, df=df, p.value=pv, rho=rr, observed=obs.tab,
              expected=exp.tab, test=test, tables=tables)
  class(pr) <- function.name
  pr
}
