#' Likelihood Ratio Test of the Proportional Odds Assumption
#' @description Provides the means of testing the parallel regression assumption
#' in the ordinal regression  models. Also available is the brant.test().
#'
#' @usage LR.test(model, call=FALSE)
#' @param model a single model object to be tested.
#' @param call a logical vector to indicate if the model call should be printed.
#' @details The parallel regression assumption for the ordinal regression  model
#' can be tested With this function. It currently supports objects of class
#' serp() and vglm().
#' @return \item{model}{the call of the model tested}
#' @return \item{df}{the degrees of freedom}
#' @return \item{rdf}{residual degrees of freedom}
#' @return \item{rdev}{residual deviance}
#' @return \item{LRT}{likelihood ratio test statistic}
#' @return \item{prob}{the p-values of test}
#' @return \item{call}{a logical vector}
#'
#' @seealso
#' \code{\link{brant.test}}, \code{\link{hosmerlem}}, \code{\link{lipsitz}},
#'  \code{\link{pulkroben}}
#'
#' @examples
#'
#' require(serp)
#'
#' sp <- serp(ordered(RET) ~ DIAB + GH + BP, link="logit",
#'            slope="parallel", reverse=TRUE, data = retinopathy)
#' LR.test(sp, call = TRUE)
#'
#' @export
#'
LR.test <- function (model, call=FALSE)
{
  function.name <- "LRT"
  modeltype <- modtype(model, measure=NULL, call.fn=function.name)
  if (is.na(modeltype))
    return(message("LRT is not available for this model, try brant test."))
  mc <- compfn(model, modeltype)$mc
  if (modeltype=="serp"){
    npom <- try(stats::update(model, slope="unparallel"), silent = TRUE)
    if (inherits(npom, "try-error"))
      stop("LRT can't be obtained, the general model is unavailable",
           call. = FALSE)
    qq <- stats::logLik(npom)
    if (is.na(qq) || !is.finite(qq))
      stop("LRT can't be obtained, log-likelihoods are unavailable",
           call. = FALSE)
    sp.av <- stats::anova(model,npom)
    dev <- -2 *sp.av$logLik
    rdf <- c(model$rdf, npom$rdf)
    df <- sp.av$df[-1L]
    pv <- sp.av$`Pr(Chi)`[-1L]
  } else {
    suppressWarnings(
    npom <- try(stats::update(model, family = VGAM::cumulative(parallel = FALSE)),
                silent = TRUE))
    if (inherits(npom, "try-error"))
      stop("LRT can't be obtained, the general model is unavailable",
           call. = FALSE)
    qq <- stats::logLik(npom)
    if (is.na(qq) || !is.finite(qq))
      stop("LRT can't be obtained, log-likelihoods are unavailable",
           call. = FALSE)
    vg.av <- VGAM::anova.vglm(model, npom, type = 1L)
    dev <- vg.av$`Resid. Dev`
    rdf <- vg.av$`Resid. Df`
    df <- vg.av$Df[-1L]
    pv <- vg.av$`Pr(>Chi)`[-1L]
  }
  df <- c(NA_integer_, df)
  ch <- c(NA_real_, -diff(dev))
  pv <- c(NA_real_, pv)
  pv <- signif(pv, 4)
  nm <- c("pom", "npom")
  res <- list(model=mc, call=call, rdf = rdf, rdev = dev, LRT = ch, df = df,
              prob = pv)
  class(res) <- function.name
  res
}
