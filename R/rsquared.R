#' R-squared for Categorical Response Models
#' @description computes the summary measures of predictive strength (i.e., pseudo-R2s)
#' of several categorical outcome models.
#'
#' @usage Rsquared(model, measure)
#' @param model single model object for which R2 is determined.
#' @param measure  selects any of the different measures available.
#' @importFrom stats coef
#' @importFrom stats logLik
#' @importFrom stats var
#' @importFrom utils capture.output
#' @details \code{Rsquared} provides different R2 indices for both binary and
#' multi-categorical response models. Supported classes include: \code{glm},
#' \code{vglm}, \code{clm}, \code{polr}, \code{multinom}, \code{mlogit},
#' \code{serp}. In other words, mainly models with binary or multi-categorical
#' outcomes are supported. The non-likelihood based measures, including the Mckelvey,
#' Tjur and Efron R2s are only available for binary models, while the rest of the
#' measures (likelihood-based) are all available for both binary and
#' multi-categorical models. The Ugba & Gertheiss's R2 in particular, computes
#' the recently proposed modification of the popular Mcfadden's R2. The likelihood
#' ratio index in the said R2 is penalized using either a square-root or logarithmic
#' stabilizing function of the response category. The two approaches yield practically
#' the same result.
#' @return \item{measure}{the name of the R-squared calculated.}
#' @return \item{R2}{realized value of the computed R2.}
#' @return \item{adj}{adjusted R2, only available when McFadden's R2 is computed.}
#' @return \item{sqrt.R2}{Modified R2 with a square root penalty, only available when
#' the Ugba & Gertheiss's R2 is computed.}
#' @return \item{log.R2}{Modified R2 with a logarithmic penalty, only available when
#' the Ugba & Gertheiss's is computed.}
#'
#' @references
#' Long, J.S. (1997). \emph{Regression Models for Categorical and Limited
#'     Dependent Variables}. California: Sage Publications.
#'
#' Ugba, E. R. and Gertheiss, J. (2018). An Augmented Likelihood Ratio Index
#' for Categorical Response Models. In \emph{Proceedings of 33rd International
#' Workshop on Statistical Modelling}, Bristol, 293-298.
#'
#' @seealso
#' \code{\link{erroR}}
#'
#' @examples
#' require(serp)
#'
#' pom <- serp(ordered(RET) ~ DIAB + GH + BP, link="logit",
#'             slope = "parallel", reverse = TRUE, data = retinopathy)
#' Rsquared(pom, measure = "mcfadden")
#' Rsquared(pom, measure = "ugba")
#'
#' @export
#'
Rsquared <- function (model, measure = c("ugba",
                                         "mcfadden",
                                         "coxsnell",
                                         "nagelkerke",
                                         "aldrich",
                                         "veall",
                                         "mckelvey",
                                         "tjur",
                                         "efron"))
{
  function.name <- "Rsquared"
  mc <- match.call()
  measure <- match.arg(measure)
  modeltype <- modtype(model, measure, call.fn=function.name)
  if (is.na(modeltype))
    return(message("Unsupported object!"))
  rr <- R2index(model, measure, modeltype)
  if (is.na(rr[[1L]])) message("Sadly, R2 computation didn't succeed.")
  qq <- round(c(as.numeric(rr[[1L]]), as.numeric(rr[[2L]])), 5L)
  if (measure == "mcfadden")
    ans <- list(measure=measure, R2=qq[1L], adj.R2=qq[2L])
  else if (measure == "ugba")
    ans <- list(measure=measure, sqrt.R2=qq[1L], log.R2=qq[2L])
  else
    ans <- list(measure=measure, R2=qq[1L])
  class(ans) <- function.name
  ans
}
