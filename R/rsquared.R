#' R-squared for Categorical Response Models
#' @description computes the summary measures of predictive strength of
#' several categorical outcome model. R-squared measures are computed based
#' on suitability to the model under consideration.
#'
#' @usage Rsquared(model, measure)
#' @param model single model object for which R2 is determined.
#' @param measure  selects any of the different measures available.
#' @details \code{Rsquared} provides different R2 indices for both binary and
#' multi-categorical response models. Supported classes include: \code{glm},
#' \code{vglm}, \code{clm}, \code{polr}, \code{multinom}, \code{mlogit},
#' \code{serp}. Thus, mainly model families with binary and multi-categorical
#' response are supported.
#' @return \item{measure}{the name of the R-squared calculated.}
#' @return \item{R2}{realized value of the computed R2.}
#' @return \item{adj}{adjusted R2, only available when McFadden's R2 is computed.}
#'
#' @references
#' Long, J.S. (1997). \emph{Regression Models for Categorical and Limited
#'     Dependent Variables}. California: Sage Publications.
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
#' Rsquared(pom, measure = "ugbagerth")
#'
#' @export
#'
Rsquared <- function (model, measure = c("ugbagerth",
                                         "coxsnell", "mcfadden",
                                         "nagelkerke", "mckelvey", "efron", "tjur"))
{
  function.name <- "Rsquared"
  mc <- match.call()
  measure <- match.arg(measure)
  modeltype <- modtype(model, measure, call.fn=function.name)
  if (is.na(modeltype))
    return(message("the supplied model is currently unsupported"))
  if (measure == "ugbagerth"){
    if (modeltype == "serp"){rho <- rho.serp(model, measure, modeltype)}
    else{
      capture <- utils::capture.output(hh <- performance::r2_mcfadden(model))
      mf <- if (length(hh)==1L) hh else hh$R2
      mf <- as.numeric(mf)
      rho <- 1 - mf
    }
    nct <- nlev(model, modeltype)
    val <- 1 - rho^sqrt(2*nct)
  } else val <- sup.index(model, measure, modeltype)

  qq <- round(as.numeric(val),5L)
  adj <- NULL
  if (measure =="mcfadden"){
    R2 <- qq[1L]
    adj <- qq[2L]
  } else R2 <- qq
  ans <- list(measure=measure, R2=R2, adj=adj)
  if (is.null(adj)) ans$adj <- NULL
  class(ans) <- function.name
  ans
}
