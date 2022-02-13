#' Brant Test of the Proportional Odds Assumption
#' @description Provides the means of testing the parallel regression assumption
#' in the ordinal regression  models. Also available is the likelihood ratio test,
#' LR.test().
#' @usage brant.test(model, global= FALSE, call = FALSE)
#' @param model a single model object to be tested.
#' @param global default to FALSE. When TRUE, a global test is made for the factor
#' variables instead of the individual factor levels.
#' @param call when TRUE the model call is printed alongside test results.
#' @importFrom Matrix Diagonal
#' @details The parallel regression assumption for the ordinal regression  model
#' can be tested With this function. The brant test (Brant, 1990) is currently
#' available for objects of class: serp(), clm(), polr() and vglm(). Objects of class
#' serp() should have the \code{slope} argument set to 'parallel', while objects of
#' class vglm() should have the \code{model} argument TRUE, if not, the model is
#' automatically updated to include the object 'model'. Moreover, family in
#' vglm() must be either "cumulative" or "propodds", with the parallel argument TRUE.
#' @return \item{model}{call of the model tested}
#' @return \item{df}{the degrees of freedom}
#' @return \item{global}{logical vector of TRUE or FALSE}
#' @return \item{modeltype}{character vector of the class of model tested}
#' @return \item{Terms}{original model terms}
#' @return \item{vnames}{character vector of variable names used in the model}
#' @return \item{chisq}{realized values of the chi-square statistic}
#' @return \item{rdf}{residual degrees of freedom}
#' @return \item{rDev}{residual deviance}
#' @return \item{prob}{the p-values of test}
#' @return \item{call}{a logical vector}
#'
#' @references
#' Brant, R. (1990). Assessing proportionality in the proportional odds model
#'     for ordinal logistic regression. \emph{Biometrics}, 46, 1171-1178.
#'
#' @seealso
#' \code{\link{LR.test}}, \code{\link{hosmerlem}}, \code{\link{lipsitz}},
#'  \code{\link{pulkroben}}
#' @examples
#'
#' require(serp)
#'
#' set.seed(1)
#' n <- 200
#' y <- ordered(rbinom(n, 2, 0.5))
#' x1 <- factor(rbinom(n, 2, 0.7))
#' x2 <- runif(n)
#'
#' ## proportional odds model
#' sp <- serp(y ~ x1 * x2, link = "logit", slope = "parallel", reverse = TRUE)
#'
#' brant.test(sp)
#' brant.test(sp, global = TRUE, call=TRUE)
#'
#' @export
#'
brant.test <- function (model, global = FALSE, call = FALSE)
{
  function.name <- "brant"
  modeltype <- modtype(model, measure=NULL, call.fn=function.name)
  if (is.na(modeltype))
    return(message("Brant test is not available for this model, ",
                   "try LR-test."))
  cf <- compfn(model, modeltype)
  mc <- cf$mc
  m <- cf$m
  y <- cf$y
  x <- cf$x
  x.var <- names(m)[-1L]
  arg.data <- data.frame(m, y)
  zeros(x.var, arg.data, m)
  J <- length(unique(y, na.rm=TRUE))
  K <- length(cf$est)
  for(u in 1:(J-1)){
    arg.data[[paste0("g", u)]] <- ifelse(y > u, 1, 0)
  }
  sep.mod <- list()
  deltaHat <- matrix(NA, nrow = J - 1, ncol = K + 1, byrow = T)
  varHat <- list()
  for(u in 1:(J-1)){
    gm <- stats::glm(paste0("g",u," ~ ",as.character(stats::formula(model)[3L])),
                     data=arg.data, family="binomial")
    sep.mod[[paste0("model",u)]] = gm
    varHat[[u]] <- stats::vcov(gm)
    deltaHat[u,] <- stats::coef(gm)
  }
  X <- cbind(1, x)
  fv <- matrix(NA, nrow=cf$n, ncol=J-1, byrow=T)
  for(u in 1:(J-1)){
    fv[,u] <- fitted(sep.mod[[u]])
  }
  model <- cf$model
  mi <- cf$mi
  sM <- chi2.fn(model, global, modeltype, deltaHat, varHat, fv, mi, X, K, J)
  Terms <- stats::terms(model)
  vnames <- names(cf$est)

  ans <- list(Terms=Terms, global=global, modeltype=modeltype, model=mc,
              vnames=vnames, chisq = sM$X2, df = sM$df.v, call=call)
  class(ans) <- function.name
  ans
}
