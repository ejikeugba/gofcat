#' Performance metrics for categorical models
#'
#' @description Calculates common error metrics of fitted binary and
#' multi-categorical response models. Available measures include: the
#' brier score, logloss and misclassification error.
#'
#' @usage erroR(model, type = c("brier", "logloss", "misclass"), thresh = 5e-1)
#' @param model a model object or data.frame of observed and predicted values.
#' The following class of objects can be directly passed to the \code{erroR}
#' function: glm(), vglm(), serp(), polr(), clm(), mlogit() and multinom(). Other
#' Other class of objects require providing a data.frame of observed and predicted
#' values.
#' @param type specifies the type of error metrics
#' @param thresh resets the default misclassification threshold
#' @importFrom stats na.omit
#' @return \item{value}{a numeric vector of the realized error value.}
#' @return \item{type}{a character vector of error type.}
#' @return \item{threshold}{a numeric vector of the misclassification threshold.}
#' @seealso
#' \code{\link{Rsquared}}
#'
#' @examples
#'
#' require(serp)
#'
#' set.seed(1)
#' n <- 100
#' y <- factor(rbinom(n, 1, 0.3))
#' x <- rnorm(n)
#' #p <- runif(n)
#'
#' m1 <- glm(y ~ x, family = binomial())
#'
#' erroR(m1, type = "brier")
#' erroR(m1, type = "logloss")
#' erroR(m1, type = "misclass")
#' erroR(m1, type = "misclass", thresh=0.3)
#'
#' # using data.frame
#' df <- data.frame(y, fitted(m1))
#' erroR(df, type = "brier")
#'
#' m2 <- serp(rating ~ temp + contact, slope = "parallel", link = "logit",
#'            data = wine)
#' erroR(m2, type = "brier")
#' erroR(m2, type = "logloss")
#' erroR(m2, type = "misclass")
#'
#' @export
#'
erroR <- function(model, type = c("brier", "logloss", "misclass"), thresh = 5e-1)
{
  function.name <- "erroR"
  type <- match.arg(type)
  cntr <- contr.fn(model, group=NULL, customFreq=NULL, measure=NULL,
                     thresh, call.fn="erroR")
  ypred <- cntr$ypred
  y <- cntr$y
  if (cntr$catg=="binary"){
    if (type=="brier"){
      y <- as.integer(y) - 1L
      value <- sum((y - ypred)^{2})/cntr$obs}
    if (type=="logloss"){
      y <- as.integer(y) - 1L
      value <- - (sum(y * log(ypred) + (1 - y) *
                        log(1 - ypred))) / length(y)}
    if (type=="misclass"){
      y <- factor(y)
      ylevs <- levels(y)
      rr <- ifelse(ypred > cntr$thresh, 1, 0)
      value <- mean(y!=rr)}
  } else {
    ym <- matrix(0, nrow=cntr$obs, ncol=cntr$nL,
                 dimnames=list(NULL, levels(y)))
    yi <- as.integer(y)
    ym[cbind(seq.int(cntr$obs), yi)] <- 1
    if (type=="brier"){
      rs <- rowSums(ym)
      value <- sum(ym * (1 - ypred)^2 + (rs - ym) *
                     ypred^2) / sum(rs)}
    if (type=="logloss"){
      value <- -sum(ym * log(ypred))/nrow(ypred)}
    if (type=="misclass"){
      rr <- apply(ypred, 1, which.max)
      rr <- rr - min(rr) + 1L
      hh <- vapply(seq_len(nrow(ym)), function(i) sum(ym[i, -rr[i]]),
                   numeric(1))
      value <- sum(hh) / sum(ym)
    }
  }
  ans <- list(err=value, type=type)
  if (cntr$catg == "binary" && type == "misclass")
    ans$threshold <- cntr$thresh
  class(ans) <- function.name
  ans
}
