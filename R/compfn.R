compfn <- function(model, modeltype){
  grpmod <- c("clm", "vglm", "serp")
  if (any(modeltype == grpmod)) {
    r <- nlev(model, modeltype)
    cf <- stats::coef(model)
    est <- cf[-c(1:r-1)]
  } else est <- model$coefficients
  if (modeltype == "vglm") {
    mc <- methods::slot(model, "call")
    m <- methods::slot(model, "model")
    if (is.matrix(m)) m <- as.data.frame(m)
    if (dim(m)[1L] == 0)
      stop("original dataframe not available! consider adding model=TRUE
           in vglm function and try again", call. = FALSE)
    Terms <- methods::slot(model, "terms")
  } else {
    mc <- model$call
    m <- model$model
    if (modeltype == "serp"){
      model$terms <- model$Terms
      model$xlevels <- stats::.getXlevels(model$terms, m)
    }
    Terms <- model$terms
  }
  grpmod <- c("clm", "polr", "serp")
  if (any(modeltype == grpmod)) n <- model$nobs else n <- model@misc$n
  y <- as.numeric(stats::model.response(m))
  x <- stats::model.matrix(Terms, m)
  xint <- match("(Intercept)", colnames(x), nomatch = 0L)
  x <- x[, -xint, drop = FALSE]
  mi <- m
  m <- ncleaner(m)
  list(model=model, est=est, x=x, y=y, mc=mc, mi=mi, m=m, n=n)
}
