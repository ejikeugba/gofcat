compfn <- function(model, modeltype){
  grpmod <- c("clm", "vglm", "serp")
  if (any(modeltype == grpmod)) {
    r <- nlev(model, modeltype)
    cf <- stats::coef(model)
    est <- cf[-c(1:r-1)]
  } else est <- model$coefficients
  if (modeltype == "vglm") {
    mc <- model@call
    m <- model@model
    if (is.matrix(m)) m <- as.data.frame(m)
    if (dim(m)[1L] == 0L){
      model <- update(model, model = TRUE)
      m <- model@model
    }
    mkeep <- m
    if(!is.null(dim(model.response(m)))) m <- longDt(model, m)
  } else {
    mc <- model$call
    m <- model$model
    mkeep <- m
    if(!is.null(dim(model.response(m)))) m <- longDt(model, m)
    if (modeltype == "serp"){
      model$terms <- model$Terms
      model$xlevels <- stats::.getXlevels(model$terms, m)
    }
  }
  dmr <- is.null(dim(model.response(mkeep)))
  if(!dmr)
    model <-   stats::update(model, formula=paste0(
      "resp"," ~ ", as.character(stats::formula(model)[3L])), data=m)
  if (modeltype=="vglm" &&
      length(unique(VGAM::model.frame(model)[,1L]))==2L)
    stop("test not available for binary models!", call. = FALSE)
  Terms <- stats::terms(model)
  y <- if(dmr) as.numeric(stats::model.response(m)) else as.numeric(m[,1L])
  x <- stats::model.matrix(Terms, m)
  xint <- match("(Intercept)", colnames(x), nomatch = 0L)
  x <- x[, -xint, drop = FALSE]
  n <- length(y)
  mi <- m
  m <- ncleaner(m)
  list(model=model, est=est, x=x, y=y, mc=mc, mi=mi, m=m, n=n)
}
