splitter <- function(vlen, grp)
{
  vec <- seq.int(vlen)
  sp <- round(length(vec)/grp)
  if (sp == 0L)
    stop("can't proceed with test! there are very few ",
         "number of observations.", call. = FALSE)
  res <- tabulate(ceiling(seq_along(vec)/sp))
  if (length(res) < grp){
    sp <- floor(length(vec)/grp)
    res <- tabulate(ceiling(seq_along(vec)/sp))
  }
  if (length(res) > grp){
    hh <- res[1:grp] + sum(res[(grp+1):length(res)])
    res <- c(res[1:grp][-1L], hh[length(hh)])
  }
  res
}

ncleaner <- function(df){
  cnames <- colnames(df)
  pp <- stringr::str_extract(string = cnames,
                             pattern = "(?<=\\().*(?=\\))")
  if (!all(is.na(pp)))
    cnames[which(!is.na(pp))] <- pp[which(!is.na(pp))]
  colnames(df) <- cnames
  df
}

rnames <- function(r){
  g1 <- c(sprintf("p%d", seq.int(r)))
  g2 <- c(sprintf("q%d", seq.int(r)))
  gg <- NA
  npat <- 2*r
  k <- j <- 0
  while(k < npat){
    j <- j+1
    k <- k+1
    gg[k] <- g1[j]
    k <- k+1
    gg[k] <- g2[j]
  }
  gg
}

tabx <- function(gm, dt, cn){
  om <- gm[order(gm$os), ]$id
  obsx <- matrix(NA, nrow(gm), ncol(dt)-3L)
  obsy <- expx <- expy <- obsx
  zerox <- rep(0, ncol(obsx))
  for (r in seq_len(nrow(gm))){
    L <- dt[which(dt$id == r),]
    Q <- split(L, L$os > gm[r, 2L])
    fe <- is.null(Q$`FALSE`)
    te <- is.null(Q$`TRUE`)
    obsx[r,] <- if (!fe) as.numeric(table(Q$`FALSE`$y)) else zerox
    obsy[r,] <- if (!te) as.numeric(table(Q$`TRUE`$y)) else zerox
    expx[r,] <- if (!fe) colSums(Q$`FALSE`[,-c(1L:3L)]) else zerox
    expy[r,] <- if (!te) colSums(Q$`TRUE`[,-c(1L:3L)]) else zerox
  }
  npat <- 2*nrow(gm)
  ncat <- ncol(dt)-3L
  obs <- exp <-  matrix(NA, npat, ncat)
  k <- j <- 0
  while(k < npat){
    j <- j+1
    k <- k+1
    obs[k,] <- obsx[om,][j,]
    exp[k,] <- expx[om,][j,]
    k <- k+1
    obs[k,] <- obsy[om,][j,]
    exp[k,] <- expy[om,][j,]
  }
  list(obs=obs, exp=exp)
}

nlev <- function(model, modeltype)
{
  if (modeltype == "glm") nct <- nlevels(factor(model$model[,1L]))
  if (modeltype == "vglm") nct <- length(model@misc$ynames)
  if (modeltype == "multinom") nct <- length(model$lev)
  if (modeltype == "clm") nct <- nlevels(model$y)
  if (modeltype == "polr") nct <- length(model$lev)
  if (modeltype == "serp") nct <- model$ylev
  if (modeltype == "mlogit") nct <- length(model$freq)
  nct
}

zeros <- function(x.var, arg.data, m){
  xnames <- NULL
  for (name in x.var){
    if (!is.numeric(m[, name])){
      xnames <- c(xnames, name)
    }
  }
  if (length(xnames) > 0L){
    mat <- table(data.frame(arg.data$y, m[,xnames]))
    nz <- sum(mat == 0L)
  } else nz <- 0L
  if(nz!=0){
    if(nz > 1)
      warning(
        paste0("with " , nz, " zero cell entries in the computation crosstab, test results may be inaccurate."), call. = FALSE)
    else
      warning(
        paste0("with 1 zero cell entry in the computation crosstab, test results may be inaccurate."), call. = FALSE)
  }
}

rho.serp <- function(model, measure, modeltype)
{
  if (modeltype == "serp"){
    L0 <- summary(model)$null.logLik
    L1 <- model$logLik
    if (is.na(L0) || is.na(L1) || L0==Inf || L1==Inf)
      stop("Log-likelihood for computing R2 is not available", call. = FALSE)
    rho <- L1/L0
  }
  rho
}

sup.index <- function(model, measure, modeltype)
{
  ls1 <- c("coxsnell", "nagelkerke", "efron", "mckelvey", "tjur")
  ls2 <- ls1[-c(1,2)]
  errmessage <- c("the requested measure is not available for this class of model")
  if (modeltype == "vglm" && any(measure == ls1))
    stop(errmessage, call. = FALSE)
  else if (modeltype == "multinom" && any(measure == ls2))
    stop(errmessage, call. = FALSE)
  else if (modeltype == "clm" && any(measure == ls2))
    stop(errmessage, call. = FALSE)
  else if (modeltype == "polr" && any(measure == ls2))
    stop(errmessage, call. = FALSE)
  else if (modeltype == "serp" && any(measure == ls1))
    stop(errmessage, call. = FALSE)
  else if (modeltype == "mlogit" && any(measure == ls2))
    stop(errmessage, call. = FALSE)
  if (modeltype == "serp" && measure == "mcfadden")
    index <- 1 - rho.serp(model, measure, modeltype)
  else if (measure == "mcfadden")
    index <- performance::r2_mcfadden(model)
  else if (measure == "coxsnell")
    index <- performance::r2_coxsnell(model)
  else if (measure == "efron")
    index <- performance::r2_efron(model)
  else if (measure == "mckelvey")
    index <- performance::r2_mckelvey(model)
  else if (measure == "nagelkerke")
    index <- performance::r2_nagelkerke(model)
  else if (measure == "tjur")
    index <- performance::r2_tjur(model)
  index
}
