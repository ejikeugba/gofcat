get_logLik <- function(model){
  bL <- FALSE
  Lf <- try(logLik(model), silent = TRUE)
  if (inherits(Lf,"try-error") || is.na(Lf) || is.infinite(Lf) || is.null(Lf))
    Lf <- try(model$logLik, silent = TRUE)
  suppressWarnings(capture <- capture.output(
    mod <- try(update(model, .~1), silent = TRUE)))

  if (inherits(mod,"try-error") && !isS4(model))
    suppressWarnings(capture <- capture.output(
    mod <- try(update(model, .~1, data = model$model), silent = TRUE)))

  if (inherits(mod,"try-error") && isS4(model))
    suppressWarnings(capture <- capture.output(
    mod <- try(update(model, .~1, data = model@model), silent = TRUE)))

  L0 <- try(logLik(mod), silent = TRUE)
  if (inherits(L0,"try-error") || is.null(L0) || is.na(L0) ||
      is.infinite(L0))
    if (!isS4(mod))
      L0 <- try(mod$logLik, silent = TRUE)
  if (inherits(Lf,"try-error") ||
      inherits(L0,"try-error") ||
      is.na(L0) ||
      is.na(Lf) ||
      is.infinite(L0) ||
      is.infinite(Lf)){
    Lf <- NA
    L0 <- NA
    bL <- TRUE
  }
  list(Lf=Lf, L0=L0, badlogL=bL)
}

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


R2index <- function(model, measure, modeltype){
  loglk <- get_logLik(model)
  Lf <- loglk$Lf
  L0 <- loglk$L0
  dv <- -2 * Lf
  d0 <- -2 * L0
  G2 <- -2 * (L0 - Lf)
  rho <- Lf/L0
  mi <- modelInfo(model, modeltype, measure)
  gp1 <- mi$gp1
  gp2 <- mi$gp2
  vr <- mi$vr
  cp <- mi$cp
  err <- mi$err
  k <- mi$k
  n <- mi$n
  p <- mi$p
  ms <- c("mcfadden", "ugba")
  if (measure == "mcfadden"){
    h1 <- 1 - rho
    h2 <- 1 - ((Lf-p)/L0)
  }
  if (measure == "ugba"){
    h1 <- 1 - (rho)^{sqrt(2*k)}
    h2 <- 1 - (rho)^{log2(2*k)}
  }
  if (measure == "nagelkerke") h1 <- (1 - exp((dv - d0)/n))/(1 - exp(-d0/n))
  if (measure == "coxsnell") h1 <- 1 - exp(-G2/n)
  if (measure == "aldrich") h1 <- G2 / (G2 + n)
  if (measure == "veall") h1 <- (G2 / (G2 + n)) * ((n - (2 * L0)) / (-2 * L0))
  if (measure == "mckelvey") h1 <- vr / (vr + err)
  if (measure == "efron") h1 <- 1 - (sum((cp[,1L] - cp[,2L])^{2}) / sum((cp[,1L] - mean(cp[,1L]))^{2}))
  if (measure == "tjur") h1 <- mean(gp2[,2L]) - mean(gp1[,2L])
  if (!measure %in% ms) h2 <- NA
  list(as.numeric(h1), as.numeric(h2))
}

modelInfo <- function(model, modeltype, measure)
{
  cp <- NULL
  if (modeltype == "glm"){
    k <- nlevels(factor(model$model[,1L]))
    n <- length(model$y)
    p <- length(model$coefficients)
    ft <- try(cbind(as.numeric_version(model$model[,1L])), silent = TRUE)
    if (inherits(ft, "try-error")){
      ft <- cbind(as.numeric(model$model[,1L]) - 1L)
    }
    tr <- as.numeric(model$fitted.values)
    lp <- as.numeric(model$linear.predictors)
    cp <- as.matrix.data.frame(cbind(ft, tr, lp))
  }
  if (modeltype == "vglm"){
    k <- length(model@misc$ynames)
    n <- nrow(depvar(model))
    p <- length(VGAM::coef(model))
  }
  if (modeltype == "multinom"){
    k <- length(model$lev)
    n <- nrow(model$fitted.values)
    p <- length(stats::coef(model))
  }
  if (modeltype == "clm"){
    k <- nlevels(model$y)
    n <- model$n
    p <- length(model$coefficients)
  }
  if (modeltype == "polr"){
    k <- length(model$lev)
    n <- model$n
    p <- length(c(model$zeta, model$coefficients))
  }
  if (modeltype == "serp"){
    k <- model$ylev
    n <- model$nobs
    p <- model$misc$npar
  }
  if (modeltype == "mlogit"){
    k <- length(model$freq)
    n <- length(model$fitted.values)
    p <- length(model$coefficients)
  }
  gp1 <- gp2 <- vr <- err <- NA
  ms <- c("mckelvey", "efron", "tjur")
  lf <- c("logit", "probit", "cloglog")
  if (any(measure == (ms)) && modeltype=="glm") {
    gp1  <- subset(cp, cp[,1] < 1)
    gp2  <- subset(cp, cp[,1] > 0)
    vr <- var(cp[,3])/(n/(n - 1))
    lk <- model$family$link
    if (lk == lf[1L]) err = ((pi)^{2})/3
    else if (lk == lf[2L]) err = 1
    else if (lk == lf[3L]) err = ((pi)^{2})/6
    else if (measure == "mckelvey" && !(lk %in% lf))
      stop("Unsupported link function!", call. = FALSE)
  }
  list(k=k, n=n, p=p, gp1=gp1, gp2=gp2, vr=vr, err=err, cp=cp)
}

longDt <- function(model, m) {
  mr <- model.response(m)
  colns <- colnames(mr)
  vb <- setdiff(all.vars(formula(model)), colns)
  pk <- apply(mr, 1, function(x) rep(colns, x))
  rs <- unlist(pk)
  names(rs) <- NULL
  rs <- ordered(rs, levels = colns)
  wts <- cbind(rowSums(mr))
  indpv <- subset(m, select = vb)
  nc <- ncol(indpv)
  rp <- matrix(rep(wts, nc), nrow(wts), nc)
  pp <- matrix(0, sum(wts), nc)
  for(i in seq_len(nc)){
    pp[,i] <- rep(indpv[,i], wts)
  }
  ld <- data.frame(rs, pp)
  colnames(ld) <- c("resp", vb)
  cl <- lapply(m[,vb, drop = FALSE], class)
  tt <- ld[vb]
  for(i in 1:length(cl)){
    ff <- cl[[i]][1L]
    if (ff=="numeric") tt[,i] <- as.numeric(tt[,i])
    if (ff=="factor") tt[,i] <- as.factor(tt[,i])
    if (ff=="ordered") tt[,i] <- as.ordered(tt[,i])
    if (ff=="character") tt[,i] <- as.character(tt[,i])
  }
  res <- data.frame(resp=rs, tt)
  res
}
