get_logLik <- function(model){
  bL <- FALSE
  Lf <- try(logLik(model), silent = TRUE)
  if (inherits(Lf,"try-error") || is.na(Lf) || is.infinite(Lf) || is.null(Lf))
    Lf <- try(model$logLik, silent = TRUE)
  suppressWarnings(capture <- capture.output(
    L0 <- try(logLik(update(model, .~1)), silent = TRUE)))
  if (inherits(L0,"try-error") || is.null(L0) || is.na(L0) || is.infinite(L0))
    suppressWarnings(capture <- capture.output(
      L0 <- as.numeric(try(update(model, .~1)$logLik, silent = TRUE))))
  if (inherits(Lf,"try-error") ||
      inherits(L0,"try-error") ||
      is.na(L0) ||
      is.na(Lf) ||
      is.infinite(L0) ||
      is.infinite(Lf)){
    Lf <- L0 <- NA
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


R2index <- function(model, measure, modeltype){
  loglk <- get_logLik(model)
  if (loglk$badlogL) stop("could not access model logLikelihood!")
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
  err <- mi$err
  k <- mi$k
  n <- mi$n
  p <- mi$p
  if (measure == "mcfadden"){
    h1 <- 1 - rho
    h2 <- 1 - ((Lf-p)/L0)
  }
  else if (measure == "ugba"){
    h1 <- 1 - (rho)^{sqrt(2*k)}
    h2 <- 1 - (rho)^{log2(2*k)}
  }
  else if (measure == "nagelkerke") h1 <- (1 - exp((dv - d0)/n))/(1 - exp(-d0/n))
  else if (measure == "coxsnell") h1 <- 1 - exp(-G2/n)
  else if (measure == "aldrich") h1 <- G2 / (G2 + n)
  else if (measure == "veall") h1 <- (G2 / (G2 + n)) * ((n - (2 * L0)) / (-2 * L0))
  else if (measure == "mckelvey") h1 <- vr / (vr + err)
  else if (measure == "efron") h1 <- 1 - (sum((cp[,1L] - cp[,2L])^{2}) / sum((cp[,1L] - mean(cp[,1L]))^{2}))
  else if (measure == "tjur") h1 <- mean(gp2[,2L]) - mean(gp1[,2L])
  ms <- c("mcfadden", "ugba")
  if (!measure %in% ms) h2 <- NA
  list(h1, h2)
}
