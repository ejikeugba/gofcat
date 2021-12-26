chi2.fn <- function(model, global, modeltype, deltaHat,
                    varHat, fv, mi, X, K, J)
{
  DM <- gDM(fv, deltaHat, varHat, X, K, J)
  deltaVar <- DM$deltaVar
  deltaStar <- DM$deltaStar
  D <- DM$D
  X2 <- t(D %*% deltaStar) %*%
    solve(D %*% deltaVar %*% t(D)) %*% (D %*% deltaStar)
  df.v <- (J - 2) * K
  if(global){
    Terms <- if(modeltype=="vglm")
      methods::slot(model, "terms")$terms else model$terms
    modmx <- stats::model.matrix(Terms, mi)
    var <- attr(modmx,"assign")[-1L]
    mix <- data.frame(r = c(1:length(var)), var=var)
    for(v in unique(mix$var)){
      k <- subset(mix, var==v)$r
      s <- c()
      df.v.temp <- 0
      for(e in k){
        s <- c(s, seq(from = e, to = K * (J - 1), by = K))
        df.v.temp <- df.v.temp + J - 2
      }
      s <- sort(s)
      Ds <- D[, s]
      if (!is.null(dim(Ds))){
        Ds <- Ds[which(!apply(Ds == 0, 1, all)), ]
      }
      if(!is.null(dim(Ds)))
        X2 <- c(X2, t(Ds %*% deltaStar[s]) %*%
                  solve(Ds %*% deltaVar[s,s] %*%
                          t(Ds)) %*% (Ds %*% deltaStar[s]))
      else
        X2 <- c(X2, t(Ds %*% deltaStar[s]) %*%
                  solve(Ds %*% deltaVar[s, s] %*% t(t(Ds))) %*%
                  (Ds %*% deltaStar[s]))
      df.v <- c(df.v,df.v.temp)
    }
  } else {
    for(k in 1:K)
    {
      s <- seq(from = k, to = K * (J - 1), by = K)
      Ds <- D[,s]
      if (!is.null(dim(Ds))){
        Ds <- Ds[which(!apply(Ds == 0, 1, all)), ]
      }
      if(!is.null(dim(Ds)))
        X2 <- c(X2, t(Ds %*% deltaStar[s]) %*%
                  solve(Ds %*% deltaVar[s,s] %*% t(Ds)) %*%
                  (Ds %*% deltaStar[s]))
      else
        X2 <- c(X2,t(Ds %*% deltaStar[s]) %*%
                  solve(Ds %*% deltaVar[s,s] %*% t(t(Ds))) %*%
                  (Ds %*% deltaStar[s]))
      df.v <- c(df.v,J - 2)
    }
  }
  list(X2 = X2, df.v = df.v)
}
