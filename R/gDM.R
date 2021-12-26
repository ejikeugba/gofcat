gDM <- function(fv, deltaHat, varHat, X, K, J)
{
  deltaVar = matrix(NA, nrow = (J - 1) * K, ncol = (J - 1) * K)
  for(u in 1:(J - 2)){
    for(q in (u + 1):(J - 1)){
      Wuq <- Matrix::Diagonal(x = fv[, q] - fv[, u] * fv[, q])
      Wu  <- Matrix::Diagonal(x = fv[, u] - fv[, u] * fv[, u])
      Wq  <- Matrix::Diagonal(x = fv[, q] - fv[, q] * fv[, q])
      Xt  <- t(X)
      deltaVar[((u - 1) * K + 1):(u * K),((q - 1) * K + 1):(q * K)] <-
        as.matrix((solve(Xt %*% Wu %*% X) %*% (Xt %*% Wuq %*% X) %*%
                     solve(Xt %*% Wq %*% X))[-1,-1])
      deltaVar[((q - 1) * K+1):(q * K),((u - 1) * K + 1):(u * K)] <-
        deltaVar[((u - 1) * K + 1):(u * K),((q - 1) * K + 1):(q * K)]}
  }
  deltaStar <- c()
  for(u in 1:(J - 1))
    deltaStar <- c(deltaStar, deltaHat[u, -1])
  for(u in 1:(J - 1))
    deltaVar[((u - 1) * K + 1):(u * K),((u - 1) * K + 1):(u * K)] <-
    varHat[[u]][-1,-1]
  I <- diag(1, K)
  E0 <- diag(0, K)
  for(r in 1:(J - 2)) {
    for(j in 1:(J - 1)){
      if(j == 1){
        temp = I
      }else if(j == r + 1){
        temp <- cbind(temp, -I)
      }else{
        temp <- cbind(temp, E0)
      }
    }
    if(r==1){
      D <- temp
    }else{
      D <- rbind(D, temp)
    }
  }
  list(deltaVar=deltaVar, deltaStar=deltaStar, D=D)
}

