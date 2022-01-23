#' Print method for an object of class r-squared
#'
#' Prints out a vector of name and value of an R2 of a fitted model.
#'
#' @param x An object of class \code{rsquared}.
#' @param ... additional arguments.
#' @return No return value
#' @seealso \code{\link{Rsquared}}
#' @export
#'
print.Rsquared <- function(x, ...)
{
  if (!inherits(x, "Rsquared"))
    stop("input should be an object of class 'Rsquared'", call. = FALSE)
  object <- x
  if (object$measure == "mcfadden"){
    cat(crayon::blue("\nMcFadden's R2:", "\n"))
    cat(object$R2, "\n")
    cat("\n")
    cat(crayon::blue("adj.R2", "\n"))
    cat(object$adj.R2,"\n")
  }
  else if (object$measure == "coxsnell"){
    cat(crayon::blue("\nCoxsnell's R2:", "\n"))
    cat("\n")
    cat(object$R2, "\n")
  }
  else if (object$measure == "nagelkerke"){
    cat(crayon::blue("\nNagelkerke's R2:", "\n"))
    cat("\n")
    cat(object$R2, "\n")
  }
  else if (object$measure == "aldrich"){
    cat(crayon::blue("\nAldrich & Nelson's R2:", "\n"))
    cat("\n")
    cat(object$R2, "\n")
  }
  else if (object$measure == "veall"){
    cat(crayon::blue("\nVeall & Zimmermann's R2:", "\n"))
    cat("\n")
    cat(object$R2, "\n")
  }
  else if (object$measure == "mckelvey"){
    cat(crayon::blue("\nMckelvey & Zavoina's R2:", "\n"))
    cat("\n")
    cat(object$R2, "\n")
  }
  else if (object$measure == "efron"){
    cat(crayon::blue("\nEfron's R2:", "\n"))
    cat("\n")
    cat(object$R2, "\n")
  }
  else if (object$measure == "tjur"){
    cat(crayon::blue("\nTjur's R2:", "\n"))
    cat("\n")
    cat(object$R2, "\n")
  }
  else if (object$measure == "ugba"){
    cat(crayon::blue("\nUgba & Gertheiss' R2:", "\n"))
    cat("\n")
    cat(crayon::blue("(sqrt)", "\n"))
    cat(object$sqrt.R2, "\n")
    cat("\n")
    cat(crayon::blue("(log)", "\n"))
    cat(object$log.R2,"\n")
  }
}


#' Print method for an object of class brant
#'
#' Prints out a summary table of brant-test for an object of class brant.
#'
#' @param x An object of class \code{brant}.
#' @param ... additional arguments.
#' @return No return value
#' @seealso \code{\link{brant.test}}
#' @export
#'
print.brant <- function (x, ...)
{
  if (!inherits(x, "brant"))
    stop("input should be an object of class 'brant'", call. = FALSE)
  object <- x
  cl <- x$call
  mc <- x$model
  Terms <- object$Terms
  vnames <- if (object$global) colnames(attr(Terms,"factors")) else object$vnames
  vnames <- c("Omnibus", vnames)
  rs <- matrix(0, length(vnames), 3L, dimnames = list(vnames, NULL))
  rs <- data.frame(a=round(object$chisq, 3), b=object$df,
                   c=stats::pchisq(object$chisq, object$df, lower.tail=FALSE))
  colnames(rs) <- c("   chi-sq", "  df","  pr(>chi)")
  rownames(rs) <- vnames
  if (cl == TRUE){
    cat(crayon::yellow("\nModel:\n"))
    print(mc)
  }
  cat(crayon::yellow("\nBrant Test:\n"))
  stats::printCoefmat(rs, digits = 3L, signif.stars = TRUE, na.print = "NA",
                      P.values = TRUE, has.Pvalue = TRUE, signif.legend = TRUE)
  cat("\nH0: Proportional odds assumption holds\n")
  cat("\n")
}

#' Print method for an object of class LRT
#'
#' Prints out a summary table of the likelihood ratio test for an object of
#' class LRI.
#'
#' @param x An object of class \code{LRI}.
#' @param ... additional arguments.
#' @return No return value
#' @seealso \code{\link{LR.test}}
#' @export
#'
print.LRT <- function (x, ...)
{
  if (!inherits(x, "LRT"))
    stop("input should be an object of class 'LRT'", call. = FALSE)
  mc <- x$model
  cl <- x$call
  x <- data.frame(x$rdf, x$rdev, x$LRT, x$df, x$prob)
  names(x) <- c("      rdf", "     rdev","    LRT", "  df", "  pr(>chi)")
  rownames(x) <- c("POM", "NPOM")
  if (cl == TRUE){
    cat(crayon::yellow("\nModel:\n"))
    print(mc)
  }
  cat(crayon::yellow("\nLR-Test:\n"))
  stats::printCoefmat(x, digits = 5L, signif.stars = TRUE, na.print = "",
                      P.values = TRUE, has.Pvalue = TRUE, signif.legend = TRUE,
                      cs.ind=2)
  cat("\nH0: Proportional odds assumption holds\n")
}

#' Print method for an object of class erroR
#'
#' Prints out a vector of name and calculated error value of fitted model.
#'
#' @param x An object of class \code{erroR}.
#' @param ... additional arguments.
#' @return No return value
#' @seealso \code{\link{erroR}}
#' @export
#'
print.erroR <- function(x, ...)
{
  if (!inherits(x, "erroR"))
    stop("input should be an object of class 'erroR'", call. = FALSE)
  if (x$type == "brier"){
    cat(crayon::blue("\nBrier Score:", "\n"))
    cat(x$err, "\n")
  }
  else if (x$type == "logloss"){
    cat(crayon::blue("\nLogLoss:", "\n"))
    cat(x$err, "\n")
  }
  else if (x$type == "misclass"){
    cat(crayon::blue("\nMisclassification Error:", "\n"))
    cat(x$err, "\n")
  }
}

#' Print method for an object of class hosmerlem
#'
#' Prints out a summary table of the goodness-of-fit test for binary,
#' multinomial and ordinal models.
#' class LRI.
#'
#' @param x An object of class \code{hosmerlem}.
#' @param ... additional arguments.
#' @return No return value
#' @seealso \code{\link{hosmerlem}}
#' @export
#'
print.hosmerlem <- function (x, ...)
{
  if (!inherits(x, "hosmerlem"))
    stop("input should be an object of class 'hosmerlem'", call. = FALSE)
  rho <- x$rho
  obs <- x$observed
  exp <- x$expected
  tables <- x$tables
  group <- nrow(x$obs)
  if (x$type == "binary"){
    colnames(obs) <- c("group",  "   1",  "   0", "total")
    colnames(exp) <- c("group",  "      1",  "      0")
  }
  tbl <- data.frame(x$chi.sq, x$df, x$p.value)
  names(tbl) <- c(" Chi-sq", " df", " pr(>chi)")
  if (x$type == "binary") rownames(tbl) <- c("binary(Hosmerlem)")
  else if (x$type == "ordinal") rownames(tbl) <- c("ordinal(Hosmerlem)")
  else if (x$type == "multinomial") rownames(tbl) <- c("multinomial(Hosmerlem)")
  cat(crayon::yellow("\nHosmer-Lemeshow Test:\n"))
  cat("\n")
  stats::printCoefmat(tbl, digits = 5L, signif.stars = TRUE, na.print = "",
                      P.values = TRUE, has.Pvalue = TRUE, signif.legend = TRUE,
                      cs.ind=2)
  cat("\nH0: No lack of fit dictated\n")
  cat("\nrho:", rho, "\n")
  cat("\n")
  if (tables == TRUE){
    cat(crayon::yellow("\nObserved Freqs:\n"))
    cat("\n")
    print(obs, row.names = FALSE)
    cat(crayon::yellow("\nExpected Freqs:\n"))
    cat("\n")
    print(exp, row.names = FALSE)
    cat("\n")
  }
  if (x$type == "binary")
    rh <- 1 - (sum(exp[,-1L] < 1)/(nrow(exp[,-1L])*ncol(exp[,-1L])))
  else
    rh <- 1 - (sum(exp[,-c(1L:2L)] < 1)/(nrow(exp[,-c(1L:2L)])*ncol(exp[,-c(1L:2L)])))
  if (rh < 0.8)
    warning(paste0("Less than 80% of the estimated frequencies are greater ",
                   "than 1, test results may be inaccurate."), call. = FALSE)
  if (group < 6L || group > 10L)
    warning(paste0("It is recommended to choose number of groups between 6 and 10",
                   " both inclusive."), call. = FALSE)
}

#' Print method for an object of class lipsitz
#'
#' Prints out a summary table of the goodness-of-fit test for an object of
#' class lipsitz.
#'
#' @param x An object of class \code{lipsitz}.
#' @param ... additional arguments.
#' @return No return value
#' @seealso \code{\link{lipsitz}}
#' @export
#'
print.lipsitz <- function (x, ...)
{
  if (!inherits(x, "lipsitz"))
    stop("input should be an object of class 'lipsitz'", call. = FALSE)
  group <- nlevels(x$newVar)
  n <- length(x$newVar)
  c <- x$nlev
  x <- data.frame(x$LRT, x$df, x$p.value)
  names(x) <- c("    LR", "  df", "  pr(>chi)")
  rownames(x) <- c("ordinal(lipsitz) ")

  cat(crayon::yellow("\nLipsitz Test:\n"))
  cat("\n")
  stats::printCoefmat(x, digits = 5L, signif.stars = TRUE, na.print = "",
                      P.values = TRUE, has.Pvalue = TRUE, signif.legend = TRUE,
                      cs.ind=2)
  cat("\nH0: No lack of fit dictated\n")
  cat("\n")

  if (group < 6L || group >= n/(5 * c))
    warning("It is not recommended to run this test with group < 6 or >= n/5k.",
            " Where n is the number of observations and k the number of response ",
            "category")
}

#' Print method for an object of class pulkroben
#'
#' Prints out a summary table of the goodness-of-fit test for an object of
#' class pulkroben.
#'
#' @param x An object of class \code{pulkroben}.
#' @param ... additional arguments.
#' @return No return value
#' @seealso \code{\link{pulkroben}}
#' @export
#'
print.pulkroben <- function (x, ...)
{
  if (!inherits(x, "pulkroben"))
    stop("input should be an object of class 'pulkroben'", call. = FALSE)
  obs <- x$observed
  exp <- x$expected
  tables <- x$tables
  test <- x$test
  rho <- x$rho
  x <- data.frame(x$stat, x$df, x$p.value)
  rownames(x) <- c("ordinal(pulkroben)")
  if (test=="chisq"){
    names(x) <- c(" chi-sq", "  df", "  pr(>chi)")
    cat(crayon::yellow("\nPulkstenis-Robinson Chi-squared Test:\n"))
  } else{
    names(x) <- c("    dev-sq", "  df", "  pr(>chi)")
    cat(crayon::yellow("\nPulkstenis-Robinson Deviance Test:\n"))
  }
  cat("\n")
  stats::printCoefmat(x, digits = 5L, signif.stars = TRUE, na.print = "",
                      P.values = TRUE, has.Pvalue = TRUE, signif.legend = TRUE,
                      cs.ind=2)
  cat("\nH0: No lack of fit dictated\n")
  cat("\nrho:", rho, "\n")
  cat("\n")
  if (tables == TRUE){
    cat(crayon::yellow("\nObserved Freqs:\n"))
    cat("\n")
    print(obs, row.names = FALSE)
    cat(crayon::yellow("\nExpected Freqs:\n"))
    cat("\n")
    print(exp, row.names = FALSE)
    cat("\n p <= median OS; q > median OS\n")
    cat("\n")
  }
  rh <- 1 - (sum(exp[,-1L] < 1)/(nrow(exp[,-1L])*ncol(exp[,-1L])))
  if (rh < 0.8)
    warning(paste0("Less than 80% of the estimated frequencies are greater ",
                   "than 1, test results may be inaccurate."), call. = FALSE)
}
