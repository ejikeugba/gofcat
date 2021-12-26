#' Hosmer-Lemeshow Test for Categorical Response Models
#' @description This is a post estimation goodness-of-fit test for categorical
#' response models. It currently supports the binary, multinomial and ordinal
#' logistic regression models.
#' @usage hosmerlem(model, group = 10, tables = FALSE, customFreq = NULL)
#' @param model a model object or data.frame of observed and estimated
#' values. The following class of objects can be directly passed to the function:
#' glm(), vglm(), serp(), polr(), clm(), and mlogit(). Other class of objects
#' require providing a data.frame of observed and predicted values.
#' @param group Default to 10. The number of groups to be formed from the
#' original observations.
#' @param tables Default to FALSE. When TRUE, both the observed and the
#' expected frequency tables are printed alongside test results.
#' @param customFreq a vector of custom group frequencies to be used instead of
#' the default equal group frequencies. The vector should, however, sum up to
#' the total number of original observations.
#' @importFrom VGAM familyname
#' @importFrom reshape cast
#' @importFrom stats predict
#' @importFrom stats xtabs
#' @details
#' The implemented tests are discussed in Hosmer and Lemeshow (1980)
#' (for the binary case), Fagerland, Hosmer, and Bofin (2008);
#' Fagerland and Hosmer (2012) (for the multinomial case) and
#' Fagerland and Hosmer (2013, 2016, 2017) (for the ordinal case). In each
#' of the three settings, one makes a split of the original observations into k
#' groups (10 by default), after ranking the observations by ordinal
#' scores (OS). Ties are separated following an additional ranking based on the
#' observed values. A Pearson chi-squared statistic is thereafter obtained
#' from a (k X c) expected and observed frequency tables. The Hosmerlem()
#' function automatically dictates which of the three HL tests to run based on
#' the type of model supplied or by inspecting the class/level of response
#' category in the supplied data.frame of predicted and observed values.
#'
#' On the choice of k-groups, Fagerland and Hosmer (2013, 2016) recommend
#' using ten groups. It is believed that number of groups below six
#' adversely affects the heterogeneity within groups thereby resulting in a
#' test with low power. Also having too many groups could lead to a sparsely
#' populated contingency tables, affecting the distributional assumptions of
#' the test statistic. The test statistic follows the chi-squared distribution
#' with (k - 2)(r - 1) + (r - 2) degrees of freedom. Where k and r are
#' respectively the number of groups and response category. For chi-square
#' estimation to hold, it is recommended to have the percentage of estimated
#' frequencies greater than 80 percent of all estimated frequencies
#' (Fagerland and Hosmer, 2017).
#'
#' Finally, it is particularly recommended to compare the results of the
#' ordinal Hosmer-Lemeshow test with the lipsitz and the Pulkstenis-Robinson
#' tests (Fagerland and Hosmer, 2016; 2017).
#'
#' @return \item{chi.sq}{realized value of the chi-square statistic.}
#' @return \item{df}{the degrees of freedom.}
#' @return \item{p.value}{the p-value of the test.}
#' @return \item{observed}{table of observed frequencies.}
#' @return \item{expected}{table of estimated frequencies.}
#' @return \item{rho}{percentage of estimated frequencies greater than one.}
#' @return \item{type}{a character vector indicating the type of HL-test conducted.}
#' @return \item{tables}{TRUE or FALSE logical vector.}
#'
#' @references
#' Hosmer, D. W. and Lemeshow, S. (1980). Goodness of fit tests for the multiple
#'     logistic regression model. \emph{Communications in Statistics-Theory and
#'     Methods}, 9, 1043-1069.
#'
#' Fagerland, M. W., Hosmer, D. W. and Bofin, A. M. (2008). Multinomial
#'     goodness-of-fit tests for logistic regression models. \emph{Statistics in
#'     Medicine}, 27, 4238-4253.
#'
#' Fagerland, M. W. and Hosmer, D. W. (2012). A generalized Hosmer-Lemeshow
#'     goodness-of-fit test for multinomial logistic regression models.
#'     \emph{Stata Journal}, 12, 447-453.
#'
#' Fagerland, M. W. and Hosmer, D. W. (2013). A goodness-of-fit test for the
#'     proportional odds regression model. \emph{Statistics in Medicine},
#'     32, 2235-2249.
#'
#' Fagerland, M. W. and Hosmer, D. W. (2016). Tests for goodness of fit in
#'     ordinal logistic regression models. \emph{Journal of Statistical
#'     Computation and Simulation}, 86, 3398-3418.
#'
#' Fagerland, M. W. and Hosmer, D. W. (2017). How to test for goodness of fit
#'     in ordinal logistic regression models. \emph{Stata Journal}, 17, 668-686.
#'
#' @seealso
#' \code{\link{lipsitz}}, \code{\link{pulkroben}}, \code{\link{brant.test}},
#' \code{\link{LR.test}}
#'
#' @examples
#' require(gofcat)
#' require(VGAM)
#'
#' # Binary L-H test
#' set.seed(1)
#' gm <- glm(rbinom(100,1,0.5) ~ rnorm(100), family=binomial)
#' hosmerlem(gm, group = 10, customFreq = NULL, tables = TRUE)
#'
#' # multinomial L-H test
#' vg <- vglm(RET ~ DIAB + GH + BP,
#'            family = multinomial(parallel = TRUE), data = retinopathy)
#' hosmerlem(vg)
#'
#' # ordinal L-H test
#' ## proportional odds model
#' cu <- update(vg, family = cumulative(link = 'logitlink', parallel = TRUE))
#' hosmerlem(cu, tables=TRUE)
#' hosmerlem(cu, group = 5, tables=TRUE, customFreq = c(rep(100,5), 113))
#'
#' ## adjacent category model
#' ac <- update(vg, family = acat(parallel = TRUE))
#' hosmerlem(ac)
#'
#' ## continuation ratio model
#' cr <- update(vg, family = cratio(parallel = TRUE))
#' hosmerlem(cr)
#'
#' ## using data.frame
#' y <- ordered(retinopathy$RET)
#' df <- data.frame(y, fitted.values(cr))
#' hosmerlem(df, tables = TRUE)
#'
#' @export
#'
hosmerlem <- function(model, group = 10, tables = FALSE,
                      customFreq = NULL)
{
  function.name <- "hosmerlem"
  cntr <- contr.fn(model, group, customFreq, measure=NULL,
                     thresh=NULL, call.fn=function.name)
  ypred <- cntr$ypred
  group <- cntr$group
  y <- cntr$y
  switch(
    cntr$catg,
    binary = {
      dt <- data.frame(ypred, y)
      dt <- dt[order(dt$ypred, dt$y), ]
      fq <- splitter(vlen=nrow(dt), grp=group)
      if (!is.null(cntr$freq)) fq <- cntr$freq
      gr <- seq.int(group)
      ct <- rep(gr, fq)
      y <- as.numeric(dt$y) - 1
      obs <- xtabs(cbind(y, 1 - y) ~ ct)
      obs.tab <- cbind(gr, obs, rowSums(obs))
      exp <- xtabs(cbind(dt$ypred, 1 - dt$ypred) ~ ct)
      exp.tab <- cbind(gr, round(exp,2L))
      colnames(obs.tab) <- c("group", "1", "0", "total")
      colnames(exp.tab) <- colnames(obs.tab)[-4L]
      df <- nrow(exp) - 2L
      type <- "binary"
    },
    multiclass = {
      dt <- data.frame(ypred, y)
      if (cntr$ord){
        dt$os <- rowSums(t(seq.int(ncol(ypred)) * t(ypred)))
        type <- "ordinal"
      } else {
        dt$os <- 1-ypred[,1L]
        type <- "multinomial"
      }
      dt <- dt[order(dt$os, dt$y), ]
      fq <- splitter(vlen=nrow(dt), grp=group)
      if (!is.null(cntr$freq)) fq <- cntr$freq
      ct <- rep(seq.int(group), fq)
      dt <- data.frame(dt, group=factor(ct))
      dt$value <- dt$y
      obs <- cast(dt, group ~ value, length)[,-1L]
      dt$y <- dt$group <- dt$value <- NULL
      nn <- mm <- matrix(NA, group, ncol(dt))
      for (r in seq_len(group)){
        Id <- which(as.numeric(ct) == r)
        hh <- dt[Id,]
        nn[r,] <- colSums(hh)
        mm[r,] <- colMeans(hh)
      }
      exp <- nn[,-ncol(nn)]
      exp.tab <- data.frame(seq.int(nrow(mm)), round(mm[,ncol(mm)], 4L),
                            round(exp, 4L))
      colnames(exp.tab) <- c("group", "OS", colnames(obs))
      obs.tab <- data.frame(seq.int(nrow(mm)), fq, obs)
      colnames(obs.tab) <- c("group", "total", colnames(obs))
      df <- (nrow(exp) - 2) * (ncol(exp) - 1)
      if (cntr$ord) df <- df + ncol(exp) - 2
    }
  )
  rho <- 1 - (sum(exp < 1)/(nrow(exp)*ncol(exp)))
  X2 <- sum(((obs - exp)^{2})/exp)
  pv <- 1 - pchisq(X2, df)
  rr <- noquote(paste0(round(rho*100,2), "%"))
  ans <- list(chi.sq=X2, df=df, p.value=pv, rho=rr, observed=obs.tab,
              expected=exp.tab, type=type, tables=tables)
  class(ans) <- function.name
  ans
}
