% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/erroR.R
\name{erroR}
\alias{erroR}
\title{Performance metrics for categorical models}
\usage{
erroR(model, type = c("brier", "logloss", "misclass"), thresh = 5e-1)
}
\arguments{
\item{model}{a model object or data.frame of observed and predicted values.
The following class of objects can be directly passed to the \code{erroR}
function: glm(), vglm(), serp(), polr(), clm(), mlogit() and multinom(). Other
Other class of objects require providing a data.frame of observed and predicted
values.}

\item{type}{specifies the type of error metrics}

\item{thresh}{resets the default misclassification threshold}
}
\value{
\item{value}{a numeric vector of the realized error value.}

\item{type}{a character vector of error type.}

\item{threshold}{a numeric vector of the misclassification threshold.}
}
\description{
Calculates common error metrics of fitted binary and
multi-categorical response models. Available measures include: the
brier score, logloss and misclassification error.
}
\examples{

require(serp)

set.seed(1)
n <- 100
y <- factor(rbinom(n, 1, 0.3))
x <- rnorm(n)
#p <- runif(n)

m1 <- glm(y ~ x, family = binomial())

erroR(m1, type = "brier")
erroR(m1, type = "logloss")
erroR(m1, type = "misclass")
erroR(m1, type = "misclass", thresh=0.3)

# using data.frame
df <- data.frame(y, fitted(m1))
erroR(df, type = "brier")

m2 <- serp(rating ~ temp + contact, slope = "parallel", link = "logit",
           data = wine)
erroR(m2, type = "brier")
erroR(m2, type = "logloss")
erroR(m2, type = "misclass")

}
\seealso{
\code{\link{Rsquared}}
}
