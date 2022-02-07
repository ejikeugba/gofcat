#' Retinopathy
#'
#' The \code{retinopathy} data contains information on persons with retinopathy.
#'
#' @name retinopathy
#' @docType data
#' @format A data frame with 613 observations on the following 5 variables:
#' - RET: RET=1: no retinopathy, RET=2 nonproliferative retinopathy,
#' RET=3 advanced retinopathy or blind
#' - SM: SM=1: smoker, SM=0: non-smoker
#' - DIAB: diabetes duration in years
#' - GH: glycosylated hemoglobin measured in percent
#' - BP: diastolic blood pressure in mmHg
#'
#' @seealso \code{\link{vaso}}, \code{\link{Fishing}}
#' @references
#' Bender and  Grouven (1998), Using binary logistic regression models for
#' ordinal data with non-proportional odds, \emph{J. Clin. Epidemiol.},
#' 51, 809-816.
#'
#' @keywords dataset
NULL


#' Vasoconstriction and Breathing
#'
#' A binary data on the neural constriction of vasculature. Three test persons
#' inhaled a certain amount of air with different rates. In some cases a
#' vasoconstriction occurred at their skin. The objective of the study was to
#' indicate a correlation between breathing and vasoconstriction.
#' The test persons repeated the test 9, 8, 22 times, making a total of
#' 39 observations.
#'
#' @name vaso
#' @docType data
#' @format A data frame with 39 observations on the following 3 variables:
#' - vol: amount of air
#' - rate: rate of breathing
#' - vaso: condition of vasculature: no vasoconstriction = 1, vasoconstriction = 2
#'
#' @seealso \code{\link{retinopathy}}, \code{\link{Fishing}}
#' @source
#' Data Archive Department of Statistics, LMU Munich.
#'
#' @references
#' Finney, D. J. (1971) \emph{Probit Analysis}. 3rd edition. Cambridge
#' University Press.
#'
#' Pregibon, D. (1982) Resistant fits for some commonly used logistic models.
#' Appl. Stat. \bold{29}, 15--24.
#'
#' Hastie, T. J. and Tibshirani, R. J. (1990) \emph{Generalized Additve Models}.
#' Chapman and Hall.
#'
#' @keywords dataset
#'
NULL


#' Choice of Fishing Mode
#'
#' A sample of 1182 individuals in the United-States for the choice of
#' 4 alternative fishing modes.
#'
#' @name Fishing
#' @docType data
#' @format A data frame containing :
#'
#' - mode: recreation mode choice, one of : beach, pier, boat and charter,
#' - price.beach: price for beach mode
#' - price.pier: price for pier mode,
#' - price.boat: price for private boat mode,
#' - price.charter: price for charter boat mode,
#' - catch.beach: catch rate for beach mode,
#' - catch.pier: catch rate for pier mode,
#' - catch.boat: catch rate for private boat mode,
#' - catch.charter: catch rate for charter boat mode,
#' - income: monthly income,
#'
#' @seealso \code{\link{retinopathy}}, \code{\link{vaso}}
#' @source
#' Cameron A, Trivedi P (2005). Microeconometrics. Cambridge University Press.
#' https://EconPapers.repec.org/RePEc:cup:cbooks:9780521848053.
#'
#' @references
#' Herriges JA, Kling CL (1999). “Nonlinear Income Effects in Random Utility
#' Models.” The Review of Economics and Statistics, 81(1), 62-72.
#' doi: 10.1162/003465399767923827
#'
#' @keywords dataset
NULL
