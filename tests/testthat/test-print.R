require(nnet)
require(VGAM)
require(serp)

# test data
data(retinopathy)
attach(retinopathy)
RET <- as.ordered(RET)
SM <- as.factor(SM)

# test models
set.seed(1)
gl <- glm(rbinom(100,1,0.5) ~ rnorm(100), family=binomial)
gm <- glm(rbinom(20,1,0.5) ~ rnorm(20), family=binomial)
dt <- data.frame(y=ordered(rbinom(100,2,0.5)), x=factor(rbinom(100,3,0.5)))
sp <- serp(dt, slope = "parallel")
sm <- serp(as.ordered(RET) ~ factor(SM) + DIAB + GH + BP, link="logit", slope = "parallel")
xg <- vglm(as.ordered(RET) ~ SM + DIAB + GH + BP, model=TRUE,
           family = cumulative(parallel = TRUE))
capture <- capture.output(mm <- multinom(RET ~ SM + DIAB + GH + BP))

#lipsitz(sp)
context("To check if tests works properly on supported class of models")
test_that("print methods works properly",
          {
            expect_output(print.brant(brant.test(sm, global="FALSE", call=TRUE)))
            expect_output(print.brant(brant.test(sm, global="TRUE")))
            expect_output(print.LRT(LR.test(sm, call=TRUE)))
            expect_output(print.erroR(erroR(sm, type="logloss")))
            expect_output(print.erroR(erroR(sm, type="misclass")))
            expect_output(print.erroR(erroR(sm, type="brier")))
            expect_output(print.hosmerlem(hosmerlem(sm)))
            expect_output(print.hosmerlem(hosmerlem(gl, tables=TRUE)))
            expect_output(suppressWarnings(print.hosmerlem(hosmerlem(mm, tables=TRUE))))
            expect_output(print.lipsitz(lipsitz(sm)))
            expect_output(print.lipsitz(lipsitz(xg)))
            expect_output(print.pulkroben(pulkroben(sm, test = "deviance", tables=TRUE)))
            expect_output(print.pulkroben(pulkroben(xg)))
            expect_output(suppressWarnings(print.pulkroben(pulkroben(sp))))

            expect_error(erroR(sm, type = "misclass", thresh = "a"))
            expect_error(print.brant("js"))
            expect_error(print.LRT("js"))
            expect_error(print.erroR("js"))
            expect_error(print.hosmerlem("js"))
            expect_error(print.lipsitz("js"))
            expect_error(lipsitz(sp))
            expect_error(print.pulkroben("js"))
            expect_error(print.pulkroben(sm))
            expect_error(print.Rsquared("js"))

            cp <- capture.output(expect_warning(print.hosmerlem(hosmerlem(gm))))
            expect_output(suppressWarnings(print.hosmerlem(hosmerlem(mm, group=5))))
            expect_vector(pulkroben(sm, test = "deviance")[[1]])
            expect_message(LR.test(mm))
          })

detach(retinopathy)
rm(RET, SM, gl, gm, dt, sp, sm, xg, mm)
