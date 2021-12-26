require(nnet)
require(MASS)
require(serp)

# test data
data(retinopathy)
attach(retinopathy)
RET <- as.ordered(RET)
SM <- as.factor(SM)

# test models
sm5 <- serp(as.ordered(RET) ~ SM + DIAB + GH + BP, link="logit", slope = "unparallel")
sm6 <- serp(rating ~ contact + temp, link="logit", slope = "unparallel", data = serp::wine)
capture <- capture.output(mm <- nnet::multinom(RET ~ SM + DIAB + GH + BP))
pm <- polr(factor(RET) ~ SM + DIAB + GH + BP, method="logistic")

test_that("misc functions works as expected on models",
          {
            expect_error(modtype(model=sm5, measure="ugbagerth", call.fn="brant"))
            yy <- sm6$model[1L][,1L]
            xv <- colnames(sm6$model)[-1L]
            argd <- data.frame(sm6$model, yy)
            expect_warning(zeros(x.var=xv, arg.data=argd, m=sm6$model))

            expect_equal(modtype(model=mm, measure="ugbagerth", call.fn="brant"), NA)

            expect_vector(compfn(model=pm, modeltype="polr"))
          })

detach(retinopathy)
rm(RET, SM, sm5, sm6, mm, pm)
