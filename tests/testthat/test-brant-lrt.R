require(VGAM)
require(serp)
require(MASS)

# test data
data(retinopathy)
attach(retinopathy)
RET <- as.ordered(RET)
SM <- as.factor(SM)

# test models
xg1 <- vglm(RET ~ SM + DIAB + GH + BP,
            family = sratio(link = "logitlink"))
xg2 <- vglm(RET ~ SM + DIAB + GH + BP,
            family = cumulative(parallel = FALSE))
xg3 <- vglm(RET ~ SM, model=TRUE,
            family = cumulative(parallel = TRUE))
xg4 <- vglm(rating ~ contact + temp, data = serp::wine,
            family = cumulative(parallel = TRUE))
rr1 <- vglm(as.ordered(RET) ~ SM + DIAB + GH + BP, model=TRUE,
            family = cumulative(parallel = TRUE))
rr2 <- vglm(as.ordered(RET) ~ SM + DIAB * GH + BP, model=TRUE,
            family = cumulative(parallel = TRUE))
rr3 <- vglm(as.ordered(RET) ~ SM*DIAB^10, model=TRUE,
            family = cumulative(parallel = TRUE))
sm2 <- serp(rating ~ contact * temp, link="cauchit",
            slope = "parallel", data = serp::wine)
sm3 <- serp(as.ordered(RET) ~ SM*DIAB^10, link="logit",
            slope = "parallel")
sm4 <- serp(as.ordered(RET) ~ DIAB, link="logit",
            slope = "parallel")
po1 <- polr(RET ~ SM, method = "logistic")

context("To check if tests works properly on supported class of models")
test_that("brant and LR tests work properly",
          {
            expect_vector(brant.test(sm4)$df[1])
            expect_message(brant.test("unknown", global="FALSE"))
            expect_equal(modtype(model=xg3, measure="ugbagerth", call.fn="brant"), "vglm")
            expect_error(modtype(model=xg1, measure="ugbagerth", call.fn="brant"))
            expect_error(modtype(model=xg2, measure="ugbagerth", call.fn="brant"))

            expect_equal(LR.test(rr1)[[2]], FALSE)
            expect_message(LR.test("xgg"))
            expect_error(LR.test(xg4))
            expect_error(LR.test(rr2))
            expect_error(LR.test(rr3))
            expect_error(LR.test(sm2))
            expect_error(LR.test(sm3))
            expect_message(LR.test(po1))
            expect_vector(compfn(model=xg3, modeltype="vglm"))
          })

rm(RET, SM, xg1, xg2, xg3, xg4, rr1, rr2, rr3, sm2, sm3, sm4)
detach(retinopathy)
