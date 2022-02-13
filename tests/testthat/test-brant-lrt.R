require(VGAM)
require(serp)
require(MASS)

retinopathy.new <- within(gofcat::retinopathy, {
  RET <- as.ordered(RET)
  SM <- as.factor(SM)
 } )

# test models
xg1 <- vglm(RET ~ SM + DIAB + GH + BP,
            family = sratio(link = "logitlink"), data = retinopathy.new)
xg2 <- suppressWarnings(vglm(RET ~ SM + DIAB + GH + BP,
            family = cumulative(parallel = FALSE), data = retinopathy.new))
xg3 <- vglm(RET ~ SM, model=TRUE,
            family = cumulative(parallel = TRUE), data = retinopathy.new)
rr1 <- suppressWarnings(vglm(rating ~ contact * temp, model=TRUE,
            family = cumulative(parallel = TRUE), data = serp::wine))
sm2 <- serp(rating ~ contact * temp, link="cauchit",
            slope = "parallel", data = serp::wine)
sm3 <- serp(RET ~ SM*DIAB^10, link="logit",
            slope = "parallel", data = retinopathy.new)
sm4 <- serp(RET ~ DIAB, link="logit",
            slope = "parallel", data = retinopathy.new)
po1 <- polr(RET ~ SM, method = "logistic", data = retinopathy.new)

context("To check if tests works properly on supported class of models")
test_that("brant and LR tests work properly",
          {
            expect_vector(brant.test(sm4)$df[1])
            expect_message(brant.test("unknown", global="FALSE"))
            expect_equal(modtype(model=xg3, measure="ugba", call.fn="brant"), "vglm")
            expect_error(modtype(model=xg1, measure="ugba", call.fn="brant"))
            expect_error(modtype(model=xg2, measure="ugba", call.fn="brant"))

            expect_equal(attributes(LR.test(rr1))$class, "LRT")
            expect_message(LR.test("xgg"))
            expect_error(LR.test(sm2))
            expect_error(LR.test(sm3))
            expect_message(LR.test(po1))
            expect_vector(compfn(model=xg3, modeltype="vglm"))
          })
rm(retinopathy.new, xg1, xg2, xg3, rr1, sm2, sm3, sm4, po1)
