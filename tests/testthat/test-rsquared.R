require(nnet)
require(ordinal)
require(MASS)
require(mlogit)
require(dfidx)
require(VGAM)
require(serp)

# test data
vaso.new <- within(gofcat::vaso, {
  vaso[vaso==2] <- 0
  y <- vaso
})
retinopathy.new <- within(gofcat::retinopathy, {
  RET <- as.ordered(RET)
  SM <- as.factor(SM)
})
Fish <- dfidx::dfidx(gofcat::Fishing, varying = 2:9, shape = "wide", choice = "mode")


# test models
gm <- glm(y ~ vol + rate, family=binomial(link = "logit"), data = vaso.new)
gx <- glm(y ~ vol + rate, family=binomial(link = "cauchit"), data = vaso.new)
fm <- formula(RET ~ SM + DIAB + GH + BP)

sm <- serp(ordered(RET) ~ SM + DIAB + GH + BP, link="logit", slope = "parallel",
           data = retinopathy.new)
capture <- capture.output(mm <- multinom(fm, data = retinopathy.new))
cm1 <- clm(fm, link="logit", data = retinopathy.new)
pm  <- polr(fm, method="logistic", data = retinopathy.new)
ml  <- mlogit(mode ~ price + catch, data = Fish)
xg1 <- vglm(fm, family = sratio(link = "logitlink"), data = retinopathy.new)
xg2 <- vglm(fm, family = VGAM::cumulative(parallel = TRUE, link = "clogloglink"),
            data = retinopathy.new)
suppressWarnings(
xg3 <- vglm(y ~ vol + rate, family=cumulative(link="probitlink"), data = vaso.new))

js <- lm(y ~ vol + rate, data = vaso.new)
bs <- glm(y ~ vol + rate, family=binomial, data = vaso.new)

idx <- c("ugba", "mcfadden", "coxsnell", "nagelkerke", "aldrich", "veall",
         "mckelvey", "tjur", "efron")
lx <- list()



context("To check if Rsquared works properly on supported class of models")
test_that("Rsquared function works properly",
          {
            for(i in seq_along(idx))
              lx[[i]] <- Rsquared(gm, idx[i])
            expect_length(lx, 9L)
            expect_message(Rsquared("rtt"))
            expect_error(Rsquared(list(gm,gm)))

            expect_vector(Rsquared(sm, measure = "ugba")$sqrt.R2)
            expect_vector(nlev(model=sm, modeltype="serp"))
            expect_error(Rsquared(sm, measure = "tjur"))

            expect_vector(Rsquared(mm, measure = "coxsnell")$R2)
            expect_vector(nlev(model=mm, modeltype="multinom"))
            expect_error(Rsquared(mm, measure = "efron"))

            expect_vector(Rsquared(cm1, measure = "ugba")$log.R2)
            expect_error(Rsquared(cm1, measure = "efron"))

            expect_vector(Rsquared(pm, measure = "ugba")$sqrt.R2)
            expect_error(Rsquared(pm, measure = "tjur"))

            expect_error(Rsquared(ml, measure = "efron"))
            expect_vector(nlev(model=ml, modeltype="mlogit"))
            expect_vector(hosmerlem(ml)[[1]])

            expect_error(Rsquared(xg1, measure = "mcfadden"))
            expect_equal(modtype(model=xg2, measure="ugba", call.fn="Rsquared"), "vglm")
            expect_vector(modelInfo(model=xg3, modeltype="vglm", measure="mckelvey")$k)
            expect_vector(modelInfo(model=ml, modeltype="mlogit", measure="mcfadden")$k)
            expect_error(Rsquared(gx, measure = "mckelvey"))

            expect_error(print.Rsquared(js))
            lx <- list()
            expect_output(
              for(i in seq_along(idx))
                lx[[i]] <- print.Rsquared(Rsquared(bs, measure=idx[i]))
            )
          })
rm(vaso.new, Fish, gm, fm, sm, mm, cm1, pm, ml, idx, lx,
   xg1, xg2, js, bs)
