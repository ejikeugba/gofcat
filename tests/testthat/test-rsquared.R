require(nnet)
require(ordinal)
require(MASS)
require(mlogit)
require(dfidx)
require(VGAM)
require(serp)

# test data
data(vaso)
attach(vaso)
y <- vaso$vaso
y[vaso$vaso==2] <- 0
volume <- vaso$vol
rate <- vaso$rate

data(retinopathy)
attach(retinopathy)
RET <- as.ordered(RET)
SM <- as.factor(SM)
data("Fishing", package = "mlogit")
Fish <- dfidx::dfidx(Fishing, varying = 2:9, shape = "wide", choice = "mode")

idx <- c("ugba", "mcfadden", "coxsnell", "nagelkerke", "aldrich", "veall",
         "mckelvey", "efron", "tjur")

# test models
gl <- glm(y ~ vol + rate, family=binomial)
gx <- glm(y ~ vol + rate, family=binomial(link = "cauchit"))
qs <- glm(y ~ vol + rate, family=poisson)
fm <- formula(RET ~ SM + DIAB + GH + BP)

sm <- serp(ordered(RET) ~ SM + DIAB + GH + BP, link="logit", slope = "parallel")
capture <- capture.output(mm <- multinom(fm))
cm1 <- clm(fm, link="logit")
cm2 <- clm2(RET ~ SM + DIAB + GH + BP, link="logistic")
pm  <- polr(fm, method="logistic")
ml  <- mlogit(mode ~ price + catch, data = Fish)
xg1 <- vglm(fm, family = sratio(link = "logitlink"))
xg2 <- vglm(fm, family = VGAM::cumulative(parallel = TRUE, link = "clogloglink"))
xg3 <- vglm(y ~ vol + rate, family=cumulative(link="probitlink"))
#xg4 <- vglm(y ~ vol + rate, family=cumulative(link = "cauchit"))

js <- lm(y ~ vol + rate)
bs <- glm(y ~ vol + rate, family=binomial)
suppressWarnings(dd <- serp(fm, slope = "unparallel", subset=c(1:20)))

lx <- list()



context("To check if Rsquared works properly on supported class of models")
test_that("Rsquared function works properly",
          {
            for(i in seq_along(idx))
              lx[[i]] <- Rsquared(gl, measure=idx[i])
            expect_length(lx, 9)
            expect_message(Rsquared("rtt"))
            expect_error(Rsquared(list(gl,gl)))
            #expect_error(Rsquared(qs, measure="ugba"))

            expect_vector(Rsquared(sm, measure = "ugba")$sqrt.R2)
            expect_vector(nlev(model=sm, modeltype="serp"))
            #expect_vector(sup.index(model=sm, measure="mcfadden", modeltype="serp"))
            expect_error(Rsquared(sm, measure = "tjur"))

            expect_vector(Rsquared(mm, measure = "coxsnell")$R2)
            expect_vector(nlev(model=mm, modeltype="multinom"))
            expect_error(Rsquared(mm, measure = "efron"))

            expect_vector(Rsquared(cm1, measure = "ugba")$log.R2)
            #expect_vector(Rsquared(cm2, measure = "mcfadden")$R2)
            expect_error(Rsquared(cm1, measure = "efron"))

            expect_vector(Rsquared(pm, measure = "ugba")$sqrt.R2)
            expect_error(Rsquared(pm, measure = "tjur"))

            #expect_vector(Rsquared(ml, measure = "mcfadden")$R2)
            expect_error(Rsquared(ml, measure = "efron"))
            expect_vector(nlev(model=ml, modeltype="mlogit"))
            expect_vector(hosmerlem(ml)[[1]])

            expect_error(Rsquared(xg1, measure = "mcfadden"))
            expect_equal(modtype(model=xg2, measure="ugba", call.fn="Rsquared"), "vglm")
            #expect_error(sup.index(model=xg2, measure="tjur", modeltype="vglm"))
            expect_vector(modelInfo(model=xg3, modeltype="vglm", measure="mckelvey")$k)
            expect_vector(modelInfo(model=ml, modeltype="mlogit", measure="mcfadden")$k)
            expect_error(Rsquared(gx, measure = "mckelvey"))

            expect_error(print.Rsquared(js))
            lx <- list()
            expect_output(
              for(i in seq_along(idx))
                lx[[i]] <- print.Rsquared(Rsquared(bs, measure=idx[i]))
            )
            expect_error(Rsquared(dd, measure = "ugba"))
          })

detach(vaso)
detach(retinopathy)
rm(y, volume, rate, RET, SM, Fish, idx, gl, qs, lx, fm, sm, mm, cm1, cm2, pm, ml,
   xg1, xg2, js, bs, dd)
