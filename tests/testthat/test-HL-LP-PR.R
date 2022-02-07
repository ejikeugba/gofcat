require(nnet)
require(ordinal)
require(VGAM)

# test data
retinopathy.new <- within(gofcat::retinopathy, {
  RET1 <- RET
  RET <- as.ordered(RET)
  SM <- as.factor(SM)
} )

# test models
set.seed(1)
gl1 <- glm(rbinom(100,1,0.5) ~ rnorm(100), family=binomial)
gl2 <- suppressWarnings(glm(rbinom(5,1,0.5) ~ rnorm(5), family=binomial))
vg <- vglm(rbinom(200,2,0.5) ~ rnorm(200), model=TRUE,
           family=cumulative(link="logitlink", parallel=TRUE))
gp <- update(gl1, family=poisson())
vg1 <- vglm(RET1 ~ DIAB + GH + BP, model = TRUE,
            family = multinomial(parallel = TRUE), data = retinopathy.new)
gh <- update(vg1, family = poissonff(parallel = TRUE))
sr <- update(vg1, family = sratio(parallel = TRUE))
vv <- vglm(RET1 ~ DIAB, family = multinomial(parallel = TRUE),
           data = retinopathy.new)
f1 <- update(vv, family = multinomial(parallel = FALSE))
f2 <- update(vv, family = cumulative(parallel = FALSE))
f3 <- update(vv, family = acat(parallel = FALSE))
f4 <- update(vv, family = cratio(parallel = FALSE))

cl <- clm(RET ~ as.numeric(SM) + DIAB + GH + BP, link="logit", data = retinopathy.new)
cm <- clm(RET ~ factor(SM) + DIAB + GH + BP, link="logit",
          data = retinopathy.new)

test_that("hosmerlem works properly on glm",
          {
            expect_vector(hosmerlem(gl1, group = 10,customFreq = rep(10,10),
                                    tables = TRUE)$df)
            expect_error(hosmerlem(gl1, customFreq = c(rep(6,6),7)))
            expect_error(hosmerlem(gl1, customFreq = c(rep(6,6),"a")))
            expect_error(hosmerlem(gl2, group = 10, tables = TRUE))
            expect_vector(hosmerlem(vg, group = 10, tables = TRUE)$df)
            expect_error(hosmerlem(cbind(rbinom(200,2,0.5),rnorm(200))))
            expect_error(hosmerlem(vg, group = 1, tables = TRUE))

            expect_error(hosmerlem(data.frame(factor(rbinom(200,2,0.5)),
                                              factor(rbinom(200,2,0.5)), rnorm(200))))

            expect_vector(hosmerlem(
              data.frame(ordered(rbinom(200,2,0.5)),matrix(runif(600),200,3)))[[1]])
            expect_error(hosmerlem(
              data.frame(ordered(rbinom(200,2,0.5)), ordered(rbinom(200,2,0.5)),
                                              matrix(runif(600),200,3))))

            expect_error(hosmerlem(data.frame(rbinom(200,2,0.5), rnorm(200))))
            expect_error(hosmerlem(data.frame(factor(rep(1,200)), rnorm(200))))

            expect_error(hosmerlem(
              data.frame(factor(rbinom(50,3,0.1)), matrix(runif(50),200,4))))
            expect_error(hosmerlem(
              data.frame(factor(rbinom(50,1,0.1)), matrix(runif(50),200,4))))

            expect_error(hosmerlem(cbind(factor(rbinom(200,1,0.5)), rnorm(200))))
            expect_warning(hosmerlem(
              data.frame(factor(rbinom(201,1,0.5)), c(runif(200),NA))))
            expect_warning(hosmerlem(
              data.frame(factor(rbinom(202,1,0.5)), c(runif(200),NA, NA))))

            expect_error(hosmerlem(gp))

            expect_error(hosmerlem(cl, customFreq = c(rep(6,6),7)))
            expect_error(hosmerlem(cl, customFreq = c(rep(6,6),"a")))
            expect_vector(hosmerlem(vg1, group = 10, tables = TRUE)$df)
            expect_vector(hosmerlem(vg1, group = 9, customFreq = c(rep(68,8),69))$df)

            expect_error(hosmerlem(vv, customFreq = c(rep(6,6),"a")))

            expect_error(hosmerlem(gh))
            expect_error(hosmerlem(f1))
            expect_error(lipsitz(f2))
            expect_error(pulkroben(f1))
            expect_error(pulkroben(f3))
            expect_error(pulkroben(f4))

            expect_error(hosmerlem(cm, customFreq = c(rep(6,6),"a")))
            expect_output(suppressWarnings(print.lipsitz(lipsitz(cl, group = 4))))
            expect_vector(lipsitz(cl, group = 9, customFreq = c(rep(68,8),69))$df)

            expect_vector(lipsitz(cl, group = 7)$df)

            expect_error(lipsitz("cm"))
            expect_error(pulkroben(cl))
            expect_error(pulkroben("cm"))
          })

rm(gl1, gl2, vg, gp, cl, vg1, gh, sr, vv, f1, f2, f3, f4, cm)
