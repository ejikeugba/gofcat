require(VGAM)

# test data
retinopathy.new <- within(gofcat::retinopathy, {
  RET <- as.ordered(RET)
  SM <- as.factor(SM)
} )

# test models
set.seed(1)
vs <- glm(rbinom(100,1,0.5) ~ rnorm(100), family=binomial)
md <- data.frame(factor(rbinom(100,2,0.5)), cbind(runif(100),runif(100),runif(100)))
xg <- VGAM::vglm(RET ~ SM, model=TRUE,
                 family = VGAM::cumulative(parallel = TRUE), data = retinopathy.new)
df <- data.frame(factor(rbinom(200,1,0.5)), rnorm(200))
dp <- data.frame(factor(rbinom(200,2,0.5)), matrix(rnorm(600),200,3))

test_that("erroR function works properly on models",
          {
            expect_vector(erroR(vs, type="brier")[[1]])
            expect_vector(erroR(vs, type="misclass")[[1]])
            expect_vector(erroR(vs, type="logloss")[[1]])
            expect_vector(erroR(md, type="brier")[[1]])
            expect_vector(erroR(md, type="misclass")[[1]])
            expect_vector(erroR(md, type="logloss")[[1]])
            expect_vector(erroR(xg)[[1]])

            expect_warning(erroR(df))
            expect_warning(erroR(dp))
          })
rm(retinopathy.new, vs, md, xg, df, dp)
