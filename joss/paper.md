---
title: 'gofcat: An R package for goodness-of-fit of categorical response models'
tags:
  - R
  - proportional odds test 
  - brant test
  - hosmer lemeshow test
  - pulkstenis robinson test
  - lipsitz test
  - logistic regression
  - ordinal regression
authors:
  - name: Ejike R. Ugba
    orcid: 0000-0003-2572-0023
    affiliation: 1
affiliations:
 - name: Department of Mathematics and Statistics, School of Economics and Social Sciences, Helmut Schmidt University, Hamburg, Germany
   index: 1
date: 20 April 2022
bibliography: paper.bib
---


# Summary 
Statistical models are considered simplification or approximation of reality [@burnham_model_2002]. How close to the target a given model is, or how it compares to competing models is always of interest in real-world applications. Answers to such questions are mostly obtained via adequate goodness-of-fit (GOF) procedures. However, while such procedures alongside software implementations are readily available for various continuous outcome models, there are just a handful of open-source implementations available for categorical response models (CRMs).  
The `gofcat` R software package provides a quick means of evaluating some widely used CRMs in empirical studies. Depending on the model of interest, functions are available for the different forms of hypothesis tests associated with CRMs and for computing the summary measures of predictive strength of fits. For instance, the proportional odds assumption in the ordinal regression model can be tested using the Brant or the Likelihood-Ratio tests available in `gofcat`. Other crucial tests like the Hosmer-Lemeshow, the Lipsitz and the Pulkstenis-Robinson tests are also available for some widely used binary, multinomial and ordinal response models. Moreover, the assessment of prediction errors through error/loss functions and several summary measures of predictive strength of fitted models (pseudo-$R^2$s) are also available in `gofcat`.


# Statement of Need
Evaluation of model adequacy via GOF tests and related measures remains a very crucial step in every model-selection procedure. Several forms of GOF tests for regression model analyses already exist in the literature and are very much in use too in empirical studies. However, as earlier noted, not only are there few GOF tests particularly suited for the CRMs, due to little or no open-source software implementations, a lot of users find somewhat ambiguous the application of such methods. R packages, such as `goftest` [@julian_goftest_2021], `ADGofTest` [@carlos_adgoftest_2011] and `AICcmodavg` [@marc_aicmodavg_2020], for instance, do provide some GOF tests, but mainly for the continuous outcome models. Other R packages like `ResourceSelection` [@subhash_resourceselection_2019], `performance` [@daniel_performance_2021], `generalhoslem` [@matthew_generalhoslem_2019] and `brant` [@benjamin_brant_2020], do provide some handful of tests applicable to CRMs. However, available tests in those packages seem restricted to binary models or some unique class of objects when extended beyond the binary settings. For instance, the Hosmer-Lemeshow Test available in the `ResourceSelection` package supports only binary models, while the Brant Test available in the `brant` R package supports only objects of class polr().

In contrast, however, while providing several GOF tests that apply to the binary, multinomial and ordinal response models, `gofcat` also supports several classes of objects, including objects from both s3 and s4 R methods. Currently supported classes include objects of class serp() from the `serp` R package [@ugba_serp_2021], vglm() from the `VGAM` R package [@thomas_vgam_2010], clm() from the `ordinal` R package [@christensen_ordinal_2019], multinom() from the `nnet` R package [@venables_modern_2002], polr() from the `MASS` R package [@venables_modern_2002], mlogit() from the `mlogit` R package [@yves_estimation_2020] and glm() from the `stats` R package [@R_language_2021]. The last two methods fit only binary models, while the rest fit multinomial/ordinal models. Supported models from these classes can all be directly evaluated using `gofcat`. However, in situations where the actual model to be evaluated is not available, some tests may still be conducted using instead a data frame of observed and fitted values.


# Features and Application
An overview of the main functions of `gofcat` is given alongside an application to a real-life data example. The data comes from a 6-year longitudinal study on diabetes and retinopathy, with records drawn from  613 diabetic patients [@jorgens_effective_1993; @muhlhauser_cigarette_1996; @bender_regression_1998]. The study aimed to investigate the relationship between retinopathy status and the available risk factors. The outcome variable, retinopathy status (RET), is an ordered factor with three categories: 1 = no retinopathy, 2 = non-proliferative retinopathy, and 3 = advanced retinopathy or blind. The risk factors of interest include smoking (SM), a binary variable with 1 if the patient was a smoker and 0 otherwise, diabetes duration (DIAB) measured in years, glycosylated haemoglobin (GH) measured in percentage, and diastolic blood pressure (BP) measured in mmHg. 

A constrained cumulative logit model (also known as proportional odds model) was fit to the data using clm() from the R `ordinal` package [@christensen_ordinal_2019]). The fit is demonstrated in the code chunk below (for brevity, direct code outputs are  omitted), with the realized estimates and tests shown in Table 1. It is observed that the effect of smoking is not significant (p = 0.187), while the association between retinopathy and the other risk factors are highly significant (p < 0.0001). 

```r
library(gofcat)
library(ordinal)

retino <- within(retinopathy, {
  RET <- ordered(RET)
  SM <- factor(SM)
})

RET.fit <- clm(RET ~ SM + DIAB + GH + BP, link = "logit", 
               data = retino)
summary(RET.fit)
```

Table: Proportional odds model of the retinopathy dataset. The common significance code "***" indicates values < 0.001.  

 Coefficients |      B    |   SE-B   |  Pr(>$|z|$)  |
:-------------|----------:|:---------|:-------------|
 (Intercept)1 |  12.303   |    1.294 |  0.000  ***  |
 (Intercept)2 |  13.673   |    1.320 |  0.000  ***  |
 SM1          |   0.255   |    0.193 |  0.187       |
 DIAB         |   0.140   |    0.014 |  0.000  ***  |
 GH           |   0.460   |    0.076 |  0.000  ***  |
 BP           |   0.072   |    0.014 |  0.000  ***  |


## Post Estimation Tests
Applicable GOF tests for the fitted model (RET.fit) together with other yardsticks of model evaluation were further obtained using the available `gofcat` functions. A brief overview of what each function does is given, followed by an application to the model under consideration.

### hosmerlem() 
This function performs the Hosmer-Lemeshow (HL) test for CRMs. Details of the test for the binary outcome model are given in @hosmer_goodness_1980. An extension to multinomial models is found in @fagerland_multinomial_2008 and @fagerland_generalized_2012, while the extension to ordinal models is found in Fagerland and Hosmer [-@fagerland_2013; -@fagerland_2016; -@fagerland_2017]. In each of the three settings, one splits the original observations into $k$ groups (default to 10) after ranking by ordinal scores and the response variable. The second ordering criterion ensures that ties within the ordinal scores are separated. A Pearson chi-squared statistic is then obtained from the expected and observed frequency tables. For a good chi-squared approximation, it is recommended to have at least 80% of the estimated frequencies $>1$ [@fagerland_2017]. About 97% of the estimated frequencies from RET.fit meet this criterion. However, as observed in Table 2, the HL test is significant (p = 0.014), indicating a possible lack of fit.    

```r
hosmerlem(RET.fit, tables = TRUE)
```

### lipsitz()
This function computes the Lipsitz test for the ordinal models [@lipsitz_goodness_1996]. For this test, one also splits the observations into $k$ separate groups using the ordinal scores of the estimated values and the response variable. An indicator variable denotes the observations belonging to each group, producing additional pseudo-variables with which the original model could be updated. Supposing the original model fits correctly, then the coefficients of the pseudo-variables all equal zero. The likelihood ratio statistic obtained from the log-likelihoods of the original and the refitted models is subsequently compared with the chi-squared distribution having $k - 1$ degrees of freedom. In contrast to the LH test (Table 2), the Lipsitz test for the RET.fit is not statistically significant $(p = 0.459)$, implying that no lack of fit was detected. More discussion on this follows shortly.

```r
lipsitz(RET.fit)
```

### pulkroben()
This function performs the Pulkstenis-Robinson (PR) test for ordinal models [@pulkstenis_goodness_2004]. It particularly forms groups of observations using covariate patterns obtained from the categorical covariates. Each covariate pattern is subsequently split in two based on the median ordinal scores. The test statistic (chi-sq or deviance) is obtained using the tabulated observed and estimated frequencies. Let’s assume that c is the number of covariate patterns, r the number of response categories and k the number of categorical variables in the model, the test statistic approximates the chi-squared distribution with (2c - 1)(r - 1) - k - 1 degrees of freedom. Considering the RET.fit once again (Table 2), the conducted PR test turns out significant (p = 0.011), supporting the initial idea of lack of fit. 

```r
pulkroben(RET.fit, test = "chisq", tables = TRUE)
```
So far, two out of the three GOF tests (HL and PR) for the RET.fit suggest a possible lack of fit. The Lipsitz test, in particular, detected no lack of fit. However, as observed in @fagerland_2017, the Lipsitz test (together with the HL test) is best suited to detect lack of fit associated with continuous covariates, while the PR test works well when lack of fit is associated with categorical predictors. So, a potential explanation of the insignificant result of the Lipsitz test is that the lack of fit is driven by the binary/categorical variable smoking. Meanwhile, it is particularly recommended to compare the results of the ordinal Hosmer-Lemeshow test with the Lipsitz and the Pulkstenis-Robinson tests [@fagerland_2016; @fagerland_2017].


### brant.test()
The Brant test provides the means of testing the parallel regression assumption in ordinal regression models. The test follows the procedures outlined in @brant_assessing_1990, also see the explanations in the Section 2 of @ugba_smoothing_2021. Looking at Table 1,  the Brant test of the RET.fit (Omnibus) is significant (p = 0.035), indicating that the proportional odds assumption is violated. However, a closer look at the individual variables in the brant test (Table 2) shows that the non-proportionality is primarily driven by the only categorical variable (smoking) in the model, which, as earlier suspected, was also causing the lack of fit in a more general sense.

```r
brant.test(RET.fit)
```

### Rsquared()
Several summary measures of predictive strength of CRMs (pseudo-$R^2$) are obtained with this function. These include both likelihood and non-likelihood-based pseudo-R2 measures. For instance, the recently proposed modification of the McFadden's R2 for binary and ordinal outcome models can be obtained via this function [@mcFadden_conditional_1974; @ugba_augmented_2018; @ugba_modification_2022]. As reported in Table 2, the McFadden's and the Ugba & Gertheiss' $R^2$s for the RET.fit are respectively 0.191 and 0.405. See, @ugba_modification_2022 for further details on the reported measures.  

```r
Rsquared(RET.fit, measure = "mcfadden")
Rsquared(RET.fit, measure = "ugba")
```

### erroR()
This function calculates some useful error metrics of fitted binary and multi-categorical response models. Available measures include the brier score [@brier_verification_1950], the cross-entropy loss (log loss) and the misclassification error. Once again, considering the RET.fit, the obtained metrics (Table 2), do not suggest a well performed fit, as more than 30% misclassification on the training data was observed. 

```r
erroR(RET.fit, type = "brier")
erroR(RET.fit, type = "logloss")
erroR(RET.fit, type = "misclass")
```

Table: Post-estimation tests for the proportional odds model of the retinopathy dataset, featuring hypothesis tests for lack of fit, tests for proportional odds assumption and summary/error metrics of fit. The significance code "*" indicates values < 0.05.  

 Hypothesis Tests        |   Chi-sq    |    df       |  pr(>chi)   |
:------------------------|:------------|:------------|:------------|
 Hosmer-Lemeshow (HL)    | 32.148      |    17       | 0.014 *     |
 Lipsitz                 |  8.764      |     9       | 0.459       |
 Pulkstenis-Robinson (PR)| 13.094      |     4       | 0.011 *     |

 Brant Test    |    chi-sq   |    df    |  pr(>chi)  |
:--------------|:------------|:---------|:-----------|
 Omnibus       |   10.38     |    4     |  0.035 *   |
 SM1           |    4.99     |    1     |  0.026 *   |
 DIAB          |    2.21     |    1     |  0.137     |
 GH            |    1.63     |    1     |  0.202     |
 BP            |    1.37     |    1     |  0.241     |

 $R-$squared              |   value  |
:-------------------------|:---------|
 McFadden's $R^2$         |   0.191  |
 Ugba & Gertheiss' $R^2$  |   0.405  |


 Error Metrics            |   value  |
:-------------------------|:---------|
 Brier Score              |   0.427  |
 LogLoss                  |   0.737  |
 Misclassification Error  |   0.318  |


# Conclusion & Outlook
The development of the `gofcat` software package is geared towards a stress-free evaluation of some widely used regression models in empirical studies. As shown in this paper, `gofcat` bundles together crucial GOF tests for CRMs, also providing some quick means of assessing their strength and performance. Several classes of objects are supported and can be directly handled by the available functions, yielding the desired test results. The provided example in this article deals with an ordinal outcome model, even though, as earlier hinted, both the binary and the multinomial outcome models can likewise be assessed using the functions in `gofcat`. Tests for the ordinal models, in particular, are currently available for the constrained forms of the proportional odds model, the adjacent-category model and the continuation-ratio model. Future development of `gofcat` will include tests other than those presently covered and tests for the unconstrained ordinal models.

# Availability
The released version of gofcat is available in the Comprehensive R Archive Network ([CRAN](https://CRAN.R-project.org/package=gofcat)) [@R_language_2021], with details about usage provided in the package [documentation](https://cran.r-project.org/web/packages/gofcat/gofcat.pdf). Alternatively, the development version is available from [GitHub](https://github.com/ejikeugba/gofcat) with further details found on a pkgdown website on [Github Pages](https://ejikeugba.github.io/gofcat) [@wickham_pkgdown_2020]. 


# Acknowledgements
The author would like to thank Jan Gertheiss for his helpful suggestions. This project was partly supported by Deutsche Forschungsgemeinschaft (DFG) under Grant GE2353/2-1.

# References
