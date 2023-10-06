
# 0 Load packages ---------------------------------------------------------

library(dplyr)
library(lavaan)
library(semPlot)

# 1 Load data -------------------------------------------------------------

neo <- readr::read_csv("colabs/barbara_mestrado/neo2.csv")


# 2 Models ----------------------------------------------------------------

## The Hospital Anxiety and Depression Scale (HADS) ----

### Bifactor

modHADS <- '

G =~ ehad_1 + ehad_2 + ehad_3 + ehad_ 4 + ehad_5 + ehad_6 + ehad_7 +
    ehad_8 + ehad_9 + ehad_10 + ehad_11 + ehad_12 + ehad_13 + ehad_14

Anx =~ ehad_1 + ehad_3 + ehad_5 + ehad_7 + ehad_9 + ehad_11 + ehad_13

Dep =~ ehad_2 + ehad_4 + ehad_6 + ehad_8 + ehad_10 + ehad_12 + ehad_14

'
fitHADS <- cfa(modHADS, data=neo, std.lv = TRUE, estimator = 'MLMV', orthogonal = TRUE)

summary(fitHADS, fit.measures = TRUE)

### Unidimensional

modHADSuni <- '

AD =~ ehad_1 + ehad_2 + ehad_3 + ehad_ 4 + ehad_5 + ehad_6 + ehad_7 +
    ehad_8 + ehad_9 + ehad_10 + ehad_11 + ehad_12 + ehad_13 + ehad_14


'
fitHADSuni <- cfa(modHADSuni, data=neo, std.lv = TRUE, estimator = 'MLMV')

summary(fitHADS, fit.measures = TRUE)

### Hierarchical model

modHADSh <- '

Anx =~ ehad_1 + ehad_3 + ehad_5 + ehad_7 + ehad_9 + ehad_11 + ehad_13

Dep =~ ehad_2 + ehad_4 + ehad_6 + ehad_8 + ehad_10 + ehad_12 + ehad_14

G =~ Anx + Dep

'
fitHADSh <- cfa(modHADSh, data=neo, std.lv = TRUE, estimator = 'MLMV')

summary(fitHADS, fit.measures = TRUE)
# does not work!

# Results:

# > anova(fitHADS, fitHADSuni)
# 
# Scaled Chi-Squared Difference Test (method = “satorra.2000”)
# 
# lavaan NOTE:
#   The “Chisq” column contains standard test statistics, not the
# robust test that should be reported per model. A robust difference
# test is a function of two standard (not robust) statistics.
# 
# Df   AIC   BIC  Chisq Chisq diff Df diff Pr(>Chisq)    
# fitHADS    63 17426 17611 131.39                                  
# fitHADSuni 77 17914 18038 647.49     405.97      14  < 2.2e-16 ***


