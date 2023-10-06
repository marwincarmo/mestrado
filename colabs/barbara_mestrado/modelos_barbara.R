library(dplyr)
library(lavaan)
library(semPlot)

model <- '

ehad_ansiedade_escore ~ neuroticismo_escore_t 

igi_escore ~ abertura_escore_t

igi_escore ~ ehad_ansiedade_escore
igi_escore ~ ehad_depressao_escore


igi_escore ~ sexo + idade + superior_completo + ocupacao

'

model2 <- '

ehad_ansiedade_escore ~ neuroticismo_escore_t 

ehad_ansiedade_escore ~ ehad_depressao_escore

igi_escore ~ abertura_escore_t

igi_escore ~ ehad_ansiedade_escore
igi_escore ~ ehad_depressao_escore


igi_escore ~ sexo + idade + superior_completo + ocupacao

'

model3 <- '

ehad =~ ehad_depressao_escore + ehad_ansiedade_escore

ehad ~ neuroticismo_escore_t 


igi_escore ~ abertura_escore_t

igi_escore ~ ehad

igi_escore ~ sexo + idade + superior_completo + ocupacao + etnia

'

fit <- sem(model3, data = neo, estimator = "MLMV")
summary(fit, standardized = TRUE, fit.measures=TRUE)

semPaths(fit, layout="tree2")

model3 <- '

ehad =~ ehad_depressao_escore + ehad_ansiedade_escore

ehad ~ neuroticismo_escore_t 


igi_escore ~ abertura_escore_t

igi_escore ~ ehad

igi_escore ~ neuroticismo_escore_t 

igi_escore ~ sexo + idade + superior_completo + ocupacao + etnia

'

fit <- sem(model3, data = neo, estimator = "MLMV")
summary(fit, standardized = TRUE, fit.measures=TRUE)

semPaths(fit, layout="tree2")

semPaths(fit, residuals=FALSE,sizeMan=6, rotation =2)


model4 <- '

ehad =~ ehad_depressao_escore + ehad_ansiedade_escore

ehad ~ neuroticismo_escore_t 


igi_escore ~ sexo + idade + superior_completo + ocupacao + etnia + abertura_escore_t +
ehad + neuroticismo_escore_t

'

fit <- sem(model4, data = neo, estimator = "MLMV")
summary(fit, standardized = TRUE, fit.measures=TRUE)


model5 <- '

ehad_ansiedade_escore ~ neuroticismo_escore_t 


igi_escore ~ sexo + idade + superior_completo + ocupacao + etnia + abertura_escore_t +
ehad_ansiedade_escore + neuroticismo_escore_t

'

fit <- sem(model5, data = neo, estimator = "MLMV")
summary(fit, standardized = TRUE, fit.measures=TRUE)

model6 <- '

ehad_depressao_escore ~ neuroticismo_escore_t 


igi_escore ~ sexo + idade + superior_completo + ocupacao + etnia + abertura_escore_t +
ehad_depressao_escore + neuroticismo_escore_t

'

fit <- sem(model6, data = neo, estimator = "MLMV")
summary(fit, standardized = TRUE, fit.measures=TRUE)
