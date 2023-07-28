
## ----packages-cfa, echo=TRUE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------
library(lavaan)
library(dplyr)
library(semTools)
library(semPlot)
library(weights)
library(bootnet)
library(ggplot2)


## ----dataset-cfa, warning=FALSE, message=FALSE, echo=FALSE---------------------------------------------------------------------------------
## read full dataset
mydata <- read.csv("data/data.csv")

## select answers from arm 1
arm1 <- mydata |> 
  dplyr::filter(redcap_event_name == "elegibilidade_arm_1",
                ## exclude previously identified outliers
                !record_id %in% c(1651, 2015, 2938, 3793)
                ) |> 
  dplyr::mutate(group = dplyr::case_when(
    dsm_1 == 0 &
    dsm_2 == 0 &
    dsm_3 == 0 &
    dsm_4 == 0 &
    dsm_5 == 0 &
    isi_total < 8 ~ "good_sleepers",
    TRUE ~ "bad_sleepers")) 

dbas <- arm1 |> 
  dplyr::select(record_id, paste0("dbas16_", 1:16), group) |> 
  dplyr::filter(!dplyr::if_all(dplyr::starts_with("dbas16_"), ~ is.na(.))) |> 
  # median inputation for one participant that left one item unanswered
  dplyr::mutate(dbas16_10 = tidyr::replace_na(dbas16_10, 7))




## ----cfa-model-ho--------------------------------------------------------------------------------------------------------------------------
mod_dbas_ho <- '
cons =~ dbas16_5 + dbas16_7 + dbas16_9 + dbas16_12 + dbas16_16
worry =~ dbas16_3 + dbas16_4 + dbas16_8 + dbas16_10 + dbas16_11 + dbas16_14
exp =~ dbas16_1 + dbas16_2
med =~ dbas16_6 + dbas16_13 + dbas16_15
gen =~ cons + worry + exp + med
'


fit_mlm_ho <- lavaan::cfa(model = mod_dbas_ho,
                        std.lv = TRUE, estimator = 'MLMV', data = dbas[,2:17])



## ----cfa-model-1---------------------------------------------------------------------------------------------------------------------------
mod_dbas <- '
cons =~ dbas16_5 + dbas16_7 + dbas16_9 + dbas16_12 + dbas16_16
worry =~ dbas16_3 + dbas16_4 + dbas16_8 + dbas16_10 + dbas16_11 + dbas16_14
exp =~ dbas16_1 + dbas16_2
med =~ dbas16_6 + dbas16_13 + dbas16_15
'


fit_mlm <- lavaan::cfa(model = mod_dbas,
                        std.lv = TRUE, estimator = 'MLMV', data = dbas[,2:17])
s_mlm <- summary(fit_mlm, fit.measures = TRUE,standardized=TRUE)


## ----compare-fit-ho-4f---------------------------------------------------------------------------------------------------------------------
fit_comparison <- anova(fit_mlm_ho, fit_mlm)


## ----robust-fit----------------------------------------------------------------------------------------------------------------------------
# RMSEA and CFI from the Mean-And-Variance Corrected Test Statistic (Salvalei, 2018)
# Savalei, V. (2018). On the Computation of the RMSEA and CFI from the Mean-And-Variance Corrected Test Statistic with Nonnormal Data in SEM, Multivariate Behavioral Research, 53(3), 419-429. https://doi.org/10.1080/00273171.2018.1455142

a <- fitmeasures(fit_mlm)["chisq.scaling.factor"]
rmsea.old<-fitmeasures(fit_mlm)["rmsea.scaled"]
ci.old.l<-fitmeasures(fit_mlm)["rmsea.ci.lower.scaled"]
ci.old.u<-fitmeasures(fit_mlm)["rmsea.ci.upper.scaled"]

rmsea.new<-sqrt(a)*rmsea.old
ci.new.l<-sqrt(a)*ci.old.l
ci.new.u<-sqrt(a)*ci.old.u

cfi.old<-fitmeasures(fit_mlm)["cfi.scaled"]
a.baseline<-fitmeasures(fit_mlm)["baseline.chisq.scaling.factor"]
cfi.new<-1-(a/a.baseline*(1-cfi.old))


## ----cfa-model-2---------------------------------------------------------------------------------------------------------------------------
mod_dbas_mi <- '
cons =~ dbas16_5 + dbas16_7 + dbas16_9 + dbas16_12 + dbas16_16
worry =~ dbas16_3 + dbas16_4 + dbas16_8 + dbas16_10 + dbas16_11 + dbas16_14
exp =~ dbas16_1 + dbas16_2
med =~ dbas16_6 + dbas16_13 + dbas16_15
dbas16_3 ~~  dbas16_4
dbas16_6 ~~ dbas16_15
'

# Model fit with MLM estimator, assuming data as continuous
fit_mlm_mi <- lavaan::cfa(model = mod_dbas_mi,
                           std.lv = TRUE, estimator = 'MLMV', data = dbas[,2:17])


## ----robust-fit-model2---------------------------------------------------------------------------------------------------------------------
# RMSEA and CFI from the Mean-And-Variance Corrected Test Statistic (Salvalei, 2018)
# Savalei, V. (2018). On the Computation of the RMSEA and CFI from the Mean-And-Variance Corrected Test Statistic with Nonnormal Data in SEM, Multivariate Behavioral Research, 53(3), 419-429. https://doi.org/10.1080/00273171.2018.1455142

a_mi <- fitmeasures(fit_mlm_mi)["chisq.scaling.factor"]
rmsea.old_mi<-fitmeasures(fit_mlm_mi)["rmsea.scaled"]
ci.old.l_mi<-fitmeasures(fit_mlm_mi)["rmsea.ci.lower.scaled"]
ci.old.u_mi<-fitmeasures(fit_mlm_mi)["rmsea.ci.upper.scaled"]

rmsea.new_mi<-sqrt(a_mi)*rmsea.old_mi
ci.new.l_mi<-sqrt(a_mi)*ci.old.l_mi
ci.new.u_mi<-sqrt(a_mi)*ci.old.u_mi

cfi.old_mi<-fitmeasures(fit_mlm_mi)["cfi.scaled"]
a.baseline_mi<-fitmeasures(fit_mlm_mi)["baseline.chisq.scaling.factor"]
cfi.new_mi<-1-(a_mi/a.baseline_mi*(1-cfi.old_mi))


## ----longitudinal-data, echo=FALSE---------------------------------------------------------------------------------------------------------
data_longitudinal <- mydata |> 
  dplyr::filter(!record_id %in% c(1651, 2015, 2938, 3793)) |>  # dupes
  dplyr::select(record_id, paste0("dbas16_", 1:16), redcap_event_name) |> 
  dplyr::filter(!dplyr::if_all(dplyr::starts_with("dbas16_"), ~ is.na(.))) |> 
  # median inputation for one participant that left one item unanswered
  dplyr::mutate(dbas16_10 = tidyr::replace_na(dbas16_10, 7)) |> 
  tidyr::pivot_longer(cols = dbas16_1:dbas16_16, names_to = "item", values_to = "score") |> 
  dplyr::mutate(item = ifelse(redcap_event_name == "elegibilidade_arm_1", paste0(item, ".1"), paste0(item, ".2"))) |> 
  dplyr::select(-redcap_event_name) |> 
  tidyr::pivot_wider(names_from = item, values_from = score)


## ----configural-model----------------------------------------------------------------------------------------------------------------------

configural_model <- '

cons.1 =~ dbas16_5.1 + dbas16_7.1 + dbas16_9.1 + dbas16_12.1 + dbas16_16.1
worry.1 =~ dbas16_3.1 + dbas16_4.1 + dbas16_8.1 + dbas16_10.1 + dbas16_11.1 + dbas16_14.1
exp.1 =~ dbas16_1.1 + dbas16_2.1
med.1 =~ dbas16_6.1 + dbas16_13.1 + dbas16_15.1
dbas16_3.1 ~~  dbas16_4.1
dbas16_6.1 ~~ dbas16_15.1

cons.2 =~ dbas16_5.2 + dbas16_7.2 + dbas16_9.2 + dbas16_12.2 + dbas16_16.2
worry.2 =~ dbas16_3.2 + dbas16_4.2 + dbas16_8.2 + dbas16_10.2 + dbas16_11.2 + dbas16_14.2
exp.2 =~ dbas16_1.2 + dbas16_2.2
med.2 =~ dbas16_6.2 + dbas16_13.2 + dbas16_15.2
dbas16_3.2 ~~  dbas16_4.2
dbas16_6.2 ~~ dbas16_15.2

'

longFacNames <- list (CONS = c("cons.1", "cons.2"),
                      WORRY = c("worry.1", "worry.2"),
                      EXP = c("exp.1", "exp.2"),
                      MED = c("med.1", "med.2"))

syntax.config <- measEq.syntax(configural.model = configural_model,
                               data = data_longitudinal,
                               parameterization = "theta",
                               ID.fac = "std.lv", ID.cat = "Wu.Estabrook.2016",
                               longFacNames = longFacNames)
mod.config <- as.character(syntax.config)

configural.fit <- cfa(mod.config, 
                      data = data_longitudinal, 
                      estimator = "MLMV", 
                      std.lv = TRUE)


## ----metric-invariance---------------------------------------------------------------------------------------------------------------------
syntax.metric <- measEq.syntax(configural.model = configural_model,
                               data = data_longitudinal,
                               parameterization = "theta",
                               ID.fac = "std.lv", ID.cat = "Wu.Estabrook.2016",
                               longFacNames = longFacNames,
                               long.equal  = c("thresholds","loadings"))

mod.metric <- as.character(syntax.metric)

fit.metric <- cfa(mod.metric, data = data_longitudinal, estimator = "MLMV",
                  parameterization = "theta", std.lv = TRUE)

conf.met <- compareFit(configural.fit, fit.metric)



## ----scalar-invariance---------------------------------------------------------------------------------------------------------------------
syntax.scalar <- measEq.syntax(configural.model = configural_model,
                               data = data_longitudinal,
                               parameterization = "theta",
                               ID.fac = "std.lv", ID.cat = "Wu.Estabrook.2016",
                               longFacNames = longFacNames,
                               long.equal  = c("thresholds","loadings",
                                               "intercepts"))
mod.scalar <- as.character(syntax.scalar)
fit.scalar <- cfa(mod.scalar, data = data_longitudinal, estimator = "MLMV",
                  parameterization = "theta", std.lv = TRUE)

## ------------------------------------------------------------------------------------------------------------------------------------------
syntax.strict <- measEq.syntax(configural.model = configural_model,
                               data = data_longitudinal,
                               parameterization = "theta",
                               ID.fac = "std.lv", ID.cat = "Wu.Estabrook.2016",
                               longFacNames = longFacNames,
                               long.equal  = c("thresholds","loadings",
                                               "intercepts", "residuals"))
mod.strict <- as.character(syntax.strict)
fit.strict <- cfa(mod.strict, data = data_longitudinal, estimator = "MLMV",
                  parameterization = "theta", std.lv = TRUE)

## ----longitudinal-fit-differences----------------------------------------------------------------------------------------------------------
inv_long <- anova(configural.fit, fit.metric, fit.scalar, fit.strict, 
                  method = "satorra.bentler.2010") |> 
  tibble::as.tibble()

fit_stats_strict <- fitmeasures(fit.strict, 
                                fit.measures = c("rmsea.robust", "cfi.robust", 
                                                 "aic", "bic"))

fit_stats_scalar <- fitmeasures(fit.scalar, 
                                fit.measures = c("rmsea.robust", "cfi.robust", 
                                                 "aic", "bic"))

fit_stats_metric <- fitmeasures(fit.metric, 
                                fit.measures = c("rmsea.robust", "cfi.robust", 
                                                 "aic", "bic"))

fit_stats_configural <- fitmeasures(configural.fit, 
                                fit.measures = c("rmsea.robust", "cfi.robust", 
                                                 "aic", "bic"))

long_fit_diffs <- tibble::enframe(c(
        fit_stats_configural - fit_stats_metric,
        fit_stats_metric - fit_stats_scalar,
        fit_stats_scalar - fit_stats_strict)) |> 
  dplyr::mutate(model = rep(c("metric", "scalar", "strict"), each=4),
                .before = name) |> 
  tidyr::pivot_wider(names_from = "name", values_from = "value")


## ----cfa-model-rep-------------------------------------------------------------------------------------------------------------------------
mod_dbas_mi <- '
cons =~ dbas16_5 + dbas16_7 + dbas16_9 + dbas16_12 
worry =~ dbas16_3 + dbas16_4 + dbas16_8 + dbas16_10 + dbas16_11 + dbas16_14
exp =~ dbas16_1 + dbas16_2
med =~ dbas16_6 + dbas16_13 + dbas16_15
dbas16_3 ~~  dbas16_4
dbas16_6 ~~ dbas16_15
'


## ----configural-model-group----------------------------------------------------------------------------------------------------------------
syntax.config.group <- measEq.syntax(configural.model = mod_dbas_mi,
                               data = dbas[,2:18],
                               group = "group",
                               parameterization = "theta",
                               ID.fac = "std.lv", ID.cat = "Wu.Estabrook.2016")
mod.config.group <- as.character(syntax.config.group)

configural.fit.group <- cfa(mod.config.group, 
                           group = "group",
                           data = dbas[,2:18],
                           estimator = "MLMV",
                           std.lv = TRUE)


## ----metric-invariance-group---------------------------------------------------------------------------------------------------------------
syntax.metric.group <- measEq.syntax(configural.model = mod_dbas_mi,
                               data = dbas[,2:18],
                               group = "group",
                               parameterization = "theta",
                               ID.fac = "std.lv", ID.cat = "Wu.Estabrook.2016",
                               group.equal  = c("loadings"))

mod.metric.group <- as.character(syntax.metric.group)

fit.metric.group <- cfa(mod.metric.group, group = "group",
                           data = dbas[,2:18],
                           estimator = "MLMV",
                           std.lv = TRUE)

conf.met <- compareFit(configural.fit.group, fit.metric.group, argsLRT = list(method = "satorra.bentler.2010"))


## ----partial-inv-model---------------------------------------------------------------------------------------------------------------------
part.inv.syntax <- measEq.syntax(configural.model = mod_dbas_mi,
                               data = dbas[,2:18],
                               group = "group",
                               parameterization = "theta",
                               ID.fac = "std.lv", ID.cat = "Wu.Estabrook.2016",
                               group.equal  = c("loadings"),
                               group.partial = "cons =~ dbas16_14")

part.inv.model <- as.character(part.inv.syntax)

part.inv.fit<- cfa(part.inv.model, group = "group",
                           data = dbas[,2:18],
                           estimator = "MLMV",
                           std.lv = TRUE)

## ----group-fit-differences-----------------------------------------------------------------------------------------------------------------
inv_group <- anova(
  configural.fit.group, fit.metric.group, method="satorra.bentler.2010") |> 
  tibble::as.tibble()

fit_stats_metric_group <- fitmeasures(fit.metric.group, 
                                fit.measures = c("rmsea.robust", "cfi.robust", 
                                                 "aic", "bic"))

fit_stats_configural_group <- fitmeasures(configural.fit.group, 
                                fit.measures = c("rmsea.robust", "cfi.robust", 
                                                 "aic", "bic"))

group_fitdiffs <- tibble::enframe(c(
        fit_stats_configural_group - fit_stats_metric_group)) |> 
  dplyr::mutate(model = c("metric"),
                .before = name) |> 
  tidyr::pivot_wider(names_from = "name", values_from = "value")


## ----fitdiffs_df---------------------------------------------------------------------------------------------------------------------------
fitdiffs_df <- inv_long[, 5:7] |> 
  dplyr::mutate(
    model = c("configural", "metric", "scalar", "strict"),
    .before = dplyr::everything()) |> 
  dplyr::left_join(long_fit_diffs, by = "model") |> 
  rbind(inv_group[, 5:7] |> 
          dplyr::mutate(
    model = c("configural", "metric"),
    .before = dplyr::everything()) |> 
          dplyr::left_join(group_fitdiffs, by = "model"))

fitdiffs_df[1, -1] <- t(as.data.frame(
  fitmeasures(
    configural.fit, fit.measures = c(
      "chisq.scaled", "df.scaled", "pvalue.scaled",
      "rmsea.robust", "cfi.robust", "aic", "bic"))
))

fitdiffs_df[5, -1] <- t(as.data.frame(
  fitmeasures(
    configural.fit.group, fit.measures = c(
      "chisq.scaled", "df.scaled", "pvalue.scaled",
      "rmsea.robust", "cfi.robust", "aic", "bic"))
))



## ----internal-consistency------------------------------------------------------------------------------------------------------------------
omegas <- readRDS("output/omegas.rds") |> 
  tibble::enframe() |> 
  tidyr::unnest_wider(value) |> 
  dplyr::transmute(omega = paste0(rd(est, 3), " [", rd(ci.lower, 3), ", ", rd(ci.upper, 3), "]"))

omega_igi <- readRDS("output/omega_igi.rds")
omega_anx <- readRDS("output/omega_anx.rds")
omega_dep <- readRDS("output/omega_dep.rds")

omegas[6,1] <- paste0(rd(omega_igi$est, 3), " [", rd(omega_igi$ci.lower, 3), ", ", rd(omega_igi$ci.upper, 3), "]")

omegas[7,1] <- paste0(rd(omega_dep$est, 3), " [", rd(omega_dep$ci.lower, 3), ", ", rd(omega_dep$ci.upper, 3), "]")

omegas[8,1] <- paste0(rd(omega_anx$est, 3), " [", rd(omega_anx$ci.lower, 3), ", ", rd(omega_anx$ci.upper, 3), "]")


## ------------------------------------------------------------------------------------------------------------------------------------------
modconv <- '
cons =~ dbas16_5 + dbas16_7 + dbas16_9 + dbas16_12 
worry =~ dbas16_3 + dbas16_4 + dbas16_8 + dbas16_10 + dbas16_11 + dbas16_14
exp =~ dbas16_1 + dbas16_2
med =~ dbas16_6 + dbas16_13 + dbas16_15
dbas16_3 ~~  dbas16_4
dbas16_6 ~~ dbas16_15

isi =~ isi_1a + isi_1b + isi_1c + isi_2 + isi_3 + isi_4 + isi_5

dep =~ hads_2 + hads_4 + hads_6 + hads_8 + hads_10 + hads_12 + hads_14

anx =~ hads_1 + hads_3 + hads_5 + hads_7 + hads_9 + hads_11 + hads_13

'

fitconv <- lavaan::sem(model = modconv,
                        std.lv = TRUE, estimator = 'MLMV', data = arm1)


## ------------------------------------------------------------------------------------------------------------------------------------------
covmat <- cov2cor(lavInspect(fitconv, "cov.lv"))
covmat[upper.tri(covmat, diag = FALSE)] <- NA
covmat_t <- corrr::fashion(covmat, decimals = 3)

covmat_t2 <- covmat_t[,-c(5:7)]

rownames(covmat_t2) <- c("1. Consequences", "2. Worry", "3. Expectations", "4. Medication", "5. ISI", "6. Depression", "7. Anxiety")
colnames(covmat_t2) <- c("1", "2", "3", "4")
covmat_t2

cor_df <- tibble::as_tibble(covmat_t2, rownames="Variable") |> 
  cbind(omegas[-5,])


## ----selected-vars-correlation-------------------------------------------------------------------------------------------------------------
conv.data <- mydata |> 
  dplyr::filter(redcap_event_name == "elegibilidade_arm_1",
                ## exclude previously identified outliers 
                !record_id %in% c(1651, 2015, 2938, 3793)) |>  
  dplyr::filter(!dplyr::if_all(dplyr::starts_with("dbas16_"), ~ is.na(.))) |> 
  # median inputation for one participant that left one item unanswered
  dplyr::mutate(dbas16_10 = tidyr::replace_na(dbas16_10, 7),
                consequences = dbas16_5 + dbas16_7 + dbas16_9 + dbas16_12 + dbas16_16,
                worry = dbas16_3 + dbas16_4 + dbas16_8 + dbas16_10 + dbas16_11 + dbas16_14,
                expectations = dbas16_1 + dbas16_2,
                medication = dbas16_6 + dbas16_13 + dbas16_15,
                dbas = rowSums(across(paste0("dbas16_", 1:16)))) |> 
  dplyr::select(consequences:dbas, hads_depression, hads_anxiety, isi_total)


## ----cor-vars-matrix-----------------------------------------------------------------------------------------------------------------------
cormat <- corrr::correlate(conv.data,method = "spearman") |> 
  corrr::shave(upper=FALSE) |> 
  corrr::fashion(na_print = "")


## ----network-model-------------------------------------------------------------------------------------------------------------------------
network_model <- estimateNetwork(conv.data[, -5], default = "EBICglasso",
                                 weighted = TRUE)



## ----cfa-aux-------------------------------------------------------------------------------------------------------------------------------
mod.isi <- '
isi =~ isi_1a + isi_1b + isi_1c + isi_2 + isi_3 + isi_4 + isi_5
'
mod.hads <- '
dep =~ hads_2 + hads_4 + hads_6 + hads_8 + hads_10 + hads_12 + hads_14

anx =~ hads_1 + hads_3 + hads_5 + hads_7 + hads_9 + hads_11 + hads_13
'

fitISI <- cfa(mod.isi, data=arm1, std.lv = TRUE, estimator = 'DWLS', ordered=TRUE)
fmISI <- fitmeasures(fitISI)

fitHADS <- cfa(mod.hads, data=arm1, std.lv = TRUE, estimator = 'DWLS', ordered=TRUE)
fmHADS <- fitmeasures(fitHADS)


