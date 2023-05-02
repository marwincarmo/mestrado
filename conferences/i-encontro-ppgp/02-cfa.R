## ----setup-cfa, warning=FALSE, message=FALSE, echo=FALSE---------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----packages-cfa, echo=TRUE, message=FALSE, warning=FALSE-------------------------------------------------------
library(lavaan)
library(dplyr)
library(semTools)
library(MVN)
library(tidySEM)
library(semPlot)
library(weights)


## ----dataset-cfa, warning=FALSE, message=FALSE, echo=FALSE-------------------------------------------------------
## read full dataset
mydata <- read.csv("../data/data.csv")

## select answers from arm 1
dbas <- mydata |> 
  dplyr::filter(redcap_event_name == "elegibilidade_arm_1",
                ## exclude previously identified outliers and duplicates
                !record_id %in% c(1446, 2152, 2611, 1683, 1588, # outliers
                                  2562, 1766, 2972, 2681, 430, 2559, 3053, 540 , 522 , 3698)) |>  # dupes
  dplyr::mutate(group = dplyr::case_when(
    dsm_1 == 0 &
    dsm_2 == 0 &
    dsm_3 == 0 &
    dsm_4 == 0 &
    dsm_5 == 0 &
    isi_total < 8 ~ "good_sleepers",
    TRUE ~ "bad_sleepers")) |> 
  dplyr::select(record_id, paste0("dbas16_", 1:16), group) |> 
  dplyr::filter(!dplyr::if_all(dplyr::starts_with("dbas16_"), ~ is.na(.))) |> 
  # median inputation for one participant that left one item unanswered
  dplyr::mutate(dbas16_10 = tidyr::replace_na(dbas16_10, 7))


## ----mvn---------------------------------------------------------------------------------------------------------
mvn_res <- MVN::mvn(dbas[,2:17],
    multivariatePlot = "qq",
    showOutliers = TRUE)


## ----univariateNormality, echo=FALSE-----------------------------------------------------------------------------
knitr::kable(mvn_res$univariateNormality)


## ----cfa-model-ho------------------------------------------------------------------------------------------------
mod_dbas_ho <- '
cons =~ dbas16_5 + dbas16_7 + dbas16_9 + dbas16_12 + dbas16_16
worry =~ dbas16_3 + dbas16_4 + dbas16_8 + dbas16_10 + dbas16_11 + dbas16_14
exp =~ dbas16_1 + dbas16_2
med =~ dbas16_6 + dbas16_13 + dbas16_15
gen =~ cons + worry + exp + med
'


fit_mlm_ho <- lavaan::cfa(model = mod_dbas_ho,
                        std.lv = TRUE, estimator = 'MLMV', data = dbas[,2:17])



## ----cfa-model-1-------------------------------------------------------------------------------------------------
mod_dbas <- '
cons =~ dbas16_5 + dbas16_7 + dbas16_9 + dbas16_12 + dbas16_16
worry =~ dbas16_3 + dbas16_4 + dbas16_8 + dbas16_10 + dbas16_11 + dbas16_14
exp =~ dbas16_1 + dbas16_2
med =~ dbas16_6 + dbas16_13 + dbas16_15
'


fit_mlm <- lavaan::cfa(model = mod_dbas,
                        std.lv = TRUE, estimator = 'MLMV', data = dbas[,2:17])
s_mlm <- summary(fit_mlm, fit.measures = TRUE,standardized=TRUE)


## ----compare-fit-ho-4f-------------------------------------------------------------------------------------------
fit_comparison <- anova(fit_mlm_ho, fit_mlm)


## ----robust-fit--------------------------------------------------------------------------------------------------
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


## ----table-results, caption = "An example table caption."--------------------------------------------------------
tidySEM::table_results(fit_mlm) |> 
  knitr::kable()

## ----table-fit---------------------------------------------------------------------------------------------------
table_fit(fit_mlm) |> 
  dplyr::select(-Name) |> 
  tidyr::pivot_longer(cols = dplyr::everything(), names_to = "output", values_to = "value") |> 
  knitr::kable()


## ----mod-indices-------------------------------------------------------------------------------------------------
lavaan::modindices(fit_mlm) |> 
  dplyr::arrange(-mi) |> 
  head(10) |> 
  knitr::kable()


## ----cfa-model-2-------------------------------------------------------------------------------------------------
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


## ----robust-fit-model2-------------------------------------------------------------------------------------------
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


## ----table-results-2---------------------------------------------------------------------------------------------
tidySEM::table_results(fit_mlm_mi) |> 
  knitr::kable()


## ----table-fit-2-------------------------------------------------------------------------------------------------
table_fit(fit_mlm_mi) |> 
  dplyr::select(-Name) |> 
  tidyr::pivot_longer(cols = dplyr::everything(), names_to = "output", values_to = "value") |> 
  knitr::kable()


## ----include=FALSE-----------------------------------------------------------------------------------------------
lay <- get_layout("dbas16_5", "",
                  "dbas16_7", "",
                  "dbas16_9", "Cons",
                  "dbas16_12", "",
                  "dbas16_16", "",
                  "dbas16_3", "",
                  "dbas16_4", "",
                  "dbas16_8", "Worry",
                  "dbas16_10", "",
                  "dbas16_11", "",
                  "dbas16_14", "",
                  "dbas16_1", "",
                  "dbas16_2", "Exp",
                  "dbas16_6", "",
                  "dbas16_13", "Med",
                  "dbas16_15", "",
                  rows = 16)


## ----graph, fig.width=8, fig.height=10, dpi=300------------------------------------------------------------------
semPaths(fit_mlm, "std", edge.label.cex = 1.0, rotation = 2, curvePivot = TRUE)


## ----longitudinal-data, echo=FALSE-------------------------------------------------------------------------------
data_longitudinal <- mydata |> 
  dplyr::filter(redcap_event_name %in% c("elegibilidade_arm_1", "reteste_arm_1"),
                ## exclude previously identified outliers and duplicates
                !record_id %in% c(1446, 2152, 2611, 1683, 1588, # outliers
                                  2562, 1766, 2972, 2681, 430, 2559, 3053, 540 , 522 , 3698)) |>  # dupes
  dplyr::select(record_id, paste0("dbas16_", 1:16), redcap_event_name) |> 
  dplyr::filter(!dplyr::if_all(dplyr::starts_with("dbas16_"), ~ is.na(.))) |> 
  # median inputation for one participant that left one item unanswered
  dplyr::mutate(dbas16_10 = tidyr::replace_na(dbas16_10, 7)) |> 
  tidyr::pivot_longer(cols = dbas16_1:dbas16_16, names_to = "item", values_to = "score") |> 
  dplyr::mutate(item = ifelse(redcap_event_name == "elegibilidade_arm_1", paste0(item, ".1"), paste0(item, ".2"))) |> 
  dplyr::select(-redcap_event_name) |> 
  tidyr::pivot_wider(names_from = item, values_from = score)


## ----configural-model--------------------------------------------------------------------------------------------

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
#summary(configural.fit, fit.measures = TRUE,standardized=TRUE)


## ----metric-invariance-------------------------------------------------------------------------------------------
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
summary(conf.met)

lavTestLRT(configural.fit, fit.metric, method="satorra.bentler.2010")


## ----scalar-invariance-------------------------------------------------------------------------------------------
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

anova(configural.fit, fit.metric, fit.scalar, method = "satorra.bentler.2010")


## ----------------------------------------------------------------------------------------------------------------
fit_stats_scalar <- fitmeasures(fit.scalar, fit.measures = c("rmsea", "rmsea.ci.lower", 
                                                                          "rmsea.ci.upper", "cfi",
                                                                          "rmsea.robust", "rmsea.ci.lower.robust", 
                                                                          "rmsea.ci.upper.robust", "tli.robust", 
                                                                          "cfi.robust", "srmr_bentler"))

fit_stats_metric <- fitmeasures(fit.metric, fit.measures = c("rmsea", "rmsea.ci.lower", 
                                                                          "rmsea.ci.upper", "cfi",
                                                                          "rmsea.robust", "rmsea.ci.lower.robust", 
                                                                          "rmsea.ci.upper.robust", "tli.robust", 
                                                                          "cfi.robust", "srmr_bentler"))

metric_scalar <- fit_stats_metric - fit_stats_scalar
fit_diffs <- t(data.frame(metric_scalar))[, c("rmsea", "cfi", "rmsea.robust", "cfi.robust", "srmr_bentler")]
fit_diffs


## ----eval=FALSE, include=FALSE-----------------------------------------------------------------------------------
## round(cbind(configural.error=inspect(configural.fit, 'fit.measures'), metric=inspect(fit.metric, 'fit.measures'), scalar=inspect(fit.scalar, 'fit.measures'),3))


## ----------------------------------------------------------------------------------------------------------------
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

anova(configural.fit, fit.metric, fit.scalar, fit.strict, method = "satorra.bentler.2010")


## ----configural-model-group--------------------------------------------------------------------------------------
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


## ----metric-invariance-group-------------------------------------------------------------------------------------
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

summary(conf.met)

lavTestLRT(configural.fit.group, fit.metric.group, method="satorra.bentler.2010")


## ----inspecting-metric-inv---------------------------------------------------------------------------------------

lavaan::modindices(fit.metric.group) |> 
  dplyr::arrange(-mi) |> 
  #dplyr::filter(op == "=~") |> 
  head(10)


## ----partial-inv-model-------------------------------------------------------------------------------------------
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

lavTestLRT(configural.fit.group, part.inv.fit, method="satorra.bentler.2010")


## ----eval=FALSE, include=TRUE------------------------------------------------------------------------------------
## dbas_factors <- list(
##   cons = dplyr::select(dbas, c(dbas16_5, dbas16_7, dbas16_9, dbas16_12, dbas16_16)),
##   worry = dplyr::select(dbas, c(dbas16_3, dbas16_4, dbas16_8, dbas16_10, dbas16_11, dbas16_14)),
##   exp = dplyr::select(dbas, c(dbas16_1, dbas16_2)),
##   med = dplyr::select(dbas, c(dbas16_6, dbas16_13, dbas16_15)),
##   total = dbas[,2:17]
## )


## ----eval=FALSE, include=TRUE------------------------------------------------------------------------------------
## future::plan(future::multisession)
## alphas <- progressr::with_progress({
##   factors <- 1:5
##   p <- progressr::progressor(length(factors))
##   furrr::future_map(
##     dbas_factors,
##     ~MBESS::ci.reliability(.x, type = "alpha", interval.type = "bca"),
##     prog = p,
##     .options = furrr_options(seed = 123)
##   )
## })
## 
## saveRDS(alphas, "../output/alphas.rds")


## ----eval=FALSE, include=TRUE------------------------------------------------------------------------------------
## library(parallel)
## cl <- parallel::makeCluster(detectCores())
## # Activate cluster for foreach library
## doParallel::registerDoParallel(cl)
## omegas <- purrr::map(dbas_factors,
##          ~MBESS::ci.reliability(.x, type="hierarchical", interval.type = "perc", B = 1000))
## # Stop cluster to free up resources
## parallel::stopCluster(cl)
## saveRDS(omegas, "../output/omegas.rds")


## ----internal-consistency----------------------------------------------------------------------------------------
omegas <- readRDS("../output/omegas.rds") |> 
  tibble::enframe() |> 
  tidyr::unnest_wider(value) |> 
  dplyr::transmute(omega = paste0(rd(est, 3), " [", rd(ci.lower, 3), ", ", rd(ci.upper, 3), "]"))


alphas <- readRDS("../output/alphas.rds") |> 
  tibble::enframe() |> 
  tidyr::unnest_wider(value) |> 
  dplyr::transmute(alpha = paste0(rd(est, 3), " [", rd(ci.lower, 3), ", ", rd(ci.upper, 3), "]"))


## ----cor-matrix--------------------------------------------------------------------------------------------------
covmat <- cov2cor(lavInspect(fit_mlm_mi, what = "est")$psi)
covmat[upper.tri(covmat, diag = TRUE)] <- NA

covmat_t <- t(corrr::fashion(covmat, decimals = 3))[,-1]
rownames(covmat_t) <- c("1. Consequences", "2. Worry/Helplessness", "3. Expectations", "4. Medication")
colnames(covmat_t) <- c("2", "3", "4")

cor_df <- tibble::as_tibble(covmat_t, rownames="factor") |> 
  cbind(omegas[-5,], alphas[-5,])




## ----selected-vars-correlation-----------------------------------------------------------------------------------
conv.data <- mydata |> 
  dplyr::filter(redcap_event_name == "elegibilidade_arm_1",
                ## exclude previously identified outliers and duplicates
                !record_id %in% c(1446, 2152, 2611, 1683, 1588, # outliers
                                  2562, 1766, 2972, 2681, 430, 2559, 3053, 540 , 522 , 3698)) |>  
  dplyr::filter(!dplyr::if_all(dplyr::starts_with("dbas16_"), ~ is.na(.))) |> 
  # median inputation for one participant that left one item unanswered
  dplyr::mutate(dbas16_10 = tidyr::replace_na(dbas16_10, 7),
                consequences = dbas16_5 + dbas16_7 + dbas16_9 + dbas16_12 + dbas16_16,
                worry = dbas16_3 + dbas16_4 + dbas16_8 + dbas16_10 + dbas16_11 + dbas16_14,
                expectations = dbas16_1 + dbas16_2,
                medication = dbas16_6 + dbas16_13 + dbas16_15,
                dbas = rowSums(across(paste0("dbas16_", 1:16)))) |> 
  dplyr::select(consequences:dbas, hads_depression, hads_anxiety, isi_total)


## ----cor-vars-matrix---------------------------------------------------------------------------------------------
corrr::correlate(conv.data) |> 
  corrr::shave() |> 
  corrr::fashion(na_print = "")

