## ----packages-nwk, echo=TRUE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------
library(dplyr)
library(psychTools)
library(EGAnet)
library(qgraph)
library(tidymodels)
library(ggpubr)


## ----dataset-ega, warning=FALSE, message=FALSE, echo=FALSE---------------------------------------------------------------------------------
## read full dataset
mydata <- read.csv("data/data.csv")

## select answers from arm 1
dbas <- mydata |> 
  dplyr::filter(redcap_event_name == "elegibilidade_arm_1",
                ## exclude previously identified outliers 
                !record_id %in% c(1651, 2015, 2938, 3793)) |>
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

items <- dbas[, 2:17]

colnames(items) <- paste0("D", 1:16)


## ----sample-split--------------------------------------------------------------------------------------------------------------------------
set.seed(1354789)

dbas_initial_split <- dbas |>  initial_split(1/2, strata=group)

dbas_train <- training(dbas_initial_split)[,2:17]
dbas_test <- testing(dbas_initial_split)[,2:17]

colnames(dbas_train) <- colnames(dbas_test) <- paste0("D", 1:16)


## ----labels--------------------------------------------------------------------------------------------------------------------------------
items_labels <- tibble::tribble(
  ~items, ~description,
  "D1" , "I need 8 hours of sleep to feel refreshed and function well during the day.",
  "D2" , "When I don't get proper amount of sleep on a given night, I need to catch up on the next day by napping or on the next night by sleeping longer.",
  "D3" ,"I am concerned that chronic insomnia may have serious consequences on my physical health.",
  "D4" ,"I am worried that I may lose control over my abilities to sleep.",
  "D5" ,"After a poor night’s sleep, I know that it will interfere with my daily activities on the next day.",
  "D6" ,"In order to be alert and function well during the day, I believe I would be better off taking a sleeping pill rather than having a poor night’s sleep.",
  "D7" ,"When I feel irritable, depressed, or anxious during the day, it is mostly because I did not sleep well the night before.",
  "D8" ,"When I sleep poorly on one night, I know it will disturb my sleep schedule for the whole week.",
  "D9" ,"Without an adequate night’s sleep, I can hardly function the next day.",
  "D10" ,"I can’t ever predict whether I’ll have a good or poor night’s sleep.",
  "D11" ,"I have little ability to manage the negative consequences of disturbed sleep.",
  "D12" ,"When I feel tired, have no energy, or just seem not to function well during the day, it is generally because I did not sleep well the night before.",
  "D13" ,"I believe insomnia is essentially the result of a chemical imbalance.",
  "D14" ,"I feel insomnia is ruining my ability to enjoy life and prevents me from doing what I want.",
  "D15" ,"Medication is probably the only solution to sleeplessness.",
  "D16" ,"I avoid or cancel obligations (social, family) after a poor night’s sleep."
  
)


## ----redund-items--------------------------------------------------------------------------------------------------------------------------
redund <- EGAnet::UVA(data=dbas_train, method="wTO", type="threshold",  corr =  "spearman",
                    reduce=FALSE, adhoc=FALSE, plot.redundancy=TRUE) 
redund.items <- redund$redundancy$redundant



## ----ega-with-redundancy-------------------------------------------------------------------------------------------------------------------
# EGA (with redundancy) 
ega.wr.tmfg <- EGA(dbas_train, algorithm = "walktrap", model = "TMFG",  corr =  "spearman",
              plot.EGA = FALSE, uni.method = "LE", key = items_labels) 

ega.wr.glasso <- EGA(dbas_train, algorithm = "walktrap", model = "glasso", corr =  "spearman",
              plot.EGA = FALSE, uni.method = "LE", key = items_labels, plot.args = list()) 


## ----dim-tab-wr----------------------------------------------------------------------------------------------------------------------------
dimtab.wr <- ega.wr.tmfg$dim.variables |> 
  dplyr::left_join(ega.wr.glasso$dim.variables, by = "items", suffix = c("_tmfg", "_glasso"))
dimtab.wr


## ----tefi----------------------------------------------------------------------------------------------------------------------------------

dbas.cor <- abs(qgraph::cor_auto(dbas_train))

tefi_tmfg <- tefi(data=dbas.cor, structure=ega.wr.tmfg$wc) 

tefi_glasso <- tefi(data=dbas.cor, structure=ega.wr.glasso$wc) 


## ----bootEGA-wr----------------------------------------------------------------------------------------------------------------------------


boot.tmfg.wr <-readRDS("output/ega/bootTMFGwr.rds")
boot.glasso.wr <-readRDS("output/ega/bootGLASSOwr.rds")


## ----stability-descriptive-----------------------------------------------------------------------------------------------------------------
# Descriptive statistics 
desc.stab.wr <- rbind(boot.tmfg.wr$summary.table, boot.glasso.wr$summary.table)
row.names(desc.stab.wr) <- c("TMFG", "GLASSO")


## ----dimension-frequency-------------------------------------------------------------------------------------------------------------------
# Frequency of dimensions 

dim.freq.wr <- rbind(boot.tmfg.wr$frequency, boot.glasso.wr$frequency)
row.names(dim.freq.wr) <- c(rep("TMFG", nrow(boot.tmfg.wr$frequency)), 
                            rep("GLASSO", nrow(boot.glasso.wr$frequency)))


## ----structural-consistency-wr-------------------------------------------------------------------------------------------------------------
# structural consistency
dim.stab.wr.tmfg <- dimensionStability(boot.tmfg.wr)
dim.stab.wr.glasso <- dimensionStability(boot.glasso.wr)


## ----dim-stab-wr---------------------------------------------------------------------------------------------------------------------------
dim.stab.wr <- tibble::enframe(list(
  "tmfg" = dim.stab.wr.tmfg$dimension.stability$structural.consistency,
"glasso" = dim.stab.wr.glasso$dimension.stability$structural.consistency)) |> 
  tidyr::unnest(value) |> 
  dplyr::with_groups(name, mutate, dim = row_number(), .before="value")
dim.stab.wr


## ----ega-d15-------------------------------------------------------------------------------------------------------------------------------
# EGA without D15
ega.d15.tmfg <- EGA(dbas_train[,-c(15)], algorithm = "walktrap", model = "TMFG", corr = "spearman",
              plot.EGA = FALSE, uni.method = "LE", key = items_labels) 

ega.d15.glasso <- EGA(dbas_train[,-c(15)], algorithm = "walktrap", model = "glasso", corr = "spearman", 
              plot.EGA = FALSE, uni.method = "LE", key = items_labels) 


## ----dim-tab-d15---------------------------------------------------------------------------------------------------------------------------
dimtab.d15 <- ega.d15.tmfg$dim.variables |> 
  dplyr::left_join(ega.d15.glasso$dim.variables, by = "items", suffix = c("_tmfg", "_glasso"))
dimtab.d15


## ----tefi-d15------------------------------------------------------------------------------------------------------------------------------

dbas.cor.d15 <- abs(qgraph::cor_auto(dbas_train[,-c(15)]))

tefi_tmfg.d15 <- tefi(data=dbas.cor.d15, structure=ega.d15.tmfg$wc) 

tefi_glasso.d15 <- tefi(data=dbas.cor.d15, structure=ega.d15.glasso$wc) 


## ----bootEGA-d15---------------------------------------------------------------------------------------------------------------------------


boot.tmfg.d15 <-readRDS("output/ega/bootTMFGd15.rds")
boot.glasso.d15 <-readRDS("output/ega/bootGLASSOd15.rds")


## ----stability-descriptive-d15-------------------------------------------------------------------------------------------------------------
# Descriptive statistics 
desc.stab.d15 <- rbind(boot.tmfg.d15$summary.table, boot.glasso.d15$summary.table)
row.names(desc.stab.d15) <- c("TMFG", "GLASSO")


## ----dimension-frequency-d15---------------------------------------------------------------------------------------------------------------
# Frequency of dimensions 

dim.freq.d15 <- rbind(boot.tmfg.d15$frequency, boot.glasso.d15$frequency)
row.names(dim.freq.d15) <- c(rep("TMFG", nrow(boot.tmfg.d15$frequency)), 
                            rep("GLASSO", nrow(boot.glasso.d15$frequency)))

## ----structural-consistency-d15------------------------------------------------------------------------------------------------------------
# structural consistency
dim.stab.d15.tmfg <- dimensionStability(boot.tmfg.d15)
dim.stab.d15.glasso <- dimensionStability(boot.glasso.d15)


## ----dim-stab-d15--------------------------------------------------------------------------------------------------------------------------
dim.stab.d15 <- tibble::enframe(list(
  "tmfg" = dim.stab.d15.tmfg$dimension.stability$structural.consistency,
"glasso" = dim.stab.d15.glasso$dimension.stability$structural.consistency)) |> 
  tidyr::unnest(value) |> 
  dplyr::with_groups(name, mutate, dim = row_number(), .before="value")


## ----ega-d3--------------------------------------------------------------------------------------------------------------------------------
# EGA without D3
ega.d3.tmfg <- EGA(dbas_train[,-c(15,3)], algorithm = "walktrap", model = "TMFG", corr = "spearman",
              plot.EGA = FALSE, uni.method = "LE", key = items_labels) 

ega.d3.glasso <- EGA(dbas_train[,-c(15,3)], algorithm = "walktrap", model = "glasso", corr = "spearman",
              plot.EGA = FALSE, uni.method = "LE", key = items_labels) 

## ----dim-tab-d3----------------------------------------------------------------------------------------------------------------------------
dimtab.d3 <- ega.d3.tmfg$dim.variables |> 
  dplyr::left_join(ega.d3.glasso$dim.variables, by = "items", suffix = c("_tmfg", "_glasso"))


## ----tefi-wo-d3----------------------------------------------------------------------------------------------------------------------------

dbasD3.cor <- abs(qgraph::cor_auto(dbas_train[,-c(15,3)]))

tefi_tmfgD3 <- tefi(data=dbasD3.cor, structure=ega.d3.tmfg$wc) 

tefi_glassoD3 <- tefi(data=dbasD3.cor, structure=ega.d3.glasso$wc) 


## ----bootEGA-d3----------------------------------------------------------------------------------------------------------------------------


boot.tmfg.d3 <-readRDS("output/ega/bootTMFGd3.rds")
boot.glasso.d3 <-readRDS("output/ega/bootGLASSOd3.rds")

## ----stability-descriptive-d3--------------------------------------------------------------------------------------------------------------
# Descriptive statistics 
desc.stab.d3 <- rbind(boot.tmfg.d3$summary.table, boot.glasso.d3$summary.table)
row.names(desc.stab.d3) <- c("TMFG", "GLASSO")


## ----dimension-frequency-d3----------------------------------------------------------------------------------------------------------------
# Frequency of dimensions 

dim.freq.d3 <- rbind(boot.tmfg.d3$frequency, boot.glasso.d3$frequency)
row.names(dim.freq.d3) <- c(rep("TMFG", nrow(boot.tmfg.d3$frequency)), 
                            rep("GLASSO", nrow(boot.glasso.d3$frequency)))


## ----structural-consistency-d3-------------------------------------------------------------------------------------------------------------
# structural consistency
dim.stab.d3.tmfg <- dimensionStability(boot.tmfg.d3)
dim.stab.d3.glasso <- dimensionStability(boot.glasso.d3)


## ----dim-stab-d3---------------------------------------------------------------------------------------------------------------------------
dim.stab.d3 <- tibble::enframe(list(
  "tmfg" = dim.stab.d3.tmfg$dimension.stability$structural.consistency,
"glasso" = dim.stab.d3.glasso$dimension.stability$structural.consistency)) |> 
  tidyr::unnest(value) |> 
  dplyr::with_groups(name, mutate, dim = row_number(), .before="value")


## ----ega-d1-tmfg---------------------------------------------------------------------------------------------------------------------------

ega.d1.tmfg <- EGA(dbas_train[,-c(15, 3, 1)], algorithm = "walktrap", model = "TMFG", corr = "spearman",
              plot.EGA = FALSE, uni.method = "LE", key = items_labels) 

ega.d1.glasso <- EGA(dbas_train[,-c(15, 3, 1)], algorithm = "walktrap", model = "glasso", corr = "spearman",
              plot.EGA = FALSE, uni.method = "LE", key = items_labels) 


## ----dim-tab-d1----------------------------------------------------------------------------------------------------------------------------
dimtab.d1 <- ega.d1.tmfg$dim.variables |> 
  dplyr::left_join(ega.d1.glasso$dim.variables, by = "items", suffix = c("_tmfg", "_glasso"))


## ----tefi-wo-d1----------------------------------------------------------------------------------------------------------------------------

dbas.d1.cor <- abs(qgraph::cor_auto(dbas_train[,-c(15, 3, 1)]))

tefi.tmfg.d1 <- tefi(data=dbas.d1.cor, structure=ega.d1.tmfg$wc) 

tefi.glasso.d1 <- tefi(data=dbas.d1.cor, structure=ega.d1.glasso$wc)

## ----bootEGA-d1----------------------------------------------------------------------------------------------------------------------------

boot.tmfg.d1 <-readRDS("output/ega/bootTMFGd1.rds")
boot.glasso.d1 <-readRDS("output/ega/bootGLASSOd1.rds")


## ----stability-descriptive-d1--------------------------------------------------------------------------------------------------------------
# Descriptive statistics 
desc.stab.d1 <- rbind(boot.tmfg.d1$summary.table, boot.glasso.d1$summary.table)
row.names(desc.stab.d1) <- c("TMFG", "GLASSO")


## ----dimension-frequency-d1----------------------------------------------------------------------------------------------------------------
# Frequency of dimensions 

dim.freq.d1 <- rbind(boot.tmfg.d1$frequency, boot.glasso.d1$frequency)
row.names(dim.freq.d1) <- c(rep("TMFG", nrow(boot.tmfg.d1$frequency)), 
                            rep("GLASSO", nrow(boot.glasso.d1$frequency)))


## ----structural-consistency-d1-------------------------------------------------------------------------------------------------------------
# structural consistency
dim.stab.d1.tmfg <- dimensionStability(boot.tmfg.d1)
dim.stab.d1.glasso <- dimensionStability(boot.glasso.d1)


## ----dim-stab-d1---------------------------------------------------------------------------------------------------------------------------
dim.stab.d1 <- tibble::enframe(list(
  "tmfg" = dim.stab.d1.tmfg$dimension.stability$structural.consistency,
"glasso" = dim.stab.d1.glasso$dimension.stability$structural.consistency)) |> 
  tidyr::unnest(value) |> 
  dplyr::with_groups(name, mutate, dim = row_number(), .before="value")


## ----redund-items-confirmation-------------------------------------------------------------------------------------------------------------
redund_test <- EGAnet::UVA(data=dbas_test, method="wTO", type="threshold",  corr =  "spearman",
                    reduce=FALSE, adhoc=FALSE, plot.redundancy=TRUE) 
redund.items.test <- redund_test$redundancy$redundant


## ----ega-tmfg-test-------------------------------------------------------------------------------------------------------------------------

ega.test.tmfg <- EGA(dbas_test[,-c(15, 3, 1)], algorithm = "walktrap", model = "TMFG", corr = "spearman",
              plot.EGA = FALSE, key = items_labels) 

ega.test.glasso <- EGA(dbas_test[,-c(15, 3, 1)], algorithm = "walktrap", model = "glasso", corr = "spearman",
              plot.EGA = FALSE, key = items_labels) 


## ----dim-tab-test--------------------------------------------------------------------------------------------------------------------------
dimtab.test <- ega.test.tmfg$dim.variables |> 
  dplyr::left_join(ega.test.glasso$dim.variables, by = "items", suffix = c("_tmfg", "_glasso"))


## ----tefi-test-----------------------------------------------------------------------------------------------------------------------------

dbas.test.cor <- abs(qgraph::cor_auto(dbas_test[,-c(15, 3, 1)]))

tefi.tmfg.test <- tefi(data=dbas.test.cor, structure=ega.test.tmfg$wc) 

tefi.glasso.test <- tefi(data=dbas.test.cor, structure=ega.test.glasso$wc) 

## ----bootEGA-test--------------------------------------------------------------------------------------------------------------------------

boot.tmfg.test <-readRDS("output/ega/bootTMFGtest.rds")
boot.glasso.test <-readRDS("output/ega/bootGLASSOtest.rds")

## ----stability-descriptive-test------------------------------------------------------------------------------------------------------------
# Descriptive statistics 
desc.stab.test <- rbind(boot.tmfg.test$summary.table, boot.glasso.test$summary.table)
row.names(desc.stab.test) <- c("TMFG", "GLASSO")

## ----dimension-frequency-test--------------------------------------------------------------------------------------------------------------
# Frequency of dimensions 

dim.freq.test <- rbind(boot.tmfg.test$frequency, boot.glasso.test$frequency)
row.names(dim.freq.test) <- c(rep("TMFG", nrow(boot.tmfg.test$frequency)), 
                            rep("GLASSO", nrow(boot.glasso.test$frequency)))

## ----structural-consistency-test-----------------------------------------------------------------------------------------------------------
# structural consistency
dim.stab.test.tmfg <- dimensionStability(boot.tmfg.test)
dim.stab.test.glasso <- dimensionStability(boot.glasso.test)


## ----dim-stab-test-------------------------------------------------------------------------------------------------------------------------
dim.stab.test <- tibble::enframe(list(
  "tmfg" = dim.stab.test.tmfg$dimension.stability$structural.consistency,
"glasso" = dim.stab.test.glasso$dimension.stability$structural.consistency)) |> 
  tidyr::unnest(value) |> 
  dplyr::with_groups(name, mutate, dim = row_number(), .before="value")


## ----cfa-models----------------------------------------------------------------------------------------------------------------------------
mod_theory <- '
cons =~ D5 + D7 + D9 + D12 + D16
worry =~ D3 + D4 + D8 + D10 + D11 + D14
exp =~ D1 + D2
med =~ D6 + D13 + D15
'
mod_mi <- '
cons =~ D5 + D7 + D9 + D12 + D16
worry =~ D3 + D4 + D8 + D10 + D11 + D14
exp =~ D1 + D2
med =~ D6 + D13 + D15
D3 ~~  D4
D6 ~~ D15
'

mod_glasso <- '
F1 =~ D2 + D5 + D7 + D8 + D9 + D12 + D16
F2 =~ D4 + D6 + D10 + D11 + D13 + D14
'

  
fit_theory <- lavaan::cfa(model = mod_theory,
                        std.lv = TRUE, estimator = 'MLMV', data = dbas_test)

fit_mi <- lavaan::cfa(model = mod_mi,
                        std.lv = TRUE, estimator = 'MLMV', data = dbas_test)

fit_glasso <- lavaan::cfa(model = mod_glasso,
                        std.lv = TRUE, estimator = 'MLMV', data = dbas_test)



## ----LRT-models----------------------------------------------------------------------------------------------------------------------------
theory_glasso_comparison <- lavaan::lavTestLRT(fit_glasso, fit_theory, method="satorra.bentler.2010")
mi_glasso_comparison <- lavaan::lavTestLRT(fit_glasso, fit_mi, method="satorra.bentler.2010")


## ----fit-measures-comparison---------------------------------------------------------------------------------------------------------------
fm_theory <- semTools::fitmeasures(fit_theory, 
                         fit.measures = c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "cfi",
                                          "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "tli.robust",
                                          "cfi.robust", "srmr_bentler"))

fm_mi <- semTools::fitmeasures(fit_mi, 
                         fit.measures = c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "cfi",
                                          "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "tli.robust",
                                          "cfi.robust", "srmr_bentler"))

fm_glasso <- semTools::fitmeasures(fit_glasso, 
                         fit.measures = c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "cfi",
                                          "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "tli.robust",
                                          "cfi.robust", "srmr_bentler"))

## ----fit-differences-----------------------------------------------------------------------------------------------------------------------
theory_glasso <- fm_theory - fm_glasso
theory_glasso_diffs <- t(data.frame(theory_glasso))[, c("rmsea", "cfi", "rmsea.robust", "cfi.robust", "srmr_bentler")]


## ----ega-lv-factors------------------------------------------------------------------------------------------------------------------------
ega_factors <- list(
  dim1 = items[, c(2,5,7,8,9,12,16)],
  dim2 = items[, c(4,6,10,11,13,14)]
)


## ----ega-lv-reliability--------------------------------------------------------------------------------------------------------------------

omega_ega <- readRDS("output/ega/omega_ega.rds")

