## ----packages-inv, echo=TRUE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------
library(dplyr)
library(EGAnet)

## ----dataset-ega-inv, warning=FALSE, message=FALSE, echo=FALSE-----------------------------------------------------------------------------
mydata <- read.csv("data/data.csv")

dbas <- mydata |> 
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
    TRUE ~ "bad_sleepers")) |> 
  dplyr::select(record_id, paste0("dbas16_", 1:16), group) |> 
  dplyr::filter(!dplyr::if_all(dplyr::starts_with("dbas16_"), ~ is.na(.))) |> 
  # median inputation for one participant that left one item unanswered
  dplyr::mutate(dbas16_10 = tidyr::replace_na(dbas16_10, 7))

items <- dbas[, c(3, 5:15, 17:18)]

colnames(items) <- c(paste0("D", c(2,4:14, 16)), "group")


## ----set-seed-inv--------------------------------------------------------------------------------------------------------------------------
set.seed(1002)


## ----ega-fs-inv----------------------------------------------------------------------------------------------------------------------------

ega.fs <- EGA(items[ ,1:13], 
              algorithm = "walktrap", 
              model = "glasso",  corr =  "spearman", 
              plot.EGA = FALSE, uni.method =  "LE") 

boot.fs <-readRDS("output/ega/boot.fs.rds")

# dimensionStability 
fs.stability <- dimensionStability(boot.fs)


## ----dim-stab-fs---------------------------------------------------------------------------------------------------------------------------
dim.stab.fs <- tibble::enframe(list(
"glasso" = fs.stability$dimension.stability$structural.consistency)) |> 
  tidyr::unnest(value) |> 
  dplyr::with_groups(name, mutate, dim = row_number(), .before="value")


## ----ega-bs-inv----------------------------------------------------------------------------------------------------------------------------
ega.bs <- EGA(items[which(items$group=="bad_sleepers"),1:13], 
              algorithm = "walktrap", model = "glasso",  corr =  "spearman",
              plot.EGA = FALSE, uni.method =  "LE") 
boot.bs <-readRDS("output/ega/boot.bs.rds")

s.boot.bs <- boot.bs$summary.table

bs.stability <- dimensionStability(boot.bs)


## ----ega-gs-inv----------------------------------------------------------------------------------------------------------------------------
ega.gs <- EGA(items[which(items$group=="good_sleepers"),1:13], 
              algorithm = "walktrap", model = "glasso",  corr =  "spearman",
              plot.EGA = FALSE, uni.method =  "LE") 

boot.gs <-readRDS("output/ega/boot.gs.rds")

s.boot.gs <- boot.gs$summary.table

gs.stability <- dimensionStability(boot.gs)



## ----stability-invariance------------------------------------------------------------------------------------------------------------------
# Descriptive statistics 
desc.stab.inv <- rbind(boot.gs$summary.table, boot.bs$summary.table)
row.names(desc.stab.inv) <- c("Good sleepers", "Bad sleepers")


## ----dimension-frequency-invariance--------------------------------------------------------------------------------------------------------
# Frequency of dimensions 

dim.freq.inv <- rbind(boot.gs$frequency, boot.bs$frequency)
row.names(dim.freq.inv) <- c(rep("Good sleepers", nrow(boot.gs$frequency)), 
                            rep("Bad sleepers", nrow(boot.bs$frequency)))


## ----metric-invariance---------------------------------------------------------------------------------------------------------------------
invariant.items <- invariance(items[,1:13],items$group, corr = "spearman", model = "glasso",
                              algorithm = "walktrap", uni.method = "LE")

# Applying BH-procedure 
adjusted.p <- p.adjust( invariant.items$results$p, method = "BH", 
                        n = length(invariant.items$results$p) ) 
invariant.items$results$p.adj <- adjusted.p 

