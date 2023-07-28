library(lavaan)
library(weights)

## read full dataset
mydata <- read.csv("data/data.csv")

## answers from arm 1 (baseline)
dbas_a1 <- mydata |> 
  dplyr::filter(redcap_event_name == "elegibilidade_arm_1") |>  # dupes
  dplyr::mutate(group = dplyr::case_when(
    dsm_1 == 0 &
      dsm_2 == 0 &
      dsm_3 == 0 &
      dsm_4 == 0 &
      dsm_5 == 0 &
      isi_total < 8 ~ "good_sleepers",
    TRUE ~ "bad_sleepers")) |> 
  dplyr::filter(!dplyr::if_all(dplyr::starts_with("dbas16_"), ~ is.na(.)))

omegas <- readRDS("output/omegas.rds") |> 
  tibble::enframe() |> 
  tidyr::unnest_wider(value) |> 
  dplyr::transmute(omega = paste0(rd(est, 3), " [", rd(ci.lower, 3), ", ", rd(ci.upper, 3), "]"))

mod.isi <- '
isi =~ isi_1a + isi_1b + isi_1c + isi_2 + isi_3 + isi_4 + isi_5
'
mod.hads <- '
dep =~ hads_2 + hads_4 + hads_6 + hads_8 + hads_10 + hads_12 + hads_14

anx =~ hads_1 + hads_3 + hads_5 + hads_7 + hads_9 + hads_11 + hads_13
'

fitISI <- cfa(mod.isi, data=dbas_a1, std.lv = TRUE, estimator = 'DWLS', ordered=TRUE)
fmISI <- fitmeasures(fitISI)
fmISI

fitHADS <- cfa(mod.hads, data=dbas_a1, std.lv = TRUE, estimator = 'DWLS', ordered=TRUE)
fmHADS <- fitmeasures(fitHADS)
