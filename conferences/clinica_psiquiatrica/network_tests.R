library(dplyr)
library(ggplot2)
library(bootnet)

raw_data <- readr::read_csv("G:/Documentos/ProjetosR/ASONO/act_insonia/dados/base_completa_031422.csv")

dados_completos <- raw_data |> 
  tidyr::fill(idade, .direction = "down") %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(sexo = ifelse(sexo == 1, "F", "M")) %>%
  dplyr::filter(!is.na(escala_breve_de_aceitao_do_sono_complete)) |> 
  dplyr::select(record_id, redcap_event_name, sexo, idade, igi_escore, ehad_depressao_escore,
                ehad_ansiedade_escore, aaq_score, dbas_score, contains("spaq_")) |> 
  dplyr::filter(redcap_event_name == "elegibilidade_arm_1") |> 
  dplyr::mutate(across(spaq_5:spaq_8, ~ 6 - .x)) %>% 
  dplyr::mutate(spaq_score = select(., spaq_1:spaq_8) %>% rowSums()) %>% 
  dplyr::select(-c(spaq_1:spaq_8))

GGally::ggpairs(dados_completos)

# DBAS Por item ----------------------------------------------------------------

base <- raw_data |> 
  dplyr::filter(redcap_event_name == "elegibilidade_arm_1") |> 
  dplyr::select(igi_escore, dbas_1:dbas_16) |> 
  dplyr::rename(igi = igi_escore)

fatores <- c("IGI", rep("Expectations", 2), rep("Worry", 2), "Consequences", "Medication",
             "Consequences", "Worry", "Consequences", rep("Worry", 2), "Consequences",
             "Medication", "Worry", "Medication", "Consequences")

Network <- estimateNetwork(base, default = "EBICglasso", weighted = TRUE)

netplot <- plot(Network, layout = "spring",
     label.cex=.70, label.scale=F, theme = "colorblind",
     nodeNames = colnames(base), groups = fatores,
     legend.mode="style2", legend.cex=.32,
     vsize = 6, esize = 15, details = F)

library("qgraph")
centralityPlot(Network, scale = c("z-scores"), 
               include = c("Strength","Closeness","Betweenness"), 
               theme_bw = TRUE, print = TRUE,
               verbose = TRUE, weighted = TRUE, 
               decreasing = T)

#Acuracia das arestas
boot1 <- bootnet(Network, nBoots = 1000)

plot(boot1, labels = FALSE,
     order = "sample")

#Estabilidade da centralidade
boot2 <- bootnet(Network, nBoots = 1000, type = "case", statistics = c("strength", "closeness", "betweenness"))

plot(boot2, statistics= c("strength","closeness","betweenness"))    

#CS-coefficient
corStability(boot2)

# weights matrix

tibble::as_tibble(Network$graph)

# DBAS Por fatores -------------------------------------------------------------


base2 <- raw_data |> 
  dplyr::filter(redcap_event_name == "elegibilidade_arm_1") |> 
  dplyr::select(igi_escore, dbas_1:dbas_16) |> 
  dplyr::mutate(cons = c(dbas_5 + dbas_7 + dbas_9 + dbas_12 + dbas_16),
               worry = c(dbas_3 + dbas_4 + dbas_8 + dbas_10 + dbas_11 + dbas_14),
               exp = c(dbas_1 + dbas_2),
               med = (dbas_6 + dbas_13 + dbas_15)
               ) |> 
  dplyr::select(-c(dplyr::starts_with("dbas_"))) |> 
  dplyr::rename(igi = igi_escore)

fatores <- c("IGI", rep("Expectations", 2), rep("Worry", 2), "Consequences", "Medication",
             "Consequences", "Worry", "Consequences", rep("Worry", 2), "Consequences",
             "Medication", "Worry", "Medication", "Consequences")

Network <- estimateNetwork(base2, default = "EBICglasso", weighted = TRUE)

plot(Network, layout = "spring",
     label.cex=.70, label.scale=F, theme = "colorblind",
     nodeNames = colnames(base2),
     legend.mode="style2", legend.cex=.32,
     vsize = 6, esize = 15, details = F)

library("qgraph")
centralityPlot(Network, scale = c("z-scores"), 
               include = c("Strength","Closeness","Betweenness"), 
               theme_bw = TRUE, print = TRUE,
               verbose = TRUE, weighted = TRUE, 
               decreasing = T)

#Acuracia das arestas
boot1 <- bootnet(Network, nBoots = 1000)

plot(boot1, labels = FALSE,
     order = "sample")

#Estabilidade da centralidade
boot2 <- bootnet(Network, nBoots = 1000, type = "case", statistics = c("strength", "closeness", "betweenness"))

plot(boot2, statistics= c("strength","closeness","betweenness"))    

#CS-coefficient
corStability(boot2)


# SPAQ --------------------------------------------------------------------

base_spaq <- raw_data |> 
  dplyr::filter(redcap_event_name == "elegibilidade_arm_1") |> 
  dplyr::mutate(dplyr::across(spaq_5:spaq_8, ~ 6 - .x)) |> 
  dplyr::select(igi_escore, spaq_1:spaq_8) |> 
  dplyr::rename(igi = igi_escore)

NetworkSPAQ <- estimateNetwork(base_spaq, default = "EBICglasso", weighted = TRUE)

plot(NetworkSPAQ, layout = "spring",
     label.cex=.70, label.scale=F, theme = "colorblind",
     nodeNames = colnames(base_spaq),
     legend.mode="style2", legend.cex=.32,
     vsize = 6, esize = 15, details = F)

library("qgraph")
centralityPlot(NetworkSPAQ, scale = c("z-scores"), 
               include = c("Strength","Closeness","Betweenness"), 
               theme_bw = TRUE, print = TRUE,
               verbose = TRUE, weighted = TRUE, 
               decreasing = T)

#Acuracia das arestas
boot1 <- bootnet(NetworkSPAQ, nBoots = 1000)

plot(boot1, labels = FALSE,
     order = "sample")

#Estabilidade da centralidade
boot2 <- bootnet(NetworkSPAQ, nBoots = 1000, type = "case", statistics = c("strength", "closeness", "betweenness"))

plot(boot2, statistics= c("strength","closeness","betweenness"))    

#CS-coefficient
corStability(boot2)


