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

summary(boot1)

plot(boot1, labels = FALSE,
     order = "sample")

#Estabilidade da centralidade
boot2 <- bootnet(NetworkSPAQ, nBoots = 1000, type = "case", statistics = c("strength", "closeness", "betweenness"))

plot(boot2, statistics= c("strength","closeness","betweenness"))    

#CS-coefficient
corStability(boot2)

## legenda spaq

base_spaq <- raw_data |> 
  dplyr::filter(redcap_event_name == "elegibilidade_arm_1") |> 
  dplyr::mutate(dplyr::across(spaq_5:spaq_8, ~ 6 - .x)) |> 
  dplyr::select(igi_escore, spaq_1:spaq_8) |> 
  dplyr::rename(isi = igi_escore,
                Q1 = spaq_1, Q2 = spaq_2,
                Q3 = spaq_3, Q4 = spaq_4,
                Q5 = spaq_5, Q6 = spaq_6,
                Q7 = spaq_7, Q8 = spaq_8
  )

fatores <- c("ISI", rep("Activities Engagement", 4), rep("Willingness", 4))
names <- c("ISI", paste0("Q", 1:8))
legend <- c("Insomnia Severity Index",
            "Although things have changed, I am living a \n normal life despite my sleeping problems",
            "I lead a full life even though I have sleeping problems",
            "My life is going well, even though I have sleeping problems",
            "Despite the sleeping problems, \n I am now sticking to a certain course in my life",
            "Keeping my sleeping problems under control takes first priority",
            "I need to concentrate on getting rid of my sleeping problems",
            "It's important to keep on fighting these sleeping problems",
            "My thoughts and feelings about my sleeping problems must change \n before I can take important steps in my life"
)

NetworkSPAQ <- estimateNetwork(base_spaq, default = "EBICglasso", weighted = TRUE)

plot(NetworkSPAQ, layout = "spring",
     label.scale=F, theme = "colorblind",
     nodeNames = legend, groups = fatores, legend.cex = 0.6,
     layoutScale = c(1,1), layoutOffset = c(0,3), GLratio = .2,
     vsize = 10,details = F, legend = TRUE)

png("G:/Documentos/ProjetosR/mestrado/conferences/clinica_psiquiatrica/spaq_legend.png", 
    width = 8, height = 6, units = 'in', res = 300)
plot(NetworkSPAQ, layout = "spring",
     label.scale=F, theme = "colorblind",
     nodeNames = legend, groups = fatores,
     layoutScale = c(1,1), layoutOffset = c(0,3), GLratio = .2,
     vsize = 10,details = F, legend = TRUE)
dev.off()

