library(tidyr)
library(dplyr)

# Database

raw.data <- readr::read_csv("~/Mestrado/data/TerapiaDeAceitaoECom_DATA_2022-08-17_1702.csv")
gc <- readr::read_csv("~/Mestrado/data/gc1708.csv")
renda_corrigida <- readr::read_csv("G:/Documentos/ProjetosR/ASONO/act_insonia/dados/renda_corrigida_2610.csv")

location <- readr::read_csv("qualifying_exam/data_analyses/location_db.csv") |> 
  dplyr::select(record_id, estado, region)

dat <- raw.data |>
  # fill idade
  dplyr::group_by(record_id) |>  
  tidyr::fill(nome, .direction = "down") |>
  tidyr::fill(idade, .direction = "down") |> 
  tidyr::fill(cpf, .direction = "down") |> 
  dplyr::ungroup() |> 
  dplyr::mutate(identificao_timestamp = lubridate::ymd_hms(identificao_timestamp),
                idade2 = lubridate::time_length(difftime(identificao_timestamp, data_nascimento, unit="weeks"), "years"),
                age = dplyr::coalesce(idade, idade2)) |> 
  dplyr::select(-c(idade, idade2)) |> 
  dplyr::filter(!is.na(escala_breve_de_aceitao_do_sono_complete),
                age >= 18 & age < 60,
                dbas_16_versao_brasileira_complete == 2) |> 
  dplyr::select(record_id, nome, cpf, redcap_event_name, sexo, age, etnia, estado_civil, escolaridade, renda, ocupacao, igi_escore, ehad_depressao_escore,
                ehad_ansiedade_escore, aaq_score, dbas_score, contains("spaq_"), ebas_1:ebas_6) |> 
  dplyr::filter(redcap_event_name == "elegibilidade_arm_1") |> 
  dplyr::filter(!stringr::str_detect(nome, stringr::regex("teste", ignore_case = TRUE))) |> 
  dplyr::mutate(grupo = dplyr::case_when(cpf %in% gc$cpf ~ 0,
                                         TRUE ~ 1)) |> 
  dplyr::left_join(renda_corrigida, by = "record_id") |> 
  dplyr::mutate(renda.x = case_when(
    !is.na(renda.y) ~ renda.y,
    # renda.x >= 1000000 ~ NA_real_,
    # renda.x == 0 ~ NA_real_,
    TRUE ~ renda.x
  )
  ) |> 
  dplyr::rename(renda = renda.x) |> 
  dplyr::select(-renda.y) |> 
  dplyr::left_join(location, by = "record_id") |> 
  dplyr::mutate(region = dplyr::case_when(
    estado == "Distrito Federal/Goiás" ~ "Região Centro-Oeste",
    TRUE ~ region
  ))

# Table 1

descriptive_data <- dat |> 
  dplyr::select(escolaridade, etnia, grupo, region, sexo) |> 
  tidyr::pivot_longer(cols = c(escolaridade, etnia, grupo, region, sexo),
                      names_to = 'variable', values_to = 'value',
                      values_transform = list(value=as.character)) |> 
  dplyr::with_groups(c(variable, value), count) |>  
  dplyr::with_groups(c(variable), mutate, p = n/sum(n)) |> 
  dplyr::mutate(vars = dplyr::case_when(
    variable == "escolaridade" & value %in% c(1:6) ~ "Primary School",
    variable == "escolaridade" & value == 7:8 ~ "Secondary School",
    variable == "escolaridade" & value == 9 ~ "College degree or higher",
    variable == "etnia" & value == 1 ~ "White",
    variable == "etnia" & value %in% c(2,3) ~ "Black",
    variable == "etnia" & value == 4 ~ "Asian",
    variable == "etnia" & !(value %in% c(1:4)) ~ "Other/Not informed",
    variable == "medicacao_semana" & value == 0 ~ "Don't use",
    variable == "medicacao_semana" & value %in% c(1:5) ~ "1-5 days a week",
    variable == "medicacao_semana" & value %in% c(6,7) ~ "6-7 days a week",
    variable == "region" & value == "Região Centro-Oeste" ~ "Central-West",
    variable == "region" & value == "Região Nordeste" ~ "Northeast",
    variable == "region" & value == "Região Norte" ~ "Northern",
    variable == "region" & value == "Região Sudeste" ~ "Southeast",
    variable == "region" & value == "Região Sul" ~ "Southern",
    variable == "sexo" & value == 1 ~ "Female",
    variable == "sexo" & value == 2 ~ "Male",
    variable == "grupo" & value == 1 ~ "Insomnia",
    variable == "grupo" & value == 0 ~ "Good sleeper",
  )) |> 
  dplyr::with_groups(c(variable, vars), summarise, n = sum(n), p = sum(p)) |> 
  dplyr::mutate(total = paste0(n, " (", signif(p*100,3), ")"))  |> 
  dplyr::mutate(variable = factor(variable, 
                                  levels = c("sexo", "etnia", "grupo", "escolaridade", "region"))) |> 
  dplyr::arrange(variable) %>% 
  dplyr::select(-c(variable, n, p)) |> 
  tidyr::replace_na(list(vars = "Not disclosed"))

continuous_data <- dat %>% 
  dplyr::select(age) %>% 
  dplyr::summarise(mean_age = mean(age),
                   sd_age = sd(age)) %>% 
  dplyr::mutate("Age (years)" = paste0(signif(mean_age,2), " (", signif(sd_age,3), ")")) %>% 
  dplyr::select(-c(mean_age, sd_age)) %>% 
  tidyr::pivot_longer(cols = everything(), names_to = 'vars', 
                      values_to = 'total', values_transform = list(total=as.character))

table1 <- dplyr::bind_rows(continuous_data, descriptive_data)
readr::write_csv(table1, "conferences/cbsono_22/table1.csv")
