library(dplyr)
library(ggplot2)

# 1. Carregar a base -----------------------------------------------------------

neoffi <- readxl::read_xlsx("~/Mestrado/data/neoffi_geral_1708.xlsx") |> 
  janitor::clean_names() |> 
  dplyr::select(-dplyr::starts_with("questao"))

gc <- readr::read_csv("~/Mestrado/data/gc1708.csv")

#redcap <- readr::read_csv("~/Mestrado/data/TerapiaDeAceitaoECom_DATA_2022-08-17_1702.csv")

# 2. Explorar a base -----------------------------------------------------------

refator <- function(x) {
  factor(x,
         levels = c("Muito Baixa", "Baixa", "Média", "Alta", "Muito Alta"))
}

lista_regex <- list(
  superior_incompleto = c(
    "Ensino Fundamental Completo",
    "Ensino Fundamental Incompleto",
    "Ensino Médio Completo",
    "Ensino Superior Incompleto",
    "Técnico/Profissionalizante Completo"
  )
) %>%
  purrr::map(stringr::str_c, collapse = "|") %>%
  purrr::map(stringr::regex, ignore_case = TRUE)

neo2 <- neoffi |>
  dplyr::mutate(grupo = dplyr::case_when(cpf %in% gc$cpf ~ 0,
                                         TRUE ~ 1)) |>
  dplyr::mutate(
    dplyr::across(dplyr::ends_with("classificacao"), refator),
    superior_completo = dplyr::case_when(
      stringr::str_detect(escolaridade, lista_regex$superior_incompleto) ~ 0,
      TRUE ~ 1
    ),
    dplyr::across(dplyr::ends_with("_t"), as.double),
    idade = as.double(idade),
    neuroticismo_escore_t = neuroticismo_escore_t - mean(neo2$neuroticismo_escore_t),
    extroversao_escore_t = extroversao_escore_t - mean(neo2$extroversao_escore_t),
    amabilidade_escore_t = amabilidade_escore_t - mean(neo2$amabilidade_escore_t),
    conscienciosidade_escore_t = conscienciosidade_escore_t - mean(neo2$conscienciosidade_escore_t),
    abertura_escore_t = abertura_escore_t - mean(neo2$abertura_escore_t)
  )

# 3. Análises ------------------------------------------------------------------

fit <- glm(
  grupo ~  superior_completo + idade + sexo +
    neuroticismo_escore_t + extroversao_escore_t + amabilidade_escore_t +
    conscienciosidade_escore_t + abertura_escore_t,
  family = binomial(link = "logit"),
  data = neo2
)

tablogit <- tableone::ShowRegTable(fit, exp=TRUE)
summary(fit)
