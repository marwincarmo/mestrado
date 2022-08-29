### Retrieving geolocation info

wiki_url <- "https://pt.wikipedia.org/wiki/Discagem_direta_a_dist%C3%A2ncia"
r_wiki <- httr::GET(wiki_url)

base_ddd <- r_wiki |> 
  xml2::read_html() |> 
  #xml2::xml_find_first('//table')
  xml2::xml_find_all("//table[@class='wikitable sortable']") |> 
  rvest::html_table() |> 
  magrittr::extract2(1) |> 
  janitor::clean_names()

### Brazilian regions

regioes_url <- "https://www.todamateria.com.br/siglas-estados-brasileiros/"
r_regioes <- httr::GET(regioes_url)

base_regioes <- r_regioes |> 
  xml2::read_html() |> 
  xml2::xml_find_all("//table") |> 
  rvest::html_table() |> 
  magrittr::extract2(1) |> 
  janitor::clean_names() |> 
  dplyr::mutate(observacao = stringr::str_remove(observacao, "Localizado na "))

### Database

dados_recrutamento <- readr::read_csv("~/Mestrado/data/TerapiaDeAceitaoECom_DATA_2022-08-17_1702.csv") |> 
  dplyr::filter(redcap_event_name == "recrutamento_arm_1",
                !is.na(telefone_1)) |> 
  dplyr::select(record_id, nome, latitude, longitude, telefone_1) |> 
  dplyr::mutate(ddd = as.double(stringr::str_extract(telefone_1, "[1-9]{2}"))) |> 
  dplyr::left_join(base_ddd, by = c("ddd" = "prefixo")) |> 
  dplyr::select(-cidades_principais_regioes)

enderecos <- dados_recrutamento |> 
  dplyr::filter(!is.na(latitude)) |> 
  tidygeocoder::reverse_geocode(lat = latitude, long = longitude, method = 'bing', full_results = TRUE)

#readr::write_rds(enderecos, "qualifying_exam/data_analyses/enderecos.rds")

addrs <- enderecos |> 
  dplyr::select(record_id, bing_address.adminDistrict, bing_address.locality)

dados_localizacao <- dados_recrutamento |> 
  dplyr::left_join(addrs, by = "record_id") |> 
  dplyr::mutate(state = dplyr::coalesce(bing_address.adminDistrict, estado)) |> 
  dplyr::left_join(base_regioes, by = c("state" = "estado_do_brasil")) |> 
  dplyr::mutate(observacao = dplyr::case_when(
    estado == "Distrito Federal/Goiás" | estado == "Federal District" ~ "Distrito Federal",
    TRUE ~ observacao
    )) |> 
  dplyr::select(-c(estado, bing_address.adminDistrict)) |> 
  dplyr::rename(c(estado = state, region = observacao, city = bing_address.locality ))

#readr::write_csv(dados_localizacao, "qualifying_exam/data_analyses/location_db.csv")
