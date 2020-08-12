library(shinythemes)
library(tidyverse)
library(leaflet)
library(sf)
library(shinyjs)
library(htmltools)
library(htmlwidgets)
library(highcharter)
library(lubridate)


estados_SIR <- read_csv("data/SIR_estados.csv") %>% 
  mutate(date = as_date(date))

pops <- c(
  'RO'=	1777225,
  'AC'=	881935,
  'AM'=	4144597,
  'RR'= 	605761,
  'PA'=	8602865,
  'AP'=	845731,
  'TO'=	1572866,
  'MA'=	7075181,
  'PI'=	3273227,
  'CE'=	9132078,
  'RN'=	3506853,
  'PB'=   4018127,
  'PE'=	9557071,
  'AL'=   3337357,
  'SE'=   2298696,
  'BA'=   14873064,
  'MG'=   21168791,
  'ES'=	4018650,
  'RJ'=   17264943,
  'SP'=   45919049,
  'PR'=   11433957,
  'SC'=   	7164788,
  'RS'=	11377239,
  'MS'=	2778986,
  'MT'=   3484466,
  'GO'=   7018354,
  'DF'=	3015268,
  'TOTAL'= 210147125) %>% 
  as.data.frame() %>% 
  rownames_to_column("state") %>% 
  rename(pop = ".")

estados_SIR <- estados_SIR %>% 
  left_join(
    pops,
    by = c('state')
  )

estados_SEIR <- read_csv("data/SEIR_estados.csv")
SEIRHUD_data <- read_csv("data/SEIRHUD_estados.csv")

TsRt <- read_csv("data/TsRt_estados.csv")

br_mapa <- read_sf("data/mapa_br/map.geojson") %>% 
  mutate(
    state = 
      case_when(
        NM_ESTADO == "SÃO PAULO" ~ "SP", NM_ESTADO == "RONDÔNIA" ~ "RO", NM_ESTADO == "ACRE" ~ "AC",
        NM_ESTADO == "AMAZONAS" ~ "AM", NM_ESTADO == "RIO DE JANEIRO" ~ "RJ",
        NM_ESTADO == "BAHIA" ~ "BA", NM_ESTADO == "ALAGOAS" ~ "AL", NM_ESTADO == "AMAPÁ" ~ "AP",
        NM_ESTADO == "CEARÁ" ~ "CE", NM_ESTADO == "DISTRITO FEDERAL" ~ "DF",
        NM_ESTADO == "ESPIRITO SANTO" ~ "ES", NM_ESTADO == "GOIÁS" ~ "GO",
        NM_ESTADO == "MARANHÃO" ~ "MA", NM_ESTADO == "MATO GROSSO" ~ "MT",
        NM_ESTADO == "MATO GROSSO DO SUL" ~ "MS", NM_ESTADO == "MINAS GERAIS" ~ "MG",
        NM_ESTADO == "PARÁ" ~ "PA", NM_ESTADO == "PARAÍBA" ~ "PB",
        NM_ESTADO == "PARANÁ" ~ "PR", NM_ESTADO == "PERNAMBUCO" ~ "PE", NM_ESTADO == "PIAUÍ" ~ "PI",
        NM_ESTADO == "RIO GRANDE DO NORTE" ~ "RN", NM_ESTADO == "RIO GRANDE DO SUL" ~ "RS",
        NM_ESTADO == "RORAIMA" ~ "RR", NM_ESTADO == "SANTA CATARINA" ~ "SC", NM_ESTADO == "SERGIPE" ~ "SE",
        NM_ESTADO == "TOCANTINS" ~ "TO",
        TRUE ~ "TOTAL"
      )
  ) %>% 
  #inner_join(
  #  estados_SIR %>% 
  #    filter(!is.na(infectado)) %>% 
  #    mutate(infectado = round(infectado)) %>% 
  #    group_by(state) %>%
  #    filter(date == max(date)) %>% ungroup() %>% 
  #    transmute(state, SIR_infec = infectado)
  #) %>%
  #inner_join( #preciso processar isso antes, dessa forma esta sendo muito custoso
  #  estados_SEIR %>% 
  #    filter(!is.na(infectado)) %>% 
  #    mutate(infectado = round(infectado)) %>% 
  #    group_by(state) %>%
  #    filter(date == max(date)) %>% 
  #    ungroup() %>% 
  #    transmute(state, SEIR_infec = infectado)
  #) %>% 
  #inner_join(
  #  SEIRHUD_data %>% 
  #    filter(!is.na(Infectado_assintomatico)) %>% 
  #    mutate(infectado = round(Infectado_assintomatico + 
  #                               Infectado_sintomatico)) %>% 
  #    group_by(state) %>%
  #    filter(date == max(date)) %>% 
  #    ungroup() %>% 
  #    transmute(state, SEIRHUD_infec = infectado)
  #) %>% 
  left_join(
    estados_SIR %>% 
      drop_na() %>% 
      dplyr::group_by(state) %>% 
      top_n(n = 1, date) %>% 
      mutate(prop = TOTAL*100/pop) %>% 
      transmute(state, SIR_prop = prop)
  )

SIR_state_sum <- read_csv(
  "data/SIR_estados_sum.csv"
)
SEIR_state_sum <- read_csv(
  "data/SEIR_estados_sum.csv"
)
SEIRHUD_state_sum <- read_csv(
  "data/SEIRHUD_estados_sum.csv"
)

# Funcoes ----
navbarPageWithText <- function(..., text) {
  navbar <- navbarPage(...)
  textEl <- tags$p(class = "navbar-text", text)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], textEl)
  navbar
}
jscode <- '
shinyjs.init = function() {
  $(".nav").on("click", ".disabled", function (e) {
    e.preventDefault();
    return false;
  });
}
'

css <- '
.disabled {
  background: #eee !important;
  cursor: default !important;
  color: black !important;
}
'