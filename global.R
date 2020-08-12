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

br_mapa <- read_sf("data/map.json") %>% 
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
      ungroup() %>% 
      transmute(state, SIR_prop = prop),
    by = c("sigla" = "state")
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