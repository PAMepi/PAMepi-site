library(shinythemes)
library(tidyverse)
library(leaflet)
library(sf)
library(shinyjs)
library(htmltools)
library(htmlwidgets)
library(highcharter)
library(lubridate)

# Base data ----
estados_sir_bv <- read_csv("data/compartimentos_sir_bv_estados.csv")

SIR_bv_state_sum <- read_csv("data/par_sir_bv_estados.csv")

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

estados_sir_bv <- estados_sir_bv %>% 
  left_join( pops, by = c('state') ) %>% 
  mutate_at(vars(suscetivel:recuperado), ~ .*pop) %>% 
  mutate_if(is.numeric, round)
estados_sir_bv_comp <- read_csv("data/data_sir_bv_estados.csv") %>% 
  left_join(pops, by = 'state')

estados_sir <- read_csv("data/compartimentos_sir_estados.csv") %>% 
  left_join(pops, by = 'state') %>% 
  mutate_at(vars(suscetivel:recuperado), ~ .*pop) %>% 
  mutate_if(is.numeric, round)
estados_sir_comp <- read_csv("data/data_sir_estados.csv")
  
TsRt <- read_csv("data/TsRt_estados.csv")

br_mapa <- read_sf("data/map.json") %>% 
  left_join(
    estados_sir_bv_comp %>% 
      drop_na() %>% 
      dplyr::group_by(state) %>% 
      top_n(n = 1, day) %>% 
      mutate(prop = totalCases*100/pop) %>% 
      ungroup() %>% 
      transmute(state, SIR_prop = prop),
    by = c("sigla" = "state")
  )

SIR_state_sum <- read_csv(
  "data/par_sir_estados.csv"
)
states_names <- br_mapa %>%
  as.data.frame() %>% 
  select(name,sigla) %>% 
  bind_rows(tibble(name = "Brasil", sigla = "TOTAL"))
# City tab ----
cidades_sir <- read_csv("data/compartimentos_sir_cidades.csv") %>% 
  select(- city) %>% mutate(ibgeID = as.character(ibgeID)) %>% 
  group_by(ibgeID) %>% top_n(1, date) %>% ungroup()
cidades_tsrt <- read_csv("data/TsRt_cidades.csv") %>% 
  select(- city) %>% mutate(ibgeID = as.character(ibgeID)) %>% 
  group_by(ibgeID) %>% top_n(1, date) %>% ungroup()

city_map <- sf::read_sf("data/malha_mun.json") %>% 
  right_join(
    cidades_tsrt, by = c("id" = "ibgeID")
  ) %>% 
  mutate(
    state_code = str_sub(id, 1, 2),
    state = 
      case_when(
        state_code == "35" ~ "SP", state_code == "11" ~ "RO", state_code == "12" ~ "AC",
        state_code == "13" ~ "AM", state_code == "33" ~ "RJ",
        state_code == "29" ~ "BA", state_code == "27" ~ "AL", state_code == "16" ~ "AP",
        state_code == "CEARÁ" ~ "CE", state_code == "DISTRITO FEDERAL" ~ "DF",
        state_code == "32" ~ "ES", state_code == "52" ~ "GO",
        state_code == "21" ~ "MA", state_code == "51" ~ "MT",
        state_code == "50" ~ "MS", state_code == "31" ~ "MG",
        state_code == "15" ~ "PA", state_code == "25" ~ "PB",
        state_code == "41" ~ "PR", state_code == "26" ~ "PE", state_code == "22" ~ "PI",
        state_code == "24" ~ "RN", state_code == "43" ~ "RS",
        state_code == "14" ~ "RR", state_code == "42" ~ "SC", state_code == "28" ~ "SE",
        state_code == "17" ~ "TO",
        TRUE ~ "TOTAL"
      )
  )
#Plot options ----
lang <- getOption("highcharter.lang")
lang$months <- c('Janeiro', 'Fevereiro', 'Março', 'Abril', 'Maio', 'Junho',
                 'Julho', 'Agosto', 'Setembro', 'Outubro', 'Novembro', 'Dezembro')
lang$shortMonths <- c('Jan', 'Fev', 'Mar', 'Abr', 'Mai', 'Jun', 'Jul', 'Ago',
                      'Set', 'Out', 'Nov', 'Dez')
lang$weekdays <- c('Domingo', 'Segunda', 'Terça', 'Quarta', 'Quinta', 'Sexta', 'Sábado')
options(highcharter.lang = lang)

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