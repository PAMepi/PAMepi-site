library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(sf)
library(shinyjs)
library(htmltools)
library(htmlwidgets)
library(highcharter)
library(lubridate)

pops <- c(
  'RO'=	1777225,
  'AC'=	881935,
  'AM'=	4144597,
  'RR'= 605761,
  'PA'=	8602865,
  'AP'=	845731,
  'TO'=	1572866,
  'MA'=	7075181,
  'PI'=	3273227,
  'CE'=	9132078,
  'RN'=	3506853,
  'PB'= 4018127,
  'PE'=	9557071,
  'AL'= 3337357,
  'SE'= 2298696,
  'BA'= 14873064,
  'MG'= 21168791,
  'ES'=	4018650,
  'RJ'= 17264943,
  'SP'= 45919049,
  'PR'= 11433957,
  'SC'= 7164788,
  'RS'=	11377239,
  'MS'=	2778986,
  'MT'= 3484466,
  'GO'= 7018354,
  'DF'=	3015268,
  'TOTAL'= 210147125) %>% 
  as.data.frame() %>% 
  rownames_to_column("state") %>% 
  rename(pop = ".")

read_compartimentos <- function(model = "sir"){
  dir_loc <- paste0("data/model_comp/compartimentos_", model, "_estados.csv")
  return(
    read_csv(dir_loc) %>% 
      left_join( pops, by = c('state') ) %>% 
      mutate_at(vars(suscetivel:recuperado), ~ .*pop) %>% 
      mutate_if(is.numeric, round)
    )
}
read_par <- function(model = "sir"){
  dir_loc <- paste0("data/model_par/par_", model, "_estados.csv")
  return(read_csv(dir_loc))
}
read_data <- function(model = "sir"){
  dir_loc <- paste0("data/model_data/data_", model, "_estados.csv")
  return(
    read_csv(dir_loc) %>% 
      left_join(pops, by = 'state')
  )
}
estados_sir <- read_compartimentos("sir")
estados_sir_bv <- read_compartimentos("sir_bv")
estados_seir <- read_compartimentos("seir")
estados_seir_bv <- read_compartimentos("seir_bv")

SIR_state_sum <- read_par("sir")
SIR_bv_state_sum <- read_par("sir_bv")
SEIR_state_sum <- read_par("seir")
SEIR_bv_state_sum <- read_par("seir_bv")

estados_sir_comp <- read_data("sir")
estados_sir_bv_comp <- read_data("sir_bv")
estados_seir_comp <- read_data("seir")
estados_seir_bv_comp <- read_data("seir_bv")

#estados_seir <- read_csv("data/compartimentos_seir_estados.csv") %>% 
#  left_join(pops, by = 'state') %>% 
#  mutate_at(vars(suscetivel:recuperado), ~ .*pop) %>% 
#  mutate_if(is.numeric, round)
#estados_seir_comp <- read_csv("data/data_seir_estados.csv")

TsRt <- read_csv("data/misc/TsRt_estados.csv")

br_mapa <- read_sf("data/misc/map.json") %>% 
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

states_names <- br_mapa %>%
  as.data.frame() %>% 
  select(name,sigla) %>% 
  bind_rows(tibble(name = "Brasil", sigla = "TOTAL"))

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