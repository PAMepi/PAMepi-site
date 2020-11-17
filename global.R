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
library(rhandsontable)
library(shinydashboard)
library(shinyWidgets)
library(reticulate)

source_python("run_models.py")

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
    read_csv(dir_loc, col_types = cols()) %>% 
      left_join( pops, by = c('state') ) %>% 
      mutate_at(vars(suscetivel:recuperado), ~ .*pop) %>% 
      mutate_if(is.numeric, round)
    )
}
read_par <- function(model = "sir"){
  dir_loc <- paste0("data/model_par/par_", model, "_estados.csv")
  return(read_csv(dir_loc,col_types = cols()))
}
read_data <- function(model = "sir"){
  dir_loc <- paste0("data/model_data/data_", model, "_estados.csv")
  return(
    read_csv(dir_loc, col_types = cols()) %>% 
      left_join(pops, by = 'state')
  )
}
comp_plot <- function(compart, state_proxy){
  df <- compart %>% 
    drop_na() %>% 
    filter(state %in% state_proxy)
  return(
    highchart() %>%
      hc_title(text = paste0("Dados vs. Ajustados ",
                             "<b>",
                             states_names %>% filter(sigla %in% state_proxy) %>% 
                               pull(name),
                             "</b>"),
               margin = 20, align = "left",
               style = list(color = "#05091A", useHTML = TRUE, fontSize = "15px")) %>% 
      hc_xAxis(title = list(text = "Dados"), min = 0, max = max(df$totalCasesPred)) %>% 
      hc_yAxis(title = list(text = "Ajustados"), min = 0, max = max(df$totalCasesPred)) %>% 
      hc_add_series(showInLegend = FALSE,
                    color = "#A9A9A9", dashStyle = 'ShortDot',
                    data = list(list(0, 0), list(max(df$totalCases),
                                                 max(df$totalCases))),
                    enableMouseTracking = FALSE) %>% 
      hc_plotOptions(line = list(color = "#4471EB",
                                 marker = list(enabled = FALSE)),
                     scatter = list(color = "black")) %>% 
      hc_add_series(data = df, hcaes(x = totalCases, y = round(totalCasesPred)),
                    tooltip = list(pointFormat = "<b>Casos Preditos<b>: {point.y}<br>",
                                   headerFormat = "<b>Casos Observados<b>: {point.x}<br>"),
                    type = "scatter", showInLegend = FALSE) %>% 
      hc_exporting(enabled = TRUE)
  )
}
res_plot <- function(compart, state_proxy){
  df <- compart %>% 
    drop_na() %>% 
    filter(state %in% state_proxy)
  return(
    highchart() %>%
      hc_title(text = paste0("Casos acumulados ",
                             "<b>",
                             states_names %>% filter(sigla %in% state_proxy) %>% 
                               pull(name),
                             "</b>"),
               margin = 20, align = "left",
               style = list(color = "#05091A", useHTML = TRUE, fontSize = "15px")) %>% 
      hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b')) %>% 
      hc_yAxis(title = list(text = "Casos acumulados")) %>% 
      hc_add_series(data = df, hcaes(x = date, y = round(totalCasesPred)), 
                    type = "line", name = "Casos Preditos") %>% 
      hc_add_series(data = df, hcaes(x = date, y = totalCases), 
                    tooltip = list(pointFormat = "Data: {point.date}",
                                   headerFormat = "<b>{point.y} Casos</b><br>"),
                    type = "scatter", name = "Casos Reportados") %>% 
      hc_plotOptions(line = list(color = "#4471EB",
                                 marker = list(enabled = FALSE)),
                     scatter = list(color = "black")) %>% 
      hc_exporting(enabled = TRUE)
  )
}


estados_sir <- read_compartimentos("sir")
estados_sir_bv <- read_compartimentos("sir_bv")
estados_seir <- read_compartimentos("seir")
estados_seir_bv <- read_compartimentos("seir_bv")
estados_seiir <- read_compartimentos("seiir")
estados_seiir_bv <- read_compartimentos("seiir_bv") 


SIR_state_sum <- read_par("sir")
SIR_bv_state_sum <- read_par("sir_bv")
SEIR_state_sum <- read_par("seir")
SEIR_bv_state_sum <- read_par("seir_bv")
SEIIR_state_sum <- read_par("seiir")
SEIIR_bv_state_sum <- read_par("seiir_bv")


estados_sir_comp <- read_data("sir")
estados_sir_bv_comp <- read_data("sir_bv")
estados_seir_comp <- read_data("seir")
estados_seir_bv_comp <- read_data("seir_bv")
estados_seiir_comp <- read_data("seiir")
estados_seiir_bv_comp <- read_data("seiir_bv")


#estados_seir <- read_csv("data/compartimentos_seir_estados.csv") %>% 
#  left_join(pops, by = 'state') %>% 
#  mutate_at(vars(suscetivel:recuperado), ~ .*pop) %>% 
#  mutate_if(is.numeric, round)
#estados_seir_comp <- read_csv("data/data_seir_estados.csv")

TsRt <- read_csv("data/misc/TsRt_estados.csv", col_types = cols())

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