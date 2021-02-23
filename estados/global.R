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

library(rintrojs)
library(shinyhelper)
library(waiter)

pops <- read_csv("data/misc/states_population.csv")

# Functions ----
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
    filter(state %in% state_proxy)# %>% 
  #drop_na()
  
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
    filter(state %in% state_proxy)# %>% 
  #drop_na()
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

pred_curt <- function(data, state_proxy, model_name){
  
  state_update <- state_proxy %>% unlist() %>% unique()
  
  df_cum <- data
  
  df_cum <- df_cum %>% filter(state %in% state_update)
  cut_date <- df_cum %>% top_n(10, date) %>% pull(date) %>% min()
  
  df_cum <- df_cum %>% 
    select(date, totalCases, totalCasesPred) %>%
    mutate(
      Obs = case_when(
        date >= cut_date ~ NA_real_,
        TRUE             ~ totalCases
      ),
      Pred = case_when(
        date >= cut_date ~ round(totalCasesPred),
        TRUE             ~ NA_real_
      ),
      is_pred = case_when(
        date >= cut_date ~ "Projeção",
        TRUE             ~ "Observado"
      )
    )
  
  highchart() %>%
    hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b')) %>%
    hc_yAxis(title = list(text = "Casos acumulados")) %>% 
    hc_title(text = paste0("Modelo ","<b>", model_name,
                           "</b>", " ", state_update),
             margin = 20, align = "left",
             style = list(color = "#05091A", useHTML = TRUE)) %>% 
    hc_add_series(
      data = df_cum,
      hcaes(date, Obs, group = is_pred), type = "line") %>% 
    hc_add_series(
      data = df_cum,
      hcaes(date, Pred, group = is_pred), type = "line") %>% 
    hc_exporting(enabled = TRUE) %>% hc_legend(enabled = FALSE)
}

long_praz_sir <- function(state){
  state_update <- state %>% unlist() %>% unique()
  
  df <- estados_sir %>% filter( state %in% state_update ) 
  pico_date <- df %>% filter(infectado == max(df$infectado)) %>% pull(day) %>% .[1]
  df <- df %>% mutate(pico = pico_date) %>% distinct()
  
  estado_s_p <- SIR_state_sum %>% filter( state == state_update ) %>% 
    transmute(beta1, gamma)
  
  aux_text <- paste0("'",
                     "<br>R0: ", round(estado_s_p$beta1/estado_s_p$gamma, 3),
                     "<br>Beta: ", round(estado_s_p$beta1, 3),
                     "<br>Gamma: ", round(estado_s_p$gamma, 3),
                     "<br>Pico no dia ", pico_date, "<br>com ",
                     round(max(df$infectado)), " infectados<br>", "'"
  )
  
  highchart() %>% 
    hc_title(text = paste0("Modelo SIR ","<b>",
                           states_names %>% filter(sigla %in% state_update) %>% 
                             pull(name),
                           "</b>"),
             margin = 20, align = "left",
             style = list(color = "#05091A", useHTML = TRUE)) %>% 
    hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b')
    ) %>% 
    hc_colors(colors = c("#377eb8", "#e41a1c", "#4daf4a")) %>% 
    hc_add_series(
      data = df %>% 
        select(day, suscetivel, infectado, recuperado) %>% 
        pivot_longer(- day, names_to = "SIR", values_to = "valor") %>% 
        mutate(SIR = SIR %>% 
                 str_to_title() %>% 
                 factor(levels = c("Suscetivel", "Infectado", "Recuperado"))
        ),
      hcaes(x = day, y = valor, group = SIR), type = "line"
    ) %>% 
    hc_tooltip(
      formatter = JS(
        paste0("function(){
                return (
                this.series.name + ': ' + this.y +
                ' <br> Data: ' + Highcharts.dateFormat('%e/%b/%y',
                new Date(this.x)
                ) + ", aux_text,
               ")}")),
      style = list(fontSize = '10px')
    ) %>%
    hc_exporting(enabled = TRUE)
}
long_praz_sir_bv <- function(state){
  
  state_update <- state %>% unlist() %>% unique()
  
  df <- estados_sir_bv %>% filter(state %in% state_update)
  pico_date <- df %>% filter(infectado == max(df$infectado)) %>% pull(day)
  
  
  estado_s_p <- SIR_bv_state_sum %>% filter( state %in% state_update ) %>% 
    transmute(beta1,beta2,beta3, gamma)
  aux_text <- paste0("'",
                     "<br>R0: ", round(estado_s_p$beta1/estado_s_p$gamma, 3),
                     "<br>Beta1: ", round(estado_s_p$beta1, 3),
                     "<br>Beta2: ", round(estado_s_p$beta2, 3),
                     "<br>Beta3: ", round(estado_s_p$beta3, 3),
                     "<br>Gamma: ", round(estado_s_p$gamma, 3),
                     "<br>Pico no dia ", pico_date, "<br>com ",
                     round(max(df$infectado)), " infectados", "'"
  )
  
  highchart() %>% 
    hc_title(text = paste0("Modelo SIR beta variante ",
                           "<b>",
                           states_names %>% filter(sigla %in% state_update) %>% 
                             pull(name),
                           "</b>"),
             margin = 20, align = "left",
             style = list(color = "#05091A", useHTML = TRUE)) %>% 
    hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b')) %>%
    hc_colors(colors = c("#377eb8", "#e41a1c", "#4daf4a")) %>% 
    hc_add_series(
      data = df %>% 
        select(day, suscetivel, infectado, recuperado) %>% 
        pivot_longer(- day, names_to = "SIR_beta_variante", values_to = "valor") %>% 
        mutate(SIR_beta_variante = SIR_beta_variante %>% 
                 str_to_title() %>% 
                 factor(levels = c("Suscetivel", "Infectado", "Recuperado"))
        ),
      hcaes(day, valor, group = SIR_beta_variante), type = "line") %>%
    hc_tooltip(
      formatter = JS(
        paste0("function(){
                return (
                this.series.name + ': ' + this.y +
                ' <br> Data: ' + Highcharts.dateFormat('%e/%b/%y',
                new Date(this.x)
                ) + ", aux_text[1],")}")),
      style = list(fontSize = '10px')
    ) %>%  
    hc_exporting(enabled = TRUE)
}
long_praz_seir <- function(state){
  state_update <- state %>% unlist() %>% unique()
  
  df <- estados_seir %>% filter(state %in% state_update)
  pico_date <- df %>% filter(infectado == max(df$infectado)) %>% pull(day)
  
  
  estado_s_p <- SEIR_state_sum %>% filter( state %in% state_update ) %>% 
    transmute(beta, gamma)
  aux_text <- paste0("'",
                     "<br>R0: ", round(estado_s_p$beta/estado_s_p$gamma, 3),
                     "<br>Beta: ", round(estado_s_p$beta, 3),
                     "<br>Gamma: ", round(estado_s_p$gamma, 3),
                     "<br>Pico no dia ", pico_date, "<br>com ",
                     round(max(df$infectado)), " infectados", "'"
  )
  
  highchart() %>% 
    hc_title(text = paste0("Modelo SEIR ",
                           "<b>",
                           states_names %>% filter(sigla %in% state_update) %>% 
                             pull(name),
                           "</b>"),
             margin = 20, align = "left",
             style = list(color = "#05091A", useHTML = TRUE)) %>% 
    hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b')) %>%
    hc_colors(colors = c("#377eb8","#E6681C", "#e41a1c", "#4daf4a")) %>% 
    hc_add_series(
      data = df %>% 
        select(day, suscetivel, exposto, infectado, recuperado) %>% 
        pivot_longer(- day, names_to = "SEIR", values_to = "valor") %>% 
        mutate(SEIR = SEIR %>% 
                 str_to_title() %>% 
                 factor(levels = c("Suscetivel","Exposto", "Infectado", "Recuperado"))
        ),
      hcaes(day, valor, group = SEIR), type = "line") %>%
    hc_tooltip(
      formatter = JS(
        paste0("function(){
                return (
                this.series.name + ': ' + this.y +
                ' <br> Data: ' + Highcharts.dateFormat('%e/%b/%y',
                new Date(this.x)
                ) + ", aux_text[1],")}")),
      style = list(fontSize = '10px')
    ) %>%  
    hc_exporting(enabled = TRUE)
}
long_praz_seir_bv <- function(state){
  state_update <- state %>% unlist() %>% unique()
  
  df <- estados_seir_bv %>% filter(state %in% state_update)
  pico_date <- df %>% filter(infectado == max(df$infectado)) %>% pull(day)
  
  
  estado_s_p <- SEIR_bv_state_sum %>% filter( state %in% state_update ) %>% 
    transmute(beta1,beta2,beta3, gamma)
  aux_text <- paste0("'",
                     "<br>R0: ", round(estado_s_p$beta1/estado_s_p$gamma, 3),
                     "<br>Beta1: ", round(estado_s_p$beta1, 3),
                     "<br>Beta2: ", round(estado_s_p$beta2, 3),
                     "<br>Beta3: ", round(estado_s_p$beta3, 3),
                     "<br>Gamma: ", round(estado_s_p$gamma, 3),
                     "<br>Pico no dia ", pico_date, "<br>com ",
                     round(max(df$infectado)), " infectados", "'"
  )
  
  highchart() %>% 
    hc_title(text = paste0("Modelo SEIR beta variante ",
                           "<b>",
                           states_names %>% filter(sigla %in% state_update) %>% 
                             pull(name),
                           "</b>"),
             margin = 20, align = "left",
             style = list(color = "#05091A", useHTML = TRUE)) %>% 
    hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b')) %>%
    hc_colors(colors = c("#377eb8","#E6681C", "#e41a1c", "#4daf4a")) %>% 
    hc_add_series(
      data = df %>% 
        select(day, suscetivel, exposto, infectado, recuperado) %>% 
        pivot_longer(- day, names_to = "SEIR", values_to = "valor") %>% 
        mutate(SEIR = SEIR %>% 
                 str_to_title() %>% 
                 factor(levels = c("Suscetivel","Exposto", "Infectado", "Recuperado"))
        ),
      hcaes(day, valor, group = SEIR), type = "line") %>%
    hc_tooltip(
      formatter = JS(
        paste0("function(){
                return (
                this.series.name + ': ' + this.y +
                ' <br> Data: ' + Highcharts.dateFormat('%e/%b/%y',
                new Date(this.x)
                ) + ", aux_text[1],")}")),
      style = list(fontSize = '10px')
    ) %>%  
    hc_exporting(enabled = TRUE)
}
long_praz_seiir <- function(state){
  state_update <- state %>% unlist() %>% unique()
  
  df <- estados_seiir %>% filter(state %in% state_update)
  pico_date <- df %>% filter(infectadoS == max(df$infectadoS)) %>% pull(day)
  
  
  estado_s_p <- SEIIR_state_sum %>% filter( state %in% state_update ) %>% 
    transmute(beta, gammaA, gammaS)
  aux_text <- paste0("'",
                     "<br>R0: ", round(estado_s_p$beta/estado_s_p$gammaA, 3),
                     "<br>Beta: ", round(estado_s_p$beta, 3),
                     "<br>GammaA: ", round(estado_s_p$gammaA, 3),
                     "<br>GammaS: ", round(estado_s_p$gammaS, 3),
                     "<br>Pico no dia ", pico_date, "<br>com ",
                     round(max(df$infectadoS)), " infectados", "'"
  )
  
  highchart() %>% 
    hc_title(text = paste0("Modelo SEIIR ",
                           "<b>",
                           states_names %>% filter(sigla %in% state_update) %>% 
                             pull(name),
                           "</b>"),
             margin = 20, align = "left",
             style = list(color = "#05091A", useHTML = TRUE)) %>% 
    hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b')) %>%
    hc_colors(colors = c("#377eb8","#E6681C", "#e41a1c","black", "#4daf4a")) %>% 
    hc_add_series(
      data = df %>% 
        select(day, suscetivel, exposto, infectadoS, infectadoA, recuperado) %>% 
        pivot_longer(- day, names_to = "SEIIR", values_to = "valor") %>% 
        mutate(SEIIR = SEIIR %>% 
                 str_to_title() %>% 
                 factor(levels = c("Suscetivel","Exposto", "Infectados","Infectadoa", "Recuperado"))
        ),
      hcaes(day, valor, group = SEIIR), type = "line") %>%
    hc_tooltip(
      formatter = JS(
        paste0("function(){
                return (
                this.series.name + ': ' + this.y +
                ' <br> Data: ' + Highcharts.dateFormat('%e/%b/%y',
                new Date(this.x)
                ) + ", aux_text[1],")}")),
      style = list(fontSize = '10px')
    ) %>%  
    hc_exporting(enabled = TRUE)
}
long_praz_seiir_bv <- function(state){
  
  state_update <- state %>% unlist() %>% unique()
  
  df <- estados_seiir_bv %>% filter(state %in% state_update)
  pico_date <- df %>% filter(infectadoS == max(df$infectadoS)) %>% pull(day)
  
  
  estado_s_p <- SEIIR_bv_state_sum %>% filter( state %in% state_update ) %>% 
    transmute(beta1, gammaA, gammaS)
  aux_text <- paste0("'",
                     "<br>R0: ", round(estado_s_p$beta1/estado_s_p$gammaA, 3),
                     "<br>Beta: ", round(estado_s_p$beta1, 3),
                     "<br>GammaA: ", round(estado_s_p$gammaA, 3),
                     "<br>GammaS: ", round(estado_s_p$gammaS, 3),
                     "<br>Pico no dia ", pico_date, "<br>com ",
                     round(max(df$infectadoS)), " infectados", "'"
  )
  
  highchart() %>% 
    hc_title(text = paste0("Modelo SEIIR beta variante ",
                           "<b>",
                           states_names %>% filter(sigla %in% state_update) %>% 
                             pull(name),
                           "</b>"),
             margin = 20, align = "left",
             style = list(color = "#05091A", useHTML = TRUE)) %>% 
    hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b')) %>%
    hc_colors(colors = c("#377eb8","#E6681C", "#e41a1c","black", "#4daf4a")) %>% 
    hc_add_series(
      data = df %>% 
        select(day, suscetivel, exposto, infectadoS, infectadoA, recuperado) %>% 
        pivot_longer(- day, names_to = "SEIIR", values_to = "valor") %>% 
        mutate(SEIIR = SEIIR %>% 
                 str_to_title() %>% 
                 factor(levels = c("Suscetivel","Exposto", "Infectados","Infectadoa", "Recuperado"))
        ),
      hcaes(day, valor, group = SEIIR), type = "line") %>%
    hc_tooltip(
      formatter = JS(
        paste0("function(){
                return (
                this.series.name + ': ' + this.y +
                ' <br> Data: ' + Highcharts.dateFormat('%e/%b/%y',
                new Date(this.x)
                ) + ", aux_text[1],")}")),
      style = list(fontSize = '10px')
    ) %>%  
    hc_exporting(enabled = TRUE)
}

suc_plot <- function(state_p){
  new_data <- read_csv("data/model_comp/estados_full_join_comp_susc.csv") %>% 
    filter(state == state_p) %>% 
    select(-state) %>% 
    pivot_longer(- day, names_to = "Modelo", values_to = "Valor")
  
  df_aux_text <- SIR_state_sum %>% filter(state %in% state_p) %>%
    select_all(~paste0("sir_",.)) %>% 
    bind_cols(
      SIR_bv_state_sum %>% filter(state %in% state_p) %>% 
        select_all(~paste0("sir_bv_",.))
    ) %>% 
    bind_cols(
      SEIR_state_sum %>% filter(state %in% state_p) %>% 
        select_all(~paste0("seir_",.))
    ) %>% 
    bind_cols(
      SEIR_bv_state_sum %>% filter(state %in% state_p) %>% 
        select_all(~paste0("seir_bv_",.))
    )
  
  aux_text <- paste0(
    "'",
    "<br>SIR R0: ", round(df_aux_text$sir_beta1/df_aux_text$sir_gamma, 3),
    "<br>SIR B.V. R0: ", round(df_aux_text$sir_bv_beta1/df_aux_text$sir_bv_gamma, 3),
    "<br>SEIR R.0: ", round(df_aux_text$seir_beta/df_aux_text$seir_gamma, 3),
    "<br>SEIR B.V. R.0: ", round(df_aux_text$seir_bv_beta1/df_aux_text$seir_bv_gamma, 3),
    "'"
  )
  
  
  highchart() %>% 
    hc_title(text = paste0("Comparação de Suscetíveis ","<b>",
                           states_names %>% filter(sigla %in% state_p) %>% 
                             pull(name),
                           "</b>"),
             margin = 20, align = "left",
             style = list(color = "#05091A", useHTML = TRUE)) %>% 
    hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b')) %>% 
    hc_add_series(new_data, hcaes(x = day, y = round(Valor), group = Modelo),
                  type = "line") %>% 
    hc_colors(colors = c("gray", "black", "#E0B373", "#946128","#a6cee3", "#377eb8")) %>% 
    hc_exporting(enabled = TRUE) %>% 
    hc_tooltip(
      shared = TRUE,
      formatter = JS(
        paste0(
          "function(){
                         return this.points.reduce(function (s, point) {
                            return s + '<br/>' + point.series.name + ': ' +
                            point.y }, '<b>' + Highcharts.dateFormat('%e/%b/%y',
                            new Date(this.x)) + '</b>' +
                            ",aux_text, "
                        )}"
        )
      )
      
    )
  
}
rec_plot <- function(state_p){
  new_data <- read_csv("data/model_comp/estados_full_join_comp_rec.csv") %>% 
    filter(state == state_p) %>% 
    select(-state) %>% 
    pivot_longer(- day, names_to = "Modelo", values_to = "Valor")
  
  df_aux_text <- SIR_state_sum %>% filter(state %in% state_p) %>%
    select_all(~paste0("sir_",.)) %>% 
    bind_cols(
      SIR_bv_state_sum %>% filter(state %in% state_p) %>% 
        select_all(~paste0("sir_bv_",.))
    ) %>% 
    bind_cols(
      SEIR_state_sum %>% filter(state %in% state_p) %>% 
        select_all(~paste0("seir_",.))
    ) %>% 
    bind_cols(
      SEIR_bv_state_sum %>% filter(state %in% state_p) %>% 
        select_all(~paste0("seir_bv_",.))
    )
  
  aux_text <- paste0(
    "'",
    "<br>SIR R0: ", round(df_aux_text$sir_beta1/df_aux_text$sir_gamma, 3),
    "<br>SIR B.V. R0: ", round(df_aux_text$sir_bv_beta1/df_aux_text$sir_bv_gamma, 3),
    "<br>SEIR R.0: ", round(df_aux_text$seir_beta/df_aux_text$seir_gamma, 3),
    "<br>SEIR B.V. R.0: ", round(df_aux_text$seir_bv_beta1/df_aux_text$seir_bv_gamma, 3),
    "'"
  )
  
  highchart() %>% 
    hc_colors(colors = c("gray", "black", "#E0B373", "#946128","#a6cee3", "#377eb8")) %>% 
    hc_title(text = paste0("Comparação de Recuperados ","<b>",
                           states_names %>% filter(sigla %in% state_p) %>% 
                             pull(name),
                           "</b>"),
             margin = 20, align = "left",
             style = list(color = "#05091A", useHTML = TRUE)) %>% 
    hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b')) %>% 
    hc_add_series(new_data, hcaes(x = day, y = round(Valor), group = Modelo),
                  type = "line") %>% 
    hc_exporting(enabled = TRUE) %>% 
    hc_tooltip(
      shared = TRUE,
      formatter = JS(
        paste0(
          "function(){
                         return this.points.reduce(function (s, point) {
                            return s + '<br/>' + point.series.name + ': ' +
                            point.y }, '<b>' + Highcharts.dateFormat('%e/%b/%y',
                            new Date(this.x)) + '</b>' +
                            ",aux_text, "
                        )}"
        )
      )
      
    )
}
inf_plot <- function(state_p){
  new_data <- read_csv("data/model_comp/estados_full_join_comp_inf.csv") %>%
    filter(state == state_p) %>% 
    select(-state) %>% 
    pivot_longer(- day, names_to = "Modelo", values_to = "Valor")
  
  df_aux_text <- SIR_state_sum %>% filter(state %in% state_p) %>%
    select_all(~paste0("sir_",.)) %>% 
    bind_cols(
      SIR_bv_state_sum %>% filter(state %in% state_p) %>% 
        select_all(~paste0("sir_bv_",.))
    ) %>% 
    bind_cols(
      SEIR_state_sum %>% filter(state %in% state_p) %>% 
        select_all(~paste0("seir_",.))
    ) %>% 
    bind_cols(
      SEIR_bv_state_sum %>% filter(state %in% state_p) %>% 
        select_all(~paste0("seir_bv_",.))
    )
  
  aux_text <- paste0(
    "'",
    "<br>SIR R0: ", round(df_aux_text$sir_beta1/df_aux_text$sir_gamma, 3),
    "<br>SIR B.V. R0: ", round(df_aux_text$sir_bv_beta1/df_aux_text$sir_bv_gamma, 3),
    "<br>SEIR R.0: ", round(df_aux_text$seir_beta/df_aux_text$seir_gamma, 3),
    "<br>SEIR B.V. R.0: ", round(df_aux_text$seir_bv_beta1/df_aux_text$seir_bv_gamma, 3),
    "'"
  )
  
  highchart() %>% 
    #hc_colors(colors = c("#FFA15E","#ED6B11","#fbb4ae", "#e41a1c")) %>% 
    hc_title(text = paste0("Comparação de Infectados ","<b>",
                           states_names %>% filter(sigla %in% state_p) %>% 
                             pull(name),
                           "</b>"),
             margin = 20, align = "left",
             style = list(color = "#05091A", useHTML = TRUE)) %>% 
    hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b')) %>% 
    hc_add_series(new_data, hcaes(x = day, y = round(Valor), group = Modelo),
                  type = "line") %>% 
    hc_exporting(enabled = TRUE) %>% 
    hc_tooltip(
      shared = TRUE,
      formatter = JS(
        paste0(
          "function(){
                         return this.points.reduce(function (s, point) {
                            return s + '<br/>' + point.series.name + ': ' +
                            point.y }, '<b>' + Highcharts.dateFormat('%e/%b/%y',
                            new Date(this.x)) + '</b>' +
                            ",aux_text, "
                        )}"
        )
      )
      
    )
}

# Read data ----
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