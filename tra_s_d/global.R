library(shiny)
library(shinythemes)
library(tidyverse)
library(shinyjs)
library(htmltools)
library(htmlwidgets)
library(highcharter)
library(lubridate)
library(rhandsontable)
library(shinydashboard)
library(shinyWidgets)
library(reticulate)

library(rintrojs)

source_python("run_models.py")

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