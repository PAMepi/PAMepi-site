library(shiny)

shinyUI(shiny::bootstrapPage(
  
  #titlePanel(windowTitle = "Painel Modelos Covid-19",
  #           title =
  #             div(
  #               img(
  #                 src = "CIDACS_logoBW.png",
  #                 height = 100,
  #                 width = 100,
  #                 style = "margin:5px 5px"
  #               ),
  #               "Painel Modelos Covid-19"
  #             )
  #),
  
  navbarPageWithText("Painel Modelos Covid-19",
                     
                     tabPanel( #Home ----
                               "Estados",# icon = icon("home"),
                               fluidPage(
                                 fluidRow(
                                   column(width = 5,
                                   leafletOutput("brasil_mapa", height = "500px")
                                   ),
                                   column(width = 7,
                                   verticalLayout(
                                     tabsetPanel(
                                       
                                       tabPanel("Modelos implementados",
                                                selectInput(width = "45%",
                                                            
                                                            inputId = "viz_mod_bas",
                                                            label = "Selecione o modelo",
                                                            choices = c("SIR" = "SIR_base_model", 
                                                                        "SIR beta variante" = "SIR_bv_base_model"
                                                                        ),
                                                            selected = "SIR_base_model"
                                                ),
                                                conditionalPanel(
                                                  condition = "input.viz_mod_bas == 'SIR_base_model'", 
                                                  highcharter::highchartOutput("SIR_model_plot", height="320px"),
                                                  highcharter::highchartOutput("SIR_TsRt", height="170px")
                                                ),
                                                conditionalPanel(
                                                  condition = "input.viz_mod_bas == 'SIR_bv_base_model'", 
                                                  highchartOutput("SIR_bv_plot")
                                                )
                                       ),
                                       tabPanel("Compare os modelos",
                                                selectInput(
                                                  
                                                  inputId = "var_sel",
                                                  label = "Selecione a variavel de comparação",
                                                  choices = c("Suscetiveis" = "suc", 
                                                              "Recuperados " = "rec",
                                                              "Infectados" = "inf"),
                                                  selected = "suc"
                                                ),
                                                conditionalPanel(
                                                  condition = "input.var_sel == 'suc'",
                                                  highchartOutput("suc_comp_plot")
                                                ),
                                                conditionalPanel(
                                                  condition = "input.var_sel == 'rec'",
                                                  highchartOutput("rec_comp_plot")
                                                ),
                                                conditionalPanel(
                                                  condition = "input.var_sel == 'inf'",
                                                  highchartOutput("inf_comp_plot")
                                                )
                                                
                                                
                                                
                                                
                                       ),
                                       tabPanel("Validação",
                                                tabPanel("Fit do modelo ao dado",
                                                         selectInput(
                                                           inputId = "fit_comp",
                                                           label = "Selecione o modelo",
                                                           choices = c(
                                                             "SIR" = "SIR_comp_model", 
                                                             "SIR beta variante" = "SIR_bv_comp_model"),
                                                           selected = "SIR_comp_model"
                                                         ),
                                                         conditionalPanel(
                                                           condition = "input.fit_comp == 'SIR_comp_model'",
                                                           splitLayout(
                                                             highchartOutput("SIR_comp_plot"),
                                                             highchartOutput("SIR_res")
                                                           )
                                                         ),
                                                         conditionalPanel(
                                                           condition = "input.fit_comp == 'SIR_bv_comp_model'",
                                                           splitLayout(
                                                             highchartOutput("SIR_bv_comp_plot"),
                                                             highchartOutput("SIR_bv_res")
                                                           )
                                                           
                                                         )
                                                         
                                                )
                                                
                                       ),
                                       tabPanel("Simulação",
                                                h1("Em construção"),
                                                img(src="em_construcao.gif", align = "center",width='500px')
                                                
                                       )
                                     )
                                     
                                   )
                                   )
                                 )
                               )
                     ),
                     text = "Municípios"
                     #tabPanel("Site da rede Covida", icon = icon("globe-americas"))
                     #tabPanel(
                     #  "Beta reg saude", h3("Apenas BA"),
                     #  splitLayout(
                     #  leafletOutput("brasil_mapa_beta", height = "550px")
                     #  )
                     #)
  ),
  tags$footer(#foot####
              HTML("<hr>"),HTML("&emsp;&emsp;"),
              a(href= "https://cidacs.bahia.fiocruz.br/", img(src="CIDACS_logoBW.png", 
                                                              width = 150), 
                target="_blank"),
              HTML("&emsp;&emsp;"),
              a(href= "https://www.rondonia.fiocruz.br/", img(src="fio_rond_bw.png", width = 150),
                target="_blank"),
              align = "center"),
  tags$style(type = "text/css", 
             HTML('img {
                      vertical-align: middle;
                      background-color: #17a2b8;
                      height: auto;
                        }')
  ),
  tags$style(type = 'text/css', HTML('background-color: #f8f9fa;')),
  tags$style(type = 'text/css', 
             HTML('.navbar { background-color: #660000;}
                  .navbar-default .navbar-nav>li>a {
                    color: darkgray;}
                   .navbar-default .navbar-brand{color: white;}
                   .navbar-default .navbar-nav > .active > a, 
                   .navbar-default .navbar-nav > .active > a:focus, 
                   .navbar-default .navbar-nav > .active > a:hover {
                   color: white;
                   background-color: #660000;
                   }'),
             HTML('.btn {
          background-color: #17a2b8; 
          border-radius: 8px;
          border: none;
          color: white; }'),
             HTML('.nav-tabs>li>a {
                   border: 1px solid #17a2b8;
                   border-radius: 4px 4px 0 0;
                   }'),
             HTML('.selectize-input, .selectize-control.single .selectize-input.input-active {
                   background: #17a2b8;}
                  .selectize-input.full {
                   background-color: #17a2b8; color: white;}
                  .select-selected:after {
                  border-color: white transparent transparent transparent;}
                  .selectize-control.single .selectize-input:after {
                  border-color: white transparent transparent transparent;}'),
             HTML('.tab {
                   border: 1px solid #17a2b8;
                   background-color: white;}
                   .tab button {
                   background-color: #17a2b8;
                   float: left;
                   border: none;
                   outline: none;}
                   .tab button.active {
                   background-color: #17a2b8;
                   color: white;}
                   nav>li>a:focus, .nav>li>a:hover {
                   background-color: #17a2b8;
                   color: white;
                   }
                  '),
             HTML('electize-input.full {
                   background-color: #17a2b8;
                   color: white;}
                  '), 
             HTML('.nav-tabs>li.active>a, .nav-tabs>li.active>a:focus, .nav-tabs>li.active>a:hover {
    color: white;
    cursor: default;
    background-color: #17a2b8;
    border: 1px solid #ddd;
    border-bottom-color: transparent;
}'), HTML('footer{ background-color: #17a2b8; text-align: center; } ')
             #      , HTML('.col-sm-4 { background-color: #00A800;
             #                   color: white;}
             #           .col-sm-4-default .col-sm-4-brand{color: white;}
             #.col-sm-4-default .col-sm-4 > .active > a, 
             #.col-sm-4-default .col-sm-4 > .active > a:focus, 
             #.col-sm-4-default .col-sm-4 > .active > a:hover {
             #         background-color: #3F9142;
             #        color: white;
             #       }
             #      '), HTML('.well { background-color: #00A800;
             #            color: white}')
  )
)
)
