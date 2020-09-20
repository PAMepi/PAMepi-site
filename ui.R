library(shiny)

shinyUI(shiny::bootstrapPage(
  
  useShinydashboard(),
  
  
  navbarPage(title = div(h3("Painel Modelos Covid-19", 
                    style = "position: relative; top: 45px; left: -1000px;"), 
                 #HTML("&emsp;&emsp;"),
                 a(href= "https://cidacs.bahia.fiocruz.br/", img(src="CIDACS_Bw.png", 
                                                                 width = 100), 
                   target="_blank"),
                 HTML("&emsp;&emsp;"),
                 a(href= "https://www.rondonia.fiocruz.br/", img(src="fio_rond_bw.png", width = 80),
                   target="_blank"), 
                 style = "position: relative; top: -70px; right: -1000px;"),
             id = "home",
             windowTitle = "Painel Modelos Covid-19",
             
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
             tabPanel("Municípios", value = "muni",
                      h1("Em construção"),
                      img(src="em_construcao.gif", align = "center",width='500px')
             ), 
             tabPanel("Regiões de Saúde", value = "reg_sau",
                      h1("Em construção"),
                      img(src="em_construcao.gif", align = "center",width='500px')
             ),
             
             tabPanel("Metodologia", value = "meto",
                      h1("Em construção"),
                      img(src="em_construcao.gif", align = "center",width='500px')
             ),
             tabPanel("Traga seus dados", value = "tsd",
                      splitLayout(
                        #radioButtons("series_info", "Indique o perfil da serie",
                        #             choices = c("Acumulada"= "a", "Diaria" = "d"),
                        #             inline = TRUE),  
                        
                        numericInput(inputId = "n_days",min = 5, max = 400,
                                    label = "# linhas", value = 5),
                        numericInput(inputId = "pop_input",min = 1e6, max = 1e10,
                                     label = "População", value = 1e6),
                        dateInput("date_input", "Data do primeiro caso",
                                  value = today(), min = "2020-01-01",
                                  max = "2020-11-20", format = "dd--mm--yyyy",
                                  language = "pt-BR"),
                        selectInput("model_ui_data", "Selecione o modelo",
                                    choices = c("SIR" = "SIR", "SIR beta variante" = "SIR_bv")
                                    ),
                        actionButton("TRD", "Rode o modelo")
                        ), 
                      br(""),
                      fluidRow(
                        column(
                          width = 2,
                          rHandsontableOutput("tab_interativa", 100, 120)
                        ),
                        column(
                          width = 5, offset = 4,
                          highchartOutput("sim_pred", height = "50%") 
                        )
                      ),
                      column(
                        width = 5, offset = 3,
                        box(
                          title = "TsRt", solidHeader = TRUE,
                          collapsible = TRUE, collapsed = TRUE,
                          plotOutput("simple_series",width = 400, height = 280) 
                        )
                      )
             ),
             tabPanel("Apoio e Equipe",
                      
                      fluidPage(
                        
                        navlistPanel(
                          tabPanel("Apoio", 
                                   img(src="logo_fiocruz.png", width = 300),
                                   h3("Programa Fiocruz de Fomento à inovação: ideias e produtos inovadores - COVID-19, 
                                               encomendas e estratégias - INOVA-FIOCRUZ (Processo VPPIS-005-FIO-20-2-40).")
                          ),
                          tabPanel("Agradecimento e Colaborações",
                                   h3("Grupo de Modelagem  Rede CoVida"), br(),
                                   a(href= "https://covid19br.org/", 
                                     img(src="logo_redecovida.png",  width = 200), target="_blank"),
                                   br(),h1(),
                                   p("Alan Amad, Aureliano Paiva, Caio Porto, Daniel Cardoso, Felipe Pereira,
                                              Gervásio Santos, José  Garcia, Juliane Oliveira, Luciana Cardim, Mateus Silva,
                                              Matheus Torquato, Moreno Rodrigues, Maurrício Barreto, Nívea Bispo, Pablo Ramos,
                                              Rafael Veiga, Roberto Andrade, Rosemeire Fiaccone, Raphael Rosário, Suani Pinho")
                          ),
                          tabPanel("Equipe",
                                   fluidRow(
                                     column(
                                       width = 4,
                                       h3("Moreno S. Rodrigues"),
                                       img(src="e_1.jpg", align = "center",width=120),
                                       h3("Pablo I. P. Ramos"),
                                       img(src="e_3.jpg", align = "center",width=120)
                                     ),
                                     column(
                                       width = 4,
                                       h3("Juliane F. Oliveira"),
                                       img(src="e_2.jpg", align = "center",width=120),
                                       h3("Arthur Rios"),
                                       img(src="e_4.jpg", align = "center",width=120)
                                     )
                                   )
                                   
                          ),
                          tabPanel("Repositório",
                                   h1("Em construção"),
                                   img(src="em_construcao.gif", align = "center",width='500px')
                          )
                          
                        )
                        
                        
                      )
             )
             #text = "Municípios"
             #tabPanel("Site da rede Covida", icon = icon("globe-americas"))
             #tabPanel(
             #  "Beta reg saude", h3("Apenas BA"),
             #  splitLayout(
             #  leafletOutput("brasil_mapa_beta", height = "550px")
             #  )
             #)
  ),
  tags$style(type = "text/css", 
             HTML('img {
                      vertical-align: middle;
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
}'), HTML('footer{ background-color: #17a2b8; text-align: center; } '),
             HTML('
                  .navbar-default .navbar-nav>li>a:focus, .navbar-default .navbar-nav>li>a:hover {
    color: #BDBDBD !important;
    background-color: #660000 !important;
}
                  ')
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
