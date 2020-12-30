library(shiny)


shinyUI(shiny::bootstrapPage(
  
  useShinydashboard(),
 
                       fluidPage(
                         fluidRow(
                           column(width = 5,
                                  leafletOutput("brasil_mapa", height = "500px")
                           ),
                           column(width = 7,
                                  verticalLayout(
                                    tabsetPanel(
                                      
                                      tabPanel("Cenário longo prazo",
                                               fluidRow(
                                                 column(width = 5,
                                                        selectInput(width = "100%",
                                                                    
                                                                    inputId = "viz_mod_bas",
                                                                    label = "Selecione o modelo",
                                                                    choices = c("SIR" = "SIR_base_model", 
                                                                                "SEIR" = "SEIR_base_model",
                                                                                "SEIIR" = "SEIIR_base_model"
                                                                    ),
                                                                    selected = "SIR_base_model"
                                                        )
                                                 ),
                                                 column(width = 4,
                                                        radioButtons(
                                                          inputId = "is_bv",
                                                          label = "",
                                                          choices = c("Padrão" = "std", 
                                                                      "Beta Variante" = "bv")
                                                        )
                                                        
                                                 )
                                               ), 
                                               highcharter::highchartOutput("model_longo", height="320px"),
                                               conditionalPanel(
                                                 condition = "input.viz_mod_bas == 'SIR_base_model' & input.is_bv == 'std'", 
                                                 #highcharter::highchartOutput("SIR_model_plot", height="320px"),
                                                 highcharter::highchartOutput("SIR_TsRt", height="170px")
                                               )
                                      ),
                                      tabPanel("Compare os modelos",
                                               selectInput("variable_selection", 
                                                           label = "Selecione a variavel de comparação",
                                                           choices = c("Suscetiveis" = "suc", 
                                                                       "Recuperados " = "rec",
                                                                       "Infectados" = "inf"),
                                                           selected = "suc"
                                               ),
                                               highchartOutput("compare_plots")
                                               
                                               
                                               
                                               
                                      ),
                                      tabPanel("Validação",
                                               fluidRow(
                                                 column(width = 5,
                                                        selectInput(
                                                          inputId = "fit_comp",
                                                          label = "Selecione o modelo",
                                                          choices = c(
                                                            "SIR" = "SIR_comp_model",
                                                            "SEIR" = "SEIR_comp_model",
                                                            "SEIIR" = "SEIIR_comp_model"),
                                                          selected = "SIR_comp_model"
                                                        )
                                                 ),
                                                 column(width = 4,
                                                        radioButtons(
                                                          inputId = "is_bv_val",
                                                          label = "",
                                                          choices = c("Padrão" = "std", 
                                                                      "Beta Variante" = "bv")
                                                        )
                                                 )
                                               ),
                                               splitLayout(
                                                 highchartOutput("comp_plot"),
                                                 highchartOutput("res_plot")
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
              .nav-pills > li.active > a, .nav-pills > li.active > a:hover, .nav-pills > li.active > a:focus {
                color:#fff;
                background-color:#17a2b8;
    }'),
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
