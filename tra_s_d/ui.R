library(shiny)


shinyUI(shiny::bootstrapPage(
  
  useShinydashboard(),
  introjsUI(),
  
  
  fluidRow(
    column(
      width = 2,
      verticalLayout(
        #radioButtons("series_info", "Indique o perfil da serie",
        #             choices = c("Acumulada"= "a", "Diaria" = "d"),
        #             inline = TRUE),  
        
        introBox(
          numericInput(inputId = "n_days",min = 5, max = 400,
                     label = "Quantidade de dias", value = 5),
          data.step = 1,
          data.intro = "Selecione a quantidade de dias que a sua série tem",
          data.hint = "click e selecione"),
        introBox(
          numericInput(inputId = "pop_input",min = 1e6, max = 1e10,
                     label = "População", value = 1e6),
          data.step = 2,
          data.intro = "Informe a população que de sua cidade ou estado",
          data.hint = "click e selecione"
          ),
        introBox(
          dateInput("date_input", "Data do primeiro caso",
                  value = today(), min = "2020-01-01",
                  max = today() + 5, format = "dd--mm--yyyy",
                  language = "pt-BR"),
          data.step = 3,
          data.intro = "Qual é a data em que apareceu o primeiro infectado ?",
          data.hint = "click e selecione"),
        introBox(
          selectInput("model_ui_data", "Selecione o modelo",
                    choices = c("SIR" = "SIR", "SEIR" = "SEIR",
                                "SEIIR" = "SEIIR")
        ),
        data.step = 4,
        data.intro = "Quais dos nossos modelos você quer utilizar ?",
        data.hint = "click e selecione"),
        introBox(
          numericInput(inputId = "n_beta", label = "Número de betas", value = 1,
                     min = 1, max = 3, step = 1),
          data.step = 5,
          data.intro = "Qual é a quantidade de (betas)[] entre 1 e 3 que deseja utilizar no modelo ?",
          data.hint = "click e selecione"), 
        introBox(
          actionButton("TRD", "Rode o modelo"),
          data.step = 6,
          data.intro = "Clique aqui para rodar o modelo.",
          data.hint = "click e selecione")
      )
      
    ),
    column(
      width = 2, 
      actionButton(inputId = "tour",
                   icon = icon("info"),
                   label = ""),
      rHandsontableOutput("tab_interativa", 100, 120)
    ),
    column(
      width = 7, 
      highchartOutput("sim_pred", height = "50%") 
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
