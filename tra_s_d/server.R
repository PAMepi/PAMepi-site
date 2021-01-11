library(shiny)

shinyServer(function(input, output, session) {
    
    observeEvent(input$tour,
                 introjs(session, options = list("nextLabel"="Próximo",
                                                 "prevLabel"="Anterior",
                                                 "skipLabel"="Pule o tour"),
                         events = list("oncomplete"=I('alert("Você está pronto para usar essa tab.")')))
    )
    
    # Traga seus dados ----
    datavalues <- reactive({
        
        validate(
            need(input$n_days >= 5,
                 "Por favor, informe um número de dias maior ou igual a 5"),
            need(input$n_days <= 400,
                 "Por favor, informe um número de dias menor ou igual a 400")
        )
        
        df = data.frame(
            date = seq(input$date_input, by = "days", length.out = input$n_days),
            user = rep("", input$n_days)
        )
    })
    
    output$tab_interativa <- renderRHandsontable({
        
        
        rhandsontable(datavalues(), 
                      rowHeaders = NULL,
                      width = 400, height = 300) %>% 
            hot_col("user", type = "numeric") %>% 
            hot_cell(1, 2, "Por favor, lembre-se de confirmar o número de linhas de sua série")
    })
    
    population_model <- reactive({
        
        validate(
            need(input$pop_input >= 1e6, 
                 "Por favor informe um número para população maior que 1 milhão")
        )
        
        validate(
            need(
                all(diff(as.numeric(hot_to_r(input$tab_interativa)$user)) >= 0),
                "Por favor adicione uma serie monotonica"
            )
        )
        
        pop_model <- input$pop_input
    })
    
    observeEvent(input$TRD,{
        
        withProgress(
            
            message='Por favor aguarde',
            detail='Running Model...',
            value=0, {
                n <- 2
                
                incProgress(1/n, detail = paste("Rodando o modelo..."))
                # User input
                user_data <- hot_to_r(input$tab_interativa)
                # Model selection
                model_output <- switch(
                    input$model_ui_data,
                    "SIR" = run_sir(
                        vector = as.numeric(as.character(user_data$user)),
                        pop = population_model(),
                        n_betas = as.numeric(as.character(input$n_beta)) 
                    ),
                    "SEIR" = run_seir(
                        vector = as.numeric(as.character(user_data$user)),
                        pop = population_model(),
                        n_betas = as.numeric(as.character(input$n_beta)) 
                    ),
                    "SEIIR" = run_seiir(
                        vector = as.numeric(as.character(user_data$user)),
                        pop = population_model(),
                        n_betas = as.numeric(as.character(input$n_beta)) 
                    )
                )
                
                
                df_model <- tibble(date = user_data$date,
                                   Input = as.numeric(as.character(user_data$user)), #Py output
                                   Modelo = model_output) %>% 
                    pivot_longer(- date, names_to = "serie", values_to = "valor")
                
                
                
                output$sim_pred <- renderHighchart({
                    highchart() %>%
                        hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b')) %>% 
                        hc_add_series(df_model, hcaes(x = date, y = round(valor), group = serie),
                                      type = "line") %>% 
                        hc_yAxis(title = list(text = "Casos acumulados")) %>% 
                        hc_title(text = paste0("Modelo ","<b>", isolate(input$model_ui_data),"</b>",
                                               " com ", "<b>", isolate(input$n_beta),"</b>",
                                               " beta variando"),
                                 margin = 20, align = "left",
                                 style = list(color = "#05091A", useHTML = TRUE))
                })
                
                
                incProgress(1/n, detail = paste("Encerrando..."))
            })
        
        
        
        
        
    })
    
})