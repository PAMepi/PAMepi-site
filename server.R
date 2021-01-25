library(shiny)

shinyServer(function(input, output, session) {
    
    # Map ----
    output$brasil_mapa <- renderLeaflet({
        
        bins <- quantile(br_mapa$SIR_prop, 
                         probs = c(seq(0, 100, by = 30), 100)/100)
        
        pal <- colorBin("Reds", domain = br_mapa$SIR_prop, bins = bins)
        
        labels <- paste0(
            "<strong>",br_mapa$name,"</strong><br/>",
            "Casos de COVID-19 <strong>", round(br_mapa$SIR_prop, 2),"%</strong>") %>%
            lapply(htmltools::HTML)
        
        mapa <- leaflet(
            data = br_mapa,
            options = leafletOptions(
                zoomControl=FALSE, doubleClickZoom =FALSE, bounceAtZoomLimits = FALSE,
                dragging = FALSE, scrollWheelZoom = FALSE, closePopupOnClick = FALSE,
                minZoom = 4, maxZoom = 4
            )
        ) %>% 
            addTiles(options = providerTileOptions(opacity = 0.5)) %>% 
            setView(lng=-52.761,lat=-14.446,zoom=4
            )
        mapa
        mapa %>% 
            #addControl(actionButton(inputId = "reset", label = "Brasil"),
            #           position = "topright") %>% 
            addPolygons(color = "#718075", layerId = ~sigla,
                        opacity = 1.0, fillOpacity = 0.9, weight = 1,
                        fillColor = ~pal(SIR_prop),
                        popup = labels,
                        dashArray = "3",
                        popupOptions = popupOptions(autoClose = TRUE, closeOnClick = TRUE ,
                                                    closeButton = FALSE),
                        highlightOptions = highlightOptions(color = "#FFEE58", weight = 3,
                                                            bringToFront = FALSE),
                        label = ~paste0(name, ": ", round(SIR_prop, 2), " %")) %>%
            addLegend(pal = pal, values = ~paste0(round(SIR_prop, 2), " %"), opacity = 0.8, title = "Casos de COVID-19<br>(% população)",
                      position = "bottomleft",
                      labFormat = labelFormat(suffix = "%",digits = 2))
        
    })
    
    state_proxy <- reactive(
        {
            click <- input$brasil_mapa_shape_click
            #reset <- input$reset
            if(is.null(click) #| isTruthy(reset)
            )
                return(
                    "TOTAL"
                )
            else
                leafletProxy("brasil_mapa");click
        }
    ) 
    # Time series plots ----
    
    output$model_longo <- renderHighchart({
        switch(
            input$viz_mod_bas,
            "SIR_base_model" = switch(
                input$is_bv,
                "bv" = long_praz_sir_bv(state_proxy()[1]),
                "std" = long_praz_sir(state_proxy()[1])
            ),
            "SEIR_base_model" = switch(
                input$is_bv,
                "bv" = long_praz_seir_bv(state_proxy()[1]),
                "std" = long_praz_seir(state_proxy()[1])
            ),
            "SEIIR_base_model" = switch(
                input$is_bv,
                "bv" = long_praz_seiir_bv(state_proxy()[1]),
                "std" = long_praz_seiir(state_proxy()[1])
            )
        )
    })
    
    output$SIR_TsRt <- renderHighchart({
        
        state_update <- state_proxy()[1] %>% unlist() %>% unique()
        
        TsRt_df <- TsRt %>% filter( state %in% state_update ) %>% 
            mutate(infec = ifelse(reproductionNumber < 1, "green", "red"),
                   date_aux = paste0(
                       month(date, label = TRUE), "-", year(date))
            )
        
        highchart() %>%
            hc_yAxis(plotLines = list(list(color = "#ACB6FF", value = 1, 
                                           width = 1.5, dashStyle = "ShortDash")),
                     min = 0, max = max(TsRt_df$reproductionNumberHigh) + .1
            ) %>% 
            hc_add_series(
                data = TsRt_df, hcaes(x = date, low = reproductionNumberLow,
                                      high = reproductionNumberHigh),
                showInLegend = FALSE,  enableMouseTracking = FALSE, 
                type = "errorbar",color = "black",
                stemWidth = 1.5,  whiskerLength = 5
            ) %>% 
            hc_add_series(TsRt_df, type = "scatter", hcaes(x = date, y = reproductionNumber,
                                                           group = infec#, group = infec
            ),
            color = c("#36B36D", "#B33024"),
            name = c("Rt < 1", "Rt >= 1")) %>%
            hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d %b')) %>% 
            hc_tooltip(formatter = JS("function(){
                                                return (
                                                        'Rt : ' + this.y +
                                                ' <br> Data: ' + Highcharts.dateFormat('%e. %b', new Date(this.x))
                                                        )
                                }")) %>% 
            hc_exporting(enabled = TRUE)
    })
    
    # Models comparison plots ----
    
    
    output$compare_plots <- renderHighchart({
        switch(
            input$var_sel,
            "suc" = suc_plot(state_proxy()[1]),
            "rec" = rec_plot(state_proxy()[1]),
            "inf" = inf_plot(state_proxy()[1])
        )
    })
    
    # Validation plots ----
    output$comp_plot <- renderHighchart({
        switch(
            input$fit_comp,
            "SIR_comp_model" = switch(
                input$is_bv_val,
                "std" = comp_plot(estados_sir_comp, state_proxy()[1]),
                "bv" = comp_plot(estados_sir_bv_comp, state_proxy()[1])
            ),
            "SEIR_comp_model" = switch(
                input$is_bv_val,
                "std" = comp_plot(estados_seir_comp, state_proxy()[1]),
                "bv" = comp_plot(estados_seir_bv_comp, state_proxy()[1])
            ),
            "SEIIR_comp_model" = switch(
                input$is_bv_val,
                "std" = comp_plot(estados_seiir_comp, state_proxy()[1]),
                "bv" = comp_plot(estados_seiir_bv_comp, state_proxy()[1])
            )
        )
    })
    
    output$res_plot <- renderHighchart({
        switch(
            input$fit_comp,
            "SIR_comp_model" = switch(
                input$is_bv_val,
                "std" = res_plot(estados_sir_comp, state_proxy()[1]),
                "bv" = res_plot(estados_sir_bv_comp, state_proxy()[1])
            ),
            "SEIR_comp_model" = switch(
                input$is_bv_val,
                "std" = res_plot(estados_seir_comp, state_proxy()[1]),
                "bv" = res_plot(estados_seir_bv_comp, state_proxy()[1])
            ),
            "SEIIR_comp_model" = switch(
                input$is_bv_val,
                "std" = res_plot(estados_seiir_comp, state_proxy()[1]),
                "bv" = res_plot(estados_seiir_bv_comp, state_proxy()[1])
            )
        )
    })
    
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
    
    
    # Misc ----
    output$repository <- renderUI({
        includeHTML("www/README.html")
        
    })
    
})