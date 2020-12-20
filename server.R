library(shiny)

shinyServer(function(input, output, session) {
    
    # Map ----
    output$brasil_mapa <- renderLeaflet({
        
        bins <- quantile(br_mapa$SIR_prop, 
                         probs = c(seq(0, 100, by = 30), 100)/100)
        
        pal <- colorBin("Reds", domain = br_mapa$SIR_prop, bins = bins)
        
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
            "SEIR_base_model" = switch(
                input$is_bv,
                "bv" = long_praz_seir_bv(state_proxy()[1]),
                "std" = long_praz_seiir(state_proxy()[1])
            )
        )
    })
    
    # Models comparison plots ----
    
    output$suc_plot <- renderHighchart({
        suc_plot(state_proxy()[1])
    })
    
    output$suc_comp_plot <- renderHighchart({
        
        new_data <- estados_sir_bv %>% dplyr::transmute(day, state, SIR_beta_variante = suscetivel) %>% 
            left_join(
                estados_sir %>% dplyr::transmute(day, state, SIR = suscetivel),
                by = c("day", "state")
            ) %>% 
            left_join(
                estados_seir %>% dplyr::transmute(day, state, SEIR = suscetivel),
                by = c("day", "state")
            ) %>% 
            left_join(
                estados_seir_bv %>% dplyr::transmute(day, state, SEIR_beta_variante = suscetivel),
                by = c("day", "state")
            ) %>% 
            left_join(
                estados_seiir %>% dplyr::transmute(day, state, SEIIR = suscetivel),
                by = c("day", "state")
            ) %>% 
            left_join(
                estados_seiir_bv %>% dplyr::transmute(day, state, SEIIR_beta_variante = suscetivel),
                by = c("day", "state")
            ) %>% 
            filter(state == state_proxy()[1]) %>% 
            select(-state) %>% 
            pivot_longer(- day, names_to = "Modelo", values_to = "Valor")
        
        df_aux_text <- SIR_state_sum %>% filter(state %in% state_proxy()[1]) %>%
            select_all(~paste0("sir_",.)) %>% 
            bind_cols(
                SIR_bv_state_sum %>% filter(state %in% state_proxy()[1]) %>% 
                    select_all(~paste0("sir_bv_",.))
            ) %>% 
            bind_cols(
                SEIR_state_sum %>% filter(state %in% state_proxy()[1]) %>% 
                    select_all(~paste0("seir_",.))
            ) %>% 
            bind_cols(
                SEIR_bv_state_sum %>% filter(state %in% state_proxy()[1]) %>% 
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
                                   states_names %>% filter(sigla %in% state_proxy()[1]) %>% 
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
        
    })
    
    output$rec_comp_plot <- renderHighchart({
        
        new_data <- estados_sir_bv %>% dplyr::transmute(day, state, SIR_beta_variante = recuperado) %>% 
            left_join(
                estados_sir %>% dplyr::transmute(day, state, SIR = recuperado),
                by = c("day", "state")
            ) %>% 
            left_join(
                estados_seir %>% dplyr::transmute(day, state, SEIR = recuperado),
                by = c("day", "state")
            ) %>% 
            left_join(
                estados_seir_bv %>% dplyr::transmute(day, state, SEIR_beta_variante = recuperado),
                by = c("day", "state")
            ) %>% 
            left_join(
                estados_seiir %>% dplyr::transmute(day, state, SEIIR = recuperado),
                by = c("day", "state")
            ) %>% 
            left_join(
                estados_seiir_bv %>% dplyr::transmute(day, state, SEIIR_beta_variante = recuperado),
                by = c("day", "state")
            ) %>% 
            filter(state == state_proxy()[1]) %>% 
            select(-state) %>% 
            pivot_longer(- day, names_to = "Modelo", values_to = "Valor")
        
        df_aux_text <- SIR_state_sum %>% filter(state %in% state_proxy()[1]) %>%
            select_all(~paste0("sir_",.)) %>% 
            bind_cols(
                SIR_bv_state_sum %>% filter(state %in% state_proxy()[1]) %>% 
                    select_all(~paste0("sir_bv_",.))
            ) %>% 
            bind_cols(
                SEIR_state_sum %>% filter(state %in% state_proxy()[1]) %>% 
                    select_all(~paste0("seir_",.))
            ) %>% 
            bind_cols(
                SEIR_bv_state_sum %>% filter(state %in% state_proxy()[1]) %>% 
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
                                   states_names %>% filter(sigla %in% state_proxy()[1]) %>% 
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
        
    })
    
    output$inf_comp_plot <- renderHighchart({
        
        new_data <- estados_sir_bv %>% dplyr::transmute(day, state, SIR_beta_variante = infectado) %>% 
            left_join(
                estados_sir %>% dplyr::transmute(day, state, SIR = infectado),
                by = c("day", "state")
            ) %>% 
            left_join(
                estados_seir %>% dplyr::transmute(day, state, SEIR = infectado),
                by = c("day", "state")
            ) %>% 
            left_join(
                estados_seir_bv %>% dplyr::transmute(day, state, SEIR_beta_variante = infectado),
                by = c("day", "state")
            ) %>% 
            left_join(
                estados_seiir %>% dplyr::transmute(day, state, SEIIR_sin = infectadoS),
                by = c("day", "state")
            ) %>% 
            left_join(
                estados_seiir_bv %>% dplyr::transmute(day, state, SEIIR_sin_beta_variante = infectadoS),
                by = c("day", "state")
            ) %>% 
            left_join(
                estados_seiir %>% dplyr::transmute(day, state, SEIIR_asin = infectadoA),
                by = c("day", "state")
            ) %>% 
            left_join(
                estados_seiir_bv %>% dplyr::transmute(day, state, SEIIR_asin_beta_variante = infectadoA),
                by = c("day", "state")
            ) %>%
            filter(state == state_proxy()[1]) %>% 
            select(-state) %>% 
            pivot_longer(- day, names_to = "Modelo", values_to = "Valor")
        
        df_aux_text <- SIR_state_sum %>% filter(state %in% state_proxy()[1]) %>%
            select_all(~paste0("sir_",.)) %>% 
            bind_cols(
                SIR_bv_state_sum %>% filter(state %in% state_proxy()[1]) %>% 
                    select_all(~paste0("sir_bv_",.))
            ) %>% 
            bind_cols(
                SEIR_state_sum %>% filter(state %in% state_proxy()[1]) %>% 
                    select_all(~paste0("seir_",.))
            ) %>% 
            bind_cols(
                SEIR_bv_state_sum %>% filter(state %in% state_proxy()[1]) %>% 
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
                                   states_names %>% filter(sigla %in% state_proxy()[1]) %>% 
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