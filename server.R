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
    output$SIR_model_plot <- renderHighchart({
        
        state_update <- state_proxy()[1] %>% unlist() %>% unique()
        
        df <- estados_sir %>% filter( state %in% state_update ) 
        pico_date <- df %>% filter(infectado == max(df$infectado)) %>% pull(day)
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
    
    output$SIR_bv_plot <- renderHighchart({
        
        state_update <- state_proxy()[1] %>% unlist() %>% unique()
        
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
        # Quando adiconar a linha vertical do beta V. usar:
        # https://stackoverflow.com/questions/46953400/r-highcharter-tooltip-customization
    })
    
    output$SEIR_model_plot <- renderHighchart({
        state_update <- state_proxy()[1] %>% unlist() %>% unique()
        
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
    })
    
    output$SEIR_bv_model_plot <- renderHighchart({
        state_update <- state_proxy()[1] %>% unlist() %>% unique()
        
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
    })
    
    output$SEIIR_model_plot <- renderHighchart({
        
        state_update <- state_proxy()[1] %>% unlist() %>% unique()
        
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
    })
    
    output$SEIIR_bv_model_plot <- renderHighchart({
        
        state_update <- state_proxy()[1] %>% unlist() %>% unique()
        
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
    })
    
    # Models comparison plots ----
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
    output$SIR_comp_plot <- renderHighchart({
        
        comp_plot(estados_sir_comp, state_proxy()[1])
        
    })
    
    output$SIR_bv_comp_plot <- renderHighchart({
        
        comp_plot(estados_sir_bv_comp, state_proxy()[1])
        
    })
    
    output$SIR_res <- renderHighchart({
        
        res_plot(estados_sir_comp, state_proxy()[1])
        
    })
    
    output$SIR_bv_res <- renderHighchart({
        
        res_plot(estados_sir_bv_comp, state_proxy()[1])
        
    })
    
    output$SEIR_comp_plot <- renderHighchart({
        
        comp_plot(estados_seir_comp, state_proxy()[1])
        
    })
    output$SEIR_bv_comp_plot <- renderHighchart({
        
        comp_plot(estados_seir_bv_comp, state_proxy()[1])
        
        
    })
    
    output$SEIR_res <- renderHighchart({
        
        res_plot(estados_seir_comp, state_proxy()[1])
        
    })
    output$SEIR_bv_res <- renderHighchart({
        
        res_plot(estados_seir_bv_comp, state_proxy()[1])
        
    })
    
    output$SEIIR_comp_plot <- renderHighchart({
        comp_plot(estados_seiir_comp, state_proxy()[1])
    })
    output$SEIIR_bv_comp_plot <- renderHighchart({
        comp_plot(estados_seiir_comp, state_proxy()[1])
    })
    
    output$SEIIR_res <- renderHighchart({
        res_plot(estados_seiir_comp, state_proxy()[1])
    })
    output$SEIIR_bv_res <- renderHighchart({
        res_plot(estados_seiir_comp, state_proxy()[1])
    })
    #output$brasil_mapa_beta <- renderLeaflet({
    #    bins <- quantile(br_mapa$SIR_infec, 
    #                     probs = c(seq(0, 100, by = 15), 100)/100) %>% 
    #        round()
    #    pal <- colorBin("Reds", domain = br_mapa$SIR_infec, bins = bins)
    #    
    #    leaflet(data = br_mapa) %>% 
    #        addTiles(options = providerTileOptions(opacity = 0.5)) %>% 
    #        setView(lng=-52.761,lat=-14.446,zoom=4) %>% 
    #        addPolygons(color = "#718075", layerId = ~state,
    #                    opacity = 1.0, fillOpacity = 0.9, weight = 1,
    #                    fillColor = ~pal(SIR_infec),
    #                    highlightOptions = highlightOptions(color = "#FFEE58", weight = 3,
    #                                                        bringToFront = FALSE),
    #                    label = ~name)  
    #})
    #
    #proxy <- leafletProxy("brasil_mapa_beta")
    
    #observe({
    #    click <- input$brasil_mapa_beta_shape_click
    #    bins <- quantile(reg_saud$rt, na.rm = TRUE,
    #                     probs = c(seq(0, 100, by = 15), 100)/100) 
    #    pal <- colorBin("Reds", domain = reg_saud$rt, bins = bins)
    #    
    #    if(is.null(click))
    #        return()
    #    else
    #        proxy %>% clearShapes() %>% clearControls() %>% 
    #        setView(lng = click$lng, lat = click$lat, zoom = 6) %>% 
    #        #clearShapes() %>% 
    #        addPolygons(data = reg_saud %>% 
    #                        dplyr::filter(startsWith(CO_REGSAUD, "29")), #so BA por enquanto
    #                    color = "white",
    #                    layerId = ~CO_REGSAUD,
    #                    opacity = 1.0, fillOpacity = 0.9, weight = 1,
    #                    fillColor = ~pal(rt),
    #                    highlightOptions = highlightOptions(color = "#FFEE58", weight = 3,
    #                                                        bringToFront = TRUE),
    #                    label = ~CO_REGSAUD
    #                    
    #                    
    #        ) 
    #})
    
})