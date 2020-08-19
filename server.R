library(shiny)


shinyServer(function(input, output, session) {
    
    output$brasil_mapa <- renderLeaflet({
        
        bins <- quantile(br_mapa$SIR_prop, 
                         probs = c(seq(0, 100, by = 30), 100)/100)
        
        pal <- colorBin("Reds", domain = br_mapa$SIR_prop, bins = bins)
        
        mapa <- leaflet(
            data = br_mapa,
            options = leafletOptions(
                zoomControl=FALSE, doubleClickZoom =FALSE, bounceAtZoomLimits = FALSE,
                dragging = FALSE, scrollWheelZoom = FALSE,
                minZoom = 4, maxZoom = 4
            )
        ) %>% 
            addTiles(options = providerTileOptions(opacity = 0.5)) %>% 
            setView(lng=-52.761,lat=-14.446,zoom=4
            )
        mapa
        mapa %>% 
            addPolygons(color = "#718075", layerId = ~sigla,
                        opacity = 1.0, fillOpacity = 0.9, weight = 1,
                        fillColor = ~pal(SIR_prop),
                        highlightOptions = highlightOptions(color = "#FFEE58", weight = 3,
                                                            bringToFront = FALSE),
                        label = ~paste0(name, ": ", round(SIR_prop, 2), " %")) %>%
            addLegend(pal = pal, values = ~paste0(round(SIR_prop, 2), " %"), opacity = 0.8, title = "Casos de COVID-19<br>(% população)",
                      position = "bottomleft")
        
        
    })
    
    state_proxy <- reactive(
        {
            click <- input$brasil_mapa_shape_click
            
            if(is.null(click))
                return(
                    "TOTAL"
                )
            else
                leafletProxy("brasil_mapa");click
        }
    ) 
    
    output$SIR_model_plot <- renderHighchart({
        
        state_update <- state_proxy()[1] %>% unlist() %>% unique()
        
        df <- estados_sir %>% filter( state %in% state_update ) 
        pico_date <- df %>% filter(infectado == max(df$infectado)) %>% pull(day)
        df <- df %>% mutate(pico = pico_date) %>% distinct()
        
        estado_s_p <- SIR_state_sum %>% filter( state == state_update ) %>% 
            transmute(beta, gamma)
        
        aux_text <- paste0("'",
                           "<br>R0: ", round(estado_s_p$beta/estado_s_p$gamma, 3),
                           "<br>Beta: ", round(estado_s_p$beta, 3),
                           "<br>Gamma: ", round(estado_s_p$gamma, 3),
                           "<br>Pico no dia ", pico_date, "<br>com ",
                           round(max(df$infectado)), " infectados<br>", "'"
        )
        
        highchart() %>% 
            hc_title(text = paste0("Modelo SIR ","<b>",state_update, "</b>"),
                     margin = 20, align = "left",
                     style = list(color = "#05091A", useHTML = TRUE)) %>% 
            hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b')
            ) %>% 
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
                'Número de pessoas ' + this.series.name + ': ' + this.y +
                ' <br> Data: ' + Highcharts.dateFormat('%e/%b/%y',
                new Date(this.x)
                ) + ", aux_text,
                           ")}")),
                style = list(fontSize = '10px')
            ) %>%
            hc_exporting(enabled = TRUE)
        
    })
    
    observe({
        
        if(is.null(input$brasil_mapa_shape_click))
            return(NULL)
        else{
            
            TsRt_df <- TsRt %>% filter( state %in% state_proxy()$id ) %>% 
                mutate(infec = ifelse(reproductionNumber < 1, "green", "red"),
                       date_aux = paste0(
                           month(date, label = TRUE), "-", year(date))
                )
            
            output$SIR_TsRt <- renderHighchart({
                highchart() %>%
                    hc_yAxis(plotLines = list(list(color = "#ACB6FF", value = 1, 
                                                   width = 1.5, dashStyle = "ShortDash")),
                             min = 0, max = max(TsRt_df$reproductionNumberHigh) + .1
                    ) %>% 
                    hc_add_series(
                        data = TsRt_df, hcaes(x = date, low = reproductionNumberLow,
                                              high = reproductionNumberHigh),
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
        }
        
        
    })
    
    output$SIR_bv_plot <- renderHighchart({
        
        state_update <- state_proxy()[1] %>% unlist() %>% unique()
        
        df <- estados_sir_bv %>% filter(state %in% state_update)
        pico_date <- df %>% filter(infectado == max(df$infectado)) %>% pull(day)
        pico_val <- max(df$infectado) 
        
        df <- df %>% 
            select(day, suscetivel, infectado, recuperado) %>% 
            pivot_longer(- day, names_to = "SIR_beta_variante", values_to = "valor") %>% 
            mutate(SIR_beta_variante = SIR_beta_variante %>% 
                       str_to_title() %>% 
                       factor(levels = c("Suscetivel", "Infectado", "Recuperado"))
            )
        
        estado_s_p <- SIR_bv_state_sum %>% filter( state %in% state_update ) %>% 
            transmute(beta1,beta2,beta3, gamma)
        aux_text <- paste0("'",
                           "<br>R0: ", round(estado_s_p$beta1/estado_s_p$gamma, 3),
                           "<br>Beta1: ", round(estado_s_p$beta1, 3),
                           "<br>Beta2: ", round(estado_s_p$beta2, 3),
                           "<br>Beta3: ", round(estado_s_p$beta3, 3),
                           "<br>Gamma: ", round(estado_s_p$gamma, 3),
                           "<br>Pico no dia ", pico_date, "<br>com ",
                           pico_val, " infectados", "'"
        )
        
        highchart() %>% 
            hc_title(text = paste0("Modelo SIR beta variante ","<b>",state_update, "</b>"),
                     margin = 20, align = "left",
                     style = list(color = "#05091A", useHTML = TRUE)) %>% 
            hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b')) %>%
            hc_add_series(df, hcaes(day, round(valor), group = SIR_beta_variante), type = "line") %>%
            hc_tooltip(
                formatter = JS(
                    paste0("function(){
                return (
                'Número de pessoas ' + this.series.name + ': ' + this.y +
                ' <br> Data: ' + Highcharts.dateFormat('%e/%b/%y',
                new Date(this.x)
                ) + ", aux_text,")}")),
                style = list(fontSize = '10px')
            ) %>%  
            hc_exporting(enabled = TRUE)
    })
    
    output$suc_comp_plot <- renderHighchart({
        
        new_data <- estados_sir_bv %>% dplyr::transmute(day, state, SIR_beta_variante = suscetivel) %>% 
            left_join(
                estados_sir %>% dplyr::transmute(day, state, SIR = suscetivel),
                by = c("day", "state")
            ) %>% 
            filter(state == state_proxy()[1]) %>% 
            select(-state) %>% 
            pivot_longer(- day, names_to = "Modelo", values_to = "Valor")
        
        
        highchart() %>% 
            hc_title(text = paste0("Comparação de Suscetíveis ","<b>",state_proxy()[1], "</b>"),
                     margin = 20, align = "left",
                     style = list(color = "#05091A", useHTML = TRUE)) %>% 
            hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b')) %>% 
            hc_add_series(new_data, hcaes(x = day, y = round(Valor), group = Modelo),
                          type = "line") %>% 
            hc_exporting(enabled = TRUE)
        
    })
    
    output$rec_comp_plot <- renderHighchart({
        
        new_data <- estados_sir_bv %>% dplyr::transmute(day, state, SIR_beta_variante = recuperado) %>% 
            left_join(
                estados_sir %>% dplyr::transmute(day, state, SIR = recuperado),
                by = c("day", "state")
            ) %>% 
            filter(state == state_proxy()[1]) %>% 
            select(-state) %>% 
            pivot_longer(- day, names_to = "Modelo", values_to = "Valor")
        
        
        highchart() %>% 
            hc_title(text = paste0("Comparação de Recuperados ","<b>",state_proxy()[1], "</b>"),
                     margin = 20, align = "left",
                     style = list(color = "#05091A", useHTML = TRUE)) %>% 
            hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b')) %>% 
            hc_add_series(new_data, hcaes(x = day, y = round(Valor), group = Modelo),
                          type = "line") %>% 
            hc_exporting(enabled = TRUE)
        
    })
    
    output$inf_comp_plot <- renderHighchart({
        
        new_data <- estados_sir_bv %>% dplyr::transmute(day, state, SIR_beta_variante = infectado) %>% 
            left_join(
                estados_sir %>% dplyr::transmute(day, state, SIR = infectado),
                by = c("day", "state")
            ) %>% 
            filter(state == state_proxy()[1]) %>% 
            select(-state) %>% 
            pivot_longer(- day, names_to = "Modelo", values_to = "Valor")
        
        
        highchart() %>% 
            hc_title(text = paste0("Comparação de Infectados ","<b>", state_proxy()[1], "</b>"),
                     margin = 20, align = "left",
                     style = list(color = "#05091A", useHTML = TRUE)) %>% 
            hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b')) %>% 
            hc_add_series(new_data, hcaes(x = day, y = round(Valor), group = Modelo),
                          type = "line") %>% 
            hc_exporting(enabled = TRUE)
        
    })
    
    output$SIR_comp_plot <- renderHighchart({
        
        df <- estados_sir_comp %>% 
            drop_na() %>% 
            filter(state == state_proxy()[1])
        
        highchart() %>%
            hc_title(text = paste0("Casos preditos e observados modelo SIR ",
                                   "<b>",state_proxy()[1], "</b>"),
                     margin = 20, align = "left",
                     style = list(color = "#05091A", useHTML = TRUE, fontSize = "15px")) %>% 
            hc_xAxis(title = list(text = "Casos reportados"), min = 0, max = max(df$totalCasesPred)) %>% 
            hc_yAxis(title = list(text = "Casos preditos"), min = 0, max = max(df$totalCasesPred)) %>% 
            hc_add_series(showInLegend = FALSE,
                          color = "#A9A9A9",
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
        
    })
    
    output$SIR_bv_comp_plot <- renderHighchart({
        
        df <- estados_sir_bv_comp %>% 
            drop_na() %>% 
            filter(state == state_proxy()[1]) 
        
        highchart() %>%
            hc_title(text = paste0("Casos preditos e observados modelo SIR beta variante ",
                                   "<b>",state_proxy()[1], "</b>"),
                     margin = 20, align = "left",
                     style = list(color = "#05091A", useHTML = TRUE, fontSize = "15px")) %>% 
            hc_xAxis(title = list(text = "Casos reportados")) %>% 
            hc_yAxis(title = list(text = "Casos preditos")) %>% 
            hc_add_series(showInLegend = FALSE,
                          color = "#A9A9A9",
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
    })
    
    output$SIR_res <- renderHighchart({
        
        df <- estados_sir_comp %>% 
            drop_na() %>% 
            filter(state == state_proxy()[1])
        
        highchart() %>%
            hc_title(text = paste0("Casos acumulados modelo SIR ",
                                   "<b>",state_proxy()[1], "</b>"),
                     margin = 20, align = "left",
                     style = list(color = "#05091A", useHTML = TRUE, fontSize = "15px")) %>% 
            hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b')) %>% 
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
        
    })
    
    output$SIR_bv_res <- renderHighchart({
        
        df <- estados_sir_bv_comp %>% 
            drop_na() %>% 
            filter(state == state_proxy()[1])
        
        highchart() %>%
            hc_title(text = paste0("Casos acumulados do modelo SIR beta variante ",
                                   "<b>",state_proxy()[1], "</b>"),
                     margin = 20, align = "left",
                     style = list(color = "#05091A", useHTML = TRUE, fontSize = "15px")) %>% 
            hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b')) %>% 
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