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
                dragging=FALSE
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
                        label = ~paste0(name, ": ", round(SIR_prop, 3), " %")) %>%
            addLegend(pal = pal, values = ~paste0(round(SIR_prop, 3), " %"), opacity = 0.8, title = "Proporção de infectados",
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
        
        df <- estados_SIR %>% filter( state %in% state_update ) 
        pico_date <- df %>% filter(infectado == max(df$infectado)) %>% pull(date)
        df <- df %>% mutate(pico = pico_date) %>% distinct()
        
        estado_s_p <- SIR_state_sum %>% filter( state == state_update ) %>% 
            transmute(beta1, beta2, gamma, dia_mudanca = round(dia_mudanca)) %>% 
            top_n(wt = dia_mudanca, n = 1)
        
        aux_text <- paste0("'",
                           "<br>R0: ", round(estado_s_p$beta1/estado_s_p$gamma, 3),
                           "<br>Beta1: ", round(estado_s_p$beta1, 3),
                           "<br>Beta2: ", round(estado_s_p$beta2, 3),
                           "<br>Gamma: ", round(estado_s_p$gamma, 3),
                           "<br>Dia mudança: ", estado_s_p$dia_mudanca,
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
                    select(date, suscetivel, infectado, recuperado) %>% 
                    pivot_longer(- date, names_to = "SIR", values_to = "valor"),
                hcaes(x = date, y = round(valor), group = SIR), type = "line"
            ) %>% 
            hc_tooltip(
                formatter = JS(
                    paste0("function(){
                return (
                'Número de pessoas ' + this.series.name + ': ' + this.y +
                ' <br> Data: ' + Highcharts.dateFormat('%b-%Y',
                new Date(this.x)
                ) + ", aux_text,"
                                          )
                            }")),
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
    
    output$SIER_plot <- renderHighchart({
        
        df <- estados_SEIR %>% filter(state == state_proxy()[1]) 
        df_aux <- df
        pico_date <- df %>% filter(infectado == max(df$infectado)) %>% pull(date)
        
        df <- df %>% 
            select(date, sucetivel,exposto, infectado, recuperado) %>% 
            pivot_longer(- date, names_to = "SEIR", values_to = "valor") %>% 
            mutate(date_aux = paste0(month(date, label = TRUE), "-", year(date)))
        
        estado_s_p <- SEIR_state_sum %>% filter( state == state_proxy()[1] ) %>% 
            transmute(beta1, gamma, dia_mudanca = round(dia_mudanca),
                      sigma) %>% 
            top_n(dia_mudanca, n = 1)
        
        highchart() %>% 
            hc_title(text = paste0("Modelo SEIR ","<b>",state_proxy()[1], "</b>"),
                     margin = 20, align = "left",
                     style = list(color = "#05091A", useHTML = TRUE)) %>% 
            hc_yAxis(plotLines = list(
                list(
                    color = NA, value = max(
                        df %>% filter(SEIR == "infectado") %>% pull(valor) %>% max()),
                    label = list(text = paste0(
                        "Beta: ", round(estado_s_p$beta1, 3),
                        "<br>Gamma: ", round(estado_s_p$gamma, 3),
                        "<br>Dia mudança: ", estado_s_p$dia_mudanca,
                        "<br>Pico no dia ", pico_date, "<br>com ",
                        round(max(
                            df %>% filter(SEIR == "infectado") %>% pull(valor) %>% max()
                        )), " infectados<br>"),
                        style = list( color = 'black', fontWeight = 'bold', fontSize = "12px"
                        ),
                        y = - 65),
                    width = 1.5))
            ) %>% 
            hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b')) %>%
            hc_add_series(df, hcaes(date, round(valor), group = SEIR), type = "line") %>% 
            #hc_tooltip(
            #    formatter = JS("function(){
            #                return ('Número de pessoas ' + this.series.name + ': ' + this.y + 
            #                ' <br> Data: ' + this.x)
            #                }")
            #) %>% 
            hc_exporting(enabled = TRUE)
    })
    
    output$SEIRUHD_plot <- renderHighchart({
        
        df <- SEIRHUD_data %>% filter(state == state_proxy()[1]) 
        df_aux <- df
        pico_date <- df %>% filter(Infectado_sintomatico == max(df$Infectado_sintomatico)) %>% 
            pull(date)
        
        df <- df %>% 
            select(date, Infectado_sintomatico, Infectado_assintomatico,
                   Hospitalizado, UTI, mortes_pred) %>% 
            pivot_longer(- date, names_to = "SEIRUHD", values_to = "valor") %>% 
            mutate(date_aux = paste0(month(date, label = TRUE), "-", year(date)))
        
        estado_s_p <- SEIRHUD_state_sum %>% filter( state == state_proxy()[1] ) %>% 
            transmute(beta1, gammaH, dia_mudanca = round(dia_mudanca)) %>% 
            top_n(dia_mudanca, n = 1)
        
        highchart() %>% 
            hc_title(text = paste0("Modelo SEIRUHD ","<b>",state_proxy()[1], "</b>"),
                     margin = 20, align = "left",
                     style = list(color = "#05091A", useHTML = TRUE)) %>% 
            hc_yAxis(plotLines = list(
                list(
                    color = NA, value = max(df_aux$Infectado_assintomatico)/2,
                    label = list(text = paste0(
                        "Beta1: ", round(estado_s_p$beta1, 3),
                        "<br>GammaH: ", round(estado_s_p$gammaH, 3),
                        "<br>Dia mudança: ", estado_s_p$dia_mudanca,
                        "<br>Pico no dia ", pico_date, "<br>com ",
                        round(max(df_aux$Infectado_assintomatico)),
                        " infectados sintomaticos<br>"),
                        style = list( color = 'black', fontWeight = 'bold', fontSize = "12px"
                        ),
                        y = - 65),
                    width = 1.5))
            ) %>% 
            hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b')) %>%
            hc_add_series(df, hcaes(date, round(valor), group = SEIRUHD), type = "line") %>% 
            #hc_tooltip(
            #    formatter = JS("function(){
            #                return ('Número de pessoas ' + this.series.name + ': ' + this.y + 
            #                ' <br> Data: ' + this.x)
            #                }")
            #) %>% 
            hc_exporting(enabled = TRUE)
        
        
    })
    
    output$suc_comp_plot <- renderHighchart({
        
        new_data <- estados_SIR %>% dplyr::transmute(date, state, SIR = suscetivel) %>% 
            left_join(
                estados_SEIR %>% dplyr::transmute(date, state, SEIR = sucetivel),
                by = c("date", "state")
            ) %>% 
            left_join(
                SEIRHUD_data %>% dplyr::transmute(date, state, SEIRHUD = sucetivel),
                by = c("date", "state")
            ) %>% 
            filter(state == state_proxy()[1]) %>% 
            select(-state) %>% 
            pivot_longer(- date, names_to = "Modelo", values_to = "Valor")
        
        
        highchart() %>% 
            hc_title(text = paste0("Comparação de Suscetíveis ","<b>",state_proxy()[1], "</b>"),
                     margin = 20, align = "left",
                     style = list(color = "#05091A", useHTML = TRUE)) %>% 
            hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b')) %>% 
            hc_add_series(new_data, hcaes(x = date, y = round(Valor), group = Modelo),
                          type = "line") %>% 
            hc_exporting(enabled = TRUE)
        
    })
    
    output$rec_comp_plot <- renderHighchart({
        
        new_data <- estados_SIR %>% dplyr::transmute(date, state, SIR = recuperado) %>% 
            left_join(
                estados_SEIR %>% dplyr::transmute(date, state, SEIR = recuperado),
                by = c("date", "state")
            ) %>% 
            left_join(
                SEIRHUD_data %>% dplyr::transmute(date, state, SEIRHUD = recuperado),
                by = c("date", "state")
            ) %>% 
            filter(state == state_proxy()[1]) %>% 
            select(-state) %>% 
            pivot_longer(- date, names_to = "Modelo", values_to = "Valor")
        
        
        highchart() %>% 
            hc_title(text = paste0("Comparação de Recuperados ","<b>",state_proxy()[1], "</b>"),
                     margin = 20, align = "left",
                     style = list(color = "#05091A", useHTML = TRUE)) %>% 
            hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b')) %>% 
            hc_add_series(new_data, hcaes(x = date, y = round(Valor), group = Modelo),
                          type = "line") %>% 
            hc_exporting(enabled = TRUE)
        
    })
    
    output$inf_comp_plot <- renderHighchart({
        
        new_data <- estados_SIR %>% dplyr::transmute(date, state, SIR = infectado) %>% 
            left_join(
                estados_SEIR %>% dplyr::transmute(date, state, SEIR = infectado),
                by = c("date", "state")
            ) %>% 
            left_join(
                SEIRHUD_data %>% dplyr::transmute(date, state, 
                                                  "SEIRHUD assintomatico" = Infectado_assintomatico, 
                                                  "SEIRHUD sintomatico" = Infectado_sintomatico),
                by = c("date", "state")
            ) %>% 
            filter(state == state_proxy()[1]) %>% 
            select(-state) %>% 
            pivot_longer(- date, names_to = "Modelo", values_to = "Valor")
        
        
        highchart() %>% 
            hc_title(text = paste0("Comparação de Infectados ","<b>", state_proxy()[1], "</b>"),
                     margin = 20, align = "left",
                     style = list(color = "#05091A", useHTML = TRUE)) %>% 
            hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b')) %>% 
            hc_add_series(new_data, hcaes(x = date, y = round(Valor), group = Modelo),
                          type = "line") %>% 
            hc_add_series(type = "line", name = "SEIRUHD Total", #hide = TRUE, 
                          visible = FALSE,
                          SEIRHUD_data %>% filter( state == state_proxy()[1] ) %>% 
                              mutate(SEIRUHD_Total = Infectado_assintomatico + Infectado_sintomatico),
                          hcaes(x = date, y = SEIRUHD_Total)
                          
            ) %>% 
            hc_exporting(enabled = TRUE)
        
    })
    
    output$SIR_comp_plot <- renderHighchart({
        
        df <- estados_SIR %>% 
            drop_na() %>% 
            filter(state == state_proxy()[1])
        
        highchart() %>%
            hc_title(text = paste0("Relação de casos preditos e observados modelo SIR ",
                                   "<b>",state_proxy()[1], "</b>"),
                     margin = 20, align = "left",
                     style = list(color = "#05091A", useHTML = TRUE, fontSize = "15px")) %>% 
            hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b')) %>% 
            hc_add_series(data = df, hcaes(x = date, y = round(totalCasesPred)), 
                          type = "line", name = "Casos Preditos") %>% 
            hc_add_series(data = df, hcaes(x = date, y = TOTAL), 
                          tooltip = list(pointFormat = "Data: {point.date}",
                                         headerFormat = "<b>{point.y} Casos</b><br>"),
                          type = "scatter", name = "Casos Reportados") %>% 
            hc_plotOptions(line = list(color = "#4471EB",
                                       marker = list(enabled = FALSE)),
                           scatter = list(color = "black")) %>% 
            hc_exporting(enabled = TRUE)
        
    })
    
    output$SIR_res <- renderHighchart({
        
        df <- estados_SIR %>% 
            drop_na() %>% 
            filter(state == state_proxy()[1])
        
        highchart() %>%
            hc_title(text = paste0("Residuo do modelo para casos acumulados ",
                                   "<b>",state_proxy()[1], "</b>"),
                     margin = 20, align = "left",
                     style = list(color = "#05091A", useHTML = TRUE, fontSize = "15px")) %>% 
            hc_xAxis(title = list(text = "Casos reportados")) %>% 
            hc_yAxis(title = list(text = "Casos preditos")) %>% 
            hc_add_series(showInLegend = FALSE, 0:max(df$TOTAL, na.rm = TRUE)) %>% 
            hc_plotOptions(line = list(color = "#4471EB",
                                       marker = list(enabled = FALSE)),
                           scatter = list(color = "black")) %>% 
            hc_add_series(data = df, hcaes(x = TOTAL, y = round(totalCasesPred)), 
                          type = "scatter", showInLegend = FALSE) %>% 
            hc_exporting(enabled = TRUE)
    })
    
    output$SEIR_comp_plot <- renderHighchart({
        
        df <- estados_SEIR %>% 
            drop_na() %>% 
            filter(state == state_proxy()[1])
        
        highchart() %>%
            hc_title(text = paste0("Relação de casos preditos e observados modelo SEIR ",
                                   "<b>",state_proxy()[1], "</b>"),
                     margin = 20, align = "left",
                     style = list(color = "#05091A", useHTML = TRUE, fontSize = "15px")) %>% 
            hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b')) %>% 
            hc_add_series(data = df, hcaes(x = date, y = round(totalCasesPred)), 
                          type = "line", name = "Casos Preditos") %>% 
            hc_add_series(data = df, hcaes(x = date, y = TOTAL), 
                          tooltip = list(pointFormat = "Data: {point.date}",
                                         headerFormat = "<b>{point.y} Casos</b><br>"),
                          type = "scatter", name = "Casos Reportados") %>% 
            hc_plotOptions(line = list(color = "#4471EB",
                                       marker = list(enabled = FALSE)),
                           scatter = list(color = "black")) %>% 
            hc_exporting(enabled = TRUE)
        
    })
    
    output$SEIRHUD_comp_plot_cas <- renderHighchart({
        
        df <- SEIRHUD_data %>% 
            drop_na() %>% 
            filter(state == state_proxy()[1])
        
        highchart() %>%
            hc_title(text = paste0("Relação de casos preditos e observados modelo SEIRHUD ",
                                   "<b>",state_proxy()[1], "</b>"),
                     margin = 20, align = "left",
                     style = list(color = "#05091A", useHTML = TRUE, fontSize = "15px")) %>% 
            hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b')) %>% 
            hc_add_series(data = df, hcaes(x = date, y = round(totalCasesPred)), 
                          type = "line", name = "Casos Preditos") %>% 
            hc_add_series(data = df, hcaes(x = date, y = TOTAL),
                          tooltip = list(pointFormat = "Data: {point.date}",
                                         headerFormat = "<b>{point.y} Casos</b><br>"),
                          type = "scatter", name = "Casos Reportados") %>% 
            hc_plotOptions(line = list(color = "#4471EB",
                                       marker = list(enabled = FALSE)),
                           scatter = list(color = "black")) %>% 
            hc_exporting(enabled = TRUE)
        
    })
    
    output$SEIRHUD_comp_plot_mor <- renderHighchart({
        
        df <- SEIRHUD_data %>% 
            drop_na() %>% 
            filter(state == state_proxy()[1])
        
        highchart() %>%
            hc_title(text = paste0("Relação de mortes preditas e observadas modelo SEIRHUD ",
                                   "<b>",state_proxy()[1], "</b>"),
                     margin = 20, align = "left",
                     style = list(color = "#05091A", useHTML = TRUE, fontSize = "15px")) %>% 
            hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b')) %>% 
            hc_add_series(data = df, hcaes(x = date, y = round(mortes_pred)), 
                          color = "#E8402C",
                          type = "line", name = "Mortes Preditas") %>% 
            hc_add_series(data = df, hcaes(x = date, y = mortes), 
                          tooltip = list(pointFormat = "Data: {point.date}",
                                         headerFormat = "<b>{point.y} Mortes</b><br>"),
                          type = "scatter", name = "Mortes Reportadas") %>% 
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