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
            "SEIIR_base_model" = switch(
                input$is_bv,
                "bv" = long_praz_seir_bv(state_proxy()[1]),
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
    
    
})