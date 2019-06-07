##### SERVER #####
server <- function(input, output, session) {
  
  averages <- CHVIdata %>%
    group_by(def, ind, strata) %>%
    summarise(stateAverage = mean(est, na.rm=T))
  
  
  #####  reactive table (tab 1 - single county) #####
  
  data.tab1 <- eventReactive(input$cnty1,{
    
    CHVIdata %>% filter(county == input$cnty1 &  race == "Total") %>% 
      left_join(averages) %>% 
      rename(County = county, 
             Region = climReg, 
             Indicator = def, 
             Strata = strata,
             Value = est, 
             CA_avg = stateAverage
      ) %>%
      mutate(label = ifelse(Strata == "none",paste0(defShort), paste0(defShort, " - ", Strata)),
             ratio = ifelse(is.na(Value), 0, Value/CA_avg),
             Category = ifelse(ratio < 0.9, "below CA average",
                               ifelse(ratio > 1.1, "above CA average","around CA average")),
             fillColor = ifelse(ratio < 0.9, "#F2F1E6",
                                ifelse(ratio > 1.1, "#685DA9","#9198AA")))
  })
  
  
  ##### Download the csv of (tab 1 - single county)  ######  
  output$downloadCountySnapshot <- downloadHandler(
    filename = function () {
      paste0("countySnapshotFigure.csv")
    },
    
    content = function(file) {
      write.csv(data.tab1(), file, row.names = F)
    }
    
  )
  
  
  
  
  ##### generate County (tab 1 - single county) Plot #####
  
  output$plotCounty <- renderPlotly({
    
    tab1.df <- data.tab1()
    
    plot_ly( 
      data = tab1.df,
      x =  ~ round(ratio,2),
      y =  ~ reorder(label, ratio),
      marker = list(color = tab1.df[["fillColor"]],
                    line = list(color = "#404040", width=.5)
      ),
      type = "bar",
      hoverinfo = 'text',
      text = ~paste('</br>', ifelse(tab1.df[["Strata"]] =="none", paste0(tab1.df[["Indicator"]]), paste0(tab1.df[["Indicator"]]," - ",tab1.df[["Strata"]])),
                    '</br> County Value:', round(tab1.df[["Value"]],2),
                    '</br> State Average:', round(tab1.df[["CA_avg"]],2)),
      
      showlegend = FALSE
    ) %>%
      layout(title = paste0('County Snapshot for ',tab1.df[["County"]], ' County -   \n (shows how the values in the county compare to the state average)' ),
             titlefont=f2,
             margin = list(l = 300,
                           t = 70),
             xaxis = list(
               title = "Ratio to State Average",
               titlefont=f1,
               size = 4,
               autotick = TRUE,
               ticks = "outside",
               tick0 = 0,
               dtick = 1,
               ticklen = 5,
               tickwidth = 2,
               tickcolor = toRGB("black")
             ),
             yaxis = list(title = "Indicator and Strata", 
                          titlefont=f1,
                          type = "category", 
                          dtick=1, 
                          size=2), 
             shapes = list(
               list(
                 type = "rect", 
                 fillcolor = "#F2F1E6",
                 line = list(color = "#F2F1E6"),
                 opacity = .1,
                 y0 = 0, 
                 y1 = 1, 
                 yref = "paper",
                 x0 = 0, 
                 x1 = 0.9 
               ), 
               list(
                 type = "rect", 
                 fillcolor = "#9198AA",
                 line = list(color = "#9198AA"),
                 opacity = .1,
                 y0 = 0, 
                 y1 = 1, 
                 yref = "paper",
                 x0 = 0.9, 
                 x1 = 1.1 
               ),
               list(
                 type = "rect", 
                 fillcolor = "#685DA9",
                 line = list(color = "#685DA9"),
                 opacity = .1,
                 y0 = 0, 
                 y1 = 1, 
                 yref = "paper",
                 x0 = 1.1, 
                 x1 = max(tab1.df[["ratio"]]) 
               ))
      )  %>%
      config(collaborate = FALSE,
             displaylogo = FALSE,
             modeBarButtonsToRemove = list(
               'toggleSpikelines',
               'sendDataToCloud',
               'hoverCompareCartesian',
               'zoom2d',
               'pan2d',
               'select2d',
               'lasso2d',
               'zoomIn2d',
               'zoomOut2d',
               'autoScale2d',
               'resetScale2d',
               'hoverClosestCartesian'
             )
      )
    
    # ggplot() + 
    # geom_bar(aes(x=reorder(label, ratio), y=ratio, fill=category), stat="identity") +
    # coord_flip() +
    # geom_hline(yintercept = 1, linetype="dashed") + 
    # xlab("Indicator and Strata") +
    # ylab("Ratio to State Average") +
    # scale_fill_discrete(name="")
    
    
  })
  
  
  ##### generate County (tab 1 - single county) Table ######  
  
  output$countyTable <- DT::renderDataTable({DT::datatable(
    
    data.tab1() %>%
      select(County, Region, Indicator, Strata, Value, CA_avg, Category), 
    options=list(pageLength = 25)
  )  %>% DT::formatRound(c(5,6), 1)
    
  })
  
  
  
  ##### generate strata selection dropdown #####
  
  output$chooseStrata <- renderUI({
    selectInput("strt",
                "Strata",{
                  
                  unique(
                    as.character({CHVIdata %>% filter(def == input$ind)}$strata)
                  )
                  
                }
    )
  })
  
  
  
  ##### create reactive table for single indicator #####
  
  data.tab2 <- reactive({
    
    CHVIdata %>%
      mutate(COUNTYFI_1 = as.character(paste0("0",COUNTYFI_1))) %>%
      filter(def == input$ind & strata == input$strt & race == "Total") %>%
      rename(
        County = county,
        Region = climReg,
        Definition = def,
        Mean = est, 
        Numerator = numratr,
        Denominator = denmntr) %>%
      mutate(selCounty = ifelse(County == input$cnty, "yes", "no"),
             selRegion = ifelse(Region == CHVIdata$climReg[CHVIdata$county == input$cnty][1], 
                                paste0("In ",CHVIdata$climReg[CHVIdata$county == input$cnty][1]," region"),
                                "Outside region"),
             countyColor = ifelse(County == input$cnty,"rgba(104,93,169, 1)", 
                                  ifelse(Region == CHVIdata$climReg[CHVIdata$county == input$cnty][1],
                                         "rgba(145,152,170, 0.5)",
                                         "rgba(242,241,230, 0.3)")))
    
  })
  
  
  ##### Download the csv of (tab 2 - single indicator)  ######  
  output$downloadSingleIndicator <- downloadHandler(
    filename = function () {
      paste0("singleIndicatorFigure.csv")
    },
    
    content = function(file) {
      write.csv(data.tab2(), file, row.names = F)
    }
    
  )
  
  
  ##### generate table of the data (tab 2 - single indicator) #####
  
  output$table <- DT::renderDataTable({DT::datatable(
    data.tab2()  %>%
      select(
        County,
        Region,
        Definition,
        strata,
        Mean,
        LL95,
        UL95,
        Numerator,
        Denominator
      )) %>% DT::formatRound(c(5:9), 1)
  })
  
  
  
  
  ##### Census tract data (tab 2 - single indicator) ######
  
  tractData <- reactive({
    
    CHVItracts %>% 
      filter(def == input$ind & strata == input$strt) # %>%
    # mutate(ct10 = as.character(paste0('0',ct10))) 
    
  })
  
  
  selectedFIPS <- eventReactive(input$cnty, {
    
    as.character(paste0(CHVIdata$COUNTYFI_1[CHVIdata$county == input$cnty][1]))
    
  })
  
  
  average <- eventReactive(c(input$ind, input$strt), {
    
    as.numeric({averages %>% filter(def == input$ind & strata == input$strt) %>%
        ungroup() %>% select(stateAverage)}[1])
  })
  
  ##### generate map (tab 2 - single indicator) #####
  
  output$map <- renderLeaflet({
    
    mapTemp <- tracts %>% 
      filter(COUNTYFI_1 == selectedFIPS()) %>%
      left_join(tractData()) 
    
    # countyTemp <- left_join(counties, data.tab2())
    
    pal <- colorQuantile(
      palette = c("#685DA9",
                  "#CB6F6B", 
                  "#EFA96E",  
                  "#2A8CC5",
                  "#F2F1E6") ,
      n = 5,
      reverse = TRUE,
      domain =  unique(na.exclude(mapTemp$est))
    )
    
    pal2 <- colorQuantile(
      palette =  c("#685DA9",
                   "#CB6F6B", 
                   "#EFA96E",  
                   "#2A8CC5",
                   "#F2F1E6"),
      n = 5,
      reverse = TRUE,
      domain = unique(na.exclude(tractData()$est))
      
    )
    
    mapTemp %>%
      leaflet()  %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%  
      addPolygons(
        color = "#444444",
        weight = 1,
        smoothFactor = 0.1,
        fillOpacity = 0.6,
        fillColor = ~ pal2(est),
        highlightOptions = highlightOptions(color = "white", weight = 2,
                                            bringToFront = TRUE),
        popup = paste0("This is tract ", mapTemp$ct10, " in ",mapTemp$county," County. The ",mapTemp$def," in this tract is ",
                       round(mapTemp$est,1),"%. The county average is ", round(mean(mapTemp$est, na.rm=T),1),
                       "%. The state average is ", round(average(),1), "%."),
        group="State Quintiles") %>%
      addPolygons(
        color = "#444444",
        weight = 1,
        smoothFactor = 0.1,
        fillOpacity = 0.6,
        fillColor = ~ pal(est),
        highlightOptions = highlightOptions(color = "white", weight = 2,
                                            bringToFront = TRUE),
        popup = paste0("This is tract ", mapTemp$ct10, " in ",mapTemp$county," County. The ",mapTemp$def," in this tract is ",
                       round(mapTemp$est,1),"%. The county average is ", round(mean(mapTemp$est, na.rm=T),1),
                       "%. The state average is ", round(average(),1), "%."),
        group="County Quintiles") %>%
      addLayersControl(
        baseGroups = c("County Quintiles", "State Quintiles"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      
      addLegend("bottomleft",
                colors =  c("#685DA9",
                            "#CB6F6B", 
                            "#EFA96E",  
                            "#2A8CC5",
                            "#F2F1E6"),
                opacity = .4,
                title = input$ind,
                labels = c("Higher than 80% of other tracts (Most Vulnerable)","Higher than 60% of other tracts","Higher than 40% of other tracts","Higher than just 20% of other tracts","Lowest 20% of tracts (Least Vulnerable)")
                ##lables = c("X%-X% (most vulnerable)","X%-X%","X%-X%","X%-X%","X%-X% (least"%. In this tract, the "mapTemp$def," is higher than "mapTemp),))##
      )
    
    
  })
  
  
  ##### Download the csv of (tab 2 - single indicator map)  ######  
  output$downloadSingleIndicatorMap <- downloadHandler(
    filename = function () {
      paste0("singleIndicatorMapData.csv")
    },
    
    content = function(file) {
      write.csv({
        tractData() %>%
          filter(county == input$cnty)
      }, file, row.names = F)
    }
    
  )
  
  
  
  
  ######   county map for (tab 2 - single indicator) ######
  ##### generate map (tab 2) #####
  
  # output$map <- renderLeaflet({
  #  
  #     mapTemp <- left_join(counties, data.tab2()) 
  #     
  #     pal <- colorQuantile(
  #       palette = "RdYlBu",
  #       n = 5,
  #       reverse = TRUE,
  #       domain = mapTemp$Mean
  #     )
  #     
  #     mapTemp %>%
  #       leaflet()  %>% 
  #       addTiles() %>%
  #       addPolygons(
  #         color = "#444444",
  #         weight = 1,
  #         smoothFactor = 0.1,
  #         fillOpacity = 0.6,
  #         fillColor = ~ pal(Mean),
  #         highlightOptions = highlightOptions(color = "white", weight = 2,
  #                                             bringToFront = TRUE),
  #         label = ~ (mapTemp$County),
  #         popup = paste0("This is ",mapTemp$County," County. The ",mapTemp$Definition," in this county is ",
  #                        round(mapTemp$Mean[mapTemp$County == mapTemp$County]),". The state average is ", round(mean(mapTemp$Mean, na.rm=T),2))
  #       ) %>%
  #       addLegend("topright",
  #                 pal = pal,
  #                 values = ~ Mean,
  #                 opacity = 1,
  #                 labFormat = labelFormat(),
  #                 title = input$ind 
  #       ) %>%
  #       clearControls()
  #   
  #   
  # })
  
  
  
  
  
  
  
  ##### generate plot (tab 2 - single indicator) #####
  
  
  output$plot <- renderPlotly({
    
    tab2.df <- data.tab2()
    
    plot_ly(
      data = tab2.df,
      x =  ~ round(Mean, 2),
      y =  ~ reorder(County, Mean),
      marker = list(color = tab2.df[["countyColor"]],
                    line = list(color = "#404040", width=.5)
      ),
      type = "bar",
      hoverinfo = 'text',
      text = ~paste('</br> County:',tab2.df[["County"]],
                    '</br> Region:', tab2.df[["Region"]],
                    '</br> Indicator:', round(tab2.df[["Mean"]],2), tab2.df[["defShort"]],
                    '</br> State Average:', round(averages$stateAverage[averages$def == input$ind & averages$strata == input$strt],2)),
      showlegend = FALSE
    ) %>%
      layout(
        title = paste0(input$ind, " \n for California Counties \n (",input$cnty," [dark], Climate region [light], CA avg [dotted])"),
        titlefont=f2,
        margin = list(l = 130,
                      t = 105),
        xaxis = list(
          title = ifelse(input$strt == "none", input$ind,paste0(input$ind," - ", input$strt)),
          titlefont=f1,
          autotick = TRUE,
          ticks = "outside",
          tick0 = 0,
          dtick = 0.25,
          ticklen = 5,
          tickwidth = 2,
          tickcolor = toRGB("black")
        ),
        yaxis = list(title = "Counties",
                     titlefont=f1,
                     type = "categorical",
                     dtick=1,
                     tickfont=list(
                       size=8
                     )
        ), 
        shapes = list(
          type = "line", 
          y0 = 0, 
          y1 = 1, 
          yref = "paper",
          x0 = averages$stateAverage[averages$def == input$ind & averages$strata == input$strt], 
          x1 = averages$stateAverage[averages$def == input$ind & averages$strata == input$strt], 
          line = list(color = "black", dash = "dot"), opacity = .5
        )
      ) %>%
      config(collaborate = FALSE,
             cloud = FALSE,
             displaylogo = FALSE,
             modeBarButtonsToRemove = list(
               'toggleSpikelines',
               'sendDataToCloud',
               'hoverCompareCartesian',
               'zoom2d',
               'pan2d',
               'select2d',
               'lasso2d',
               'zoomIn2d',
               'zoomOut2d',
               'autoScale2d',
               'resetScale2d',
               'hoverClosestCartesian'
             )
      )
    
    
  })
  
  
  ##### make vulnerability table #####
  
  triple <- reactive({
    
    foo <- {CHVIdata %>% 
        filter(def  == input$exposure & strata %in% c("2070-2099", 2070-2099, "none") & race == "Total") %>%
        mutate(expTer = ntile(est, 3),
               def =  ifelse(strata == "none", def,paste0(def," - ", strata))) %>%
        select(county, climReg, COUNTYFI_1, def, est, expTer) %>% 
        spread(key = def, value = est)
    } %>% left_join({
      
      CHVIdata %>% 
        filter(def  == input$sensitivity & strata %in% c("Overall","ViolentCrime","total","2006-2010","2009-2013","All Non-English","none", "population-weighted") & race =="Total") %>%
        mutate(sensTer = ntile(est, 3),
               def =  ifelse(strata == "none", def,paste0(def," - ", strata))) %>%
        select(county, climReg, COUNTYFI_1, def, est, sensTer) %>% 
        spread(key = def, value = est) %>% 
        left_join({
          CHVIdata %>%
            filter(def  == "Percent of population aged 65 years or older" & race == "Total") %>%
            select(county, denmntr)
        }) %>%
        rename(Population = denmntr)
    }) %>%  
      mutate(Population = as.numeric(as.character(Population)),
             vulnerability = factor(ifelse(expTer == 1 & sensTer == 1, "Low Exposure, Low Sensitivity",
                                           ifelse(expTer == 1 & sensTer == 2, "Low Exposure, Medium Sensitivity",
                                                  ifelse(expTer == 1 & sensTer == 3, "Low Exposure, High Sensitivity",
                                                         ifelse(expTer == 2 & sensTer == 1, "Medium Exposure, Low Sensitivity",
                                                                ifelse(expTer == 3 & sensTer == 1, "High Exposure, Low Sensitivity",
                                                                       ifelse(expTer == 2 & sensTer == 2, "Medium Exposure, Medium Sensitivity",
                                                                              ifelse(expTer == 2 & sensTer == 3, "Medium Exposure, High Sensitivity",
                                                                                     ifelse(expTer == 3 & sensTer == 2, "High Exposure, Medium Sensitivity","High Exposure, High Sensitivity"
                                                                                     )))))))), levels = c("Low Exposure, Low Sensitivity",
                                                                                                          "Low Exposure, Medium Sensitivity",
                                                                                                          "Medium Exposure, Low Sensitivity",
                                                                                                          "Medium Exposure, Medium Sensitivity",
                                                                                                          "High Exposure, Low Sensitivity",
                                                                                                          "Low Exposure, High Sensitivity",
                                                                                                          "High Exposure, Medium Sensitivity",
                                                                                                          "Medium Exposure, High Sensitivity",
                                                                                                          "High Exposure, High Sensitivity"
                                                                                     )),
             sign = ifelse(expTer == 1 & sensTer == 1, "rgba(242,241,230, 0.5)",
                           ifelse(expTer == 1 & sensTer == 2,"rgba(42,140,197, 0.7)",
                                  ifelse(expTer == 1 & sensTer == 3, "rgba(51,101,147, 0.8)",
                                         ifelse(expTer == 2 & sensTer == 1, "rgba(239,169,110,.7)",
                                                ifelse(expTer == 3 & sensTer == 1, "rgba(203,111,107,0.8)",
                                                       ifelse(expTer == 2 & sensTer == 2, "rgba(145,152,170,0.7)",
                                                              ifelse(expTer == 2 & sensTer == 3, "rgba(149,123,152,0.9)",
                                                                     ifelse(expTer == 3 & sensTer == 2, "rgba(137,136,164,0.9)","rgba(104,93,169,1)"
                                                                     )))))))),
             size = ntile(Population,29)
      )
    
    foo <- na.omit(foo)
    
  })
  
  
  
  
  ##### Download the csv of (vulnerability tab)  ######  
  output$downloadVulnerabilityFigure <- downloadHandler(
    filename = function () {
      paste0("vulnerabilityFigure.csv")
    },
    
    content = function(file) {
      write.csv(triple(), file, row.names = F)
    }
    
  )
  
  
  ##### Download the csv of (vulnerability tab)  ######  
  output$downloadVulnerabilityMap <- downloadHandler(
    filename = function () {
      paste0("vulnerabilityMap.csv")
    },
    
    content = function(file) {
      write.csv(triple(), file, row.names = F)
    }
    
  )
  
  
  ##### make triple plot (tab 3) #####
  
  output$triplePlot <- renderPlotly({
    
    tri <- triple()
    
    plot_ly(
      data = tri,
      x =  ~ round(tri[[5]],2),
      y =  ~ round(tri[[7]],2),
      hoverinfo = 'text',
      text = ~paste('</br> County:',tri[["county"]],
                    '</br> Population:',format(tri[["Population"]], big.mark = ","),
                    '</br> Exposure:', round(tri[[5]],2), names(tri)[5],
                    '</br> Sensitivity:', round(tri[[7]],2),  names(tri)[7]),
      showlegend = FALSE
    ) %>%
      add_markers(type = 'scatter', 
                  mode = 'markers',
                  size = ~tri[["Population"]]+50,
                  marker = list(color = tri[["sign"]], 
                                size = tri[["size"]]*25,   
                                line = list(color = 'rgba(99,99,99, .8)',width = 0.5))) %>%
      add_text(type = 'scatter',mode = 'text', text = tri[["county"]], textposition = 'top right',
               textfont = list(
                 size = 10,
                 color = toRGB("grey40"))) %>%
      layout(title = paste('Combined Vulnerability from Exposure (',names(tri)[5],') \n and Sensitivity (',names(tri)[7],")") ,
             titlefont=f2,
             margin = list(l = 50,
                           t = 70),
             xaxis = list(
               title = names(tri)[5],
               titlefont = f1, 
               autotick = TRUE,
               ticks = "outside",
               tick0 = 0,
               dtick = 0.25,
               ticklen = 5,
               tickwidth = 2,
               tickcolor = toRGB("black")
             ),
             yaxis = list(title = names(tri)[7],
                          titlefont = f1, 
                          autotick = TRUE,
                          ticks = "outside",
                          tick0 = 0,
                          dtick = 0.25,
                          ticklen = 5,
                          tickwidth = 2,
                          tickcolor = toRGB("black"))
      ) %>%
      config(collaborate = FALSE,
             displaylogo = FALSE,
             modeBarButtonsToRemove = list(
               'toggleSpikelines',
               'sendDataToCloud',
               'hoverCompareCartesian',
               'hoverClosestCartesian'
             )
      )
    
  })
  
  
  ##### generate map (vulnerability Tab) #####
  #### Attention
  
  output$vulnMap <- renderLeaflet({
    
    mapTemp2 <- left_join(counties, triple()) %>% rename(County = county)
    
    pal <- colorFactor(c("#F2F1E6", 
                         "#2A8CC5", 
                         "#EFA96E",  
                         "#9198AA", 
                         "#CB6F6B",
                         "#336593",
                         "#8988A4",
                         "#957B98",
                         "#685DA9"), mapTemp2$vulnerability)
    
    
    mapTemp2 %>%
      leaflet()  %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addPolygons(
        color = "#444444",
        weight = 1,
        smoothFactor = 0.1,
        fillOpacity = 0.6,
        fillColor =  ~pal(vulnerability),
        highlightOptions = highlightOptions(color = "white", weight = 2,
                                            bringToFront = TRUE),
        label = ~ (County),
        popup = paste0("This is ",mapTemp2$County," County. </br> Its vulnerability is defined by ", mapTemp2$vulnerability, 
                       '</br> Exposure: ', round(triple()[[5]],2), ' ', names(triple())[5],
                       '</br> Sensitivity: ', round(triple()[[7]],2), ' ',  names(triple())[7])) %>%
      addLegend("topright",
                pal = pal,
                values = ~ vulnerability,
                opacity = 1,
                labFormat = labelFormat(),
                title = input$ind 
      ) %>%
      clearControls()
    
  })
  
  
  
  
  data.dnld <- eventReactive(c(input$cntyDNLD, input$indDNLD),{
    
    if(input$cntyDNLD  == "All" & input$indDNLD != "All") {
      
      CHVIdata %>%
        mutate(COUNTYFI_1 = as.character(paste0("0",COUNTYFI_1))) %>%
        filter(def == input$indDNLD) %>%
        rename(
          County = county,
          Region = climReg,
          Definition = def,
          Mean = est, 
          Numerator = numratr,
          Denominator = denmntr,
          Strata = strata, 
          Race = race)
    } else { if(input$cntyDNLD  != "All" & input$indDNLD != "All") {
      
      CHVIdata %>%
        mutate(COUNTYFI_1 = as.character(paste0("0",COUNTYFI_1))) %>%
        filter(def == input$indDNLD & county == input$cntyDNLD) %>%
        rename(
          County = county,
          Region = climReg,
          Definition = def,
          Mean = est, 
          Numerator = numratr,
          Denominator = denmntr,
          Strata = strata, 
          Race = race)
    } else { if(input$cntyDNLD  != "All" & input$indDNLD == "All") {
      
      CHVIdata %>%
        mutate(COUNTYFI_1 = as.character(paste0("0",COUNTYFI_1))) %>%
        filter(county == input$cntyDNLD) %>%
        rename(
          County = county,
          Region = climReg,
          Definition = def,
          Mean = est, 
          Numerator = numratr,
          Denominator = denmntr,
          Strata = strata, 
          Race = race)
    } else {   
      
      CHVIdata %>%
        mutate(COUNTYFI_1 = as.character(paste0("0",COUNTYFI_1))) %>%
        rename(
          County = county,
          Region = climReg,
          Definition = def,
          Mean = est, 
          Numerator = numratr,
          Denominator = denmntr,
          Strata = strata, 
          Race = race)
    } } }
    
  })
  
  
  ##### Download the csv of the selected data (download tab)  ######  
  output$downloadData <- downloadHandler(
    filename = function () {
      paste0("selectedCHVIdata.csv")
    },
    
    content = function(file) {
      write.csv({
        data.dnld() %>%
          select(
            County,
            Region,
            Definition,
            Strata,
            Race,
            Mean,
            LL95,
            UL95,
            Numerator,
            Denominator
          )
      }, file, row.names = F)
    }
    
  )
  
  
  # ##### Download the kml of the selected data (download tab)  ######  
  # output$downloadSpatial <- downloadHandler(
  #   filename = function () {
  #     paste0("selectedCHVIdata.kml")
  #   },
  #   
  #   content = function(file) {
  #    
  #     left_join(counties, data.dnld()) %>% st_transform(crs = 4326) %>% 
  #       st_write(dsn = file)
  #     
  #   }
  #   
  # )
  
  
  
  ##### generate (download tab) Table ######  
  
  output$downloadTable <- DT::renderDataTable({DT::datatable(
    
    data.dnld() %>%
      select(
        County,
        Region,
        Definition,
        Strata,
        Race,
        Mean,
        LL95,
        UL95,
        Numerator,
        Denominator
      ), 
    options=list(pageLength = 25)
  )  %>% DT::formatRound(c(6:8), 1) %>%
      DT::formatRound(c(9:10), 0)
    
  })
  
  
  
  ##### Download the County Health Profile Report  ######  
  
  
  output$downloadCHPR <- renderUI({ 
    HTML(paste0('<a href =', links$CHPR.Link[links$County %in% c(input$cntyCHPR, paste0(input$cntyCHPR," "))],' target="_blank"  onclick="trackOutboundLink("', links$CHPR.Link[links$County %in% c(input$cntyCHPR, paste0(input$cntyCHPR," "))],'"); return false;">Download County Health Profile</a>'))
  })
  
  output$downloadCHPR1 <- renderUI({ 
    HTML(paste0('<a href =', links$CHPR.Link[links$County %in% c(input$cnty1, paste0(input$cnty1," "))],' target="_blank"  onclick="trackOutboundLink("', links$CHPR.Link[links$County %in% c(input$cnty1, paste0(input$cnty1," "))],'"); return false;">Download County Health Profile</a>'))
  })
  
  output$downloadNarrative <- renderUI({ 
    HTML(paste0('<a href =', narratives$narrativeLink[narratives$def == input$ind],' target="_blank"  onclick="trackOutboundLink("', narratives$narrativeLink[narratives$def == input$ind],'"); return false;">Download the Narrative for this Indicator</a>'))
  })
  
  
  output$blurb <- renderUI({ 
    h3(unique(CHVIdata$defHealth[CHVIdata$def == input$ind]))
  })
  
  
  
  # output$downloadCHPR2 <- downloadHandler(
  #   filename = function () {
  #     paste0(input$cnty,"CountyHealthProfileReport.pdf")
  #   },
  # 
  #   content = function(file) {
  #     file.copy(links$CHPR.Link[links$County %in% c(input$cnty, paste0(input$cnty," "))], file)
  #   }
  # 
  # )
  
  outputOptions(output, "map", suspendWhenHidden = FALSE)
  
}