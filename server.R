##### SERVER #####
server <- function(input, output, session) {
  
  
  showModal(
    modalDialog(
      title = HTML("<h3><center>We are looking for feedback from users like you!</center></h3>"),
      HTML("<ul>
       <li>What you are using the information on the site to do?</li> 
           <li>What do you like? What don't you like?</li>
           <li>What is missing that you'd like to see?</li></ul> <h1> <center><a href='mailto:cchep@cdph.ca.gov?subject=CCHVIz feedback'>Email us to share your ideas</a></h1>"),
      footer = NULL,
      easyClose = T
      # ,
      # footer = modalButton("Close")
    )
  )
  
  CHVIdata <- data.table::copy(CHVIdata)
  
  
  
  ##############   begin County Snapshot Tab ##########
  
  
  #####  reactive table (tab 1 - single county) #####
  
  data.tab1 <- eventReactive(input$cnty1,{
    
    CHVIdata[county == input$cnty1 &  race == "Total" & latest == "Y" & strata %in% c( "physical","mental","total", "2040-2060",  "2080-2099",  "population-weighted", "All Non-English","none","Overall","ViolentCrime")] %>% 
      merge(averages, c("def","ind","strata")) %>% 
      rename(County = county, 
             Region = climReg, 
             Indicator = def, 
             Strata = strata,
             County_Value = est, 
             CA_avg = stateAverage,
             category = catjv
      ) %>%
      mutate(label = ifelse(ind != "disability", defShort, paste0(defShort, " - ", Strata)),
             ratio = ifelse(is.na(County_Value), 0, County_Value/CA_avg),
             Category = ifelse(ratio < 0.9, "below CA average",
                               ifelse(ratio > 1.1, "above CA average","around CA average")),
             catlevel = ifelse(ratio < 0.9, "level4",
                                ifelse(ratio > 1.1, "level4","level4")))
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
  
  
  
  # 
  # output$test <- renderDataTable({
  #   
  #   foo <- input$cntyDNLD
  #   
  #   CHVIdata})
  # output$test2 <- renderDataTable(triple())
  # 
  # 
  
  
  ##### generate County (tab 1 - single county) Plot #####
  
  output$plotCounty <- renderPlotly({
    
    tab1.df <- data.tab1() %>% merge(colorCoder, by = c("category","catlevel"))
    
    plot_ly( 
      data = tab1.df,
      x =  ~ round(ratio,2),
      y =  ~ reorder(label, ratio),
      marker = list(color = tab1.df[["catcolor"]],
                    line = list(color = "#404040", width=.5),
                    opacity = .7
      ),
      type = "bar",
      hoverinfo = 'text',
      text = ~paste('</br>', ifelse(tab1.df[["Strata"]] =="none", paste0(tab1.df[["Indicator"]]), paste0(tab1.df[["Indicator"]]," - ",tab1.df[["Strata"]])),
                    '</br> County Value:', round(tab1.df[["County_Value"]],2),
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
             yaxis = list(title = "Indicator", 
                          titlefont=f1,
                          type = "category", 
                          dtick=1, 
                          size=2), 
             shapes = list(
               list(
                 type = "rect", 
                 fillcolor = "#f7f7f7",
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
                 fillcolor = "#cccccc",
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
                 fillcolor = "#969696",
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
    

  })
  
  
  
  
  ##### generate County (tab 1 - single county) Table ######  
  
  output$countyTable <- DT::renderDataTable({DT::datatable(
    
    data.tab1() %>%
         select(County, Region, Indicator, Strata, County_Value, CA_avg, Category), filter = 'top',
    options=list(pageLength = 25)
  )  %>% DT::formatRound(c(5,6), 1)
    
  })
  

  
  
  
  
  ##### generate Top Environmenta Indicators (tab 1 - single county)  ######  
  
  
  # 
  # ranks[catjv =="adaptive capacity" & county == "Alameda"] %>% setorder(-rankjv) %>%.[] .[,.(defShort, rankjv)]
  
  output$topindicators <- renderUI({
    HTML(
      paste0("<h3>Top Indicators for ",input$cnty1," County</h3>
        <h2><font color='#3182bd'>Environmental Exposures</font></h2>
        <h3>",
             {ranks[catjv =="environment" & county == input$cnty1] %>% setorder(-rankjv) %>% .[1, label]},
             "</h3><h5>",round({ranks[catjv =="environment" & county == input$cnty1] %>% setorder(-rankjv) %>% .[1, est]}, digits = 2),{ranks[catjv =="environment" & county == input$cnty1] %>% setorder(-rankjv) %>% .[1, units]},"</h5>
        <h3>",
             {ranks[catjv =="environment" & county == input$cnty1] %>% setorder(-rankjv) %>% .[2, label]},
             "</h3><h5>",round({ranks[catjv =="environment" & county == input$cnty1] %>% setorder(-rankjv) %>% .[2, est]},2),{ranks[catjv =="environment" & county == input$cnty1] %>% setorder(-rankjv) %>% .[2, units]},"</h5>",
             
          
             "<h2><font color='#d95f0e'>Population Sensitivity</font></h2> 
              <h3>",
             {ranks[catjv =="sensitivity" & county == input$cnty1] %>% setorder(-rankjv) %>% .[1, label]},
             "</h3><h5>",round({ranks[catjv =="sensitivity" & county == input$cnty1] %>% setorder(-rankjv) %>% .[1, est]},2),{ranks[catjv =="sensitivity" & county == input$cnty1] %>% setorder(-rankjv) %>% .[1, units]},"</h5>
        <h3>",
             {ranks[catjv =="sensitivity" & county == input$cnty1] %>% setorder(-rankjv) %>% .[2, label]},
             "</h3><h5>",round({ranks[catjv =="sensitivity" & county == input$cnty1] %>% setorder(-rankjv) %>% .[2, est]},2),{ranks[catjv =="sensitivity" & county == input$cnty1] %>% setorder(-rankjv) %>% .[2, units]},"</h5>
             <h3>",
             {ranks[catjv =="sensitivity" & county == input$cnty1] %>% setorder(-rankjv) %>% .[3, label]},
             "</h3><h5>",round({ranks[catjv =="sensitivity" & county == input$cnty1] %>% setorder(-rankjv) %>% .[3, est]},2),{ranks[catjv =="sensitivity" & county == input$cnty1] %>% setorder(-rankjv) %>% .[3, units]},"</h5>
             
             
             <h2><font color='#54278f'>Adaptive Capacity</font></h2>
             <h3>",
             {ranks[catjv =="adaptive capacity" & county == input$cnty1] %>% setorder(-rankjv) %>% .[1, label]},
             "</h3><h5>",round({ranks[catjv =="adaptive capacity" & county == input$cnty1] %>% setorder(-rankjv) %>% .[1, est]},2),{ranks[catjv =="adaptive capacity" & county == input$cnty1] %>% setorder(-rankjv) %>% .[1, units]},"</h5>
        <h3>",
             {ranks[catjv =="adaptive capacity" & county == input$cnty1] %>% setorder(-rankjv) %>% .[2, label]},
             "</h3><h5>",round({ranks[catjv =="adaptive capacity" & county == input$cnty1] %>% setorder(-rankjv) %>% .[2, est]},2),{ranks[catjv =="adaptive capacity" & county == input$cnty1] %>% setorder(-rankjv) %>% .[2, units]},"</h5>"
      )
    )
  })
  

  
  
  
  ##############   begin Single Indicator Tab ##########
  
  
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
    req(input$ind, input$strt)
    
    CHVIdata[def == input$ind &
               strata == input$strt & race == "Total" & latest == "Y"] %>%
      rename(
        County = county,
        Region = climReg,
        Definition = def,
        Mean = est,
        Numerator = numratr,
        Denominator = denmntr
      ) %>%
      mutate(
        selCounty = ifelse(County == input$cnty, "yes", "no"),
        selRegion = ifelse(
          Region == CHVIdata$climReg[CHVIdata$county == input$cnty][1],
          paste0("In ", CHVIdata$climReg[CHVIdata$county == input$cnty][1], " region"),
          "Outside region"
        ),
        countyColor = ifelse(
          County == input$cnty,
          "rgba(99,99,99, 1)",
          ifelse(
            Region == CHVIdata$climReg[CHVIdata$county == input$cnty][1],
            "rgba(189,189,189, 0.6)",
            "rgba(240,240,240, 0.3)"
          )
        )
      )
    
  })
  
  
  
  
  ##### Download the csv of (tab 2 - single indicator)  ######  
  output$downloadRacePlot <- downloadHandler(
    filename = function () {
      paste0("singleIndicatorByRaceFigure.csv")
    },
    
    content = function(file) {
      write.csv(race_data(), file, row.names = F)
    }
    
  )
  
  
  
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
      ), filter = 'top') %>% DT::formatRound(c(5:9), 1)
  })
  

  
  
  fivecolors <- reactive({
    
   if (unique(CHVIdata$catjv[CHVIdata$def == input$ind]) == "adaptive capacity") {
      c("#e6550d",
        "#fd8d3c",
        "#fdae6b",
        "#fdd0a2",
        "#feedde")
    } else if (unique(CHVIdata$catjv[CHVIdata$def == input$ind]) == "environment") {
      c("#08519c",
        "#3182bd",
        "#6baed6",
        "#bdd7e7",
        "#eff3ff")
    } else {
      c("#993404",
         "#d95f0e",
         "#fe9929",
         "#fed98e",
         "#ffffd4")
    }
    
    
  })
  
  
  ##### Census tract data (tab 2 - single indicator) ######
  
  tractData <- reactive({
    req(input$ind, input$strt)
    
    CHVItracts[def == input$ind & strata == input$strt & latest == "Y" & race == "Total"] # %>%
    # mutate(ct10 = as.character(paste0('0',ct10))) 
    
  })
  
  
  selectedFIPS <- eventReactive(input$cnty, {
    
    
    # as.character(paste0(CHVIdata$COUNTYFI_1[CHVIdata$county == "Alameda"][1]))
    
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
      left_join(tractData(), by = "ct10" ) 
    
    # countyTemp <- left_join(counties, data.tab2())
    
    
   pal <-  colorQuantile(
      palette = fivecolors(),
      n = 5,
      reverse = TRUE,
      domain =  unique(na.exclude(mapTemp$est))
    )
    
    
    # pal2 <- colorQuantile(
    #   palette =  c("#685DA9",
    #                "#CB6F6B", 
    #                "#EFA96E",  
    #                "#2A8CC5",
    #                "#F2F1E6"),
    #   n = 5,
    #   reverse = TRUE,
    #   domain = unique(na.exclude(tractData()$est)))
    
    mapTemp %>%
      leaflet()  %>% 
      addProviderTiles(providers$Esri.WorldStreetMap, group = "Street Map") %>%
      addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Sattelite") %>%
      addProviderTiles(providers$Stamen.Toner, group = "B/W") %>% 
      # addPolygons(
      #   color = "#444444",
      #   weight = 1,
      #   smoothFactor = 0.1,
      #   fillOpacity = 0.7,
      #   fillColor = ~ pal2(est),
      #   highlightOptions = highlightOptions(color = "white", weight = 2,
      #                                       bringToFront = TRUE),
      #   popup = paste0("This is tract ", mapTemp$ct10, " in ",mapTemp$county," County. The ",mapTemp$def," in this tract is ",
      #                  round(mapTemp$est,1),"%. The county average is ", round(mean(mapTemp$est, na.rm=T),1),
      #                  "%. The state average is ", round(average(),1), "%."),
      #   group="State Quintiles") %>%
      addPolygons(
        color = "#444444",
        weight = 1,
        smoothFactor = 0.1,
        fillOpacity = 0.7,
        fillColor = ~ pal(est),
        highlightOptions = highlightOptions(color = "white",
                                            weight = 2,
                                            bringToFront = TRUE),
        popup = paste0(
          "This tract is near ",
          mapTemp$Place,
          " in ",
          mapTemp$county,
          " County. The ",
          mapTemp$defShort,
          " in this tract is ",
          round(mapTemp$est, 1),
          mapTemp$units,
          ". The county average is ",
          round(mean(mapTemp$est, na.rm = T), 1)
          ,
          mapTemp$units,
          ". The state average is ",
          round(average(), 1),
          mapTemp$units,
          "."
        ),
        group = "County Quintiles") %>%
      addLayersControl(baseGroups = c("Street Map", "Terrain", "Sattelite", "B/W"),
                       # overlayGroups = c("County Quintiles", "State Quintiles"),
                       options = layersControlOptions(collapsed = TRUE)
      ) %>% 
      addLegend("bottomleft",
                colors = fivecolors(), 
                opacity = 1,
                title = input$ind,
                labels = c("Higher than 80% of other tracts (Most Vulnerable)","Higher than 60% of other tracts","Higher than 40% of other tracts","Higher than just 20% of other tracts","Lowest 20% of tracts (Least Vulnerable)")
                ##lables = c("X%-X% (most vulnerable)","X%-X%","X%-X%","X%-X%","X%-X% (least"%. In this tract, the "mapTemp$def," is higher than "mapTemp),))##
      ) %>% 
      hideGroup("State Quintiles")
    
    
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
  
  
  ##### generate plot (tab 2 - single indicator) #####
  
  
  output$plot <- renderPlotly({
    
    tab2.df <- data.tab2()
    
    plot_ly(
      data = tab2.df,
      y =  ~ round(Mean, 2),
      x =  ~ reorder(County, Mean),
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
        title = paste0(input$ind, " for California Counties \n (",input$cnty," [dark grey], Climate region [grey], CA avg [dotted line])"),
        titlefont=f2,
        margin = list(l = 90,
                      t = 75),
        yaxis = list(
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
        xaxis = list(title = "Counties",
                     titlefont=f1,
                     type = "categorical",
                     dtick=1,
                     tickangle=315,
                     tickfont=list(
                       size=8
                     )
        ), 
        shapes = list(
          type = "line", 
          x0 = 0, 
          x1 = 1, 
          xref = "paper",
          y0 = averages$stateAverage[averages$def == input$ind & averages$strata == input$strt], 
          y1 = averages$stateAverage[averages$def == input$ind & averages$strata == input$strt], 
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
  
  ##### generate race data (tab 2 - single indicator) #####
  
  race_data <- eventReactive(c(input$ind, input$strt, input$cnty),
                             {
                               
                               req(input$ind, input$strt)
                               
                               CHVIdata[def == input$ind &
                                          strata == input$strt &
                                          county == input$cnty & latest == "Y"] %>%
                                 mutate(race = ifelse(race == "Total", "County Average", race),
                                        category = catjv)
                               
                                                            
                               })
  
  
  ##### generate race plot (tab 2 - single indicator) #####
  
  
  output$racePlot <- renderPlotly({
    
  cntyavg <- race_data()$est[race_data()$race == "County Average"]
    
    
     foo <- race_data()  %>%
       mutate(catlevel = ifelse(est > cntyavg,
                                "level5",
                                ifelse(est < cntyavg,
                                       "level3",
                                       "level1"))) %>%
       data.table() %>%
       merge(colorCoder, by = c("category", "catlevel"))
     
    
    
    plot_ly( type = "bar",
             data = foo,
             x =  ~ reorder(race, est),
             y =  ~ est,
             marker = list(color = foo[["catcolor"]],
                           line = list(color = "#404040", width=.5))) %>%
      layout(
        title = paste0(foo$defShort[foo$def == input$ind], " within each race/ethnicity \n  in ",input$cnty," County"),
        titlefont=f2,
        margin = list(l = 90,
                      t = 75),
        yaxis = list(
          title = ifelse(input$strt == "none",foo$defShort[foo$def == input$ind],paste0(foo$defShort[foo$def == input$ind]," - ", input$strt)),
          titlefont=f1,
          autotick = TRUE,
          ticks = "outside",
          tick0 = 0,
          dtick = 0.25,
          ticklen = 5,
          tickwidth = 2,
          tickcolor = toRGB("black")
        ),
        xaxis = list(title = "Race/Ethnicity",
                     titlefont=f1,
                     type = "categorical",
                     dtick=1,
                     tickangle=315,
                     tickfont=list(
                       size=8
                     )
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
  
  
 ############## generate place list names  ##################
  
  placeNames <- eventReactive(c(input$ind, input$strt,  input$cnty),{
    
  req(input$ind, input$strt,  input$cnty)
    
    CHVItracts[def == input$ind & strata == input$strt & county == input$cnty & latest == "Y", .(value = round(mean(est), 2)), by=.(Place)] %>% setorder(-value)
    
  })
  
  
  output$placeList <- renderDataTable(
    DT::datatable(placeNames(), options = list(pageLength = 5))
    
  )
  
  
  
  
  output$downloadTable <- DT::renderDataTable({DT::datatable(
    
    data.dnld() %>%
      select(
        County,
        FIPS,
        Region,
        Definition,
        Strata,
        Race,
        Year,
        Mean,
        LL95,
        UL95,
        Numerator,
        Denominator
      ),  filter = 'top',
    options=list(pageLength = 25)
  )  %>% DT::formatRound(c(8:10),ifelse(input$indDNLD == "Average Daily Maximum Ozone Concentration", 3, 1)) %>%
      DT::formatRound(c(11:12), 0)
    
  })
  
  
  
##############   begin Vulnerability Tab ##########
  
  
  exposureData <- eventReactive(input$exposure, {
    
    if(input$exposure == "Population living in sea level rise inundation areas"){
   
      CHVIdata[est > 0 & race == "Total" & latest == "Y"]
      
    } else {
      
      CHVIdata
      
    }
    
    
    
  })
  
  
  ##### make vulnerability table #####
  
  triple <- reactive({
    req(input$exposure, input$sensitivity)

     foo <- {exposureData() %>%
        .[def  == input$exposure & latest =="Y" & race == "Total"] %>%
        mutate(expTer = ntile(est, 3),
               def =  ifelse(strata == "none", def,paste0(def," - ", strata))) %>%
        select(county, climReg, COUNTYFI_1, def, est, expTer) %>%
        spread(key = def, value = est)
    } %>% left_join({

      CHVIdata[def  == input$sensitivity & race == "Total" & latest == "Y" & {strata %in% c("none","All Non-English","Overall", "population-weighted", "total")}] %>%
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
                           ifelse(expTer == 1 & sensTer == 2,"rgba(239,169,110, 0.7)",
                                  ifelse(expTer == 1 & sensTer == 3, "rgba(203,111,107, 0.8)",
                                         ifelse(expTer == 2 & sensTer == 1, "rgba(42,140,197,.7)",
                                                ifelse(expTer == 3 & sensTer == 1, "rgba(51,101,147,0.8)",
                                                       ifelse(expTer == 2 & sensTer == 2, "rgba(145,152,170,0.7)",
                                                              ifelse(expTer == 2 & sensTer == 3, "rgba(137,136,164,0.9)",
                                                                     ifelse(expTer == 3 & sensTer == 2, "rgba(149,123,152,0.9)","rgba(104,93,169,1)"
                                                                     )))))))),
             size = ntile(Population,29)
      )

    foo <- na.omit(foo)

  })



  
  # vulnRegion <- reactive({unique(CHVIdata$climReg[CHVIdata$county == input$cnty1])})  
  # 
  # 
  # triple <- reactive({
  #   
  #   CHVItracts <- copy(CHVItracts)
  #   setkey(CHVItracts, def, race, climReg)
  #   
  #   CHVItracts[.(input$exposure, "Total",vulnRegion())] %>%
  #     .[latest == "Y"] %>%
  #     .[order(-est)] %>%
  #     .[, expTer := ntile(est, 100)] %>%
  #     .[, def :=  ifelse(strata == "none", def, paste0(def, " - ", strata))] %>%
  #        .[, .(county, climReg, ct10, def, est)] %>%
  #     dcast(... ~ def,value.var  = "est") %>%
  #     merge({
  #       CHVItracts[.(input$sensitivity, "Total", vulnRegion())] %>%
  #         .[strata %in% c(
  #           "Overall",
  #           "ViolentCrime",
  #           "total",
  #           "2006-2010",
  #           "2009-2013",
  #           "All Non-English",
  #           "none",
  #           "population-weighted"
  #         )] %>%
  #         .[order(-est)] %>%
  #         .[, sensTer := ntile(est, 100)] %>%
  #         .[, def :=  ifelse(strata == "none", def, paste0(def, " - ", strata))] %>%
  #          .[, .(county, climReg, ct10, def, est )] %>%
  #         dcast(... ~ def,value.var  = "est") %>%
  #         merge({
  #           CHVItracts[.("Percent of population aged 65 years or older")] %>%
  #             .[,.(ct10, denmntr)]
  #         }, by = "ct10") %>%
  #         .[, !c("county", "climReg")] %>% 
  #         rename(Population = denmntr)
  #     }) %>%
  #     .[, Population := as.numeric(as.character(Population))] %>%
  #     .[, vulnerability := factor(
  #       ifelse(
  #         expTer == 1 & sensTer == 1,
  #         "Low Exposure, Low Sensitivity",
  #         ifelse(
  #           expTer == 1 & sensTer == 2,
  #           "Low Exposure, Medium Sensitivity",
  #           ifelse(
  #             expTer == 1 & sensTer == 3,
  #             "Low Exposure, High Sensitivity",
  #             ifelse(
  #               expTer == 2 & sensTer == 1,
  #               "Medium Exposure, Low Sensitivity",
  #               ifelse(
  #                 expTer == 3 & sensTer == 1,
  #                 "High Exposure, Low Sensitivity",
  #                 ifelse(
  #                   expTer == 2 & sensTer == 2,
  #                   "Medium Exposure, Medium Sensitivity",
  #                   ifelse(
  #                     expTer == 2 & sensTer == 3,
  #                     "Medium Exposure, High Sensitivity",
  #                     ifelse(
  #                       expTer == 3 &
  #                         sensTer == 2,
  #                       "High Exposure, Medium Sensitivity",
  #                       "High Exposure, High Sensitivity"
  #                     )
  #                   )
  #                 )
  #               )
  #             )
  #           )
  #         )
  #       ),
  #       levels = c(
  #         "Low Exposure, Low Sensitivity",
  #         "Low Exposure, Medium Sensitivity",
  #         "Medium Exposure, Low Sensitivity",
  #         "Medium Exposure, Medium Sensitivity",
  #         "High Exposure, Low Sensitivity",
  #         "Low Exposure, High Sensitivity",
  #         "High Exposure, Medium Sensitivity",
  #         "Medium Exposure, High Sensitivity",
  #         "High Exposure, High Sensitivity"
  #       )
  #     )]%>%
  #     .[,sign := ifelse(expTer == 1 & sensTer == 1, "rgba(242,241,230, 0.5)",
  #                 ifelse(expTer == 1 & sensTer == 2,"rgba(239,169,110, 0.7)",
  #                        ifelse(expTer == 1 & sensTer == 3, "rgba(203,111,107, 0.8)",
  #                               ifelse(expTer == 2 & sensTer == 1, "rgba(42,140,197,.7)",
  #                                      ifelse(expTer == 3 & sensTer == 1, "rgba(51,101,147,0.8)",
  #                                             ifelse(expTer == 2 & sensTer == 2, "rgba(145,152,170,0.7)",
  #                                                    ifelse(expTer == 2 & sensTer == 3, "rgba(137,136,164,0.9)",
  #                                                           ifelse(expTer == 3 & sensTer == 2, "rgba(149,123,152,0.9)","rgba(104,93,169,1)"
  #                                                           ))))))))] %>%
  #     .[,size := ntile(Population, 30)] %>%
  #     na.omit()
  # })
  # 
  
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
    
    # mapTemp2 <- left_join(tracts, triple()) %>% rename(County = county) %>%
    #   filter(vulEXP ==1 | vulSENS ==1)
    
    pal <- colorFactor(c(
      "#F2F1E6",
      "#EFA96E",
      "#2A8CC5",
      "#9198AA",
      "#336593",
      "#CB6F6B",
      "#957B98",
      "#8988A4",
      "#685DA9"
    ), mapTemp2$vulnerability)
    
    
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
  
  
  data.dnld <- eventReactive(c(input$cntyDNLD, input$indDNLD, input$scaleDNLD),{
    
    holder <-
      if (input$scaleDNLD == "County") {
        data.table::copy(CHVIdata)
      } else {
        data.table::copy(CHVItracts)
      }
    
    
    
    if(input$cntyDNLD  == "All" & input$indDNLD != "All") {
      
      holder[def == input$indDNLD] %>% 
        .[, COUNTYFI_1 := as.character(paste0("0",COUNTYFI_1))] %>%
        rename(
          County = county,
          FIPS = geotypv, 
          Region = climReg,
          Definition = def,
          Year = tperiod,
          Mean = est, 
          Numerator = numratr,
          Denominator = denmntr,
          Strata = strata, 
          Race = race)
    } else { if(input$cntyDNLD  != "All" & input$indDNLD != "All") {
      
      holder[def == input$indDNLD  & county == input$cntyDNLD] %>% 
        .[, COUNTYFI_1 := as.character(paste0("0",COUNTYFI_1))] %>%
        rename(
          County = county,
          FIPS = geotypv, 
          Region = climReg,
          Definition = def,
          Year = tperiod,
          Mean = est, 
          Numerator = numratr,
          Denominator = denmntr,
          Strata = strata, 
          Race = race)
    } else { if(input$cntyDNLD  != "All" & input$indDNLD == "All") {
      
      holder[county == input$cntyDNLD] %>% 
        .[, COUNTYFI_1 := as.character(paste0("0",COUNTYFI_1))] %>%
        rename(
          County = county,
          FIPS = geotypv, 
          Region = climReg,
          Definition = def,
          Year = tperiod,
          Mean = est, 
          Numerator = numratr,
          Denominator = denmntr,
          Strata = strata, 
          Race = race)
    } else {   
      
      holder[, COUNTYFI_1 := as.character(paste0("0",COUNTYFI_1))] %>%
        rename(
          County = county,
          FIPS = geotypv, 
          Region = climReg,
          Definition = def,
          Year = tperiod,
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
            FIPS,
            Region,
            Definition,
            Strata,
            Race,
            Year,
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
        FIPS,
        Region,
        Definition,
        Strata,
        Race,
        Year,
        Mean,
        LL95,
        UL95,
        Numerator,
        Denominator
      ),  filter = 'top',
    options=list(pageLength = 25)
  )  %>% DT::formatRound(c(8:10),ifelse(input$indDNLD == "Average Daily Maximum Ozone Concentration", 3, 1)) %>%
      DT::formatRound(c(11:12), 0)
    
  })
  
  
  
  ##### Download the County Health Profile Report  ######  
  
 
  output$downloadCHPR1 <- renderUI({
    HTML(
      paste0(
        '<a href =',
        links$CHPR_link[links$County %in% c(input$cnty1, paste0(input$cnty1, " "))],
        ' target="_blank">Download County Health Profile</a>'
      )
    )
  })
  
  output$downloadNarrative <- renderUI({
    HTML(
      paste0(
        '<a href =',
        narratives$narrativeLink[narratives$def == input$ind],
        ' target="_blank"  onclick="trackOutboundLink("',
        narratives$narrativeLink[narratives$def == input$ind],
        '"); return false;">Download the Narrative for this Indicator</a>'
      )
    )
  })
  
  
  output$readMore <- renderUI({
    HTML(
      paste0(
        '<h2><a href =',
        narratives$resourceLink[narratives$def == input$ind],
        ' target="_blank"  onclick="trackOutboundLink("',
        narratives$resourceLink[narratives$def == input$ind],
        '"); return false;">Learn More Here</a></h2>'
      )
    )
  })
  
  
  output$VAtext <- renderUI({
    HTML(
      paste0(
        '<h3>What is the climate change challenge?</h3><p>',
        narratives$challenge[narratives$def == input$ind],
        '<h3>Why is this climate change impact important to health?</h3><p>',
        narratives$healthImport[narratives$def == input$ind],
        '<h3>Who is most impacted?</h3><p>',
        narratives$vulnerable[narratives$def == input$ind]
      )
    )
  })
  
  
  output$snapshottext <- renderUI({
    HTML(
      paste0("<h3>",
        input$cnty1,
        " County faces climate change exposures that pose considerable health risks to the population, especially to a number of vulnerable groups. </h3>
        <h2><font color='#3182bd'>Environmental Exposures</font></h2>
        <p>More frequent extreme weather patterns will pose a hazard to ",
        input$cnty1,
        " County's population health. If greenhouse gas emissions continue to grow as they have in the past, the number of <strong>extreme heat days</strong> over ",
        round(data.tab1()$numratr[data.tab1()$ind_strt == 'heat_2040-2060']*9/5+32, 1),
        "&#176;F are expected to be ",
        round(data.tab1()$County_Value[data.tab1()$ind_strt == 'heat_2040-2060'], 0),
        " days per year in 2040-2060 and ",
        round(data.tab1()$County_Value[data.tab1()$ind_strt == 'heat_2080-2099'], 0),
        " days  per year in 2080-2099. Higher temperatures can also increase hazardous air pollution. In 2012-2014, ",
        input$cnty1,
        " County's average maximum <strong>ozone</strong> concentration was ",
        round(data.tab1()$County_Value[data.tab1()$ind == 'ozone'], 3),
        " ppm and average <strong>fine particulate matter</strong> (PM2.5) was ",
        round(data.tab1()$County_Value[data.tab1()$ind == 'pm'], 1),
        " µg/m3 which is ",
        ifelse(data.tab1()$County_Value[data.tab1()$ind == 'pm'] < 12, 'lower', 'higher'),
        " than the state standard of 12 µg/m3 (the statewide averages were ",
        round(data.tab1()$CA_avg[data.tab1()$ind == 'ozone'], 3),
        " ppm for ozone and ",
        round(data.tab1()$CA_avg[data.tab1()$ind == 'pm'], 1),
        " µg/m3 for PM2.5).</p>             <p>Higher temperatures and changes in precipitation are leading to longer, more severe droughts which, in turn, contribute to increased risk of <strong>wildfires</strong>. Around ",
        round(data.tab1()$County_Value[data.tab1()$ind == 'wildfire'], 1),
        "% (",
        format(round(data.tab1()$numratr[data.tab1()$ind == 'wildfire'], 0),
               big.mark = ",",
               scientific = FALSE),
        " residents) of the ",
        input$cnty1,
        " County population lived in very high wildfire risk areas in 2010 (statewide average was ",
        round(data.tab1()$CA_avg[data.tab1()$ind == 'wildfire'], 0),
        "%).</p>
        <h2><font color='#d95f0e'>Population Sensitivity</font></h2> <p>Certain populations will experience the health impacts of climate change earlier, more often, or more severely, such as <strong>children and elderly</strong>, and those with disabilities. In 2011-2015, ",
        input$cnty1,
        " County's population included ",
        round(data.tab1()$County_Value[data.tab1()$ind == 'children'], 1),
        "% children (",
        format(round(data.tab1()$numratr[data.tab1()$ind == 'children'], 0),
               big.mark = ",",
               scientific = FALSE),
        " persons under 5) and ",
        round(data.tab1()$County_Value[data.tab1()$ind == 'elderly'], 1),
        "% elderly (",
        format(round(data.tab1()$numratr[data.tab1()$ind == 'elderly'], 0),
               big.mark = ",",
               scientific = FALSE),
        " persons 65 years and older) (state population included ",
        round(data.tab1()$CA_avg[data.tab1()$ind == 'children'], 1),
        "% children and ",
        round(data.tab1()$CA_avg[data.tab1()$ind == 'elderly'], 1),
        "% elderly). </p>
        <p>Between 2011-2015, ",
        round(data.tab1()$County_Value[data.tab1()$ind_strt == 'disability_physical'], 1),
        "% (",
        format(round(data.tab1()$numratr[data.tab1()$ind_strt == 'disability_physical'], 0),
               big.mark = ",",
               scientific = FALSE),
        " people) of ",
        input$cnty1,
        " County's population reported having <strong>physical disabilities</strong> (statewide average was 6%). In 2011-2015, ",         input$cnty1,         " County's population included ",
        round(data.tab1()$County_Value[data.tab1()$ind_strt == 'disability_mental'], 1),
        "% (",
        format(round(data.tab1()$numratr[data.tab1()$ind_strt == 'disability_mental'], 0),
               big.mark = ",",
               scientific = FALSE),
        " people) with <strong>mental disabilities</strong> (statewide average was 4%).</p><p>
        Social or cultural isolation can also limit protective behaviors, community support, or the efficacy of emergency notifications  during extreme weather and disasters.  In 2011-2015,  ",
        round(data.tab1()$County_Value[data.tab1()$ind == 'linguistic'], 1),
        "% of the county's households were considered <strong>linguistically isolated</strong> (",
        format(round(data.tab1()$numratr[data.tab1()$ind == 'linguistic'], 0),
               big.mark = ",",
               scientific = FALSE),
        " households), where no one aged 14 or older spoke English (statewide average was 9.5%). </p><p>
        Climate change and its impacts add to the cumulative stresses already experienced by populations without adequate financial resources, those living in communities with high incidence of violence, or who have limited access to supports like higher education, insurance or personal transportation. In ",         input$cnty1,         " County, ",
        round(data.tab1()$County_Value[data.tab1()$ind == 'poverty'], 0),
        "% (",
        format(round(data.tab1()$numratr[data.tab1()$ind == 'poverty'], 0),
               big.mark = ",",
               scientific = FALSE),
        " residents) of the <strong>population was living below 200% of the poverty level</strong> in 2011-2015 (statewide average was 36%).
        ",         input$cnty1,         " County's <strong>violent crime rate</strong> in 2013 was ",
        round(data.tab1()$County_Value[data.tab1()$ind == 'crime'], 1),
        " per 100,000 persons (statewide average was 4.0 per 100,000).
        In 2011-2015, ",
        round(data.tab1()$County_Value[data.tab1()$ind == 'education'], 0),
        "% of people (",
        format(round(data.tab1()$numratr[data.tab1()$ind == 'education'], 0),
               big.mark = ",",
               scientific = FALSE),
        " residents) in ",         input$cnty1,         " County aged 25 years or older had an <strong>educational attainment</strong> of less than a four-year college degree (statewide average was 68.7%).
        In 2011-2015 ",
        round(data.tab1()$County_Value[data.tab1()$ind == 'insurance'], 0),
        "% of the population (",
        format(round(data.tab1()$numratr[data.tab1()$ind == 'insurance'], 0),
               big.mark = ",",
               scientific = FALSE),
        " residents) in the county were <strong>without health insurance</strong> (statewide average was 14.7%). Please note that data is provided for 2011-2015.  This covers a time period both before and after the full implementation of the Affordable Care Act, which increased access to insurance.
        In 2011-2015, ",
        round(data.tab1()$County_Value[data.tab1()$ind == 'vehicles'], 0),
        "% of households (",
        format(round(data.tab1()$numratr[data.tab1()$ind == 'vehicles'], 0),
               big.mark = ",",
               scientific = FALSE),
        " households) <strong>lacked a personal vehicle</strong> (statewide average was 8%). Finally, ",
        round(data.tab1()$County_Value[data.tab1()$ind == 'outdoor'], 0),
        "% (",
        format(round(data.tab1()$numratr[data.tab1()$ind == 'outdoor'], 0),
               big.mark = ",",
               scientific = FALSE),
        " workers) of ",         input$cnty1,         " County's  labor force <strong>worked outdoors</strong> in 2011-2015, and face an elevated risk to heat's effects on health (statewide average was 6.4%).</p>
<h2><font color='#54278f'>Adaptive Capacity</font></h2>
<p>Adaptive capacity is important for responding to the impacts of climate change. Expanses of concrete and asphalt (<strong>impervious surfaces</strong>) make hot summers even hotter, while parks and trees can help make future heat waves more bearable. 
In the portions of ",         input$cnty1,         " County where people resided in 2011, the land was about ",
        round(data.tab1()$County_Value[data.tab1()$ind == 'impervious'], 0),
        "% impervious surfaces and ",
        round(data.tab1()$County_Value[data.tab1()$ind == 'canopy'], 0),
        "% areas without <strong>tree canopy cover</strong> (statewide averages were 50% and 92%, respectively). 
Access to public transit and <strong>air conditioning</strong> helps people relocate to cooler or safer spaces in the event of heat waves, extreme weather, or other events like wildfires.  According to a 2009 survey, ",
        round(data.tab1()$County_Value[data.tab1()$ind == 'ac'], 0),
        "% (",
        format(round(data.tab1()$numratr[data.tab1()$ind == 'ac'], 0),
               big.mark = ",",
               scientific = FALSE),
        " households) of households in ",         input$cnty1,         " County did not have air conditioning (statewide average was 36%). 
These findings highlight the aspects of vulnerability and the populations in ",         input$cnty1,         " County most susceptible to health risks from current and future climate change exposures. Responding to and preparing for a changing climate presents opportunities for local health departments and partners to consider policies, actions, and infrastructure design that will not just protect the public, but also promote health equity, resiliency, and sustainability.
"
      )
    )
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