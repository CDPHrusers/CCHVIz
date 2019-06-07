
##### Define UI for application that draws a histogram #####
ui <-  fluidPage(
     div(style="background-color:#FEFEFE;padding: 1px 0px;height: 0px",
      titlePanel(
        title="",
        windowTitle="CCHVIz"
      )
  ),
  
  
  navbarPage(
    position = "fixed-top",
    header = tags$head(
      
      ######## Google Analytics Script Start ###############
      HTML("<script async src='https://www.googletagmanager.com/gtag/js?id=UA-8317364-4'></script>"), 
      includeScript("g-analytics.js"),
      ######## Google Analytics Script End ###############
      
      tags$style(
        HTML(
          "
          
          a {
          color: #1F86C8;
          text-decoration: none;
          background-color: transparent;
          -webkit-text-decoration-skip: objects;
          }
          
          a:hover {
          color: #1E5493;
          text-decoration: underline;
          }
          
          "
        )
      ),
      tags$style(type = "text/css", "body {padding-top: 70px;}")
    ),
    
    theme = shinytheme("flatly"),
    title = div(
      "CCHVIz",
      a(
        href = "https://www.cdph.ca.gov/Programs/OHE/Pages/CCHEP.aspx" 
        ,
        img(
          src = "CDPHLogo.gif",
          height = "45",
          style = "position: relative; top: -12px; right: 0 px;"
        )
      )
    ),
    
             
             
    tabPanel("About",
             fluidRow(
               includeMarkdown("about.md"), hr()
             ),
             fluidRow(
               column(5,includeMarkdown("about2.md")),
               column(7,br(),br(),
                      img(
                        class = "img-polaroid",
                        src = "chviTable.png",
                        alt = "Table of the Indicators",
                        objectfit = "contain",
                        height = "auto",
                        width ="auto"
                      ))
               
             )
    ),
             
             tabPanel(title = "Vulnerability", 
                      fluidRow(
                        column(8,
                               includeMarkdown("vulnerability.md"
                               )),  
                        column(2,
                               selectInput("exposure",
                                           "Exposure Indicator",
                                           c("Projected number of extreme heat days",
                                             "Three-year ozone concentration exceedance",
                                             "Annual Mean Ambient Concentration of Fine Particulate Matter (PM2.5)",
                                             "Population living in sea level rise inundation areas",
                                             "Percent of population currently living in very high wildfire risk areas"
                                           )),
                       selectInput("sensitivity",
                                           "Sensitivity Indicator",
                                           c("Percent of population aged 65 years or older",
                                             "Percent of population age less than 5 years",
                                             "Number of Violent Crimes per 1,000 Population",
                                             "Percent of population with a disability",
                                             "Percent of adults with less than HS education" ,
                                             "Percent of adults aged 18 - 64 without health insurance",
                                             "Percent of households with no one aged > 14 years speaking English",
                                             "Percent of population employed and aged > 16 working outdoors",
                                             "Overall, concentrated, and child (0 to 18 years of age) poverty rate",
                                             "Percent of households with no vehicle ownership",
                                             "Percent of households without air conditioning",
                                             "Percent without tree canopy coverage",
                                             "Percent impervious surface cover"
                                           ))
                                ),
                        column(2,
                               img(
                                 class = "img-polaroid",
                                 src = "vulnerabilityLegend.png",
                                 alt = "Vulnerability Bivariate Legend"
                               )
                        )
                        
                      ),
                      
                      fluidRow(
                        column(8,wellPanel(plotlyOutput("triplePlot",height = "600px"),
                                           downloadLink(outputId = "downloadVulnerabilityFigure",label = "Download the data in this figure")
                                           )
                        ), 
                        column(4,wellPanel(leafletOutput("vulnMap",height = "600px"),
                                           downloadLink(outputId = "downloadVulnerabilityMap",label = "Download the data in this Map")))
                      )),
             
             tabPanel("County Snapshot",
                      # Create a new Row in the UI for selectInputs
                      
                      
                      #####  Select an County  #####       
                      
                      fluidRow(
                        column(8, includeMarkdown("countyPlot.md")),
                        column(2,
                               selectInput("cnty1",
                                           "Select a County",
                                           c(sort(unique(as.character(CHVIdata$county)))
                                           )),
                               p(uiOutput("downloadCHPR1"))
                        )),
                      wellPanel(plotlyOutput("plotCounty",height = "600px"),
                                downloadLink(outputId = "downloadCountySnapshot",label = "Download the data in this figure")),
                      wellPanel(DT::dataTableOutput("countyTable"))
                      
             ),
         
             #####  Select an Indicator Tool  #####    
             
             
             tabPanel(
               "Single Indicator",
               
               fluidRow(
                 column(2,
                        selectInput("cnty",
                                    "Highlight County",
                                    c(sort(unique(as.character(CHVIdata$county)))
                                    ))
                 ),
                 
                 column(2,
                        selectInput("ind",
                                    "Select an Indicator",
                                    c("Percent of population aged 65 years or older",
                                      "Percent of population age less than 5 years",
                                      "Percent of population with a disability",
                                      "Three-year ozone concentration exceedance",
                                      "Annual Mean Ambient Concentration of Fine Particulate Matter (PM2.5)",
                                      "Population living in sea level rise inundation areas",
                                      "Percent of population currently living in very high wildfire risk areas",
                                      "Number of Violent Crimes per 1,000 Population",
                                      "Percent of adults with less than HS education" ,
                                      "Percent of adults aged 18 - 64 without health insurance",
                                      "Percent of households with no one aged > 14 years speaking English",
                                      "Percent of population employed and aged > 16 working outdoors",
                                      "Overall, concentrated, and child (0 to 18 years of age) poverty rate",
                                      "Percent of households with no vehicle ownership",
                                      "Percent without tree canopy coverage",
                                      "Percent impervious surface cover"
                                    ))),
                 
                 # column(2,
                 #        selectInput("ind",
                 #                    "Select an Indicator",
                 #                    c("Projected number of extreme heat days",
                 #                      "Three-year ozone concentration exceedance",
                 #                      "Annual Mean Ambient Concentration of Fine Particulate Matter (PM2.5)",
                 #                      "Population living in sea level rise inundation areas",
                 #                      "Percent of population currently living in very high wildfire risk areas",
                 #                      "Percent of population aged 65 years or older",
                 #                      "Percent of population age less than 5 years",
                 #                      "Number of Violent Crimes per 1,000 Population",
                 #                      "Percent of population with a disability",
                 #                      "Percent of adults with less than HS education" ,
                 #                      "Percent of adults aged 18 - 64 without health insurance",
                 #                      "Percent of households with no one aged > 14 years speaking English",
                 #                      "Percent of population employed and aged > 16 working outdoors",
                 #                      "Overall, concentrated, and child (0 to 18 years of age) poverty rate",
                 #                      "Percent of households with no vehicle ownership",
                 #                      "Percent of households without air conditioning",
                 #                      "Percent without tree canopy coverage",
                 #                      "Percent impervious surface cover"
                 #                    ))),
                 column(2,
                        uiOutput("chooseStrata")
                 ),
                 column(6,
                        p(uiOutput("blurb")),
                        p(uiOutput("downloadNarrative"))
                 )
                 
                 
               ),
               
               fluidRow(column(8,
                               wellPanel(leafletOutput("map", height = "600px"),
                                         downloadLink(outputId = "downloadSingleIndicatorMap",label = "Download the data in this Map")
                                         )
               ), 
               column(4,
                      wellPanel(plotlyOutput("plot", height = "600px"),
                                downloadLink(outputId = "downloadSingleIndicator",label = "Download the data in this figure")
                                )  
               )),
               wellPanel(DT::dataTableOutput("table"))
             ),

             tabPanel("Query the Data",
                      fluidRow(
                        column(3,
                               selectInput("cntyDNLD",
                                           "Select a County",
                                           c("All",sort(unique(as.character(CHVIdata$county)))
                                           ))
                        ),
                        column(3,
                               selectInput("indDNLD",
                                           "Select an Indicator",
                                           c("All",
                                             "Projected number of extreme heat days",
                                             "Three-year ozone concentration exceedance",
                                             "Annual Mean Ambient Concentration of Fine Particulate Matter (PM2.5)",
                                             "Population living in sea level rise inundation areas",
                                             "Percent of population currently living in very high wildfire risk areas",
                                             "Percent of population aged 65 years or older",
                                             "Percent of population age less than 5 years",
                                             "Number of Violent Crimes per 1,000 Population",
                                             "Percent of population with a disability",
                                             "Percent of adults with less than HS education" ,
                                             "Percent of adults aged 18 - 64 without health insurance",
                                             "Percent of households with no one aged > 14 years speaking English",
                                             "Percent of population employed and aged > 16 working outdoors",
                                             "Overall, concentrated, and child (0 to 18 years of age) poverty rate",
                                             "Percent of households with no vehicle ownership",
                                             "Percent of households without air conditioning",
                                             "Percent without tree canopy coverage",
                                             "Percent impervious surface cover"
                                           ))),
                        # column(2,
                        #         selectInput("raceDNLD",
                        #                     "Race",
                        #                     c("Total",
                        #                       "AIAN",
                        #                       "Asian",
                        #                       "AfricanAm",
                        #                       "Latino",
                        #                       "NHOPI",
                        #                       "White",
                        #                       "Multiple",
                        #                       "Other")
                        #  )),  
                        # column(2,
                        #              uiOutput("chooseStrataDNLD") # this criteria and race were creating problems in the queries
                        #  ), 
                        column(3,
                               p(),
                               downloadButton(outputId = "downloadData", label = "Download Selected Data")
                        )
                        
                        # ,
                        # 
                        # column(3,
                        #        p(),
                        #        downloadButton(outputId = "downloadSpatial", label = "Download Spatial Data")
                        # )
                      ),             
                      fluidRow(
                        wellPanel(DT::dataTableOutput("downloadTable"))
                      )),
             
             
             #####  Additional Page  ####
             
             navbarMenu(
               "How to Use",
              tabPanel("The Vulnerability Page",
                       fluidRow(
                         column(3,
                                wellPanel(includeMarkdown("howToVuln.md"))),
                         column(9, 
                                wellPanel(img(
                                  class = "img-polaroid",
                                  src = "vulnGif2.gif",
                                  alt = "How To Vulnerability Page",
                                  objectfit = "contain"
                                )))
                                
                         )
                         ),
              tabPanel("The County Snapshot Page",
                                    fluidRow(
                                      column(3,
                                             wellPanel(includeMarkdown("howToSnapshot.md"))),
                                      column(9, 
                                             wellPanel(img(
                                               class = "img-polaroid",
                                               src = "snapGif.gif",
                                               alt = "How To County Snapshot Page",
                                               objectfit = "contain",
                                               height = "auto",
                                               width ="auto"
                                             )))
                                      
                                    )
                         ),
              tabPanel("The Single Indicator Page",
                       fluidRow(
                         column(3,
                                wellPanel(includeMarkdown("howToIndicator.md"))),
                         column(9, 
                                wellPanel(img(
                                  class = "img-polaroid",
                                  src = "indGif.gif",
                                  alt = "How To Indicator Page", 
                                  height = "auto",
                                  width ="auto"
                                )))
                         
                       )
              ),
              tabPanel("The Query Your Data Page",
                       fluidRow(
                         column(3,
                                wellPanel(includeMarkdown("howToQuery.md"))),
                         
                         column(9, 
                                wellPanel(img(
                                  class = "img-polaroid",
                                  src = "queryGif.gif",
                                  alt = "How To Query Page"
                                )))
                        
                       )
              )
                       )
  #####  Finish Additional  #####
             
  )
)
