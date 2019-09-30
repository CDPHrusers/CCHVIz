




##### Define UI for application that draws a histogram #####
ui <-  fluidPage(
  div(style = "background-color:#FEFEFE;padding: 1px 0px;height: 0px",
      titlePanel(
        title = "",
        windowTitle = "CCHVIz"
      )),
  
  navbarPage(
    position = "fixed-top",
    header = tags$head(
      
      tags$script("src"="func.js"),
      ######## Google Analytics Script Start ###############
      HTML(
        "<script async src='https://www.googletagmanager.com/gtag/js?id=UA-8317364-4'></script>"
      ),
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
    
    
    
    
    
    tabPanel("Home",value = "home",
             # fluidRow(includeMarkdown("about.md"), hr()),
             fluidRow(
               column(2, ""),
               column(8,
                 HTML(paste0(
                   "<div style='text-align:center'><span style='color:black; font-family:Arial; font-size:6em;'>
          Welcome to the CCHVIz </span>  <br>
          <h2>CCHVIz is the interactive data visualization platform for the Climate Change & Health Vulnerability Indicators for California (CCHVIs). It is produced by the CA Department of Public Health\'s <a href = 'https://www.cdph.ca.gov/Programs/OHE/Pages/OfficeHealthEquity.aspx' target = '_blank'>Office of Health Equity</a>.</h2>
          <br><hr><br>

          <h2>The indicators are organized into <strong><font color='#3182bd'>Environmental Exposures</font></strong>, <strong><font color='#d95f0e'>Population Sensitivity</font></strong>, and <strong><font color='#54278f'>Adaptive Capacity</font></strong> </h2>
             <br>
          <img src = 'chviTable.png' width = '75%'> </img>
           <br><hr><br>

          <h2>Start with the \'<strong>",tags$a(onclick="customHref('county_snapshot')","County Snapshot"),"</strong>\' to find out which indicators are of greatest concern for you.</h2>
           <br>
           <img src = 'snapshot_screenshot.png' width = '75%'> </img>
           <h2>It looks like older adults and wildfire are among the chief concerns for this county. </h2>

            <br><hr><br>

          <h2>Next examine specific indicators in detail for your County on the \'<strong>",tags$a("Single Indicator", onclick="customHref('single_indicator')"),"</strong>\' tab.</h2>
           <br>
           <img src = 'indicator_screenshot.png' width = '75%'> </img>

              <br><hr><br>

          <h2>Consider exposure and population senstivity together by exploring the '<strong>",tags$a("Vulnerability", onclick="customHref('vulnerability')"),"</strong>\' tab.</h2>
           <br>
           <img src = 'vulnerability_screenshot.png' width = '90%'> </img>
           <h2>Many nearby counties share this vulnerability. Regional action on this topic is merited.</h2>


            <br><hr><br>

          <h2>Do more with the data by downloading it from the '<strong>",tags$a("Query the Data", onclick="customHref('query')"),"</strong>\' tab.</h2>
           <br>
           <img src = 'query_screenshot.png' width = '75%'> </img>

             <br><hr><br>

           <h4>The CalBRACE Project produced Climate Change and Health
Vulnerability Indicators to help stakeholders better understand the people and places that are more susceptible to adverse health impacts associated with climate change. They are a suite of 21 indicators (18 available here) of climate exposure, population sensitivity, and adaptive capacity to the impacts of climate change. These indicators are being used by local and state programs to plan to meet the needs of the communities most at risk of harm from climate change.  For additional climate and health planning resources see the <a href='https://cdphdata.maps.arcgis.com/apps/MapSeries/index.html?appid=4093397556b4450ea563f23fcf353c64' target='_blank'>CalBRACE Adaptation Toolkit</a>.<br><br>

Indicator data are available for download on the 'Query the Data' tab or from the <a href='https://www.cdph.ca.gov/Programs/OHE/Pages/CalBRACE.aspx' target = '_blank'>CalBRACE website</a>. You can also download narratives describing each indicator’s significance to climate change and health, the evidence that links the indicator to health outcomes, data sources, bibliographic references, methodology, and limitations that impact the interpretation of the indicator.<br><br>

If you have questions, comments, or ideas for the site, please do not hesitate to <a href='mailto:cchep@cdph.ca.gov?subject=CCHVIz contact'>contact us</a></h4>.




          </div>"
                   
                   
                   
                   
                   
                 ))
               ),
               column(2, "")
               
               # column(5, includeMarkdown("about2.md")),
               # # column(7,  box(
               # #   title = "Indicator List",
               # #   width = 12,
               # #   HTML(
               # #     "<img src = 'chviTable.png' width = '100%'> </img>"
               # #   )
               # # ))
               #
               # column(
               #   6,
               #   br(),
               #   br(),
               #   img(
               #     class = "img-polaroid",
               #     src = "chviTable.png",
               #     alt = "Table of the Indicators",
               #     width = "100%"
               #   )
               # ),
               # column(1, "")
               
               
               
               
             )),
    
    
    tabPanel(
      "County Snapshot",value = "county_snapshot",
      # Create a new Row in the UI for selectInputs
      
      
      #####  Select an County  #####
      
      fluidRow(column(8, includeMarkdown("countyPlot.md")),
               column(
                 2,
                 selectInput("cnty1",
                             "Select a County",
                             c(sort(
                               unique(as.character(CHVIdata$county))
                             ))),
                 p(uiOutput("downloadCHPR1"))
               )),
      fluidRow(column(
        12,
        wellPanel(
          HTML("<h3>County compared to the state average</h3>"),
          plotlyOutput("plotCounty", height = "800px"),
          downloadLink(outputId = "downloadCountySnapshot", label = "Download the data in this figure")
        )
      )
      # column(3,
      #        wellPanel(
      #          uiOutput("topindicators")
      #        ))
      ),
      wellPanel(uiOutput("snapshottext")),
      
      wellPanel(DT::dataTableOutput("countyTable"))
      
    ),
    
    #####  Select an Indicator Tool  #####
    
    
    tabPanel(
      "Single Indicator",value = "single_indicator",
      
      fluidRow(
        column(2,
               selectInput("cnty",
                           "Highlight County",
                           c(sort(
                             unique(as.character(CHVIdata$county))
                           )))),
        
        column(2,
               selectInput(
                 "ind",
                 "Select an Indicator",
                 c(
                   "Percent of population aged 65 years or older",
                   "Percent of population age less than 5 years",
                   "Percent of population with a disability",
                   "Projected number of extreme heat days 2040-2060",
                   "Projected number of extreme heat days 2080-2099",
                   "Average Daily Maximum Ozone Concentration",
                   "Annual Mean Ambient Concentration of Fine Particulate Matter (PM2.5)",
                   "Population living in sea level rise inundation areas",
                   "Percent of population currently living in very high wildfire risk areas",
                   "Number of Violent Crimes per 1,000 Population",
                   "Percent of adults with less than college education" ,
                   "Percent of population without health insurance",
                   "Percent of households with no one aged > 14 years speaking English",
                   "Percent of population employed and aged > 16 working outdoors",
                   "Overall Poverty Rate",
                   "Percent of households with no vehicle ownership",
                   # "Percent of households without air conditioning",
                   "Percent without tree canopy coverage",
                   "Percent impervious surface cover"
                 )
               )),
        
        # column(2,
        #        selectInput("ind",
        #                    "Select an Indicator",
        #                    c("Projected number of extreme heat days 2040-2060",                                                 "Projected number of extreme heat days 2080-2099",
        #                      "Average Daily Maximum Ozone Concentration",
        #                      "Annual Mean Ambient Concentration of Fine Particulate Matter (PM2.5)",
        #                      "Population living in sea level rise inundation areas",
        #                      "Percent of population currently living in very high wildfire risk areas",
        #                      "Percent of population aged 65 years or older",
        #                      "Percent of population age less than 5 years",
        #                      "Number of Violent Crimes per 1,000 Population",
        #                      "Percent of population with a disability",
        #                      "Percent of adults with less than college education" ,
        #                      "Percent of population without health insurance",
        #                      "Percent of households with no one aged > 14 years speaking English",
        #                      "Percent of population employed and aged > 16 working outdoors",
        #                      "Overall Poverty Rate",
        #                      "Percent of households with no vehicle ownership",
        #                      "Percent of households without air conditioning",
        #                      "Percent without tree canopy coverage",
        #                      "Percent impervious surface cover"
        #                    ))),
        column(2,
               uiOutput("chooseStrata")),
        column(6,
               p(uiOutput("blurb")),
               p(uiOutput(
                 "downloadNarrative"
               )))
        
        
      ),
      
      fluidRow(column(
        7,
        wellPanel(
          HTML("<h3>County Map</h3>"),
          leafletOutput("map", height = "700px"),
          downloadLink(outputId = "downloadSingleIndicatorMap", label = "Download the data in this Map")
        )
      ),
      column(5,
             wellPanel(uiOutput(
               "VAtext"
             )))),
      
      fluidRow(
        column(6,
               wellPanel(
                 HTML("<h3>Statewide comparison</h3>"),
                 plotlyOutput("plot"),
                 downloadLink(outputId = "downloadSingleIndicator", label = "Download the data in this figure")
               )),
        column(6,
               wellPanel(
                 HTML("<h3>Differences by race</h3>"),
                 plotlyOutput("racePlot"),
                 downloadLink(outputId = "downloadRacePlot", label = "Download the data in this figure")
               )),
        column(12,
               wellPanel(
                 HTML("<h3>Most vulnerable places in the county</h3>"),
                 DT::dataTableOutput("placeList")
               ))
      )
    ),
    
    tabPanel(
      title = "Vulnerability",value = "vulnerability",
      fluidRow(
        column(8,
               includeMarkdown("vulnerability.md")),
        column(
          2,
          selectInput(
            "exposure",
            "Exposure Indicator",
            c(
              "Projected number of extreme heat days 2040-2060",
              "Projected number of extreme heat days 2080-2099",
              "Average Daily Maximum Ozone Concentration",
              "Annual Mean Ambient Concentration of Fine Particulate Matter (PM2.5)",
              "Population living in sea level rise inundation areas",
              "Percent of population currently living in very high wildfire risk areas"
            )
          ),
          selectInput(
            "sensitivity",
            "Sensitivity Indicator",
            c(
              "Percent of population aged 65 years or older",
              "Percent of population age less than 5 years",
              "Number of Violent Crimes per 1,000 Population",
              "Percent of population with a disability",
              "Percent of adults with less than college education" ,
              "Percent of population without health insurance",
              "Percent of households with no one aged > 14 years speaking English",
              "Percent of population employed and aged > 16 working outdoors",
              "Overall, concentrated, and child (0 to 18 years of age) poverty rate",
              "Percent of households with no vehicle ownership",
              "Percent of households without air conditioning",
              "Percent without tree canopy coverage",
              "Percent impervious surface cover"
            )
          )
        ),
        column(
          2,
          img(class = "img-polaroid",
              src = "vulnerabilityLegend.png",
              alt = "Vulnerability Bivariate Legend")
        )
        
      ),
      
      fluidRow(column(
        8, wellPanel(
          plotlyOutput("triplePlot", height = "600px"),
          downloadLink(outputId = "downloadVulnerabilityFigure", label = "Download the data in this figure")
        )
      ),
      column(
        4, wellPanel(
          leafletOutput("vulnMap", height = "600px"),
          downloadLink(outputId = "downloadVulnerabilityMap", label = "Download the data in this Map")
        )
      ))
    ),
    
    
    tabPanel(
      "Query the Data",value = "query",
      fluidRow(
        column(3,
               selectInput(
                 "cntyDNLD",
                 "Select a County",
                 c("All", sort(unique(
                   as.character(CHVIdata$county)
                 )))
               )),
        column(3,
               selectInput(
                 "indDNLD",
                 "Select an Indicator",
                 c(
                   "All",
                   "Projected number of extreme heat days 2040-2060",
                   "Projected number of extreme heat days 2080-2099",
                   "Average Daily Maximum Ozone Concentration",
                   "Annual Mean Ambient Concentration of Fine Particulate Matter (PM2.5)",
                   "Population living in sea level rise inundation areas",
                   "Percent of population currently living in very high wildfire risk areas",
                   "Percent of population aged 65 years or older",
                   "Percent of population age less than 5 years",
                   "Number of Violent Crimes per 1,000 Population",
                   "Percent of population with a disability",
                   "Percent of adults with less than college education" ,
                   "Percent of population without health insurance",
                   "Percent of households with no one aged > 14 years speaking English",
                   "Percent of population employed and aged > 16 working outdoors",
                   "Overall Poverty Rate",
                   "Percent of households with no vehicle ownership",
                   "Percent of households without air conditioning",
                   "Percent without tree canopy coverage",
                   "Percent impervious surface cover"
                 )
               )),
        column(
          2,
          radioButtons(
            inputId = "scaleDNLD",
            label = "Select a Geography",
            choices = c("County", "Census Tract"),
            selected = "County"
          )
        ),
        column(
          3,
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
      fluidRow(wellPanel(DT::dataTableOutput("downloadTable")))
    ),
    
    
    #####  Additional Page  ####
    
    navbarMenu(
      "Take Action",
      tabPanel(
        "Integrating Climate Change in to Public Health ",
        fluidRow(column(3, ""),
                 column(
                   6,
                   HTML(
                     "<div style='text-align:center'>
                   <h1>Integrating Climate Change into Public Health</h1> </div><br>
<h4>California faces considerable risks from climate change to human health. This is especially true for a number of vulnerable groups. In addition to direct hazards like rising sea levels and extreme heat, climate change can impact service provision by government, business, and community organizations. </p>


<p>
Public health actions – particularly by local health departments with strong connections to surrounding communities – can protect people from some of the impacts of climate change. Preparing for a changing climate will require a combination of:</h4>
<ul><li>Cross-sector collaboration to address root causes of inequity which are exacerbated by climate change (e.g., planning climate-resilient transportation systems, and installing         cooling trees and green  infrastructure) </li><li>New and modified public health practices and systems </li><li>Increased capacity to respond to, and prepare for, both slow-moving environmental changes and extreme events </li><li>Engagement of impacted and vulnerable communities;  giving them decision-making power over resource allocation</li></ul>

<h4>Early actions for public health prevention provide the greatest benefits. Planning can begin with leveraging community knowledge and population data that is already available.  Local health departments can convene and work with  stakeholders to prioritize populations with specific vulnerabilities that are unique to their jurisdiction.</h4>

<hr>
<h3>Outreach and Engagement</h3><h4>
The CalBRACE Climate Change and Health Vulnerability Assessment Report can be used to identify the people and places most at risk to the health impacts of climate change. Once these communities are identified, it is important to engage agencies, community groups, and organizations that are inclusive of, represent, and serve these populations and that are interested in reducing health risks. An integrated and cross-sector approach may involve coordination between emergency management, community and neighborhood groups and organizations, businesses, community clinics, health care providers, and other partners. Public health      departments should also integrate information on community resilience into training and educational programs related to climate change preparedness and response.</p><p>
Many local jurisdictions are already engaged in Climate    Action Planning and Sustainable Communities Strategies Planning. </p><p>
Senate Bill 375 (SB 375) (Steinberg, Chapter 728, Statutes of 2008),also known as \"The Sustainable Communities and      Climate Protection Act of 2008\", affects regional Metropolitan Planning Organizations (MPO) in California. Each MPO has developed a Sustainable Communities Strategy (SCS) Plan to achieve mandated targets for greenhouse gas     emissions reductions to comply with SB 375 and share     members and partnerships with local climate change collaboratives.</p><p>
Under Senate Bill 1000, (SB1000) (Leva, Chapter 587, Statutes of 2018)also known as “The Planning for Healthy Communities Act”, cities and counties are required to adopt an Environmental Justice element, or integrate EJ-related policies, objectives, and goals throughout other elements of their General Plan. The bill also includes a process for communities to become meaningfully involved in the decision-making processes that govern land use planning in their neighborhoods.</p><p>
Other partners may include local water, public works, transportation, utility, fire, and planning departments, as well as neighborhood groups, housing and environmental advocacy groups, community coalitions, social service providers, family resource centers, senior centers, United Way, faith communities, health care providers, and farmworker groups. New partners, including tribal, and conservation and working lands stewards are also key to successful outcomes across the state. </p><p>
Finally, opportunities to integrate climate adaptation planning and activities into existing public health programs may exist. For example efforts to reduce environmental exposure to air pollution or reduce respiratory disease burden will benefit from incorporating the impacts of heat and wildfire into their work.</h4>
<hr>
<h3>Public Health Emergency Preparedness Programs</h3><h4>
Emergency Preparedness, Hazard Mitigation, and Response plans are important for adaptation planning.7 Public Health Emergency Preparedness (PHEP) programs are already responding to climate-related natural disasters, and are often the first traditional public health programs to incorporate climate change considerations into their daily work. PHEP programs and expertise need to be included in local government climate adaptation and resiliency plans as well as General Plan Safety elements and Local Hazard Mitigation Plans.</h4>

<hr>
<h3>Chronic Disease Prevention and Health Promotion</h3><h4>
Public health professionals in chronic disease prevention and health promotion are important messengers for integrating climate change into healthy eating and active lifestyles. Those with existing chronic diseases are more vulnerable to the effects of climate change.  Many of the same activities that prevent chronic diseases, such as safe and active communities and nutrition and food security, also increase community resilience to climate change. </h4>

<hr>
<h3>Built Environment</h3><h4>
Promoting healthy lifestyles through new policies and strategies can increase resilience to climate change. Policies, such as land use and transportation guidance to promote mixed use development, active transportation and public green space  improve health and slow climate change by reducing vehicles miles traveled, urban warming, and related greenhouse gas emissions.</h4>

<hr>
<h3>Active and Accessible Transportation
</h3><h4>
Partnerships with metropolitan planning organizations, transit agencies, air districts, non-profits, transportation departments, and others support active transportation resulting in active lifestyles. Active lifestyles (regular walking, bicycling, or other physical exercise) reduce chronic diseases, such as stroke, diabetes, heart diseases, and respiratory diseases. A healthier population is more resilient at the personal and community levels. For example, populations with lower rates of Type 2 diabetes and other treatable chronic diseases will be better able to cope with service disruption in pharmaceutical supplies that may result from extreme weather and/or evacuations.


</h4>

<hr>
<h3>Nutrition and Food Policy
</h3><h4>
Fruit and vegetable consumption is good for health and can reduce greenhouse gas emissions.10,11 Encouraging evidence-based public health strategies for healthy eating, that prevent obesity and diabetes and promote cardiovascular health, can increase resilience to heat waves and other extreme weather conditions.  Such strategies include supporting programs that provide access to healthy food choices for mothers, children, and those without adequate financial resources, or support for home and community gardens to reduce food insecurity and maintain healthy food supplies for families.   Other useful strategies include: </h4>
<ul><li>Promote zoning and land use policies that allow agricultural production in urban and suburban areas.</li><li>Create and support local farmer’s markets, farm stands, community gardens, orchards, and garden programs, and ensure accessibility to residents with limited financial resources.</li><li>Develop education materials about the value of selected vegetables, legumes, and nut protein sources.</li><li>Consider education materials about climate change and healthy eating, such as the curriculum  developed and piloted by San Luis Obispo (SLO) Public Health Department’s <a href='https://www.healslo.com/outsidein-slo/' target='_blank'>OutsideIn SLO  campaign</a>.</li></ul>


</h4>


<hr>
<h3>Family Services & Nursing
</h3><h4>
Maternal, Child, and Adolescent Health (MCAH) programs can provide families, children’s health advocates, and medical providers information about the unique hazards that      extreme heat and wildfire smoke pose for pregnant women, infants and young children.3 Public health nurses (PHN) and promotoras (i.e., Hispanic/Latino community members who received health education training) reach families that can benefit from education about home- and school-based      interventions to protect against heat, drought, wildfires, air pollution, and floods. PHNs are excellent messengers for    integrating education about climate change curriculum  and systems change into  their  professional collaboratives  and coalitions for family health.</h4>


<hr>
<h3>Mental Health Promotion and Treatment </h3><h4>
People with mental illness are at higher risk of death and illness in extreme weather events. Some medications that treat mental illness interfere with self-regulation of body temperature, and can thus increase vulnerability to heat.14 Trauma from wildfire, floods, drought, or other disasters can aggravate mental health problems and increase stress for individuals and communities.15 Mental health practitioners can prepare for climate change related traumas and mental health impacts, and work with health prevention programs to create conditions for community resilience and prevention.

</h4>
<hr>
<h3>Environmental Health and Vector-Borne Diseases

</h3><h4>
Food borne illnesses, caused by contaminated meats or vegetables, drinking water, wastewater or recreational water, are monitored and regulated by environmental health      departments or divisions. Practitioners in this field are                            increasingly addressing climate change as an environmental health issue. Hazardous environmental conditions during and after extreme weather events, such as heat waves, algal blooms, and unsafe air or water quality, are more frequently linked to climate change. Environmental health practitioners plan and mobilize surveillance and monitoring systems, and they provide warnings to prevent or reduce exposure to new and more intense hazards including vector-borne diseases.
</h4>
"
   )
                   
                   
                 ),
                 column(3, ""))
      ),
      tabPanel(
        "Resources for Climate Change Adaptation Planning",
        fluidRow(column(3, ""),
                 column(
                   6,
                   HTML(
                     "<div style='text-align:center'>
                       <h1>Resources for Climate Change Adaptation Planning</h1> </div><br>
                       <h4>In California, the importance of health, equity, and sustainability are evident in the California Climate Action Team and its Public Health Work Group. Through the California Health in All Policies Task Force, the Office of Health Equity Advisory Committee, and the diverse sectors of the California Climate Action Team, both climate adaption and mitigation are proceeding with an eye to the Health in All Policies approach. The following resources provide additional information related to climate change impacts and adaptation planning across sectors: </h4>
                       
                       <ul>
                       
                       
                            <li>
                       <a href='https://www.phi.org/resources/?resource=climate-change-health-and-equity-a-guide-for-local-health-departments' target='_blank'>Climate Change, Health, and Equity: A Guide for Local Health Departments</a> helps local health departments prepare for and mitigate climate change effects—from drought and heat to flooding and food security—with concrete, implementable suggestions. This guide:<ul><li>Provides a basic summary of climate change and climate impacts on health;</li><li>Prioritizes health equity, explains the disproportionate impacts of climate change on vulnerable communities, and targets solutions first to the communities where they are most needed, including low-income, elderly and people of color communities;</li><li>Connects what we know about climate impacts and climate solutions with the work of local health departments; and</li><li>Offers specific examples of how local health departments can address and ameliorate the impacts of climate change in every area of public health practice.</li></ul>
                       </li> <li>The 
                       <a href='https://healthyplacesindex.org/' target='_blank'>California Healthy Places Index (HPI)</a> developed by the Public Health Alliance of Southern California (Alliance) in partnership with the Virginia Commonwealth University’s Center on Society and Health, can be used to explore and change those community conditions that predict life expectancy. The tool includes detailed policy guides to support specific policy interventions that improve community conditions and health. 

                       </li>
                       
                       <li>
                       <a href='http://climatechange.ca.gov/climate_action_team/index.html' target='_blank'>
                       California Climate Action Team (CAT) 
                       </a>coordinates statewide efforts to implement global warming emission reduction programs and the state’s Climate Adaptation Strategy.
                       </li>
                            <li>
                       The <a href='http://www.arb.ca.gov/cc/ab32publichealth/ab32publichealth.htm' target='_blank'>CAT Public Health Work Group</a> addresses the cross-cutting issues related to climate change and health by providing a forum for communication, coordination, and education across agencies and with stakeholders.

                       </li>
                            <li>Through the <a href='https://resilientca.org/' target='_blank'>California Adaptation Clearinghouse</a> you can explore resources in the Adaptation Clearinghouse database by topic area, including climate impacts and potential response measures. These topics are directly linked to Safeguarding California, California’s Climate Adaptation Strategy.
                      
                       </li>
                            <li>
                       <a href='https://trackingcalifornia.org' target='_blank'>Tracking California</a> is a program of the Public Health Institute, in partnership with the California Department of Public Health and the Centers for Disease Control's (CDC) National Environmental Public Health Tracking Program. Tracking California works to make environmental health data and information publicly-available through the development of a web-based data query system, state-of-the-art data displays, and innovative web tools and services. We aim to make these data and information accessible and useful to a variety of stakeholders including communities, governments, academia, and private partners.</li>
                            <li>
                       <a href='http://resources.ca.gov/climate/safeguarding/' target='_blank'>Safeguarding California: Implementation Action Plans</a>, published by the California Natural Resources Agency, incorporates ten sectors that include water,  <a href='http://resources.ca.gov/docs/climate/safeguarding/Public%20Health%20Sector%20Plan.pdf' target='_blank'>Public Health</a>, agriculture, and biodiversity. It shows the path forward by concisely presenting:
                       <ul><li>
risks posed by climate change,</li><li>
adaptation efforts underway, and</li><li>
actions that will be taken to safeguard residents,  property, communities, and natural systems.</li></ul>

                       </li>
                            <li>
                       <a href='https://www.climatechange.ca.gov/climate_action_team/reports/Preparing_California_for_Extreme_Heat.pdf' target='_blank'>Preparing California for Extreme Heat: Guidance and Recommendations</a> provides an overview of current climate projections for increased temperatures and extreme heat conditions for California, describes the health effects of extreme heat, and presents recommendations for state and local planners, local governments, emergency response, and public health and health care professionals and institutions.
               </li>
                            <li>The
                       <a href='https://www.cdc.gov/climateandhealth/default.htm' target='_blank'>Climate and Health Program</a>, is the Centers for Disease Control and Prevention’s only HHS investment in climate change adaptation. It supports state and city health department efforts to develop and pilot methods to adapt to the present and future health effects of climate change. Funded states use the Building Resilience Against Climate Effects (BRACE) framework develop and implement health adaptation plans that impact health and address gaps in critical public health functions and services. <a href='https://www.noaa.gov/climate' target='_blank'>NOAA Climate.gov</a> provides timely and authoritative information about climate to promote public understanding of climate science and climate-related events through videos, stories, images, and data visualizations; we make common data products and services easy to access and use; and it provides tools and resources that help people make informed decisions about climate risks, vulnerability, and resilience.
                       </li>
                            <li>
                       <a href='https://practicegreenhealth.org/topics/climate-and-health/climate-and-health' target='_blank'>Addressing Climate Change in the Health Care Setting</a> provides a toolkit for reducing green house gas emissions with several kinds of resources including tips on getting started, potential mitigation programs in different sectors, and an outline of some of the many resources, model programs and guidelines available to move the process forward. 

                       </li>
                            <li>The
                       <a href='http://www.opc.ca.gov/updating-californias-sea-level-rise-guidance/' target='_blank'>State of California Sea-Level Rise Guidance Document</a>, 2018 Update reflects advances in sea-level rise science and addresses the needs of state agencies and local governments as they incorporate sea-level rise into their planning, permitting, and investment decisions. 

                       </li>
                            <li>The Office of Environmental Health Hazard Assessment (OEHHA) develops research on    <a href='https://oehha.ca.gov/climate-change/general-info/human-health-impacts-climate-change' target='_blank'>human health</a> studies related to the impacts of climate change, the     <a href='https://oehha.ca.gov/climate-change/document/indicators-climate-change-california' target='_blank'>Indicators of Climate Change in California report</a>, and the    <a href='https://oehha.ca.gov/climate-change/document/research-climate-change-archived-bibliographies' target='_blank'>Recent Research on Climate Change report</a>. 
                       </li>
                            <li>
                       <a href='https://oehha.ca.gov/calenviroscreen/report/calenviroscreen-30' target='_blank'>CalEnviroScreen</a> is a science-based tool that identifies the California communities most burdened by pollution from multiple sources and most vulnerable to its effects.

                       </li>
                            <li>
                       <a href='https://cal-adapt.org/' target='_blank'>Cal-Adapt</a> provides access to data and information that has been, and continues to be, produced by California’s scientific and research community to predict likely climate change scenarios in California through the 21st Century. The data available on this site offers a view of how climate change might affect California at the local level. You can work with visualization tools, access data, and participate in community sharing to contribute your own knowledge.

                       </li>
                            <li>The 
                       <a href='http://www.opr.ca.gov/planning/general-plan/' target='_blank'>General Plan Guidelines</a> from the Governor’s Office of Planning and Research provides guidance for local governments for the local general plan- the long- term blueprint for the community’s vision of future growth, including considerations for climate change, land use, housing, and other elements that affect health.

                       </li>
                           
                            <li>The <a href='http://resources.ca.gov/climate/safeguarding/local-action/' target='_blank'>California Climate Adaptation Planning Guide</a> developed by CNRA, provides guidance to support regional and local communities in proactively addressing climate change, including step-by- step process for local and regional climate vulnerability assessments and adaptation strategy development.

                       
                       </li>
                           
                       </ul>
                       
                       "
                   )
                   
                   
                 ),
                 column(3, ""))
      ),
      tabPanel(
        "The Ten Essential Services and Climate Change",
        fluidRow(column(2, ""),
                 column(
                   8,
                   HTML(
                     "<div style='text-align:center'>
                   <h1>The Ten Essential Services and Climate Change</h1>
                   <br>
                   <h4>The <a href='http://www.cdc.gov/nphpsp/essentialservices.html' target+'_blank'>Ten Essential Public Health Services</a> are tenets of public health practice that can be applied to climate change planning and implementation activities by a health department. Many activities for climate change planning, interventions, and capacity building are public health evidence-based practices that address adaptation and resilience planning, as well as the social determinants of health. Climate adaptation planning also supports the <a href='http://www.phaboard.org/' target='_blank'>public health accreditation</a> efforts already underway by many health departments. </h4>


                      <hr>
           <img src = 'ten_essential_services.png' width = '90%'> </img>

              <br><hr><br>





          </div>"
                   )
                   
                   
                 ),
                 column(2, ""))
      )
    )
    
    
    
    # navbarMenu(
    #   "How to Use",
    #  tabPanel("The County Snapshot Page",
    #            fluidRow(
    #              column(3,
    #                     wellPanel(includeMarkdown("howToSnapshot.md"))),
    #              column(9,
    #                     wellPanel(
    #                       img(
    #                         class = "img-polaroid",
    #                         src = "snapGif.gif",
    #                         alt = "How To County Snapshot Page",
    #                         width = "100%"
    #                       )
    #                     ))
    #
    #            )),
    #   tabPanel("The Single Indicator Page",
    #            fluidRow(
    #              column(3,
    #                     wellPanel(includeMarkdown(
    #                       "howToIndicator.md"
    #                     ))),
    #              column(9,
    #                     wellPanel(
    #                       img(
    #                         class = "img-polaroid",
    #                         src = "indGif.gif",
    #                         alt = "How To Indicator Page",
    #                         width = "100%"
    #                       )
    #                     ))
    #
    #            )),
    #  tabPanel("The Vulnerability Page",
    #           fluidRow(
    #             column(3,
    #                    wellPanel(includeMarkdown("howToVuln.md"))),
    #             column(9,
    #                    wellPanel(
    #                      img(
    #                        class = "img-polaroid",
    #                        src = "vulnGif2.gif",
    #                        alt = "How To Vulnerability Page",
    #                        width = "100%"
    #                      )
    #                    ))
    #
    #           )),
    #   tabPanel("The Query Your Data Page",
    #            fluidRow(
    #              column(3,
    #                     wellPanel(includeMarkdown("howToQuery.md"))),
    #
    #              column(9,
    #                     wellPanel(
    #                       img(
    #                         class = "img-polaroid",
    #                         src = "queryGif.gif",
    #                         alt = "How To Query Page",
    #                         width = "100%"
    #                       )
    #                     ))
    #
    #            ))
    # )
    # tabPanel(
    #   "Testing",
    #   dataTableOutput("test"),
    #   dataTableOutput("test2"))
    #####  Finish Additional  #####
    
  )
)
