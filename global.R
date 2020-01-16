# setwd("//mnt/projects/ohe/CCHVIz")
# setwd("R://CCHVIz")
library(shiny)
library(tidyverse)
library(plotly)
library(leaflet)
library(shinythemes)
library(data.table)
library(ggthemes)
library(sf)
library(DT)
library(ggiraph)

links <- data.frame(
  County = c(
    "Alameda",
    "Alpine",
    "Amador",
    "Butte",
    "Calaveras",
    "Colusa",
    "Contra Costa",
    "Del Norte",
    "El Dorado" ,
    "Fresno"     ,
    "Glenn"       ,
    "Humbolt"      ,
    "Imperial"      ,
    "Inyo"           ,
    "Kern"           ,
    "Kings"          ,
    "Lake"           ,
    "Lassen"         ,
    "Los Algeles"    ,
    "Madera"         ,
    "Marin"          ,
    "Mariposa"       ,
    "Mendocino"      ,
    "Merced"         ,
    "Modoc"          ,
    "Mono"           ,
    "Monterey"       ,
    "Napa"           ,
    "Nevada"         ,
    "Orange"         ,
    "Placer"         ,
    "Plumas"         ,
    "Riverside"      ,
    "Sacramento"     ,
    "San Benito"     ,
    "San Bernadino"  ,
    "San Diego"      ,
    "San Francisco"  ,
    "San Joaquin"    ,
    "San Luis Obispo",
    "San Mateo"      ,
    "Santa Barbara"  ,
    "Santa Clara"    ,
    "Santa Cruz"     ,
    "Shasta"         ,
    "Sierra"         ,
    "Siskiyou"       ,
    "Solano"         ,
    "Sonoma"         ,
    "Stanislaus"     ,
    "Sutter"         ,
    "Tehama"         ,
    "Trinity"        ,
    "Tulare"         ,
    "Tuolumne"       ,
    "Ventura"        ,
    "Yolo"           ,
    "Yuba"
  ),
  CHPR_link = c(
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR001Alameda_County2-23-17.pdf"      ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR003Alpine_County2-23-17.pdf"       ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR0005Amador_County2-23-17.pdf"      ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR007Butte_County2-23-17.pdf"        ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR009Calaveras_County2-23-17.pdf"    ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR011Colusa_County2-23-17.pdf"       ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR013ContraCosta_County2-23-17.pdf"  ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR015DelNorte_County2-23-17.pdf"     ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR017ElDorado_County2-23-17.pdf"     ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR019Fresno_County11-18-17.pdf"      ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR021Glenn_County2-23-17.pdf"        ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR023Humboldt_County2-23-17.pdf"     ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR025Imperial_County2-23-17.pdf"     ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR027Inyo_County2-23-17.pdf"         ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR029Kern__County2-23-17.pdf"        ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR031Kings_County2-23-17.pdf"        ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR033Lake__County2-23-17.pdf"        ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR035Lassen_County2-23-17.pdf"       ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR037LosAngeles_County2-23-17.pdf"   ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR039Madera__County2-23-17.pdf"      ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR041Marin_County2-23-17.pdf"        ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR043MariposaCounty2-23-17.pdf"      ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR045Mendocino_County2-23-17.pdf"    ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR047Merced_County2-23-17.pdf"       ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR049Modoc_County2-23-17.pdf"        ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR051Mono_County2-23-17.pdf"         ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR053Monterey_County2-23-17.pdf"     ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR055Napa_County2-23-17.pdf"         ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR057Nevada_County2-23-17.pdf"       ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR059Orange_County2-23-17.pdf"       ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR061Placer_County2-23-17.pdf"       ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR063Plumas_County2-23-17.pdf"       ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR065Riverside__County2-23-17.pdf"   ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR067Sacramento_County2-23-17.pdf"   ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR069SanBenito_County2-23-17.pdf"    ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR071SanBernardino_County2-23-17.pdf",
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR073SanDiego_County2-23-17.pdf"     ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR075SanFrancisco_County2-23-17.pdf" ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR077SanJoaquin_County2-23-17.pdf"   ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR079SanLuisObispo_County7-17-17.pdf",
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR081SanMateo_County2-23-17.pdf"     ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR083SantaBarbara_County2-23-17.pdf" ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR085SantaClara_County2-23-17.pdf"   ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR087SantaCruz_County2-23-17.pdf"    ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR089Shasta_County2-23-17.pdf"       ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR091Sierra_County2-23-17.pdf"       ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR093Siskiyou_County2-23-17.pdf"     ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR095Solano_County2-23-17.pdf"       ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR097Sonoma_County2-23-17.pdf"       ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR099Stanislaus_County2-23-17.pdf"   ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR101Sutter_County2-23-17.pdf"       ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR103Tehama_County2-23-17.pdf"       ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR105Trinity_County2-23-17.pdf"      ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR107Tulare_County2-23-17.pdf"       ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR109Tuolumne_County2-23-17.pdf"     ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR111Ventura_County2-23-17.pdf"      ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR113Yolo_County2-23-17.pdf"         ,
    "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHPRs/CHPR115Yuba_County2-23-17.pdf"
  )
  
)



# links <- fread("CHPRlinks.csv")
CHVIdata <-
  data.table(readRDS("chviCountyTidyRace3.RDS"), key = c("def", "county"))
CHVItracts <-
  data.table(readRDS("chviTractTidy3.RDS"), key = c("def", "county"))
counties <-
  st_read("counties.geojson", stringsAsFactors = F)  %>% st_transform(crs = 4326)
tracts <-
  st_read("tractsSM.GeoJSON", stringsAsFactors = F) %>% st_transform(crs = 4326) %>%
  mutate(COUNTYFI_1 = as.character(paste0(STATE, COUNTY)))

# done to fix issue with Leaflet maps not showing in browser
names(st_geometry(counties)) = NULL
names(st_geometry(tracts)) = NULL

# styles for labels in figures
f1 <- list(family = "Arial, sans-serif",
           size = 13,
           color = "darkgrey")

f2 <- list(family = "Arial, sans-serif",
           size = 14,
           color = "black")

extras <- data.table(
  def = c(
    "Percent of households without air conditioning",
    "Percent without tree canopy coverage",
    "Percent of population age less than 5 years",
    "Number of Violent Crimes per 1,000 Population",
    "Percent of population with a disability",
    "Percent of adults with less than college education"   ,
    "Percent of population aged 65 years or older",
    "Projected number of extreme heat days 2040-2060",
    "Projected number of extreme heat days 2080-2099",
    "Percent impervious surface cover",
    "Percent of population without health insurance",
    "Percent of households with no one aged > 14 years speaking English",
    "Percent of population employed and aged > 16 working outdoors",
    "Average Daily Maximum Ozone Concentration",
    "Annual Mean Ambient Concentration of Fine Particulate Matter (PM2.5)",
    "Poverty Rate (200% FPL)",
    "Population living in sea level rise inundation areas",
    "Percent of households with no vehicle ownership",
    "Percent of population currently living in very high wildfire risk areas"
  ),
  defHealth = c(
    "Air conditioning (AC) is an important protective factor against heat-related morbidity and mortality.",
    "Urban greening, such as parks and trees, may have a local cooling effect.",
    "Children are especially vulnerable because they are rapidly growing, both physically and mentally.",
    "Safe neighborhoods that are free of crime and violence are an integral component of healthy neighborhoods.",
    "Those with disabilities face disadvantages with limited resources and capacity during the phases of evacuation, response, and recovery.",
    "Education is a key pathway to employment, housing, transportation, health insurance, and other basic necessities for a healthy life.",
    "People aged 65 and older are especially vulnerable to the health impacts of climate change.",
    "Heat waves are associated with increased hospital admissions for cardiovascular, kidney stones, mental health, diabetes, and respiratory disorders.",
    "Heat waves are associated with increased hospital admissions for cardiovascular, kidney stones, mental health, diabetes, and respiratory disorders.",
    "Impervious surfaces retain heat and make urban areas warmer than the surrounding non-urban areas.",
    "Insurance coverage is a key determinant of timely access and utilization of health services.",
    "Linguistic isolation may limit understanding of health warnings. Also extreme weather can disrupt the management of chronic conditions for the socially or linguistically isolated.",
    "Working in an environment that is excessively hot poses a risk factor for heat health effects among persons who work outdoors.",
    "Climate change is projected to increase cardiovascular and respiratory health impacts associated with ground-level ozone.",
    "PM2.5 is capable of reaching deep into the lungs and causing a host of diseases.",
    "Poverty limits the acquisition of basic material necessities and it can impact the ability to live a healthy life.",
    "Rising sea levels can contaminate drinking water, flood homes and infrastructure, and displace residents and employers. The impacts include toxic exposures, mental and physical trauma, and food insecurity.",
    "Transportation improves access to evacuation and shelter from climate hazards, such as wildfire, air pollution, heat waves, and flooding.",
    "Wildfires can lead to injuries and deaths from burns, smoke inhalation, and displacement."
  ),
  catjv = c(
    "adaptive capacity",
    "adaptive capacity",
    "sensitivity",
    "sensitivity",
    "sensitivity",
    "sensitivity" ,
    "sensitivity",
    "environment",
    "environment",
    "adaptive capacity",
    "sensitivity",
    "sensitivity",
    "sensitivity",
    "environment",
    "environment",
    "sensitivity",
    "environment",
    "sensitivity",
    "environment"
  ),
  units = c(
    "%",
    "%",
    "%",
    "Crimes/1,000",
    "%",
    "%",
    "%",
    "days/yr",
    "days/yr",
    "%",
    "%",
    "%",
    "%",
    "ppm",
    "µg/m3",
    "%",
    "%",
    "%",
    "%"
  ),
  defShort = c(
    "% HH w/o AC",
    "% w/o Tree Canopy",
    "% under 5",
    "Violent Crimes/1,000",
    "% with a Disability",
    "% w/o college education",
    "% over 65",
    "Extreme Heat Days - midcentury",
    "Extreme Heat Days - end of century",
    "% Impervious Surface",
    "% w/o Health Insurance",
    "% HH w/o English Speaker",
    "% outdoor workers",
    "Avg Daily Max O3 Concentration",
    "Annual Mean PM2.5 Concentration",
    "% in Poverty",
    "% in Sea Level Rise Risk Areas",
    "% HH w/o Vehicle",
    "% in Very High Wildfire Risk"
  )
)

narratives <-
  data.frame(
    def = c(
      "Percent of households without air conditioning",
      "Percent without tree canopy coverage",
      "Percent of population age less than 5 years",
      "Number of Violent Crimes per 1,000 Population",
      "Percent of population with a disability",
      "Percent of adults with less than college education"   ,
      "Percent of population aged 65 years or older",
      "Projected number of extreme heat days 2040-2060",
      "Projected number of extreme heat days 2080-2099",
      "Percent impervious surface cover",
      "Percent of population without health insurance",
      "Percent of households with no one aged > 14 years speaking English",
      "Percent of population employed and aged > 16 working outdoors",
      "Average Daily Maximum Ozone Concentration",
      "Annual Mean Ambient Concentration of Fine Particulate Matter (PM2.5)",
      "Poverty Rate (200% FPL)",
      "Population living in sea level rise inundation areas",
      "Percent of households with no vehicle ownership",
      "Percent of population currently living in very high wildfire risk areas"
    ),
    narrativeLink = c(
      "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/AirConditioning_797_Narrative_12-14-2016.pdf",
      "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_TreeCanopy_458_Narrative_12-5-2016.pdf",
      "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/Children0to4_788_Narrative.pdf",
      "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/HCI_Crime_752-Narrative_Examples-10-30-15.pdf",
      "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_Disability_Narrative_795_11-16-2016.pdf",
      "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/Educ_attain_HS_Narrative_Examples4-28-13.pdf",
      "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/Elderly_789_Narrative.pdf",
      "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_ExtremeHeat_Narrative.pdf",
      "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_ExtremeHeat_Narrative.pdf",
      "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/ImperviousSurfaces_423_Narrative_12-2-2016.pdf",
      "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_Insurance_187_Narrative_11-29-2016.pdf",
      "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_LinguisticIsolation_Narrative_11-15-2016.pdf",
      "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_OutdoorsWorkers_Narrative_790_12-5-2016.pdf",
      "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_Ozone_801_Narrative.pdf",
      "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/BRACE_PM25_776_Narrative.pdf",
      "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/HCI_PovertyRate_754_Narrative_Examples11-5-13rev3-12-14.pdf",
      "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/Sealevelrise_Narrative_11-1-2016.pdf",
      "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/CarOwnership_37_Narrative_9-6-16.pdf",
      "https://www.cdph.ca.gov/Programs/OHE/CDPH%20Document%20Library/CHVIs/WildfireZone_786_Narrative_11-8-2016.pdf"
    ),
    resourceLink = c(
      "http://opr.ca.gov/docs/20180312-Vulnerable_Communities_Descriptions.pdf#page=13",
      "http://opr.ca.gov/docs/20180312-Vulnerable_Communities_Descriptions.pdf#page=4",
      "http://opr.ca.gov/docs/20180312-Vulnerable_Communities_Descriptions.pdf#page=2",
      "http://opr.ca.gov/docs/20180312-Vulnerable_Communities_Descriptions.pdf",
      "http://opr.ca.gov/docs/20180312-Vulnerable_Communities_Descriptions.pdf#page=12",
      "http://opr.ca.gov/docs/20180312-Vulnerable_Communities_Descriptions.pdf",
      "http://opr.ca.gov/docs/20180312-Vulnerable_Communities_Descriptions.pdf#page=7",
      "http://www.phi.org/uploads/application/files/t25ttiwnzfhq9cw5do2b3hllzrc0f9o4hbri8e8yngt1wyfvjd.pdf#page=32",
      "http://www.phi.org/uploads/application/files/t25ttiwnzfhq9cw5do2b3hllzrc0f9o4hbri8e8yngt1wyfvjd.pdf#page=32",
      "http://opr.ca.gov/docs/20180312-Vulnerable_Communities_Descriptions.pdf#page=4",
      "http://opr.ca.gov/docs/20180312-Vulnerable_Communities_Descriptions.pdf#page=3",
      "http://opr.ca.gov/docs/20180312-Vulnerable_Communities_Descriptions.pdf#page=6",
      "http://opr.ca.gov/docs/20180312-Vulnerable_Communities_Descriptions.pdf#page=8",
      "http://www.phi.org/uploads/application/files/t25ttiwnzfhq9cw5do2b3hllzrc0f9o4hbri8e8yngt1wyfvjd.pdf#page=54",
      "http://www.phi.org/uploads/application/files/t25ttiwnzfhq9cw5do2b3hllzrc0f9o4hbri8e8yngt1wyfvjd.pdf#page=54",
      "http://opr.ca.gov/docs/20180312-Vulnerable_Communities_Descriptions.pdf#page=11",
      "http://www.phi.org/uploads/application/files/t25ttiwnzfhq9cw5do2b3hllzrc0f9o4hbri8e8yngt1wyfvjd.pdf#page=64",
      "http://opr.ca.gov/docs/20180312-Vulnerable_Communities_Descriptions.pdf#page=16",
      "http://www.phi.org/uploads/application/files/t25ttiwnzfhq9cw5do2b3hllzrc0f9o4hbri8e8yngt1wyfvjd.pdf#page=48"
    ),
    challenge = c(
      # Air Conditioning
      "Increasing temperatures and the risk of heat waves pose a serious public health concern. Air conditioning (AC) is an important protective factor against heat-related morbidity and mortality.",
      #  Tree cover
      "Increasing temperatures and the risk of heat waves poses a serious public health concern. Increasing temperatures in urban areas can cause the urban heat island effect (a phenomenon in which urban areas are warmer than the surrounding non-urban areas).",
      # Children
      "Children, primarily because of physiological and developmental factors, are disproportionately impacted from the effects of heat waves, air pollution, infectious illnesses, and trauma resulting from climate change.",
      #violent crime
      "There are a number of pathways between climate change and violence. Natural disasters worsened by climate change increase the displacement of victims, which in turn increase population densities and tensions over resources. Violent crime also increases during heat events. Increased population densities and decreased resources, whether factual or perceived, are likely to further violent crime.",
      # disability
      "Climate change will bring a range of more frequent, long lasting and severe adverse environmental changes, which can affect the severity and incidence of mental disabilities and mental health problems. Persons with physical disabilities face disadvantages as they have limited resources and mobility during the phases of evacuation, response, and recovery. ",
      #education
      "Completion of formal education such as high school and college is a key pathway to employment and access to higher paying jobs that can provide food, housing, transportation, health insurance, and other determinants of health. Lack of educational attainment increases the vulnerability to climate-related stressors and hazards by decreasing adaptability and protective measures. Climate exposures, such as heat, drought and wildfires amplify the pre-existing health inequities associated with low educational attainment.",
      # elderly
      "Climate change is increasing the severity and frequency of heat waves and extreme weather events which poses a risk to elderly populations. Aging impairs muscle strength, coordination, cognitive ability, immune system, and regulation of body temperature. In addition, the proportion of the elderly population in the United States is increasing and California is one of nine states where most Americans aged 65 years and older reside.",
      # heat
      "Periods of warmer temperatures and heat waves are expected to increase in frequency, intensity, and duration throughout the 21st century. Warmer temperatures increase the heat inside buildings and the need for cooling in urban areas and intensify existing urban heat islands (a phenomenon in which urban areas are warmer than the surrounding non-urban areas) in areas that are most heavily populated. There will be increases in annual average temperature of up to 5&#176;F by 2030 and up to 10&#176;F by the end of the century or sooner, although not every day will be hotter than current averages. Minimum nighttime temperatures are also projected to increase. For example, the 2006 California heat wave brought higher temperatures combined with increased humidity, particularly at nighttime. Increased daytime temperatures, reduced nighttime cooling, and higher air pollution levels associated with urban heat islands can affect human health and exacerbate the impact of heat waves.",
      # heat
      "Periods of warmer temperatures and heat waves are expected to increase in frequency, intensity, and duration throughout the 21st century. Warmer temperatures increase the heat inside buildings and the need for cooling in urban areas and intensify existing urban heat islands (a phenomenon in which urban areas are warmer than the surrounding non-urban areas) in areas that are most heavily populated. There will be increases in annual average temperature of up to 5&#176;F by 2030 and up to 10&#176;F by the end of the century or sooner, although not every day will be hotter than current averages. Minimum nighttime temperatures are also projected to increase. For example, the 2006 California heat wave brought higher temperatures combined with increased humidity, particularly at nighttime. Increased daytime temperatures, reduced nighttime cooling, and higher air pollution levels associated with urban heat islands can affect human health and exacerbate the impact of heat waves.",
      # impervious
      "Impervious surfaces are areas covered by material that impedes the infiltration of water into the soil. Examples of impervious surfaces are buildings, pavements, concrete, and severely compacted soils. Impervious surfaces retain heat and limit absorption of water into the ground, which can lead to the urban heat island effect, a phenomenon in which urban areas are warmer than the surrounding non-urban areas. Measures of impervious surfaces are important for assessing impacts from infrastructure development and built environment on urban temperatures, precipitation runoff, and water quality.",
      # health insurance
      "Excessive heat exposure, elevated levels of air pollutants, and extreme weather conditions are expected to cause direct and indirect health impacts, particularly for vulnerable populations with limited or no access to health services. Health insurance enables access to care by connecting people to health care providers and by protecting persons against the high and often unexpected costs of medical care. A lack of health insurance among vulnerable populations that are exposed to the effects of climate changes may lead to greater health impacts.",
      # language
      "Climate change and resulting natural disasters and extreme temperatures pose a serious public health concern for people who are linguistically isolated. A household is linguistically isolated when all persons 14 years of age or older speak a language other than English and no one over 14 speaks English very well.",
      # outdoor workers
      "Working in an environment that is excessively hot poses a risk for heat-related health effects among persons who work outdoors.",
      # Ozone
      "Higher temperatures increase ground-level ozone and other secondary air pollutants created from chemical reactions with pollutants directly emitted from power plants, motor vehicles, and other sources, creating smog and air pollution. With the projected increasing temperatures, demand for electric power generation will increase and may contribute further to poor air quality.",
      # PM
      "There are multiple ways that climate change will worsen air quality. Particulate matter and ozone that forms at the ground-level are two indicators of air pollution that are linked to short- and long-term adverse health effects. Particulate matter (PM2.5) is an extremely small pollutant that is capable of reaching deep into the lungs causing a host of diseases including lung cancer, heart disease, respiratory disease, and acute respiratory infections, particularly in children. More frequent and intense wildfires will expose people to smoke that contains particulate matter and numerous chemicals. Warming causes plants, such as ragweed, to grow and produce more pollen, and lengthens the pollen season. With projected increasing temperatures, demand for electric power generation will increase and may contribute further to poor air quality.",
      # poverty
      "Poverty increases the vulnerability to climate change impacts, and communities in poverty have fewer resources to evacuate during natural disasters such as wildfires. Poverty reduces the capacity to adapt to rising food, water, or energy prices. It is harder for low-income communities to rebuild after a disaster, especially since fewer low-income people have insurance. Additionally, people with low to middle income receive worse quality of care than people with high income and face barriers to accessing care, even if insured. Those with low incomes are also more likely to reside in housing that sustain worse damage due to lower quality construction.",
      # SLR
      "Sea level has already risen along the California coastline, with 7 inches of rise documented at the San Francisco tidal gauge. By 2100, sea level rise is projected to reach up to 66 inches above the 2000 sea level along much of the California coast. Adaptive measures to sea level rise can prevent damage to coastal resources and displacement from homes and employment. Sea level rise threatens coastal wetlands, which contribute to community resilience by guarding against flooding and erosion, and protecting groundwater from contamination. Impacts of sea level rise are intensified by storms and high tides.",
      # vehicles
      "Vehicle ownership is a measure of mobility and access to transportation. Transportation is a critical resource for evacuation and survival during heat waves and other extreme weather events. For example, access to a vehicle is important during flooding which may require emergency evacuation of populations living in coastal and low-lying areas, and may also require adequate sheltering for displaced populations.",
      # WF
      "Wildfire activity in California has greatly increased in recent years. The increase has been attributed to warmer spring and summer temperatures, reduced precipitation associated with warmer temperatures, reduced snow pack and earlier snowmelts, and longer, drier summer seasons in some middle and upper elevation forests. These trends are expected to continue under plausible climate change scenarios."
    ),
    
    healthImport = c(
      # Air Conditioning
      "Air conditioning varies greatly by income, the age of the house, and geographic location. Studies have shown that having working AC was the strongest protective factor against death during a heat wave, followed by access to an air-conditioned place for some time. Research specific to California found that a 10 percent increase in AC ownership would reduce heat-related mortality by 1.4 percent per 10&#176;C change in temperature. A similar protective effect was found for the excess risk of hospitalizations.",
      #  Tree cover
      "Evidence links tree cover to reducing air pollution from particulate matter, which in turn, reduces heart disease, respiratory illness, and lung cancer. Urban greening, such as parks and trees, may have a local cooling effect through shade and evapotranspiration. Tree canopy creates environments that reduce stress and neighborhood violence. Research has shown a positive effect from a natural, green environment on physical health, mental health, and longevity. Green spaces have also been shown to lessen flood risk and increase community safety, while simultaneously promoting an active lifestyle and physical activity.",
      # Children
      "Children under 5 years old are especially vulnerable to the health impacts of climate change, particularly air pollution, because they are rapidly growing, both physically and mentally: their lungs are developing, they breathe at a higher rate than adults, and they spend more time outdoors. During the 2003 wildfires in Southern California, respiratory hospital admissions related to wildfires increased 8.3% among children under 5 years old. Exposure to air pollution increases the risk for allergen sensitization in children under 5 years old and increases risk for hospitalization for bronchiolitis and death among infants. Increased levels of air pollution also can increase asthma related emergency department visits and hospitalizations. Mold growth at home after flooding events can be associated with increases in lower respiratory symptoms. Children, infants, and pregnant women are vulnerable to increased heat exposure because they may not be able to efficiently thermoregulate; in particular, infants have a greater risk for mortality4 and heat-related illnesses. Children are dependent on their caregivers for response to extreme weather events such as hurricanes and floods. Conditions like injury, death, infectious diseases, malnourishment, and posttraumatic stress are more common in children than adults after extreme weather events. Finally, intensely stressful exposures may lead to adverse birth outcomes including pre-term birth, low-birth weight, stillbirth, and maternal complications. ",
      #violent crime
      "Safe neighborhoods that are free of crime and violence are an integral component of healthy neighborhoods. Intentional injuries - both physical and mental - from violence and crime contribute greatly to the overall burden of disease and death. Post-traumatic stress in victims, families, and community members add significantly to mental health problems and is a risk factor for chronic illnesses like cardiovascular disease. In addition to direct physical and mental impacts, fear of crime and violence inhibit the use of community assets and social interactions that promote health. These include stifling opportunities for physical exercise at nearby parks and playgrounds, or walking or bicycling as a way to commute to local destinations for basic needs. Furthermore, violence contributes to negative perceptions of neighborhoods and impacts real estate, housing, and economic development.",
      # disability
      "Climate change may affect people with mental disabilities directly through exposure to trauma or by affecting their physical health.  People with disabilities are disadvantaged in their ability to adapt to physical displacement due to difficulty in mobility and access to resources. Population displacement compromises medical care which increases the risk of disease exposure and can worsen existing medical conditions. People with disability may have multiple medical conditions and may be recipients of multiple treatments; thus, their decreased access to medical services (health care) and increased risk of disease exposure can greatly impact their health.  Rates of depression, anxiety disorders, post-traumatic stress disorders, substance abuse, and suicides are all expected to rise as the effect of climate change worsen. Following disasters, mental health problems increase, among both people with no history of mental illness or disability, and those with pre-existing risk.",
      #education
      "Education is a determinant of health with broad impacts on standards of living and social interaction, with consequences for the health of individuals and communities. Education is linked with social and psychological factors, including sense of control, social position, and support. These factors affect health by their influence on stress, health-related behaviors, and practical and emotional support. Approximately 245,000 (10%) of the 2.4 million U.S. deaths in 2000 were attributable to low education. Low education is also associated with poorer self-reported health status, higher infant mortality rates, lower cancer screening rates, and many other negative health outcomes and behaviors. A 2006 study estimated that raising the health of all adults with less than a bachelor's degree up to the health of those who do not have at least a bachelor's degree would have resulted in annual gains of over 1 trillion dollars worth of increased health, where health value was calculated using life years lost and lowered health-related quality of life.",
      # elderly
      "Growing evidence suggests that injury, disease, and death are greatest among the elderly during heatwaves. Acute kidney failure, electrolyte imbalance and inflammation were the most common heat related health effects among elderly in the 2006 California heat wave. Side effects of some medications intensified the heat-related conditions in elderly. Elderly have increased risk of other climate impacts as well. For example, elderly with limited mobility can have increased risk of flood-related impacts. During the 2003 Southern California wildfires, respiratory hospital admissions related to wildfire smoke increased 10% among adults 65 years of age and older. Pre-existing health conditions among the elderly can increase susceptibility to more severe consequences of climate-related infectious diseases. Several studies show that elderly are at increased risk of West Nile virus infection with climate change predicted to increase the overall risk of transmission in California. ",
      # heat
      "Sustained high heat days and heat waves directly affect human health through heat-related illnesses such as heat stroke, heat exhaustion, and dehydration, as well as other illnesses and premature deaths from cardiovascular or respiratory diseases. Heat waves are associated with increased hospital admissions for cardiovascular, kidney (including kidney stones), mental health, diabetes, and respiratory disorders. Extremely stressful climate exposures such as heat waves may lead to adverse birth outcomes including pre-term birth, low-birth weight, stillbirth, and maternal complications. In California, two separate examinations of a statewide heat wave in 2006 showed excess deaths ranging from 6% to 9% daily for each 10&#176;F increase in temperature.",
      # heat
      "Sustained high heat days and heat waves directly affect human health through heat-related illnesses such as heat stroke, heat exhaustion, and dehydration, as well as other illnesses and premature deaths from cardiovascular or respiratory diseases. Heat waves are associated with increased hospital admissions for cardiovascular, kidney (including kidney stones), mental health, diabetes, and respiratory disorders. Extremely stressful climate exposures such as heat waves may lead to adverse birth outcomes including pre-term birth, low-birth weight, stillbirth, and maternal complications. In California, two separate examinations of a statewide heat wave in 2006 showed excess deaths ranging from 6% to 9% daily for each 10&#176;F increase in temperature.",
      # impervious
      "Increased heat exposure due to increasing temperatures over time and heat waves can lead to adverse health effects. Studies in cities in the United States, Montreal, Barcelona, Hong Kong, and Taiwan found associations between heat-related health effects and impervious surfaces. A New York City study found that extensive urban development has the potential to increase afternoon temperatures.",
      # health insurance
      "Insurance coverage is a key determinant of timely access and utilization of health services which is a fundamental pathway to improved health outcomes. A national study demonstrated an increased risk of mortality among the uninsured compared with the insured and estimated 44,789 annual deaths among Americans aged 18 to 54 associated with lack of health insurance. A systematic review of literature in 2008 found consistent evidence demonstrating that health insurance increases utilization of health care services and improves health. A national systematic review in 2010 found that patients who were uninsured were less likely to receive critical care services than those with insurance.",
      # language
      "Linguistic isolation may hinder protective behaviors during extreme weather and disasters by limiting access to or understanding of emergency or health warnings. Failure to evacuate or take shelter in place during coastal storms or other extreme weather events increases vulnerability to injury or death. Low literacy in people who are linguistically isolated can be another barrier to accessing critical health and safety information. Additionally, natural disasters and extreme weather can disrupt management of chronic conditions for people who are socially or linguistically isolated. A study found that people who live in linguistically isolated households were at increased risk of extreme heat-related health problems and more heat distress calls to 911.",
      # outdoor workers
      "A review of miners, construction workers, farm workers, first responders, and military personnel emphasized that heat-related illness may be the most common cause of nonfatal environmental emergency department admission in the United States. California's agricultural and construction workers have experienced severe heat-related illness and death. During 1992-2006, the United States had a total of 68 farm workers die from heat stroke, representing a heat stroke rate of nearly 20 times greater than all civilian workers in the country.",
      # Ozone
      "Climate change is projected to increase cardiovascular and respiratory morbidity and mortality associated with ground-level ozone. Most California residents are currently exposed to levels at or above the current State ozone standard during some parts of the year. Studies have shown that exposure to ozone is associated with decreased lung function, respiratory symptoms, hospitalizations for cardiopulmonary causes, emergency room visits for asthma, and premature death. At higher daily concentrations, ozone increases asthma attacks, hospital admissions, daily mortality, and days of restricted activity and school absences. In California, the Air Resources Board estimated that 630 deaths, 4,200 hospital admissions, and 4.7 million lost school days could be prevented each year if California met its current statewide standard of 0.070 ppm for ozone (8-hour average). ",
      # PM
      "In California, the Air Resources Board estimated that, given the PM2.5 levels between 2009 and 2011, more than seven thousand deaths could be prevented each year if California met its current statewide PM2.5 standard of 12 µg/m3. Both short-term and long-term exposures to PM2.5 increase the risk of cardiovascular disease and death. Exposure is linked to adverse respiratory outcomes such as chronic obstructive lung disease, hospital and emergency department admissions for asthma, increased respiratory symptoms, altered pulmonary function, and pulmonary inflammation among asthmatic children. Reports have indicated that PM2.5 affects preterm birth, low birth weight, and infant mortality.",
      # poverty
      "The disproportionate impacts of climate change on individuals with pre-existing chronic illness and socially disadvantaged groups threaten to greatly exacerbate existing health and social inequities. It has been estimated that 133,250 (6%) of the 2.4 million U.S. deaths in 2000 could be attributed to poverty. For example, the impacts of climate change on higher food cost and food scarcity will magnify current inequalities in food access, food choices, and chronic diseases. The existing disparities in health status, living conditions, and other inequities increase vulnerability of low-income communities to the health impacts of climate change.",
      # SLR
      "Rising sea levels can harm mental health due to displacement, trauma, or changes to known surroundings. Flooding of crops, death of livestock or destruction of food stores or transport routes can lead to food insecurity resulting in malnutrition or obesity. Sea level rise can intensify or increase coastal flooding from storms and result in drowning, motor vehicle accidents, electrocutions, injuries, hypothermia, and stress. Flood waters contaminated with sewage overflow and hazardous substances can cause respiratory, skin or wound infections, and illness. Drinking water may become contaminated, causing gastrointestinal illness. Mold in flood-damaged homes may worsen allergies and cause other respiratory conditions.",
      # vehicles
      "Vehicle ownership is important during extreme weather events because it improves access to evacuation or access to cooling centers or shelter from environmental exposures such as wildfire, air pollution, heat waves, or flooding. A survey among predominantly poor and African American Hurricane Katrina evacuees revealed that 34% reported lack of a car or other means of transportation as the main reason for not evacuating the storm's danger. In the Los Angeles-Long Beach Metropolitan Area, higher proportions of African Americans (20%), Latino (17%), and Asian (10%) households do not have access to a car compared to White households (8%).",
      # WF
      "Wildfires can lead to injuries and deaths from burns, smoke inhalation, and displacement. Persons with chronic health conditions may face challenges in accessing health services and other public resources which may be under increased pressure during wildfires. Further health impacts include psychological reactions to an extreme event, such as trauma from displacement and evacuation. Wildfire smoke is composed of thousands of microscopic chemical compounds. Smoke and ash particles can travel many miles from the original fire location. Very small particles can penetrate deep into the lungs and can cause changes in lung function. Air pollution from wildfires affects eye and respiratory conditions such as asthma, chronic obstructive pulmonary disease, and other cardiovascular and respiratory diseases. Chemical residues from burning vegetation may contaminate land, soil, or watersheds through sediment run-off from landslides or mudslides and particulate matter deposition; these affects may lead to longer term threats to human and ecosystem health."
    ),
    vulnerable = c(
      # Air Conditioning
      "<ul><li>Areas currently unaccustomed to heat waves, such as coastal communities, northern latitudes and higher elevations.9
Houses, schools, workplaces, and older buildings (e.g., in older urban neighborhoods and rural areas) generally do not have insulation or air conditioning.</li><li>The cost of air conditioning can be a barrier to low income households. Even in households that have AC, use may be limited by financial considerations. One study found that more than one third of elderly people in the US restrict use of AC during hot weather due to financial burden.</li><li>The elderly are at an increased risk for mortality during heat waves, and children may be at increased risk for heat-related illnesses.</li><li>Some communities of color have both higher heat-related morbidity and mortality and lower access to AC. One study found heat-associated mortality to be two times higher in African Americans than in Whites, and 64 percent of this disparity is attributable to central AC prevalence.</li></ul>",
      #  Tree cover
      "<ul><li>In urban settings, places with higher concentrations of populations of color, often due to historical residential segregation, had lower tree canopy cover and higher potential risks to heat exposure.</li><li>Children and youth are particularly vulnerable because they rarely demonstrate ‘shade seeking’ behavior. At school, children can spend up to 25% of their time outdoors, typically during the period of highest UV exposure — between 10 a.m. and 4 p.m.</li></ul>",
      # Children
      "<ul><li>Young children, infants, and pregnant women</li><li>Children in low-income, rural, immigrant, or linguistically isolated households</li><li>Children with pre-existing disease (especially cardiac and respiratory)</li><li>Children who spend considerable time outdoors</li></ul>",
      #violent crime
      "<ul><li>Some communities of color: For example, a 2011 national survey found that African-American and Hispanics are at higher risk of violent crime victimization than non-Hispanic Whites.</li><li>In 2013, California law enforcement agencies reported 1,832 murders, 39,811 cases of violence against women, and over 109,000 aggravated assaults.</li><li>In California, ten percent of all deaths among people aged 15-44 years are related to assault and homicide in 2015.</li></ul>",
      # disability
      "Populations with physical disability who have increased vulnerability to the health impacts of climate change include the following:<ul><li>Persons with less education, less income, and living in an urban area</li><li>Persons who belong to marginalized groups based on gender, race, ethnicity, or language</li><li>Elderly and children with disabilities and their caregivers</li></ul><br>Among those with metal disability:<ul><li>The effects will be felt most among children, those without adequate financial resources, the elderly, and those with existing mental health conditions.</li><li>People with previous traumas or multiple losses may have longer recovery periods. Long-term mental health services are needed for low-income disaster survivors.</li><li>Increasing heat exposure can also worsen the condition of people with pre-existing chronic diseases and mental health problems. Pre-existing dementia is a risk factor for hospitalization and death during heat waves.</li><li>In Australia, recent droughts substantially increased the incidence of suicide in rural populations, particularly among male farmers and their families.</li></ul>",
      #education
      "<ul><li>Communities of low socio-economic status</li><li>People who are discriminated against or treated differentially in the education system</li><li>Both of these tend to be people of color</li></ul>",
      # elderly
      "Elderly populations with the following characteristics are at an increased risk of health impacts from climate change:
      <ul><li>Elderly ages 65 years or older</li><li>Elderly living alone, with limited mobility, who are socially isolated, residents of institutions, or dependent of care</li><li>Elderly women, low socioeconomic status, or of African American race</li><li>Elderly with multiple chronic conditions (e.g., cardiovascular diseases, respiratory illnesses, diabetes) or pre-existing health conditions</li></ul>",
      # heat
      "Populations with the greatest risk of health impacts from extreme heat, due to physical vulnerability and/or lack of resources to prepare or respond to heat, may include: <ul><li>Elderly, particularly elderly over 65 years of age and elderly living alone</li>
      <li>children, women, infants, and pregnant women</li><li>People with pre-existing chronic health conditions (e.g., respiratory disease, cardiovascular disease, diabetes, cerebrovascular diseases, respiratory diseases, and acute allergies)</li> <li>People who engage in vigorous physical activity including agricultural and outdoor workers, indoor workers, athletes (especially young athletes), military personnel, and outdoor recreationists</li><li>Populations with low socioeconomic status</li><li>Socially or geographically isolated populations</li><li>People with mental or physical disability</li><li>People in cooler areas less acclimatized to heat, with less awareness of ways to reduce exposure, and with housing not designed for warmer conditions</li><li>Residents of urban areas, of the highest floors of apartment buildings, and without air-conditioning</li><li>Some race/ethnic groups, particularlyAfrican Americans</li><li>People taking certain medications related to specific heart or mental health conditions</li></ul>",
      # heat
      "Populations with the greatest risk of health impacts from extreme heat, due to physical vulnerability and/or lack of resources to prepare or respond to heat, may include: <ul><li>Elderly, particularly elderly over 65 years of age and elderly living alone</li>
      <li>children, women, infants, and pregnant women</li><li>People with pre-existing chronic health conditions (e.g., respiratory disease, cardiovascular disease, diabetes, cerebrovascular diseases, respiratory diseases, and acute allergies)</li> <li>People who engage in vigorous physical activity including agricultural and outdoor workers, indoor workers, athletes (especially young athletes), military personnel, and outdoor recreationists</li><li>Populations with low socioeconomic status</li><li>Socially or geographically isolated populations</li><li>People with mental or physical disability</li><li>People in cooler areas less acclimatized to heat, with less awareness of ways to reduce exposure, and with housing not designed for warmer conditions</li><li>Residents of urban areas, of the highest floors of apartment buildings, and without air-conditioning</li><li>Some race/ethnic groups, particularlyAfrican Americans</li><li>People taking certain medications related to specific heart or mental health conditions</li></ul>",
      # impervious
      "<ul><li>Communities of color are disproportionately represented in densely populated areas with more impervious surfaces which increases their risk of exposure to heat stress.</li><li>Elderly: A study in New York City found a significant positive association between impervious land cover and heat-related deaths among elderly persons.</li></ul>",
      # health insurance
      "<ul><li>Low-income households</li><li>Women: A study in Los Angeles County revealed lack of access to health care services as a significant barrier to health promotion and wellness, particularly for women and especially for women of Black, Latino, or Asian/Pacific Islander ethnicity</li><li>Displaced populations: A study among populations displaced to shelters from Hurricane Katrina found lack of health insurance was a significant risk factor for lacking medications and arriving at the shelter needing immediate medical intervention</li><li>Undocumented immigrant populations, especially children</li></ul>",
      # language
      "<ul><li>Mothers with babies (particularly single mothers), young children, those with low socioeconomic status, people who are homeless, people with limited English, and those who are socially isolated within culturally and linguistically diverse communities.</li><li>New immigrants, political asylum seekers, and refugees from non-English speaking countries.</li><li>Older first generation immigrants who revert to their first languages later in life due to aging. Also, recently arrived people who are older may find it difficult to learn English and sometimes can only communicate with people their own age, if the younger generation does not speak the traditional dialects.</li></ul>",
      # outdoor workers
      "<ul><li>Farm workers and day laborers: This population tends to have lower incomes and belong to communities of color, both of which are associated with adverse health effects due to climate change.</li><li>Immigrants who work outdoors: The socioeconomic status of immigrants in California who work in the agricultural and construction sectors makes them particularly vulnerable because of long workdays under strenuous conditions, language barriers, limited capacity to protect their rights, and exposure to chemicals such as pesticides.</li><li>Outdoor occupations most at risk of heat stroke include construction, refining, surface mining, hazardous waste site activities, agriculture, forestry, and fishing.</li></ul>",
      # Ozone
      "<ul><li>Children may be more affected by ozone than the general population due to effects on the developing lung and to relatively higher exposure than adults</li><li>People with chronic diseases (e.g., respiratory diseases)</li><li>Agricultural and outdoor workers, and those who are active outdoors</li></ul>",
      # PM
      "<ul><li>Elderly, children, pregnant women, and people with chronic underlying disease (e.g., diabetes, cardiovascular diseases, obesity, respiratory diseases)</li><li>Urban residents, communities with low socioeconomic status</li><li>Agricultural and outdoor workers, and those who are active outdoors</li></ul>",
      # poverty
      "Climate-related health burdens due to poverty disproportionately impact the following populations:<ul><li>Populations who are unemployed, disabled, homeless, or uneducated</li><li>Racial, ethnic, and linguistic groups and migrants</li><li>Low-income children living in conditions that are harmful to their development and health. Children in low-income immigrant families with reduced access to services</li><li>Those with psychiatric disorders, including neurotic disorders, functional psychoses and alcohol and drug dependence</li></ul>",
      # SLR
      "Populations with the greatest exposure, difficulty preparing, evacuating, and receiving access to care during coastal inundation and flood events may include: <ul><li>Residents in coastal flood-prone areas</li><li>Populations with lower socioeconomic status, language and communication difficulties</li><li>Populations with mobility barriers (such as physical disability or limited access to transportation)</li><li>Elderly, children, and women</ul></li>",
      # vehicles
      "<ul><li>Urban areas generally have lower rates of automobile ownership, particularly in inner city populations with low income.</li><li>Some communities of color are more likely to have limited or no car ownership, which increases their risk of being impacted during heat and other extreme weather events. Populations with higher rates of people of color and poverty are less likely to own cars.</li></ul>",
      # WF
      "<ul><li>People who live in the vicinity of fires</li><li>People who live downwind from wildfire smoke</li><li>Elderly, young children, pregnant women, those of low socioeconomic status, smokers, and people with pre-existing diseases (especially cardiac and respiratory)</li><li>Agricultural and outdoor workers, and firefighters</li><li>Communities with decreased access to information about risk mitigation and without adequate emergency preparedness, including the presence of an early warning system</li></ul>"
    )
  )


colorCoder <-
  data.table(
    category = c(
      "adaptive capacity",
      "adaptive capacity",
      "adaptive capacity",
      "adaptive capacity",
      "adaptive capacity",
      "environment",
      "environment",
      "environment",
      "environment",
      "environment",
      "sensitivity",
      "sensitivity",
      "sensitivity",
      "sensitivity",
      "sensitivity"
    ),
    catcolor = c(
      "#54278f",
      "#756bb1",
      "#9e9ac8",
      "#cbc9e2",
      "#f2f0f7",
      "#08519c",
      "#3182bd",
      "#6baed6",
      "#bdd7e7",
      "#eff3ff",
      "#993404",
      "#d95f0e",
      "#fe9929",
      "#fed98e",
      "#ffffd4"
    ), 
    catlevel = c(
      "level5",
      "level4",
      "level3",
      "level2",
      "level1",
      "level5",
      "level4",
      "level3",
      "level2",
      "level1",
      "level5",
      "level4",
      "level3",
      "level2",
      "level1"
    )
  )

CHVIdata <- merge(CHVIdata, extras, by = "def")
CHVIdata <- data.table::copy(CHVIdata)


places <- fread("mssa.csv")
  
  

CHVItracts <- merge(CHVItracts, extras, by = "def")
CHVItracts <- merge(CHVItracts, places, by = "COUNTYFI_1")
CHVItracts <- data.table::copy(CHVItracts)


ranks <-
  CHVIdata[race == "Total" &
             latest == "Y", .(
               county,
               ind_strt,
               catjv,
               est,
               units,
               rankjv = ntile(est, n = 58),
               ca_est = mean(est, na.rm = T)
             ), by = .(defShort, strata)]%>%
  .[,label := ifelse(strata == "none", defShort, paste0(defShort," - ", strata))]


averages <-
  CHVIdata[latest == "Y", mean(est, na.rm = T), by = .(def, ind, strata)][, .(def, ind, strata, stateAverage = V1)]
