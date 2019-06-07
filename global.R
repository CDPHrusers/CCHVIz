library(shiny)
library(tidyverse)
library(leaflet)
library(shinythemes)
library(ggthemes)
library(sf)
library(DT)
library(plotly)

links <- read.csv("CHPRlinks.csv", header = T)
CHVIdata <- readRDS("chviCountyTidyRace.RDS")
CHVItracts <- readRDS("chviTractTidy.RDS")
counties <-
  st_read("counties.geojson", stringsAsFactors = F)  %>% st_transform(crs = 4326)
tracts <-
  st_read("tracts.GeoJSON", stringsAsFactors = F) %>% st_transform(crs = 4326) %>%
  mutate(COUNTYFI_1 = as.character(paste0(STATE, COUNTY)))

# names(st_geometry(counties)) = NULL
# names(st_geometry(tracts)) = NULL

f1 <- list(
  family = "Arial, sans-serif",
  size = 11,
  color = "darkgrey"
)

f2 <- list(
  family = "Arial, sans-serif",
  size = 14,
  color = "black"
)


CHVIdata$def <-
  ifelse(
    CHVIdata$def == "percent impervious surface cover",
    "Percent impervious surface cover",
    CHVIdata$def
  )


CHVIdata$strata <-
  ifelse(
    CHVIdata$strata == "2085",
    "2070-2099",
    ifelse(
      CHVIdata$strata == "2050",
      "2035-2064",
      CHVIdata$strata
    ))


CHVIdata <- left_join(x = CHVIdata, y = {
  data.frame(
    def = c(
      "Percent of households without air conditioning",
      "Percent without tree canopy coverage",
      "Percent of population age less than 5 years",
      "Number of Violent Crimes per 1,000 Population",
      "Percent of population with a disability",
      "Percent of adults with less than HS education" ,
      "Percent of population aged 65 years or older",
      "Projected number of extreme heat days",
      "Percent impervious surface cover",
      "Percent of adults aged 18 - 64 without health insurance",
      "Percent of households with no one aged > 14 years speaking English",
      "Percent of population employed and aged > 16 working outdoors",
      "Three-year ozone concentration exceedance",
      "Annual Mean Ambient Concentration of Fine Particulate Matter (PM2.5)",
      "Overall, concentrated, and child (0 to 18 years of age) poverty rate",
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
    defShort = c(
      "% HH w/o AC",
      "% w/o Tree Canopy",
      "% under 5",
      "Violent Crimes/1,000",
      "% with a Disability",
      "% w/o HS Education",
      "% over 65",
      "Extreme Heat Days",
      "% Impervious Surface",
      "% w/o Health Insurance",
      "% HH w/o English Speaker",
      "% outdoor workers",
      "O3 Concentration above Standard",
      "Annual Mean PM2.5 Concentration",
      "% in Poverty",
      "% in Sea Level Rise Risk Areas",
      "% HH w/o Vehicle",
      " % in Very High Wildfire Risk"
    )
  )
})


narratives <-
  data.frame(
    def = c(
      "Percent of households without air conditioning",
      "Percent without tree canopy coverage",
      "Percent of population age less than 5 years",
      "Number of Violent Crimes per 1,000 Population",
      "Percent of population with a disability",
      "Percent of adults with less than HS education" ,
      "Percent of population aged 65 years or older",
      "Projected number of extreme heat days",
      "Percent impervious surface cover",
      "Percent of adults aged 18 - 64 without health insurance",
      "Percent of households with no one aged > 14 years speaking English",
      "Percent of population employed and aged > 16 working outdoors",
      "Three-year ozone concentration exceedance",
      "Annual Mean Ambient Concentration of Fine Particulate Matter (PM2.5)",
      "Overall, concentrated, and child (0 to 18 years of age) poverty rate",
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
    )
  )


