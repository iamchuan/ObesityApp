library(shiny)
library(readxl)
library(dplyr)
library(maps)
library(mapproj)
library(leaflet)
library(rgdal)
library(plotly)
library(googleVis)
library(car)

#################
## Data import ##
#################
# Data source: http://www.ers.usda.gov/data-products/
health <- read_excel('Data/Health.xls')
sec <-  read_excel('Data/Socioeconomic.xls')
access <-  read_excel("Data/access.xls")
stores <-  read_excel("Data/stores.xls")
restaurants <-  read_excel("Data/restaurants.xls")
insecurity <-  read_excel("Data/insecurity.xls")
local <-  read_excel("Data/local.xls")

# load county codes form package 'maps'
data("county.fips")

#######################
## Data manipulation ##
#######################
## Data for map using Health and SEC

fipsjoin = function(x) {
  # append county codes to data
  merge(county.fips, x, by = 'fips') 
}

health$fips <- as.integer(health$FIPS)
sec$fips <- as.integer(sec$FIPS)

health_fips = fipsjoin(health)
sec_fips = fipsjoin(sec)

## Join all the data together by the fips, id number of counties
access$fips <- as.integer(access$FIPS)
stores$fips <- as.integer(stores$FIPS)
restaurants$fips <- as.integer(restaurants$FIPS)
insecurity$fips <- as.integer(insecurity$FIPS)
local$fips <- as.integer(local$FIPS)

fulldf = merge(sec, health, by = "fips") %>% 
  merge(., access, by = "fips") %>% 
  merge(., stores, by = "fips") %>%
  merge(., restaurants, by = "fips") %>% 
  merge(., insecurity, by = "fips") %>%
  merge(., local, by = "fips")

# Select cols of interest.
fulldf = fulldf[, c("fips", "State", "County", "PCT_LACCESS_POP10", "GROCPTH12",
                    "CONVSPTH12", "FFRPTH12", "FOODINSEC_10_12", "FSRPTH12",
                    "DIRSALES_FARMS07", "FMRKTPTH13", "VEG_FARMS07", "PCT_DIABETES_ADULTS10",
                    "PCT_OBESE_ADULTS10", "PCT_HSPA09", "RECFACPTH12", "MEDHHINC10", 
                    "POVRATE10", "PCT_65OLDER10", "PCT_18YOUNGER10")]

## Creating data tab
tabledb = fulldf
names(tabledb) = c("County ID (FIPS)", "State", "County", "Population, low access to store (%), 2010",
                   "Grocery stores/1000 pop, 2012", "Convenience stores/1,000 pop, 2012",
                   "Fastfood restaurants/1000 pop,2012", "Household food insecurity (%), 2010-12",
                   "Full-service restaurants/1000 pop, 2012", "Farms with direct sales, 2007", "Farmers markets/1000 pop, 2013", 
                   "Vegetable farms, 2007", "Adult diabetes rate, 2010", "Adult obesity rate, 2010",
                   "High schoolers physically active (%), 2009", "Recreation and fitness facilities/1000 pop, 2012",
                   "Median household income, 2010", "Poverty rate, 2010", "Population percent 65 years or older, 2010",
                   "Population percent under age 18, 2010")

