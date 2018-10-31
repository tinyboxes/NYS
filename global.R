# Global for Maternity Trends in NY State App

library(rgdal)
library(sp)
library(shiny)
library(tidyverse)
library(plotly)
library(leaflet)
library(googleVis)
library(mapdata)

### Load data ###
# Load clean maternity data set
maternity <- read_csv('./maternity.csv')
# Load shapefiles, transform from UTM to longlat
shape <-  readOGR('./Shapes/Counties_Shoreline.shp')
NY <- spTransform(shape, CRS('+proj=longlat +ellps=GRS80'))

# Subset data for plots, etc.

# State Only Data (Get rows with County == Statewide | County == 'Rest Of State')
statewide <- maternity %>%
  filter(., County == 'Statewide' | County == 'Rest Of State')
# Rename HospitalName and County for uniform data
statewide$HospitalName = 'Statewide - All Hospitals'
statewide$County = 'Statewide'

# Statewide Averages 
stateAve <- statewide %>%
  mutate(., ave = round(Count/52, 2))

# County Only Data
countyMaternity <- maternity %>%
  filter(., !(County == 'Statewide') & !(County == 'Rest Of State'))

# get value sums by county, year
county_sum <- countyMaternity %>%
  group_by(County, Year, Measure) %>%
  dplyr::summarise(n= sum(Count))

# get total births by county, year
total <- countyMaternity %>%
  filter(Measure == 'Total Births') %>%
  group_by(County, Year) %>%
  dplyr::summarise(t = sum(Count))

# merge data frames to be used for county percent computations
merged <- merge(county_sum, total)

# append NY to hospital name for Google Vis Geochart
hospital <- countyMaternity
hospital$HospitalName <- hospital$HospitalName %>%
  paste0(., ', NY')

### Scatter Plot Analysis of 2010 data

county10 <- county_sum %>%
  filter(., Year %in% 2010) %>%
  spread(., key = Measure, value = n)

pop10 <- NY@data[c('NAME','POP2010')]

pop10 <- pop10 %>%
  rename(., County = NAME)

twentyTen <- left_join(county10, pop10)
twentyTen <- rename(twentyTen, Population = POP2010)
twentyTen$Population <- as.integer(as.character(twentyTen$Population))
