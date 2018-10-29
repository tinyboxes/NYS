# Global for Maternity Trends in NY State App

library(rgdal)
library(sp)
library(shiny)
library(tidyverse)
library(ggplot2)
library(leaflet)
library(googleVis)

### Load data ###
# County shape files, transform from UTM to longlat
shape <-  readOGR('~/Desktop/NYS/Shapes/Counties_Shoreline.shp')
NY <- spTransform(shape, CRS('+proj=longlat +ellps=GRS80'))

df <- read_csv('~/Desktop/NYS/Data/Hospital_Maternity_Information__Beginning_2008.csv')

# reformatting columns
cols = colnames(df)
cols <- gsub("[[:space:]]", "", cols)
colnames(df) <- cols

# Subset necessary columns and rename
maternity <- df %>%
  dplyr::select(.,-FacilityID, -MeasureID, -Denominator) %>%
  rename(., Measure = MeasureName, County = HospitalCounty)

# Change county names to title string format
maternity$County <- stringr::str_to_title(maternity$County)

# get value sums by county, year
county_sum <- maternity %>%
  group_by(County, Year, Measure) %>%
  dplyr::summarise(n= sum(Count))

# get total births by county, year
total <- maternity %>%
  filter(Measure == 'Total Births') %>%
  group_by(County, Year) %>%
  dplyr::summarise(t = sum(Count))

# merge data frames to be used for percent computations
merged <- merge(county_sum, total)


# append NY to hospital name for Google Vis Geochart
hospital <- maternity
hospital$HospitalName <- hospital$HospitalName %>%
  paste0(., ', NY')
