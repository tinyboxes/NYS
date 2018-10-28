# Global for Maternity Trends in NY State App

library(rgdal)
library(sp)
library(shiny)
library(tidyverse)
library(ggplot2)
library(leaflet)

### Load data ###
df <- read_csv('~/Desktop/Maternity/Hospital_Maternity_Information__Beginning_2008.csv')

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

# County shape files, transform from UTM to longlat
shape <-  readOGR('~/Desktop/NYS/Shapes/Counties_Shoreline.shp')
NY <- spTransform(shape, CRS('+proj=longlat +ellps=GRS80'))

# Yearly shape data
# sh08 <-  readOGR('~/Desktop/NYS/shape_files/sh08.shp')
# sh09 <-  readOGR('~/Desktop/NYS/shape_files/sh09.shp')
# sh10 <-  readOGR('~/Desktop/NYS/shape_files/sh10.shp')
# sh11 <-  readOGR('~/Desktop/NYS/shape_files/sh11.shp')
# sh12 <-  readOGR('~/Desktop/NYS/shape_files/sh12.shp')
# sh13 <-  readOGR('~/Desktop/NYS/shape_files/sh13.shp')
# sh14 <-  readOGR('~/Desktop/NYS/shape_files/sh14.shp')
# sh15 <-  readOGR('~/Desktop/NYS/shape_files/sh15.shp')
# sh16 <-  readOGR('~/Desktop/NYS/shape_files/sh16.shp')
