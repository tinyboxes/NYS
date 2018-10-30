library(tidyverse)
library(rgdal)
library(maps)
library(mapdata)

# Script to clean data for Shiny WebApp

### Load data ###
df <- read_csv('~/Desktop/Maternity/Hospital_Maternity_Information__Beginning_2008.csv')

# reformatting columns
cols = colnames(df)
cols <- gsub("[[:space:]]", "", cols)
colnames(df) <- cols

# Subset necessary columns and rename for proper joining with shape data
maternity <- df %>%
  select(.,-FacilityID, -MeasureID, -Denominator) %>%
  rename(., Measure = MeasureName, County = HospitalCounty)

# Change county names to title string format
maternity$County <- stringr::str_to_title(maternity$County)

# county shape files, transform from UTM to longlat

shape <-  readOGR('./Shapes/Counties_Shoreline.shp')

shapeData <- spTransform(shape, CRS("+proj=longlat +ellps=GRS80"))

# Data with matching countynames 
# Remove Statewide and Rest of State Data
mat = maternity %>%
  group_by(., County) %>%
  # change name to match shape file
  # mutate(., NAME = County) %>%
  filter(., County %in% shapeData$NAME)

county_data = mat %>%
  filter(.,Year == 2008) %>%
  group_by(., County, Measure) %>%
  summarise(., c = sum(Count))


# Add a column for each measure with count
df08_meas = county_data%>%
  spread(., key = Measure, value = c) %>%
  mutate(., NAME = County)

# Join 2008 data to modified shape object
shapeData@data <- shapeData@data %>%
  left_join(., df08_meas)
write.csv(sh08, "shape08.csv")

writeOGR(shapeData, '.', 'sh08', driver="ESRI Shapefile")

sh08@data
