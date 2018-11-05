library(tidyverse)
library(rgdal)
library(maps)
library(mapdata)

# Script to clean data for Shiny WebApp

### Load data ###

# Maternity dataset raw
df <- read_csv('./Hospital_Maternity_Information__Beginning_2008.csv')

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

# Save clean Maternity data set
write.csv(maternity, 'maternity.csv', row.names = FALSE)

# County shape files, transform from UTM to longlat
shape <-  readOGR('./Shapes/Counties_Shoreline.shp')
shapeData <- spTransform(shape, CRS("+proj=longlat +ellps=GRS80"))

# Data with matching countynames 
mat = maternity %>%
  group_by(., County) %>%
  # change name to match shape file
  # mutate(., NAME = County) %>%
  filter(., County %in% shapeData$NAME)

county_data = mat %>%
  filter(.,Year == 2010) %>%
  group_by(., County, Measure) %>%
  summarise(., c = sum(Count))


# Add a column for each measure with count
df10_meas = county_data%>%
  spread(., key = Measure, value = c) %>%
  mutate(., NAME = County)

# Join 2010 data to modified shape object
shapeData@data <- shapeData@data %>%
  left_join(., df10_meas)
write.csv(sh10, "shape10.csv")

writeOGR(shapeData, '.', 'sh10', driver="ESRI Shapefile") # save merged shapefile
