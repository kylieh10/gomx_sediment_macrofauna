# this is a fun little workspace :)


# Load libraries ----------------------------------------------------------

library(dplyr)
install.packages("sbtools")
library(sbtools)

# Read data from ScienceBase ----------------------------------------------
# link to data: https://www.sciencebase.gov/catalog/item/5a709594e4b0a9a2e9d88e4e

sb_id <- '5a709594e4b0a9a2e9d88e4e'
sbtools::item_get(sb_id = sb_id)

sb_filenames <- item_list_files(sb_id = sb_id)

BTA <-readr::read_csv(file = sb_filenames$url[1])
Infauna <- readr::read_csv(file = sb_filenames$url[2])
SedChem <- readr::read_csv(file = sb_filenames$url[3])


# Renaming columns to DwC -------------------------------------------------

names(Infauna)

#site level event table
Infauna %>%
  rename(
    decimalLatitude = Latitude,
    decimalLongitude = Longitude,
    eventDate = DateCollected,
    locationRemarks = Location,
    eventID = Site
  ) %>%
  mutate(
    geodeticDatum = "WGS84",
    minimumDepthInMeters = Depth,
    maximumDepthInMeters = Depth
    
  ) %>%
  select(
    eventID,
    Station,
    eventDate,
    decimalLatitude,
    decimalLongitude,
    minimumDepthInMeters,
    maximumDepthInMeters,
    geodeticDatum
  ) %>%
  distinct() %>%
 group_by(eventID,Station) %>% 
  count() %>% 
  arrange(eventID, n)

#station level event table
Infauna_Station <- Infauna1 %>% 
  
  rename(
    parentEventID = Site,
    eventID = Station
  ) %>% 
  mutate(
    samplingProtocol =
      paste(Gear, CoreDiameter, sep = "_")
  ) %>% 
  select(decimalLatitude,
         decimalLongitude,
         parentEventID,
         eventID,
         samplingProtocol,
         geodeticDatum
         )

#core level event table
Infauna_Core <- Infauna1 %>% 

  rename(
    parentEventID = Station,
    eventID = CoreID
  ) %>% 
  mutate(
    samplingProtocol = 
      paste(Gear, CoreDiameter, sep = "_")
  ) %>% 
  select(
    decimalLatitude,
    decimalLongitude,
    locationRemarks,
    samplingProtocol,
    eventDate,
    eventID,
    parentEventID,
    maximumDepthInMeters,
    minimumDepthInMeters,
    geodeticDatum
  )
View(Infauna_Core)






