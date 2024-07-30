# this is a fun little workspace :)


# Load libraries ----------------------------------------------------------

library(dplyr)
library(sbtools)

# Read data from ScienceBase ----------------------------------------------
# link to data: https://www.sciencebase.gov/catalog/item/5a709594e4b0a9a2e9d88e4e

sb_id <- '5a709594e4b0a9a2e9d88e4e'
sbtools::item_get(sb_id = sb_id)

sb_filenames <- item_list_files(sb_id = sb_id)

BTA <-readr::read_csv(file = sb_filenames$url[1])
Infauna <- readr::read_csv(file = sb_filenames$url[2])
SedChem <- readr::read_csv(file = sb_filenames$url[3])


# Core/Station Level Event Table -------------------------------------------------

#events representing unique cores (coreID)

Infauna_StationCore <- Infauna %>%
  rename(
    locationRemarks = Location,
    materialEntityID = CoreID,
    locationID = Station,
    decimalLatitude = Latitude,
    decimalLongitude = Longitude
  ) %>%
  mutate(
    geodeticDatum = "WGS84",
    eventDate = DateCollected %>% 
      as.Date("%m/%d/%Y"),
    eventID = paste(Site, eventDate %>% as.character(), locationID, materialEntityID,
                    sep = "_") %>% stringr::str_remove_all(pattern = "-"),
    # footprintWKT = NA, #get polygon from BOEM; not sure if we should include since event is lat/long specific?
    minimumDepthInMeters = Depth,
    maximumDepthInMeters = Depth,
    locality = paste("BOEM Lease Block", Site),
    higherGeography = paste("Gulf of Mexico",
                            paste("BOEM Lease Block", 
                                  Site), sep = " | "),
    samplingProtocol = paste(Gear, CoreDiameter, sep = "_")
  ) %>%
  select(
    eventID,
    eventDate,
    materialEntityID,
    locationID,
    decimalLatitude,
    decimalLongitude,
    higherGeography,
    locality,
    geodeticDatum,
    # footprintWKT,
    minimumDepthInMeters,
    maximumDepthInMeters,
    samplingProtocol,
    locationRemarks
  )


# Sample Level Event Table -------------------------------------------------

## need to split fraction into upper and lower limit (maximumDistanceAboveSurface & minimum...)
## not sure if there is a way to use dplyr for this, but the internet likes tidyr and stringr)

Infauna_Sample <- Infauna_StationCore %>% 
  rename(
    parentEventID = eventID,
    eventID = materialEntityID
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
    #and fraction once it's split
  )




