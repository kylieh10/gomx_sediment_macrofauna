# Load libraries ----------------------------------------------------------

library(dplyr)
library(sbtools)
library(stringr)

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
  bind_rows(SedChem) %>% 
  
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
    eventID = paste(Site, eventDate %>% as.character(), materialEntityID, 
                    sep = "_") %>% stringr::str_remove_all(pattern = "-"),
    minimumDepthInMeters = Depth,
    maximumDepthInMeters = Depth,
    locality = paste("BOEM Lease Block", Site),
    higherGeography = paste("Gulf of Mexico",
                            paste("BOEM Lease Block",
                                  Site), sep = " | "),
    samplingProtocol = Gear
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
    minimumDepthInMeters,
    maximumDepthInMeters,
    samplingProtocol,
    locationRemarks
  ) %>%

  distinct()

# Sample Level Event Table -------------------------------------------------

Infauna_Sample <- Infauna %>% 

  rename(
    locationRemarks = Location,
    materialEntityID = SampleID,
    locationID = Station,
    decimalLatitude = Latitude,
    decimalLongitude = Longitude
  ) %>%
  
  mutate(
    geodeticDatum = "WGS84",
    eventDate = DateCollected %>% 
      as.Date("%m/%d/%Y"),
    parentEventID = paste(Site, eventDate %>% as.character(), CoreID, 
                          sep = "_") %>% stringr::str_remove_all(pattern = "-"),
    eventID = paste(Site, eventDate %>% as.character(), materialEntityID, sep = "_") %>%
      stringr::str_remove_all(pattern = "-"),
    minimumDepthInMeters = Depth,
    maximumDepthInMeters = Depth,
    locality = paste("BOEM Lease Block", Site),
    higherGeography = paste("Gulf of Mexico",
                            paste("BOEM Lease Block", 
                                  Site), sep = " | "),
    samplingProtocol = Gear,
    Fraction=str_extract(Fraction, pattern= ".*\\d"),
    #splitting fraction into new columns for upper limit and lower limit
    maximumDistancesAboveSurfaceInMeters = str_split_i(
        Fraction, pattern = "-", i = 2) %>% 
      as.numeric()/-100,
    minimumDistanceAboveSurfaceInMeters = str_split_i(Fraction, pattern = "-", i = 1) %>% 
      as.numeric()/-100
      ) %>% 
  
  select(
    eventID,
    parentEventID,
    eventDate,
    locationID,
    decimalLatitude,
    decimalLongitude,
    higherGeography,
    locality,
    geodeticDatum,
    minimumDepthInMeters,
    maximumDepthInMeters,
    samplingProtocol,
    locationRemarks,
    minimumDistanceAboveSurfaceInMeters,
    maximumDistancesAboveSurfaceInMeters,
    materialEntityID
  ) %>% 
  distinct()


# Bind tables and write out to csv ----------------------------------------

Infauna_Event <- bind_rows(Infauna_StationCore, Infauna_Sample) %>% 
  select(parentEventID, eventID, everything()) %>% 
  filter(eventID == "MC885_20140501_AT26144705046"|eventID == "VK826_20090903_RB13D466PC03"|parentEventID == "MC885_20140501_AT26144705046"|parentEventID == "VK826_20090903_RB13D466PC03") %>%
  slice(1:10) %>%
  readr::write_csv(paste0("gomx_sediment_macrofauna_event_", Sys.Date(), ".csv"), na = "NA")
