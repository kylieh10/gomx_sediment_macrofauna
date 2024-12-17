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


# Core Level Event Table -------------------------------------------------
# these events represent unique cores (according to the coreID provided by the researchers)

# create table with the core samples' min/max values to join later with events
core_minmax <- Infauna %>%
  bind_rows(SedChem) %>% 
  
  rename(
    materialEntityID = CoreID,
    locationID = Station,
    decimalLatitude = Latitude,
    decimalLongitude = Longitude,
    eventRemarks = EnvironmentalGroup
  ) %>%
# For eventID, I created a unique ID for each coring event by combining several other fields
  mutate(
    eventDate = DateCollected %>%
      as.Date("%m/%d/%Y"),
    eventID = paste(Site, eventDate %>% as.character(), materialEntityID, 
                    sep = "_") %>% stringr::str_remove_all(pattern = "-")) %>% 
# Fractions (in the original data) represent the depth of the core samples relative to the sea floor
# We use `group_by` so that we can pull the min/max values of fraction for min/max depths of each core
  group_by(eventID) %>% 
  summarise(Fraction = str_remove_all(Fraction, pattern= " cm") %>% 
              str_split(pattern = "-") %>% 
              unlist() %>% 
              as.integer()) %>% 
  group_by(eventID) %>% 
  summarise(maximumDistanceAboveSurfaceInMeters = max(Fraction) %>% 
               as.numeric()/-100,
             minimumDistanceAboveSurfaceInMeters = min(Fraction) %>% 
               as.numeric()/-100) %>% 
  distinct()

# create event table without core min/max values
SedChem <- SedChem %>%
  mutate(
    Gear = "Push core")

Infauna_StationCore <- Infauna %>%
  bind_rows(SedChem) %>% 
  
  rename(
    materialEntityID = CoreID,
    locationID = Station,
    decimalLatitude = Latitude,
    decimalLongitude = Longitude,
    eventRemarks = EnvironmentalGroup
  ) %>%

  mutate(
    locationRemarks = case_when(Location == "Near" ~ paste("within 1 meter of", Coral),
                                Location == "Background" ~ paste("14 to 1000 meters away from", Coral)),
    geodeticDatum = "WGS84",
    eventDate = DateCollected %>%
      as.Date("%m/%d/%Y"),
    eventID = paste(Site, eventDate %>% as.character(), materialEntityID, 
                    sep = "_") %>% stringr::str_remove_all(pattern = "-"),
    minimumDepthInMeters = Depth,
    maximumDepthInMeters = Depth,
    countryCode = "US",
    locality = paste("BOEM Lease Block", Site),
    higherGeography = paste("Gulf of Mexico",
                            paste("BOEM Lease Block",
                                  Site), sep = " | ")) %>% 

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
    countryCode,
    minimumDepthInMeters,
    maximumDepthInMeters,
    locationRemarks,
    Gear
  ) %>%
# joining the event table and the table with the min/max core depths
  distinct() %>% 
  left_join(., core_minmax) %>% 
  
  mutate(
    samplingProtocol = paste(Gear, ",", maximumDistanceAboveSurfaceInMeters, "m long")
  ) %>% 
  
  select(everything(),
         -Gear)


# Sample Level Event Table -------------------------------------------------
# Here we are making an event table for the samples taken within each core, which are child events
Infauna_Sample <- Infauna %>% 

  rename(
    materialEntityID = SampleID,
    locationID = Station,
    decimalLatitude = Latitude,
    decimalLongitude = Longitude
  ) %>%
# the eventID from the previous table is now the parentEventID, and a new eventID is made for the samples
  mutate(
    locationRemarks = paste(Location, "coral"),
    geodeticDatum = "WGS84",
    eventDate = DateCollected %>% 
      as.Date("%m/%d/%Y"),
    parentEventID = paste(Site, eventDate %>% as.character(), CoreID, 
                          sep = "_") %>% stringr::str_remove_all(pattern = "-"),
    eventID = paste(Site, eventDate %>% as.character(), materialEntityID, sep = "_") %>%
      stringr::str_remove_all(pattern = "-"),
    minimumDepthInMeters = Depth,
    maximumDepthInMeters = Depth,
    countryCode = "US",
    locality = paste("BOEM Lease Block", Site),
    higherGeography = paste("Gulf of Mexico",
                            paste("BOEM Lease Block", 
                                  Site), sep = " | "),
    samplingProtocol = paste(Gear, "fraction"),
# Because we want the depths of core samples, we don't group by core 
    Fraction = str_extract(Fraction, pattern= ".*\\d"),
    maximumDistanceAboveSurfaceInMeters = str_split_i(Fraction, pattern = "-", i = 2) %>% 
      as.integer()/-100,
    minimumDistanceAboveSurfaceInMeters = str_split_i(Fraction, pattern = "-", i = 1) %>% 
      as.integer()/-100
      ) %>% 
  select(
    everything(),
    -c(Analysis,
       EnvironmentalGroup,
       TSN,
       AphiaID,
       Abundance,
       TaxaName)
  ) %>% 
  distinct()


# Bind tables and write out to csv ----------------------------------------

Infauna_Event <- bind_rows(Infauna_StationCore, Infauna_Sample) %>% 
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
    countryCode,
    minimumDepthInMeters,
    maximumDepthInMeters,
    samplingProtocol,
    locationRemarks,
    minimumDistanceAboveSurfaceInMeters,
    maximumDistanceAboveSurfaceInMeters,
    materialEntityID
  ) %>% 
  distinct() %>% 
  mutate(
    minimumDistanceAboveSurfaceInMeters = sprintf("%.2f", minimumDistanceAboveSurfaceInMeters),
    maximumDistanceAboveSurfaceInMeters = sprintf("%.2f", maximumDistanceAboveSurfaceInMeters)
  )

Infauna_Event %>% 
  write.csv(
    paste0("data/gomx_sediment_macrofauna_event_", Sys.Date(), ".csv"),
    na = "",
    fileEncoding = "UTF-8", 
    row.names = FALSE
  )
