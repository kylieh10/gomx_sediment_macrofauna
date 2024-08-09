#this is my eMoF table :)
## there's only events here, and no occurrence-associated measurements exist

# Load libraries ----------------------------------------------------------

library(dplyr)
library(sbtools)
library(stringr)
library(tidyr)

# Read data from ScienceBase ----------------------------------------------
# link to data: https://www.sciencebase.gov/catalog/item/5a709594e4b0a9a2e9d88e4e

sb_id <- '5a709594e4b0a9a2e9d88e4e'
sbtools::item_get(sb_id = sb_id)

sb_filenames <- item_list_files(sb_id = sb_id)

BTA <-readr::read_csv(file = sb_filenames$url[1])
Infauna <- readr::read_csv(file = sb_filenames$url[2])
SedChem <- readr::read_csv(file = sb_filenames$url[3])

# eMoF table ------------------
       
       
Infauna_emof <- Infauna %>% 
  bind_rows(SedChem) %>%
  
  rename(
    materialEntityID = SampleID,
    locationID = Station
  ) %>% 
  
  mutate(
    eventDate = DateCollected %>%
      as.Date("%m/%d/%Y"),
    #edit eventID, don't need station info (and make sure this works for sedchem cores)
    #check NERC names
    eventID = paste(Site, eventDate %>% as.character(), locationID, materialEntityID, sep = "_") %>%
      stringr::str_remove_all(pattern = "-"),
    maximumCoreDepth = str_split_i(Fraction, pattern = "-", i = 2) %>% readr::parse_number() %>% 
      as.character(),
    minimumCoreDepth = str_split_i(Fraction, pattern = "-", i = 1) %>% readr::parse_number() %>% 
      as.character(),
    "proportionSand(63-2000um)" = as.character(Sand),
    "proportionMud(<63um)" = as.character(Mud),
    "proportionGravel(>2000um)" = as.character(Gravel),
    depthBelowSurface = as.character(Depth),
    CoreWidth = as.character(CoreDiameter)
  ) %>% 
  
  pivot_longer(
    cols = c("minimumCoreDepth", "maximumCoreDepth", "CoreWidth", "depthBelowSurface",
             "proportionSand(63-2000um)", "proportionMud(<63um)", "proportionGravel(>2000um)"
             ),
    names_to = "measurementType",
    values_to = "measurementValue",
    values_drop_na = TRUE
  ) %>% 
  
  mutate(
    measurementTypeID = case_when(measurementType == "CoreWidth" ~ "http://vocab.nerc.ac.uk/collection/P01/current/COREWDTH/",
                                  measurementType == "maximumCoreDepth" ~ "http://vocab.nerc.ac.uk/collection/P01/current/MAXCDIST/",
                                  measurementType == "minimumCoreDepth" ~ "http://vocab.nerc.ac.uk/collection/P01/current/MINCDIST/",
                                  measurementType == "depthBelowSurface" ~ "http://vocab.nerc.ac.uk/collection/P01/current/ADEPZZ01/",
                                  measurementType == "proportionSand(63-2000um)" ~ "http://vocab.nerc.ac.uk/collection/P01/current/PRPCL064/",
                                  measurementType == "proportionMud(<63um)" ~ "http://vocab.nerc.ac.uk/collection/P01/current/PRPCL088/"
                                  #measurementType == "proportionGravel(>2000um)" ~ "LINK"
    ),
    measurementUnit = case_when(measurementType == "CoreWidth" ~ "cm",
                                measurementType == "minimumCoreDepth" ~ "cm",
                                measurementType == "maximumCoreDepth" ~ "cm",
                                measurementType == "depthBelowSurface" ~ "m",
                                measurementType == "proportionSand(63-2000um)" ~ "percent",
                                measurementType == "proportionMud(<63um)" ~ "percent"
                                #measurementType == "proportionGravel(>2000um)" ~ "percent"
    ),
    measurementUnitID = case_when(measurementType == "CoreWidth" ~ "http://vocab.nerc.ac.uk/collection/P06/current/ULCM/",
                                  measurementType == "minimumCoreDepth" ~ "http://vocab.nerc.ac.uk/collection/P06/current/ULCM/",
                                  measurementType == "maximumCoreDepth" ~ "http://vocab.nerc.ac.uk/collection/P06/current/ULCM/",
                                  measurementType == "depthBelowSurface"~ "http://vocab.nerc.ac.uk/collection/P06/current/ULAA/",
                                  measurementType == "proportionSand(63-2000um)" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UPCT/",
                                  measurementType == "proportionMud(<63um)" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UPCT/"
                                  #measurementType == "proportionGravel(>2000um)" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UPCT/"
    )
  ) %>% 
  
  select(
    eventID,
    measurementType,
    measurementTypeID,
    measurementValue,
    measurementUnit,
    measurementUnitID
  ) %>% 
  
  distinct()
                                  
   

                                 
                                