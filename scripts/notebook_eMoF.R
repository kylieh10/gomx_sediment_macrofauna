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
## I commented out sedchem components until i figure out how to incorporate the table better

Infauna_emof <- Infauna %>% 
  # bind_rows(SedChem) %>% 
  
  rename(
    materialEntityID = SampleID,
    locationID = Station
  ) %>% 
  
  mutate(
    eventDate = DateCollected %>% 
      as.Date("%m/%d/%Y"),
    eventID = paste(Site, eventDate %>% as.character(), locationID, materialEntityID,
                    sep = "_") %>% stringr::str_remove_all(pattern = "-"),
    maximumCoreDepth = str_split_i(Fraction, pattern = "-", i = 2) %>% readr::parse_number() %>% 
      as.character(),
    minimumCoreDepth = str_split_i(Fraction, pattern = "-", i = 1) %>% readr::parse_number() %>% 
      as.character(),
    # "proportionSand(63-2000um)" = as.character(Sand),
    # "proportionMud(<63um)" = as.character(Mud),
    # "proportionGravel(>2000um)" = as.character(Gravel),
    depthBelowSurface = as.character(Depth),
    CoreWidth = as.character(CoreDiameter)
  ) %>% 
  
  pivot_longer(
    cols = c("minimumCoreDepth", "maximumCoreDepth", "CoreWidth", "depthBelowSurface",
             # "proportionSand(63-2000um)", "proportionMud(<63um)", "proportionGravel(>2000um)"
             ),
    names_to = "measurementType",
    values_to = "measurementValue",
    values_drop_na = TRUE
  ) %>% 
  
  #omg omg omg, i can't believe this worked (it took me way too long and too many tries to come to this conclusion)
  mutate(
    measurementTypeID = ifelse (str_detect(measurementType, "CoreWidth"), "http://vocab.nerc.ac.uk/collection/P01/current/COREWDTH/", NA), 
    measurementUnit = ifelse(str_detect(measurementType, "CoreWidth|maximumCoreDepth|minimumCoreDepth"), "cm", NA),
    measurementUnitID = ifelse(str_detect(measurementType, "CoreWidth"), "http://vocab.nerc.ac.uk/collection/P06/current/ULCM/", NA),
  ) %>% 
  
   select(
    eventID,
    measurementType,
    measurementValue,
    measurementTypeID,
    measurementUnit,
    measurementUnitID)  

#mapping: corediameter -> CoreWdth http://vocab.nerc.ac.uk/collection/P01/current/COREWDTH/
  ## cm: ULCM http://vocab.nerc.ac.uk/collection/P06/current/ULCM/
#fraction -> maximum depth relative to bed surface (from the bottom of the core) http://vocab.nerc.ac.uk/collection/P01/current/MAXCDIST/;
  #minimum depth relative to bed surface (from the top of the core) http://vocab.nerc.ac.uk/collection/P01/current/MINCDIST/
  ## cm: ULCM http://vocab.nerc.ac.uk/collection/P06/current/ULCM/
#sedimentcomp -> no term for proportion of >2000 micrometers (gravel), just 2000 - crazy high um; 
  # proportion of mud (<63um): http://vocab.nerc.ac.uk/collection/P01/current/PRPCL088/;
  # proportion of sand (63-2000um):http://vocab.nerc.ac.uk/collection/P01/current/PRPCL064/
  ## percent: http://vocab.nerc.ac.uk/collection/P06/current/UPCT/
#depth (have to include bc of sedchem table depths):http://vocab.nerc.ac.uk/collection/P01/current/ADEPZZ01/
  ## metres: http://vocab.nerc.ac.uk/collection/P06/current/ULAA/
