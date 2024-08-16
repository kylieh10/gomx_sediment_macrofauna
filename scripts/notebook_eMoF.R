
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

SedChem <- SedChem %>% 
  mutate(
    SampleID = CoreID
  )
       
Infauna_emof <- Infauna %>% 
  bind_rows(SedChem) %>%
  
  rename(
    materialEntityID = SampleID,
  ) %>% 
  
  mutate(
    eventDate = DateCollected %>%
      as.Date("%m/%d/%Y"),
    eventID = paste(Site, eventDate %>% as.character(), materialEntityID, sep = "_") %>%
      stringr::str_remove_all(pattern = "-"),
    MAXCDIST = str_split_i(Fraction, pattern = "-", i = 2) %>% readr::parse_number() %>% 
      as.character(),
    MINCDIST = str_split_i(Fraction, pattern = "-", i = 1) %>% readr::parse_number() %>% 
      as.character(),
    #proportion sand (63-2000um)
    "PRPCL064" = as.character(Sand),
   #proportion mud (<63um)
    "PRPCL088" = as.character(Mud),
    "proportionGravel(>2000um)" = as.character(Gravel),
    COREWDTH = as.character(CoreDiameter)
  ) %>% 
  
  pivot_longer(
    cols = c("COREWDTH", "MINCDIST", "MAXCDIST", "PRPCL064", "PRPCL088", "proportionGravel(>2000um)"
             ),
    names_to = "measurementType",
    values_to = "measurementValue",
    values_drop_na = TRUE
  ) %>% 
  
  mutate(
    measurementTypeID = case_when(measurementType == "COREWDTH" ~ "http://vocab.nerc.ac.uk/collection/P01/current/COREWDTH/",
                                  measurementType == "MAXCDIST" ~ "http://vocab.nerc.ac.uk/collection/P01/current/MAXCDIST/",
                                  measurementType == "MINCDIST" ~ "http://vocab.nerc.ac.uk/collection/P01/current/MINCDIST/",
                                  measurementType == "PRPCL064" ~ "http://vocab.nerc.ac.uk/collection/P01/current/PRPCL064/",
                                  measurementType == "PRPCL088" ~ "http://vocab.nerc.ac.uk/collection/P01/current/PRPCL088/"
                                  #measurementType == "proportionGravel(>2000um)" ~ "LINK"
    ),
    measurementUnit = case_when(measurementType == "COREWDTH" ~ "ULCM",
                                measurementType == "MINCDIST" ~ "ULCM",
                                measurementType == "MAXCDIST" ~ "ULCM",
                                measurementType == "PRPCL064" ~ "UPCT",
                                measurementType == "PRPCL088" ~ "UPCT"
                                #measurementType == "proportionGravel(>2000um)" ~ "UPCT"
    ),
    measurementUnitID = case_when(measurementType == "COREWDTH" ~ "http://vocab.nerc.ac.uk/collection/P06/current/ULCM/",
                                  measurementType == "MINCDIST" ~ "http://vocab.nerc.ac.uk/collection/P06/current/ULCM/",
                                  measurementType == "MAXCDIST" ~ "http://vocab.nerc.ac.uk/collection/P06/current/ULCM/",
                                  measurementType == "PRPCL064" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UPCT/",
                                  measurementType == "PRPCL088" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UPCT/"
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
  
  distinct() %>% 
  slice(1:6, 899:908) %>% 
  readr::write_csv(paste0("gomx_sediment_macrofauna_emof_", Sys.Date(), ".csv"), na = "NA")   
                                 
                                