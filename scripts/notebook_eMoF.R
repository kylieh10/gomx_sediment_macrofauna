
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
    "Depth (spatial coordinate) maximum relative to bed surface in the bed" = str_split_i(Fraction, pattern = "-", i = 2) %>% readr::parse_number() %>% 
      as.character(),
    "Depth (spatial coordinate) minimum relative to bed surface in the bed" = str_split_i(Fraction, pattern = "-", i = 1) %>% readr::parse_number() %>% 
      as.character(),
    #proportion sand (63-2000um)
    "Proportion by volume of particles (63-2000um) in the sediment" = as.character(Sand),
   #proportion mud (<63um)
    "Proportion by volume of particles (0-63um) in the sediment" = as.character(Mud),
    "proportionGravel(>2000um)" = as.character(Gravel),
   "Thickness (transverse) of core" = as.character(CoreDiameter)
  ) %>% 
  
  pivot_longer(
    cols = c("Thickness (transverse) of core", "Depth (spatial coordinate) minimum relative to bed surface in the bed", "Depth (spatial coordinate) maximum relative to bed surface in the bed", "Proportion by volume of particles (63-2000um) in the sediment", "Proportion by volume of particles (0-63um) in the sediment", "proportionGravel(>2000um)"
             ),
    names_to = "measurementType",
    values_to = "measurementValue",
    values_drop_na = TRUE
  ) %>% 
  
  mutate(
    measurementTypeID = case_when(measurementType == "Thickness (transverse) of core" ~ "http://vocab.nerc.ac.uk/collection/P01/current/COREWDTH/",
                                  measurementType == "Depth (spatial coordinate) maximum relative to bed surface in the bed" ~ "http://vocab.nerc.ac.uk/collection/P01/current/MAXCDIST/",
                                  measurementType == "Depth (spatial coordinate) minimum relative to bed surface in the bed" ~ "http://vocab.nerc.ac.uk/collection/P01/current/MINCDIST/",
                                  measurementType == "Proportion by volume of particles (63-2000um) in the sediment" ~ "http://vocab.nerc.ac.uk/collection/P01/current/PRPCL064/",
                                  measurementType == "Proportion by volume of particles (0-63um) in the sediment" ~ "http://vocab.nerc.ac.uk/collection/P01/current/PRPCL088/"
                                  #measurementType == "proportionGravel(>2000um)" ~ "LINK"
    ),
    measurementUnit = case_when(measurementType == "Thickness (transverse) of core" ~ "ULCM",
                                measurementType == "Depth (spatial coordinate) minimum relative to bed surface in the bed" ~ "ULCM",
                                measurementType == "Depth (spatial coordinate) maximum relative to bed surface in the bed" ~ "ULCM",
                                measurementType == "Proportion by volume of particles (63-2000um) in the sediment" ~ "UPCT",
                                measurementType == "Proportion by volume of particles (0-63um) in the sediment" ~ "UPCT"
                                #measurementType == "proportionGravel(>2000um)" ~ "UPCT"
    ),
    measurementUnitID = case_when(measurementType == "Thickness (transverse) of core" ~ "http://vocab.nerc.ac.uk/collection/P06/current/ULCM/",
                                  measurementType == "Depth (spatial coordinate) minimum relative to bed surface in the bed" ~ "http://vocab.nerc.ac.uk/collection/P06/current/ULCM/",
                                  measurementType == "Depth (spatial coordinate) maximum relative to bed surface in the bed" ~ "http://vocab.nerc.ac.uk/collection/P06/current/ULCM/",
                                  measurementType == "Proportion by volume of particles (63-2000um) in the sediment" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UPCT/",
                                  measurementType == "Proportion by volume of particles (0-63um) in the sediment" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UPCT/"
                                  #measurementType == "proportionGravel(>2000um)" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UPCT/"
    ), 
    measurementRemarks = case_when(measurementType == 'Thickness (transverse) of core' ~ '"CoreDiameter"',
                                   measurementType == "Depth (spatial coordinate) minimum relative to bed surface in the bed" ~ 'lower value of "Fraction"',
                                   measurementType == 'Depth (spatial coordinate) maximum relative to bed surface in the bed' ~ 'upper value of "Fraction"',
                                   measurementType == 'Proportion by volume of particles (63-2000um) in the sediment' ~ '"Sand"',
                                   measurementType == 'Proportion by volume of particles (0-63um) in the sediment' ~ '"Mud"'
                                   # measurementType == 'proportionGravel(>2000um)' ~ '"Gravel"')
    )
  ) %>% 
  
  select(
    eventID,
    measurementType,
    measurementTypeID,
    measurementValue,
    measurementUnit,
    measurementUnitID,
    measurementRemarks
  ) %>% 
  
  distinct() %>% 
  slice(1:6, 899:908) %>% 
  readr::write_csv(paste0("gomx_sediment_macrofauna_emof_", Sys.Date(), ".csv"), na = "NA")   
                                 
                                