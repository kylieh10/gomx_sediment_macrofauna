
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

# Wrangle SedChem
SedChem <- SedChem %>% 
  rename(
    SampleID = CoreID
  ) %>% 
  mutate(
    CoreDiameter = 6.35
  )

# Combine Infauna and SedChem  
Infauna_emof <- Infauna %>% 
  bind_rows(SedChem) %>%
  rename(
    materialEntityID = SampleID,
  ) %>% 
  
  # Wrangle SedChem into standardized terms that can be found in the NERC Vocabulary Server
  mutate(
    eventDate = DateCollected %>%
      as.Date("%m/%d/%Y"),
    eventID = paste(Site, eventDate %>% as.character(), materialEntityID, sep = "_") %>%
      stringr::str_remove_all(pattern = "-"),
    "Depth (spatial coordinate) maximum relative to bed surface in the bed" = str_split_i(Fraction, pattern = "-", i = 2) %>% readr::parse_number() %>% 
      as.character(),
    "Depth (spatial coordinate) minimum relative to bed surface in the bed" = str_split_i(Fraction, pattern = "-", i = 1) %>% readr::parse_number() %>% 
      as.character(),
    "Proportion by dry weight of particles (63-2000um) in the sediment" = as.character(Sand),
    "Proportion by dry weight of particles (<63um) in the sediment" = as.character(Mud),
    "Proportion by dry weight of particles (>2000um) in the sediment" = as.character(Gravel),
    "Thickness (transverse) of core" = as.character(round(CoreDiameter, digits = 2))
  ) %>% 
  
  # Change from wide to long format
  pivot_longer(
    cols = c("Thickness (transverse) of core", 
             "Depth (spatial coordinate) minimum relative to bed surface in the bed", 
             "Depth (spatial coordinate) maximum relative to bed surface in the bed", 
             "Proportion by dry weight of particles (63-2000um) in the sediment", 
             "Proportion by dry weight of particles (<63um) in the sediment", 
             "Proportion by dry weight of particles (>2000um) in the sediment"
             ),
    names_to = "measurementType",
    values_to = "measurementValue",
    values_drop_na = TRUE
  ) %>% 
  
  # Wrangle DwC terms to align with eMoF structure
  mutate(
    measurementTypeID = case_when(measurementType == "Thickness (transverse) of core" ~ "http://vocab.nerc.ac.uk/collection/P01/current/COREWDTH/",
                                  measurementType == "Depth (spatial coordinate) maximum relative to bed surface in the bed" ~ "http://vocab.nerc.ac.uk/collection/P01/current/MAXCDIST/",
                                  measurementType == "Depth (spatial coordinate) minimum relative to bed surface in the bed" ~ "http://vocab.nerc.ac.uk/collection/P01/current/MINCDIST/",
                                  measurementType == "Proportion by dry weight of particles (63-2000um) in the sediment" ~ "http://vocab.nerc.ac.uk/collection/P01/current/PRPCL048/",
                                  measurementType == "Proportion by dry weight of particles (<63um) in the sediment" ~ "http://vocab.nerc.ac.uk/collection/P01/current/PRSD0249/",
                                  measurementType == "Proportion by dry weight of particles (>2000um) in the sediment" ~ "	http://vocab.nerc.ac.uk/collection/P01/current/PRSD0274/"
    ),
    measurementUnit = case_when(measurementType == "Thickness (transverse) of core" ~ "Centimetres",
                                measurementType == "Depth (spatial coordinate) minimum relative to bed surface in the bed" ~ "Centimetres",
                                measurementType == "Depth (spatial coordinate) maximum relative to bed surface in the bed" ~ "Centimetres",
                                measurementType == "Proportion by dry weight of particles (63-2000um) in the sediment" ~ "Percent",
                                measurementType == "Proportion by dry weight of particles (<63um) in the sediment" ~ "Percent",
                                measurementType == "Proportion by dry weight of particles (>2000um) in the sediment" ~ "Percent"
    ),
    measurementUnitID = case_when(measurementType == "Thickness (transverse) of core" ~ "http://vocab.nerc.ac.uk/collection/P06/current/ULCM/",
                                  measurementType == "Depth (spatial coordinate) minimum relative to bed surface in the bed" ~ "http://vocab.nerc.ac.uk/collection/P06/current/ULCM/",
                                  measurementType == "Depth (spatial coordinate) maximum relative to bed surface in the bed" ~ "http://vocab.nerc.ac.uk/collection/P06/current/ULCM/",
                                  measurementType == "Proportion by dry weight of particles (63-2000um) in the sediment" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UPCT/",
                                  measurementType == "Proportion by dry weight of particles (<63um) in the sediment" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UPCT/",
                                  measurementType == "Proportion by dry weight of particles (>2000um) in the sediment" ~ "http://vocab.nerc.ac.uk/collection/P06/current/UPCT/"
    ), 
    measurementRemarks = case_when(measurementType == 'Thickness (transverse) of core' ~ paste("Verbatim Label:", "CoreDiameter"),
                                   measurementType == "Depth (spatial coordinate) minimum relative to bed surface in the bed" ~ paste("Verbatim Label:", 'lower value of "Fraction'),
                                   measurementType == 'Depth (spatial coordinate) maximum relative to bed surface in the bed' ~ paste("Verbatim Label:", 'upper value of "Fraction'),
                                   measurementType == 'Proportion by dry weight of particles (63-2000um) in the sediment' ~ paste("Verbatim Label:", 'Sand'),
                                   measurementType == 'Proportion by dry weight of particles (<63um) in the sediment' ~ paste("Verbatim Label:", 'Mud'),
                                   measurementType == "Proportion by dry weight of particles (>2000um) in the sediment" ~ paste("Verbatim Label:", "Gravel")
    )
  ) %>% 
  
  # Select columns we want included in the output
  select(
    eventID,
    measurementType,
    measurementTypeID,
    measurementValue,
    measurementUnit,
    measurementUnitID,
    measurementRemarks
  ) %>% 
  
  distinct()


# Exporting the table as a .csv to upload to the IPT ----------------------

# checks if data directory exists and if not, creates it
if(!dir.exists("../data")){
  
  dir.create("data")
}

# exports the table
Infauna_emof %>% 
  write.csv(
    paste0(here::here("data", "gomx_sediment_macrofauna_emof_"), Sys.Date(), ".csv"),
    na = "",
    fileEncoding = "UTF-8", 
    row.names = FALSE
  )       
                                