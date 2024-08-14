# starting the occurrence extension table

# Load libraries ----------------------------------------------------------

library(dplyr)
library(sbtools)
library(stringr)
library(worrms)

# Read data from ScienceBase ----------------------------------------------
# link to data: https://www.sciencebase.gov/catalog/item/5a709594e4b0a9a2e9d88e4e

sb_id <- '5a709594e4b0a9a2e9d88e4e'
sbtools::item_get(sb_id = sb_id)

sb_filenames <- item_list_files(sb_id = sb_id)

BTA <-readr::read_csv(file = sb_filenames$url[1])
Infauna <- readr::read_csv(file = sb_filenames$url[2])
SedChem <- readr::read_csv(file = sb_filenames$url[3])

# Occurrence Table ----------  

Infauna_Occurrence <- Infauna %>% 
  
  rename(
    materialEntityID = SampleID,
    locationID = Station
    
  ) %>% 
  
  mutate(
    eventDate = DateCollected %>% 
      as.Date("%m/%d/%Y"),
    eventID = paste(Site, eventDate %>% as.character(), materialEntityID,
                    sep = "_") %>% str_remove_all(pattern = "-"),
    # occurrenceStatus = "present",
    occurrenceStatus = if_else(TaxaName == "No individuals", "absent", "present"),
    basisOfRecord = "HumanObservation",
    verbatimIdentification = TaxaName,
    individualCount = Abundance,
    associatedOccurrence = Coral,
    taxonRank = NA
  ) %>% 
  
  group_by(materialEntityID) %>% 
    mutate(
       occurrenceID = paste(materialEntityID, row_number(), sep = "_")
       #find alternative to row number here
      ) %>% 
  ungroup() %>% 
  
select(
  eventID,
  occurrenceID,
  eventDate,
  verbatimIdentification,
  occurrenceStatus,
  basisOfRecord,
  individualCount,
  associatedOccurrence,
  AphiaID,
  TSN
)

myAphiaID <- Infauna$AphiaID %>% na.omit() %>% unique()

myAphiaID <- lapply(myAphiaID, function(x) wm_record(id = x)) %>% 
  data.table::rbindlist()

uniqueAphiaSelectColumns <- select(.data = myAphiaID,
scientificname, rank, kingdom, phylum, class, order, family, genus, lsid, AphiaID
) %>%
  rename(
    scientficName = scientificname,
    taxonRank = rank,
    scientificNameID = lsid
  )


# in this case, TSN was provided, however it is not required to have both AphiaID and LSID
Occurrence_Ext <- left_join(Infauna_Occurrence, uniqueAphiaSelectColumns, by = c("AphiaID" = "AphiaID")) %>% 
  mutate(
    TSN = paste("urn:lsid:itis.gov:itis_tsn:", TSN),
    scientificNameID = paste(scientificNameID, TSN, sep = ", ")
  )


  
  
  
  