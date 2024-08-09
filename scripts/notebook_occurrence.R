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
  
  filter(!is.na(AphiaID)) %>% 
  
  rename(
    materialEntityID = SampleID,
    locationID = Station
    
  ) %>% 
  
  mutate(
    eventDate = DateCollected %>% 
      as.Date("%m/%d/%Y"),
    eventID = paste(Site, eventDate %>% as.character(), locationID, materialEntityID,
                    sep = "_") %>% stringr::str_remove_all(pattern = "-"),
    occurrenceStatus = "present",
    basisOfRecord = "HumanObservation",
    verbatimIdentification = TaxaName,
    individualCount = Abundance,
    associatedOccurrence = Coral,
    # scientificNameID = paste("urn:lsid:marinespecies.org:taxname:", AphiaID, ", urn:lsid:itis.gov:itis_tsn::", TSN),
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

uniqueAphia <- lapply(myAphiaID, function(x) wm_record(id = x)) %>% 
  data.table::rbindlist()

uniqueAphiaSelectColumns <- select(.data = uniqueAphia,
scientificname, rank, kingdom, phylum, class, order, family, genus, lsid, AphiaID
) %>%
  rename(
    scientficName = scientificname,
    taxonRank = rank,
    scientificNameID = lsid
  )


#check separator for scientificNameID
Occurrence <- left_join(Infauna_Occurrence, uniqueAphiaSelectColumns, by = c("AphiaID" = "AphiaID")) %>% 
  mutate(
    scientificNameID = paste(scientificNameID, "urn:lsid:itis.gov:itis_tsn::", TSN),
    
  )

  
  
  
  