
# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyr)
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


# Revise AphiaID ----------------------------------------------------------

# Manually add AphiaID for Caridea
# Although NA in original data, it exists in WoRMS as an infraorder

Infauna <- Infauna %>% 
  mutate(AphiaID = case_when(TaxaName == "Caridea" ~ 106674,
                             TRUE ~ AphiaID)
  )


# Occurrence Table ----------  

Infauna_Occurrence <- Infauna %>% 
  
  rename(
    materialEntityID = SampleID
  ) %>% 
  # We have to filter out samples with no individuals since they don't have any occurrences
  filter(
    !TaxaName == "No individuals"
  ) %>% 
  # For eventID, I used the same combination of variables that's in `notebook_event`
  mutate(
    eventDate = DateCollected %>% 
      as.Date("%m/%d/%Y"),
    eventID = paste(Site, eventDate %>% as.character(), materialEntityID,
                    sep = "_") %>% str_remove_all(pattern = "-"),
    occurrenceStatus = "present",
    basisOfRecord = "HumanObservation",
    verbatimIdentification = TaxaName,
    individualCount = Abundance,
    associatedTaxa = paste("livesNear:", Coral),
    taxonRank = NA,
    locality = paste("BOEM Lease Block", Site),
    higherGeography = paste("Gulf of Mexico",
                            paste("BOEM Lease Block",
                                  Site), sep = " | "),
    occurrenceRemarks = case_when(Location == "Near" ~ paste("within 1 meter of", Coral),
                                  Location == "Background" ~ paste("14 to 1000 meters away from", Coral)
    )
  ) %>% 
# Here we group by materialEntityID to allow us to use row numbers to more easily create unique occurrenceIDs
# In this scenario, we can use row numbers because this is a static dataset, however this is likely not appropriate for continuously growing datasets 
  group_by(materialEntityID) %>% 
    mutate(
       occurrenceID = paste(materialEntityID, row_number(), sep = "_")
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
  associatedTaxa,
  occurrenceRemarks,
  AphiaID,
  TSN,
  locality,
  higherGeography
)


# Pulling AphiaIDs and adding them to the main table ----------------------

# Here we pull AphiaIDs from WoRMS, using `wm_records` then `lapply` to circumvent limitations on the number of input values
myAphiaID <- Infauna$AphiaID %>% na.omit() %>% unique()

myAphiaID <- lapply(myAphiaID, function(x) wm_record(id = x)) %>% 
  data.table::rbindlist()

uniqueAphiaSelectColumns <- select(.data = myAphiaID,
scientificname, rank, kingdom, phylum, class, order, family, genus, lsid, AphiaID
) %>%
  rename(
    scientificName = scientificname,
    taxonRank = rank,
    scientificNameID = lsid
  )

# Joining the AphiaID and taxanomic table to our occurrence table by the common term "AphiaID"
Occurrence_Ext <- left_join(Infauna_Occurrence, uniqueAphiaSelectColumns, by = c("AphiaID" = "AphiaID")) %>%
# We tell it to only add the TSN to the scientificNameID if there is a TSN provided
  mutate(
    TSN = ifelse(is.na(TSN), NA, paste0("urn:lsid:itis.gov:itis_tsn:", TSN)),
    countryCode = "US"
  ) %>% 
  unite(., col = "scientificNameID", scientificNameID, TSN, na.rm = TRUE, sep = ", ") %>% 
  subset(select = -c(AphiaID)) %>%
  select(eventID,
         occurrenceID,
         eventDate,
         scientificName,
         scientificNameID,
         everything())


# Exporting the table as a .csv to upload to the IPT ----------------------

Occurrence_Ext %>% 
  write.csv(paste0("data/gomx_sediment_macrofauna_occurrence_", Sys.Date(), ".csv"),
            na = "",
            fileEncoding = "UTF-8", 
            row.names = FALSE
  )
 
  
  
  