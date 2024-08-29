#QAQC data before publishing to IPT

library(dplyr)
library(obistools)

event <- readr::read_csv("data/gomx_sediment_macrofauna_event_2024-08-28.csv")
occ <- readr::read_csv("data/gomx_sediment_macrofauna_occurrence_2024-08-28.csv")
emof <- readr::read_csv("data/gomx_sediment_macrofauna_emof_2024-08-28.csv")


# Check events ------------------------------------------------------------

event %>% 
  pull(eventID) %>%
  unique() %>% 
  length() == nrow(event)

event %>% 
  select(-eventID, -parentEventID) %>% 
  distinct() %>% 
  nrow()

event %>% 
  group_by(eventID) %>% 
  count() %>% 
  filter(n > 1) %>% 
  arrange(desc(n))


# Check occurrences ------------------------

occ %>% 
  pull(occurrenceID) %>% 
  unique() %>% 
  length() == nrow(occ)
#True :)


obistools::plot_map_leaflet(event)
