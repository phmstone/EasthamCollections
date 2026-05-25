# Filterine the Eastham collections from GBIF
# Make sure that everything was collected by Eastham

# Not of all these libraries are needed
# Need to go through and check which ones can be removed here

#################### Load libraries ######################

library(tidyverse)


###################### Load data ########################

eastham_records <- readRDS("jw_eastham_gbif_specimens.rds")


###################### Filter out by date ########################

# Eastham moved to Canada in 1906 and died in 1968
eastham_dates <- eastham_records %>% 
  filter(year>=1906) %>% 
  filter(year<1968)


###################### Filter out by Kingdom ########################

# not interested in his fungal collections at the moment

eastham_plants <- eastham_dates %>% 
  filter(kingdom == "Plantae")


###################### Geographical checks ########################

# change provinces for things listed wrong
# both specimens labelled Ontario are from BC
eastham_plants <- eastham_plants %>% 
  mutate( stateProvince = if_else(
    stateProvince == "Ontario",
    "British Columbia", stateProvince))

# some specimens wrongly listed as in Washington
WashingtonIDs <- c(1988173304, 5218269244)
eastham_plants <- eastham_plants %>% 
  mutate(
    stateProvince = if_else(
      gbifID %in% WashingtonIDs & stateProvince == "Washington",
      "British Columbia",
      stateProvince))

# some specimens wrongly listed as in Montana
MontanaIDs <- c(2575079174)
eastham_plants <- eastham_plants %>% 
  mutate(
    stateProvince = if_else(
      gbifID %in% MontanaIDs & stateProvince == "Montana",
      "British Columbia",
      stateProvince))

# specimen labelled as California is from Washington
eastham_plants <- eastham_plants %>% 
  mutate( stateProvince = if_else(
    stateProvince == "California",
    "Washington", stateProvince))

# specimen labelled as Maine is from BC
eastham_plants <- eastham_plants %>% 
  mutate( stateProvince = if_else(
    stateProvince == "Maine (State)",
    "British Columbia", stateProvince))

# specimen labelled as Alaska is from BC
eastham_plants <- eastham_plants %>% 
  mutate( stateProvince = if_else(
    stateProvince == "Alaska (State)",
    "British Columbia", stateProvince))

# remove coordinates for things with lat/long at 0.0000
eastham_plants <- eastham_plants %>% 
  mutate(
    decimalLatitude  = if_else(decimalLatitude  == 0, NA_real_, decimalLatitude),
    decimalLongitude = if_else(decimalLongitude == 0, NA_real_, decimalLongitude))

# remove individual records that seem off
# based on coordinates/province
# removing collections from high latitudes collected by "John Eastham"
IDsToRemove <- c(4958342356, 1988239527, 4958342473, 1988225777, 1988183312, 1988122223, 1843223268, 1804517714, 1949522993, 1949522141, 1949466404, 1949477647, 1949477621, 1949477745, 1949466986)
records_removed <- eastham_plants %>%
  filter(gbifID %in% IDsToRemove)
eastham_plants <- eastham_plants %>%
  filter(!gbifID %in% IDsToRemove)


###################### Geographical renaming ########################

# rename all of the different versions of BC to British Columbia (easier down the line)
eastham_plants <- eastham_plants %>%
  mutate(
    stateProvince = case_when(
      str_detect(stateProvince, regex(
        "B\\.C\\.|British Columbia( \\(Prov\\.\\))?|British Columbia / Colombie Britanique|Nelson|Vancouver Island",
        ignore_case = TRUE
      )) ~ "British Columbia",
      TRUE ~ stateProvince
    )
  )


###################### Save plant data ########################

saveRDS(eastham_plants, "Eastham_plants.rds")



#################### Save fungal data in case I want it later ###############

eastham_fungi <- eastham_dates %>% 
  filter(kingdom == "Fungi")

saveRDS(eastham_fungi, "Eastham_fungi.rds")



###################### basic herbarium stats ########################

eastham_herbaria <- eastham_plants %>% 
  count(collectionCode)



###################### basic fungarium stats ########################

eastham_fungarium <- eastham_fungi %>% 
  count(collectionCode)



###################### basic geographical stats ########################

# for plants
eastham_province <- eastham_plants %>% 
  count(stateProvince)

# basically all from BC
# small number from neighbouring areas
# singletons from other places could be collected by other people?
# maybe should filter out more stringently based on geographic location





