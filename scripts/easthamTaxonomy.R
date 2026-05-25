###########################################################################################################################
# Script: Using VasCan to figure out taxonomic breadth and species status of Eastham's collections
###########################################################################################################################

#################### Load libraries ######################

library(tidyverse)
library(rgbif)
library(taxize)
library(rnaturalearth)


###################### Load data ########################

gbif_all <- readRDS("gbif_NA_all_records.rds")
eastham_records <- readRDS("Eastham_plants.rds")
vascan_species_data <- readRDS("VasCanSpecies.rds")
eastham_first_canada <- readRDS("Eastham_first_to_Canada.rds")
eastham_first_bc <- readRDS("Eastham_first_to_BC.rds")

############################################################
# 9. Taxonomic breadth of Eastham's collections
############################################################


# Step 1: Classify both Eastham's records and GBIF dataset by plant group
classify_plant_groups <- function(df) {
  df %>%
    mutate(
      plant_group = case_when(
        class %in% c("Magnoliopsida") ~ "Eudicots",
        class %in% c("Liliopsida")    ~ "Monocots",
        class %in% c("Polypodiopsida", "Psilotopsida", "Equisetopsida", "Lycopodiopsida") ~ "Monilophytes",  # include other monilophytes if present
        class %in% c("Pinopsida", "Gnetopsida", "Cycadopsida") ~ "Gymnosperms",
        TRUE ~ "Other angiosperms"  # covers rare basal angiosperms
      )
    )
}



# Classify Eastham's records
eastham_records <- classify_plant_groups(eastham_records)

# Classify GBIF dataset (all specimens)
gbif_all <- classify_plant_groups(gbif_all)

# Step 2: Summarize taxonomic diversity for both datasets

# Function to summarize diversity by plant group
summarize_taxonomic_diversity <- function(df) {
  df %>%
    filter(!is.na(plant_group)) %>%
    group_by(plant_group) %>%
    summarise(
      total_specimens = n(),                        # Count total specimens
      unique_families = n_distinct(family),         # Count unique families
      unique_genera = n_distinct(genus),             # Count unique genera
      unique_species = n_distinct(speciesKey)      # Count unique species
    ) %>%
    ungroup()
}

# Calculate taxonomic diversity for Eastham's records
eastham_taxonomic_summary <- summarize_taxonomic_diversity(eastham_records)

# Calculate taxonomic diversity for GBIF dataset (all specimens)
gbif_taxonomic_summary <- summarize_taxonomic_diversity(gbif_all)

# Step 3: Calculate the percentage of each plant group that Eastham collected relative to the total in GBIF

# Merge Eastham's summary with GBIF summary to calculate percentages
taxonomic_comparison <- eastham_taxonomic_summary %>%
  left_join(gbif_taxonomic_summary, by = "plant_group", suffix = c("_eastham", "_gbif")) %>%
  mutate(
    percentage_specimens = (total_specimens_eastham / total_specimens_gbif) * 100,
    percentage_species = (unique_species_eastham / unique_species_gbif) * 100,
    percentage_families = (unique_families_eastham / unique_families_gbif) * 100,
    percentage_genera = (unique_genera_eastham / unique_genera_gbif) * 100
  )

# Step 4: Print the comparison between Eastham's collections and GBIF dataset
print(taxonomic_comparison)


# ----------------------------------------------------------
# 5. Getting taxonomic summary ready for output
# ----------------------------------------------------------

# re order
eastham_taxonomic_summary <- eastham_taxonomic_summary %>%
  slice(match(c("Monilophytes", "Gymnosperms", "Monocots", "Eudicots"), plant_group)) %>%
  dplyr::select(plant_group, unique_families:unique_species, total_specimens)

# save the file
write_csv(eastham_taxonomic_summary, "eastham_taxonomic_summary.csv")


# ----------------------------------------------------------
# 5.1 heavy hitting families
# ----------------------------------------------------------

# Which families did Eastham collect the most
EasthamTopFamilies <- eastham_records %>% 
  count(family, sort = TRUE)

# save the file
write_csv(EasthamTopFamilies, "EasthamTopFamilies.csv")


############################################################
##################### VASCAN section #######################
############################################################


# ----------------------------------------------------------
# 6. Basic VASCAN stuff
# ----------------------------------------------------------

# How many of Eastham's collections were of what status in BC (invasive, ephemeral, etc.)


# need to remove things that don'e have a species name (e.g. only the genus is there)


# Create 'eastham_species_BC' with unique rows for each species but ONLY if found in BC
eastham_species_BC <- eastham_records %>%
  filter(stateProvince == "British Columbia") %>%  # Filter for British Columbia
  dplyr::select(kingdom, phylum, class, order, family, genus, species, scientificName, speciesKey, decimalLatitude, decimalLongitude) %>%  # select relevant columns
  filter(!is.na(species), species != "") %>%
  distinct(species, .keep_all = TRUE)  # Keep one row per unique 'species'


# Initial VASCAN join for temperature check
eastham_raw_join <- eastham_species_BC %>%
  left_join(vascan_species_data,
            by = c("species" = "Scientific name"))
raw_unmatched <- eastham_raw_join %>%
  filter(is.na(`British Columbia`)) %>%
  dplyr::select(species)
cat("\nRAW unmatched (before taxize):\n")
print(raw_unmatched)

# use Taxize to sort out all the names 
taxize_results <- gna_verifier(
  names = eastham_species_BC$species,
  best_match = TRUE
)
taxize_lookup <- setNames(
  taxize_results$currentCanonicalFull,
  taxize_results$submittedName
)
eastham_species_BC <- eastham_species_BC %>%
  mutate(
    species_taxize = taxize_lookup[species],
    species_final = ifelse(!is.na(species_taxize), species_taxize, species)
  )

# Check which species are unmatched in VASCAN
after_taxize_join <- eastham_species_BC %>%
  left_join(vascan_species_data,
            by = c("species_final" = "Scientific name"))
taxize_unmatched <- after_taxize_join %>%
  filter(is.na(`British Columbia`)) %>%
  dplyr::select(species, species_final)
cat("\nUnmatched AFTER taxize:\n")
print(taxize_unmatched)


# manually type out name replacements for things which need it 
# used Kew plants of the world online to figure this out
manual_synonyms <- tribble(
  ~eastham_name,                     ~vascan_name,
  "Carex aquatilis var. dives",           "Carex aquatilis", # subspecies rank
  "Carex sitchensis",           "Carex aquatilis", # Vascan synonym
  "Falona echinata",           "Cynosurus echinatus", # Vascan synonym
  "Rhododendron glandulosum",           "Rhododendron columbianum", # VASCAN via Rhododendron neoglandulosum
  "Equisetum praealtum",        "Equisetum hyemale", # Vascan synonym
  "Drosera × anglica",          "Drosera anglica", # issue introduced by taxize
  "Boechera drummondii",          "Boechera stricta", # issue introduced by taxize
  "Dracocephalum officinale subsp. officinale",          "Dracocephalum officinalis", # VASCAN synonym
  "Symphyotrichum occidentale",          "Symphyotrichum spathulatum", # issue introduced by taxize
  "Potentilla × villosula",          "Potentilla villosula", # issue introduced by taxize
  "Cornus × unalaschkensis",          "Cornus unalaschkensis", # issue introduced by taxize
  "Pseudolycopodium densum",            "Selaginella densa", # issue introduced by taxize
  "Convolvulus sepium subsp. sepium",            "Calystegia sepium", # issue introduced by taxize
  "Melanoseris violifolia",            "Nabalus hastatus", # issue introduced by taxize, synonym via Prenanthes alata (VASCAN)
  "Carex oederi",                         "Carex viridula", # Vascan synonym
  "Dichanthelium scribnerianum",          "Dichanthelium oligosanthes", # Kew synonym
  "Bromus carinatus",                     "Bromus sitchensis",          # Vascan synonym
  "Habenaria obtusa",            "", # a cultivated plant, need to deal with
  "Festuca bromoides",          "Vulpia bromoides", # Vascan synonym
  "Potentilla × diversifolia",       "Potentilla glaucophylla", # Vascan synonym
  "Graphephorum cernuum",            "Trisetum cernuum", # Kew synonym
  "Graphephorum canescens",          "Trisetum cernuum", # Kew and VASCAN synonym
  "Athyrium filix-femina",           "Athyrium cyclosorum", # accepted name, not thought to be in Canada. supspecies eastham collected actually yhis.
  "Conioselinum tataricum",          "Conioselinum pacificum", # had to do GBIF -> VASCAN synonym via Conioselinum gmelinii
  "Achnatherum calamagrostis",          "Calamagrostis stricta", # had to do GBIF -> VASCAN synonym via Calamagrostis neglecta
  "Cerastium holosteoides",          "Cerastium fontanum", # Vascan synonym
  "Pseudathyrium alpestre subsp. americanum", "Pseudathyrium alpestre", # subspecies
  "Sorbus occidentalis",             "Sorbus sitchensis", # Vascan synonym
  "Anemone multifida subsp. multifida", "Anemone multifida", # subspecies
  "Anemone grayi",                   "Anemone lyallii", # GBIF scientific name
  "Plectritis brachystemon",         "Plectritis congesta", # VASCAN synonym
  "Lupinus burkei",                  "Lupinus polyphyllus", # VASCAN synonym
  "Podagrostis thurberiana",         "Podagrostis humilis", # VASCAN synonym
  "Astragalus tenellus",             "Astragalus multiflorus", # VASCAN synonym
  "Andersonglossum virginianum",     "Andersonglossum boreale", # accepted, only found in eastern USA synonym via Cynoglossum virginianum
  "Carex styloflexa",                "", # accepted, only found in eastern USA, no obvious synonym
  "Koeleria pyramidata",                "", # accepted, only found Eurasia, no obvious synonym
  "Aristida longiseta",              "Aristida purpurea", # VASCAN synonym
  "Dactylorhiza viridis",            "Coeloglossum viride", # VASCAN synonym
  "Phlox gracilis",                  "Microsteris gracilis", # VASCAN synonym
  "Bromus polyanthus",               "Bromus sitchensis", # VASCAN synonym
  "Phlox rigida",                    "Phlox caespitosa", # VASCAN synonym
  "Penstemon lyalli",                "Penstemon lyallii", # spelling error
  "Oxytropis campestris var. spicata", "Oxytropis campestris", # subspecies
  "Elymus smithii",                  "Pascopyrum smithii", # VASCAN synonym
  "Poa alpigena",                    "Poa pratensis", # VASCAN synonym
  "Bromus marginatus",               "Bromus sitchensis", # VASCAN synonym
  "Bromus anomalus",                 "Bromus porteri", # VASCAN synonym
  "Festuca microstachys",            "Vulpia microstachys", # VASCAN synonym
  "Calamagrostis inexpansa",         "Calamagrostis stricta", # VASCAN synonym
  "Festuca myuros",                  "Vulpia myuros", # VASCAN synonym
  "Koeleria pyramidata",             "", # accepted, only found in europe
  "Achnatherum calamagrostis",       "", # accepted, only found in europe/asia
  "Cornus torreyi",                  "Cornus occidentalis", # Kew synonym
  "Lotus pedunculatus",              "Lotus uliginosus", # VASCAN synonym
  "Rhododendron menziesii",          "Menziesia ferruginea", # Kew synonym
  "Caltha leposepala",               "Caltha leptosepala", # typo
  "Dodecatheon pulchellum",          "Primula pauciflora", # VASCAN synonym
  "Kreidion gmelinii",               "Conioselinum pacificum", # used GBIF and VASCAN for synonym
  "Cerastium elongatum",             "Cerastium arvense", # VASCAN synonym
  "Sporobolus rigidus var. rigidus", "Sporobolus rigidus", # subspecies
  "Leontodon saxatilis subsp. saxatilis", "Leontodon saxatilis", # subspecies
  "Equisetum hyemale subsp. affine", "Equisetum hyemale", # VASCAN synonym
  "Koeleria pyramidata",             "", # accepted, only found in europe
  "Lappula diploloma",               "Lappula montana",  # Kew synonym
  "Dryopteris dilatata",             "Dryopteris campyloptera", # VASCAN synonym
  "Castilleja rhexifolia",           "Castilleja rhexiifolia", # typo
  "Dodecatheon dentatum",            "Primula latiloba", # VASCAN synonym
  "Centaurea debeauxii",             "Centaurea nigra", # Kew synonym
  "Pinguicula macroceras",           "Pinguicula vulgaris", # VASCAN synonym
  "Erigeron podolicus",              "Erigeron acris", # Kew synonym
  "Salsola tragus",                  "Kali tragus", # VASCAN synonym
  "Wyethia sagittata",               "Balsamorhiza sagittata", # Kew synonym
  "Brassica rapa subsp. sylvestris", "Brassica rapa", # subspecies
  "Blitum capitatum subsp. capitatum", "Blitum capitatum", # subspecies
  "Festuca octoflora",               "Vulpia octoflora", # VASCAN synonym
  "Eucephalus engelmannii",          "Doellingeria engelmannii", # VASCAN synonym
  "Mycelis muralis",                 "Cicerbita muralis", # VASCAN synonym
  "Heterotheca hirsutissima",        "Heterotheca villosa", # VASCAN and Kew synonym
  "Matteuccia struthiopteris",       "Matteuccia pensylvanica", # VASCAN synonym
  "Centaurea australis",             "Centaurea stoebe",  # VASCAN and Kew synonym
  "Eleocharis palustris subsp. palustris", "Eleocharis palustris", # subspecies
  "Angelica leiocarpa",              "Glehnia leiocarpa", # found synonym here: https://www.catalogueoflife.org/annual-checklist/2019/details/species/id/53639202
  "Rhododendron glandulosum",        "", # accepted, native to alaska
  "Carex pyrenaica",                 "Carex micropoda", # VASCAN synonym
  "Convolvulus sepium",              "Calystegia sepium", # VASCAN synonym
  "Raphanus raphanistrum subsp. raphanistrum", "Raphanus raphanistrum", # subspecies
  "Dracocephalum officinale subsp. officinale", "", # accepted, native to europe
  "Microseris cuspidata",            "Nothocalais cuspidata", # VASCAN synonym
  "Equisetum telmateia",             "Equisetum braunii", # VASCAN synonym
  "Lactuca pulchella",               "Lactuca oblongifolia", # VASCAN synonym
  "Micranthes mertensiana",          "Saxifraga mertensiana", # VASCAN synonym
  "Microseris troximoides",          "Nothocalais troximoides", # VASCAN synonym
  "Poa humilis",                     "Poa pratensis", # GBIF synonym (in other species column)
  "Cystopteris dickieana",           "Cystopteris fragilis", # VASCAN synonym
  "Agropyron desertorum",            "Agropyron cristatum", # VASCAN synonym
  "Rhinanthus groenlandicus",        "Rhinanthus minor", # VASCAN synonym
  "Artemisia norvegica subsp. saxatilis", "Artemisia norvegica", # subspecies
  "Centaurea jacea subsp. jacea",    "Centaurea jacea", # VASCAN synonym
  "Phacelia nemoralis",              "", # accepted, native to california, oregon, washington
  "Polygonum kelloggii",             "Polygonum polygaloides", # VASCAN synonym
  "Cladothamnus pyrolaeflorus",     "Elliottia pyroliflora", # synonym from flora of Oregon
  "Stellaria sitchana",              "Stellaria borealis", # VASCAN synonym
  "Chenopodium betaceum",            "Chenopodium album", # accepted, took synonym from Kew (both introduced)
  "Crepis barbigera",                "", # accepted, native to washinton, oregon, idaho
  "Pseudofumaria lutea",             "", # Pseudo-fumaria lutea is accepted, native to Europe
  "Amaranthus bouchonii",            "Amaranthus powellii", # VASCAN synonym
  "Lithophragma tenella",            "Lithophragma tenellum", # typo
  "Hackelia deflexa",                "Hackelia americana", # VASCAN synonym
  "Dodecatheon hendersonii",         "Primula hendersonii", # VASCAN synonym
  "Hemieva ranunculifolia",          "Suksdorfia ranunculifolia", # VASCAN synonym
  "Amaranthus graecizans",           "Amaranthus blitoides", # VASCAN synonym - maps to two species but both introduced in BC
  "Salicornia europaea",             "Salicornia virginica", # VASCAN synonym
  "Carex subbracteata",              "", # accepted, native to California
  "Poa angustifolia",                "Poa pratensis", # VASCAN synonym
  "Arabidopsis lyrata subsp. kamchatica", "Arabidopsis lyrata", # subspecies
  "Heterocodon rariflorum",          "Heterocodon rariflorus", # typo
  "Arctium nemorosum",               "Arctium minus", # VASCAN synonym
  "Calamagrostis purpurea",          "Calamagrostis canadensis", # accepted, kew says native to canada, synonym via GBIF
  "Poa lanata",                       "Poa arctica", # VASCAN synonym
  "Lappula redowskii",               "Lappula occidentalis", # VASCAN synonym
  "Pulsatilla patens",               "Pulsatilla nuttalliana", # VASCAN synonym
  "Ranunculus reptans",              "Ranunculus flammula", # VASCAN synonym
  "Descurainia longipedicellata",    "Descurainia incisa", # synonym from Burke herbarium
  "Pectocarya linearis",             "Pectocarya penicillata", # VASCAN synonym
  "Glyceria occidentalis",            "Glyceria × occidentalis", # VASCAN synonym
  "Valeriana dioica",                "Valeriana septentrionalis", # VASCAN synonym
  "Bromus remotiflorus",             "" # accepted, native to East Asia
)


# Create a named vector for manual replacement
manual_vector <- setNames(
  manual_synonyms$vascan_name,
  manual_synonyms$eastham_name
)


# Apply manual mapping only to species that are still unmatched
eastham_species_BC <- eastham_species_BC %>%
  mutate(
    species_final = ifelse(
      species_final %in% names(manual_vector) &
        manual_vector[species_final] != "",
      manual_vector[species_final],
      species_final
    )
  )




# final clean up 
eastham_species_BC <- eastham_species_BC %>%
  mutate(species_final = trimws(species_final)) %>%
  filter(!is.na(species_final), species_final != "") %>%
  distinct(species_final, .keep_all = TRUE)

# do another check
eastham_final <- eastham_species_BC %>%
  left_join(
    vascan_species_data,
    by = c("species_final" = "Scientific name")
  )

# final unmatched check
remaining_unmatched <- eastham_final %>%
  filter(is.na(`British Columbia`)) %>%
  dplyr::select(species, species_final)

cat("\nFINAL unmatched AFTER taxize + manual:\n")
print(remaining_unmatched)


# count the categories of VASCAN distribution in BC
EasthamStatusBC <- eastham_final %>%
  count(`British Columbia`) %>%
  rename(
    `Distribution in BC` = `British Columbia`,
    `Number of species` = n
  )
# save df
write_csv(EasthamStatusBC, "Eastham_VASCAN_BC.csv")



# how many of Canada's species in BC are native, introduced, etc.
# honestly this is not a smart thing to do, lists every single species in canada's relationship to BC
# often highly irrelevant
vascanStatusBC <- vascan_species_data %>% 
  group_by(`British Columbia`) %>% 
  summarize(species_count = n())
  
  
# ----------------------------------------------------------------------
# 7. How many FIRSTS to Canada/BC were native/introduced to BC?
# ----------------------------------------------------------------------

# use the species key to avoid doing synonymy work all over again

eastham_first_bc_VASCAN <- eastham_final %>% 
  filter(speciesKey %in% eastham_first_bc$speciesKey.x)

eastham_first_canada_VASCAN <- eastham_final %>% 
  filter(speciesKey %in% eastham_first_canada$speciesKey.x)

vascanStatus_firstBC <- eastham_first_bc_VASCAN %>% 
  group_by(`British Columbia`) %>% 
  summarize(species_count = n())

print(vascanStatus_firstBC)

vascanStatus_firstCanada <- eastham_first_canada_VASCAN %>% 
  group_by(`British Columbia`) %>% 
  summarize(species_count = n())

print(vascanStatus_firstCanada)

  

# ----------------------------------------------------------------------
# 8. plotting invasive vs native on a map?
# ----------------------------------------------------------------------

eastham_invasive_map_points <- eastham_species_BC %>% 
  left_join(eastham_final %>% 
            dplyr::select(speciesKey, `British Columbia`), 
            by = "speciesKey")

eastham_nonNative_map_points <- eastham_invasive_map_points %>% 
  filter(`British Columbia` != "Native")

# Get the map data for Canada provinces
bc_neighbor <- ne_states(country = "canada", returnclass = "sf") %>%
  filter(name == "British Columbia")

ggplot() +
  # Layer 1: The BC Border
  geom_sf(data = bc_neighbor, fill = "gray", color = "black") +
  # Layer 2: Your Points
  geom_point(data = eastham_nonNative_map_points, 
             aes(x = decimalLongitude, y = decimalLatitude, color = `British Columbia`),
             size = 2, alpha = 0.8) +
  coord_sf() +
  theme_void() + # Cleans up the background
  labs(color = "Category")

