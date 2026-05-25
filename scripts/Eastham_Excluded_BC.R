###########################################################################################################################
# Script: Using VasCan to figure out species status of "Excluded species" from Eastham's addendum
###########################################################################################################################

################################## Load libraries ####################################

library(tidyverse)
library(taxize)


#################################### Load data ######################################

vascan_species_data <- readRDS("VasCanSpecies.rds")
Eastham_Excluded_BC <- read_csv("Eastham_Excluded_BC.csv")


################################# sort out quirks  ##################################

# the x vs X symbol for hybrids
# replace " × " with " x " in VASCAN species name 

vascan_species_data <- vascan_species_data %>%
  mutate(`Scientific name` = str_replace_all(`Scientific name`, " × ", " x "))


################## get VASCAN BC distribution for the excluded species ################

# do a left join
Eastham_Excluded_BC_VASCAN <- Eastham_Excluded_BC %>% 
  left_join(vascan_species_data %>% 
              dplyr::select(`British Columbia`, `Scientific name`),
             by = c("VASCAN name" = "Scientific name"))

# write out the file
write_csv(Eastham_Excluded_BC_VASCAN, "Eastham_addendumExcluded.csv")

################## summarise the BC distribution for the excluded species ################

Eastham_Excluded_BC_VASCAN_summary <- Eastham_Excluded_BC_VASCAN %>% 
  count(`British Columbia`)


################## How many of these excluded species did Eastham collect? ################

# WARNING: need to run easthamTaxonomy R script first in the same environment
# need to 

eastham_excluded_collections <- inner_join(eastham_final, Eastham_Excluded_BC,
                                           by = c("species_final" = "VASCAN name"))


