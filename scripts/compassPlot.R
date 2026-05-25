#########################################
# Script: Compare Eastham specimens to all Canadian specimens
#########################################

# Load libraries
library(tidyverse)
library(geosphere)
library(RANN)

# GBIF credentials
user <- "" # Your GBIF account username
pwd  <- "" # Your GBIF account password
email <- "" # Your GBIF account email address


############################################################
# 1. Load Eastham specimens
############################################################

Eastham_GBIF <- readRDS("Eastham_plants.rds")

# Keep rows with coordinates and speciesKey
Eastham_GBIF <- Eastham_GBIF %>%
  filter(!is.na(decimalLatitude),
         !is.na(decimalLongitude),
         !is.na(speciesKey),
         !is.na(eventDate))

# Filter out records where lat long is 0,0
Eastham_GBIF <- Eastham_GBIF %>% 
  filter(decimalLatitude != 0,
  decimalLongitude != 0)

Eastham_GBIF$eventDate <- as.Date(Eastham_GBIF$eventDate)

# Keep earliest Eastham specimen per species
Eastham_GBIF <- Eastham_GBIF %>%
  group_by(speciesKey) %>%
  slice_min(order_by = eventDate, with_ties = FALSE) %>%
  ungroup()

Eastham_GBIF <- Eastham_GBIF %>%
  rename(
    ID = gbifID,
    Latitude = decimalLatitude,
    Longitude = decimalLongitude
  )

Eastham_GBIF$key_to_run <- Eastham_GBIF$speciesKey

############################################################
# 2. Load ALL historical NA plant/fungal specimens
############################################################

gbif_all <- readRDS("gbif_NA_all_records.rds")

# Filter out records where lat long is 0,0
gbif_all <- gbif_all %>% 
  filter(decimalLatitude != 0,
         decimalLongitude != 0)

#################################################################################
# 3. Filter GBIF dataset so that we only look at species Eastham collected
#################################################################################

 species_list <- unique(Eastham_GBIF$speciesKey)

# gbif_NA <- gbif_all %>%
#   
#   filter(speciesKey %in% species_list) %>%
#   
#   filter(!grepl("Eastham", recordedBy, ignore.case = TRUE)) %>%
#   
#   select(
#     ID = gbifID,
#     Latitude = decimalLatitude,
#     Longitude = decimalLongitude,
#     species,
#     speciesKey,
#     eventDate,
#     year
#   ) %>%
#   
#   filter(!is.na(Latitude),
#          !is.na(Longitude))
# 
# saveRDS(gbif_NA, "gbif_NA_all_species.rds")

gbif_NA <- readRDS("gbif_NA_all_species.rds")

############################################################
# 4. Fast nearest neighbour with correct date handling
############################################################


calc_next_closest_fast <- function(ref_plants, target_plants){
  
  results <- list()
  
  species_list <- unique(ref_plants$key_to_run)
  
  for(sp in species_list){
    
    ref_subset <- ref_plants %>%
      filter(key_to_run == sp)
    
    # skip species with no date
    if(all(is.na(ref_subset$eventDate))) next
    
    earliest_year <- min(ref_subset$year, na.rm = TRUE)
    
    target_subset <- target_plants %>%
      filter(speciesKey == sp) %>%
      filter(year < earliest_year)
    
    if(nrow(target_subset) == 0){
      
      results[[as.character(sp)]] <- tibble(
        ReferenceID = ref_subset$ID,
        NearestID = NA,
        NearestDistance = NA,
        NearestBearing = NA
      )
      
      next
    }
    
    ref_coords <- as.matrix(ref_subset[,c("Longitude","Latitude")])
    target_coords <- as.matrix(target_subset[,c("Longitude","Latitude")])
    
    nn <- nn2(
      data = target_coords,
      query = ref_coords,
      k = 1
    )
    
    nearest_indices <- nn$nn.idx[,1]
    
    nearest_points <- target_coords[nearest_indices,]
    
    distances <- distHaversine(ref_coords, nearest_points)
    
    bearings <- (bearing(nearest_points, ref_coords) + 360) %% 360
    
    results[[as.character(sp)]] <- tibble(
      ReferenceID = ref_subset$ID,
      NearestID = target_subset$ID[nearest_indices],
      NearestDistance = distances,
      NearestBearing = bearings
    )
  }
  
  bind_rows(results)
}

############################################################
# 5. Calculate closest earlier specimen
############################################################

next_closest_results <- calc_next_closest_fast(Eastham_GBIF, gbif_NA)

saveRDS(next_closest_results, "Eastham_GBIF_next_closest_earlier_specimens.rds")

# remove identical coordinates
next_closest_results <- next_closest_results %>%
  filter(NearestDistance != 0)

############################################################
# 6. Compass plot
############################################################

# Define breaks and labels
distance_breaks <- c(100, 1000, 10000, 100000, 1000000, 10000000)
distance_labels <- c("100 m", "1 km", "10 km", "100 km", "1000 km", "10, 000 km")
compassPlot <-ggplot(next_closest_results,
       aes(x = NearestBearing,
           y = NearestDistance)) +
  
  geom_point(color = "darkslategray", size = 2) +
  
  geom_segment(aes(xend = NearestBearing,
                   y = 0,
                   yend = NearestDistance),
               alpha = 0.2,
               color = "darkslategray") +
  
  scale_y_log10(
    breaks = distance_breaks,
    labels = distance_labels,
    expand = c(0,0)
  ) +
  
  scale_x_continuous(
    limits = c(0,360),
    breaks = seq(0,315,45),
    labels = c("N","NE","E","SE","S","SW","W","NW")
  ) +
  
  geom_text(data = tibble(
    y = distance_breaks,
    x = rep(115, length(distance_breaks)),
    label = distance_labels
  ),
  aes(x = x, y = y, label = label),
  inherit.aes = FALSE,
  fontface = "bold",
  colour = "black",
  hjust = 0.5,
  vjust = -0.5) +
  
  coord_polar() +   # North at top
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(face = "bold", colour = "black", size = 12),
    axis.title = element_blank()  # optional: removes axis titles
  )

plot(compassPlot)

############################################################
# 7. Summary statistics for closest specimens
############################################################

cat(
  "Mean distance (m):",
  mean(next_closest_results$NearestDistance, na.rm = TRUE),
  "\n"
)

cat(
  "SD distance (m):",
  sd(next_closest_results$NearestDistance, na.rm = TRUE),
  "\n"
)

calculate_mean_bearing <- function(bearings){
  
  radians <- bearings * pi / 180
  
  mean_x <- mean(cos(radians), na.rm = TRUE)
  mean_y <- mean(sin(radians), na.rm = TRUE)
  
  mean_bearing <- atan2(mean_y, mean_x) * 180 / pi
  
  (mean_bearing + 360) %% 360
}

cat("Mean bearing (deg):",
  calculate_mean_bearing(next_closest_results$NearestBearing),
  "\n")


############################################################
# 8. Save plot
############################################################

ggsave("compassPlot.png", plot = compassPlot, width = 8, height = 8, units = "in" )



