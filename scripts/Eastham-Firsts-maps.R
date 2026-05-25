###########################################################################################################################
# Script: Compare Eastham specimens to all Canadian specimens to see which were first to Canada or BC
###########################################################################################################################

# Not of all these libraries are needed
# Need to go through and check which ones can be removed here

# Load libraries
library(tidyverse)
library(sf)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(bcmaps)
library(ggspatial)
library(MASS)


# GBIF credentials
gbif_all <- readRDS("gbif_NA_all_records.rds")
eastham_records <- readRDS("Eastham_plants.rds")
VasCanSpecies <- readRDS("VasCanSpecies.rds")


############################################################
# 8. Determine which of Eastham's collections were first for the species in Canada
############################################################


# Convert eventDate to Date format for both datasets (Eastham and GBIF)
eastham_records$eventDate <- as.Date(eastham_records$eventDate)
gbif_all$eventDate <- as.Date(gbif_all$eventDate)

# Identify the first GBIF record for each species in Canada
first_gbif_canada <- gbif_all %>%
  filter(countryCode == "CA" & !is.na(eventDate)) %>%             # Only Canadian records with a date
  filter(taxonRank == "SPECIES") %>%                             # Not interested in subspecies
  group_by(speciesKey) %>%
  filter(eventDate == min(eventDate, na.rm = TRUE)) %>%
  slice_min(gbifID, with_ties = FALSE) %>%     # pick one if multiple have same date
  ungroup() %>%
  dplyr::select(speciesKey, gbifID, eventDate, countryCode, stateProvince, institutionCode, collectionCode, catalogNumber)

# Join Eastham's records with the first Canadian GBIF record by gbifID
eastham_first_canada <- eastham_records %>%
  inner_join(first_gbif_canada, by = "gbifID")

# Identify the first GBIF record for each species in British Columbia
first_gbif_bc <- gbif_all %>%
  filter(countryCode == "CA" & stateProvince == "British Columbia" & !is.na(eventDate)) %>%  # Only BC records
  filter(taxonRank == "SPECIES") %>% 
  group_by(speciesKey) %>%
  filter(eventDate == min(eventDate, na.rm = TRUE)) %>%
  slice_min(gbifID, with_ties = FALSE) %>%     # pick one if multiple have same date
  ungroup() %>%
  dplyr::select(speciesKey, gbifID, eventDate, countryCode, stateProvince, institutionCode, collectionCode, catalogNumber)

# Join Eastham's records with the first BC GBIF record by gbifID
eastham_first_bc <- eastham_records %>%
  inner_join(first_gbif_bc, by = "gbifID")

# Check results
head(eastham_first_canada)  # First specimens by Eastham in Canada
head(eastham_first_bc)      # First specimens by Eastham in BC

# save results
saveRDS(eastham_first_canada, "Eastham_first_to_Canada.rds")
saveRDS(eastham_first_bc, "Eastham_first_to_BC.rds")

# OTHER THINGS I COULD DO WITH THIS INFO
# use the vascan database to see how many of these "first" species are introduced in BC

# Assuming 'species' column exists in both dataframes and matches in both
eastham_records <- eastham_records %>%
  left_join(VasCanSpecies, by = c("species" = "Scientific name"))


# Step 1: Add the 'status' column to eastham_records and mark all first collections to Canada as "First to Canada"
eastham_records$status <- ifelse(eastham_records$gbifID %in% eastham_first_canada$gbifID, "First to Canada", "Other")

# Mark all first collections to British Columbia as "First to British Columbia"
# But only mark as "First to BC" if it hasn't been already marked as "First to Canada"
eastham_records$status <- ifelse(eastham_records$gbifID %in% eastham_first_bc$gbifID & 
                                   eastham_records$status != "First to Canada", 
                                 "First to British Columbia", eastham_records$status)


# How many of Eastham's "firsts" were invasive or ephemeral 
EasthamFirstSummary <- eastham_records %>%
  count(status, `British Columbia`)  # Count occurrences of each combination


# View the summary
print(EasthamFirstSummary)


############################################################
# 9. Plot Eastham's collections on a map of B.C.
############################################################



# Make map of BC
bc <- bc_bound()

# Getting BC railways in

# Get BC bounding box
rail_shp <- st_read("../ShapeFiles/BCrail/nrwn_rfn_bc_shp_en/NRWN_BC_2_0_TRACK.shp")
rail_shp <- st_transform(rail_shp, st_crs(bc))

# THINGS TO DO HERE
# need to add in grey black points to the legend
# need to fix erroneous coordinates
# need to force things collected at 48, -123 to 48.45, -123 (Saanich latitude)
eastham_records <- eastham_records %>%
  mutate(decimalLatitude = ifelse(decimalLatitude == 48 & decimalLongitude == -123,
                             48.45,  # new latitude
                             decimalLatitude),
         decimalLongitude = ifelse(decimalLatitude == 48.45 & decimalLongitude == -123,
                                   -123.4,  # new longitude
                                   decimalLongitude))

# Transform to match BC map
points_sf <- eastham_records %>%
  filter(!is.na(decimalLongitude), !is.na(decimalLatitude)) %>%  # remove missing coords
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
           crs = 4326)  # WGS84
points_sf <- st_transform(points_sf, st_crs(bc))


# Only plot points inside the BC boundary
points_bc <- st_intersection(points_sf, bc)


PointsMapPlot <- ggplot() +
  geom_sf(data = bc, fill = "lightgray", color = "black") +
  geom_sf(data = rail_shp, aes(color = "Railway"), size = 0.3, alpha = 0.7) +   # rail layer
  geom_sf(data = points_bc %>% filter(status == "Other"),
          aes(color = "Other"),
          alpha = 0.25, size = 1.25) +
  geom_sf(data = points_bc %>% 
            filter(status %in% c("First to Canada", "First to British Columbia")),
          aes(color = status),
          alpha = 0.7, size = 1.25) +
  scale_color_manual(values = c(
    "Other" = "darkblue",
    "First to Canada" = "hotpink",
    "First to British Columbia" = "red",
    "Railway" = "black"
  )) +
  labs(title = "Eastham's British Columbian Collections",
       x = "Longitude", y = "Latitude",
       color = "Collection Type") +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_fancy_orienteering
  ) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  theme_minimal()

plot(PointsMapPlot)

ggsave("EasthamCollectionsBC.png", plot = PointsMapPlot, width = 10, height = 7.5)









# Simpler map for Linda

# getting country and province boundaries
world_sf <- ne_countries(scale = "large", returnclass = "sf")
canada_provinces_sf <- ne_states(country = "Canada", returnclass = "sf")

# make sure points are plotted on the map in this order
points_bc_ordered <- points_bc %>%
  mutate(status = factor(status,
                         levels = c("Other",
                                    "First to British Columbia",
                                    "First to Canada"))) %>%
  arrange(status)  # ensures Other rows are first

# clip rail layer to land 
world_sf <- st_make_valid(world_sf)
rail_shp <- st_transform(rail_shp, st_crs(world_sf))
rail_clipped <- st_intersection(rail_shp, st_union(world_sf))

# make the map
simplerMap <- ggplot() +
  geom_sf(data = world_sf, fill = "lightgray", color = "black") +
  geom_sf(data = canada_provinces_sf, fill = NA, color = "black") +
  geom_sf(data = rail_clipped, aes(color = "Railway"), size = 0.3, alpha = 0.7) +
  geom_sf(data = points_bc_ordered,
          aes(color = status, shape = status),
          size = 2,
          alpha = 0.7) +
  coord_sf(xlim = c(-131, -114), ylim = c(48, 56), expand = FALSE) +
  scale_color_manual(
    values = c(
      "Other" = "darkslategray",
      "First to Canada" = "darkorange1",
      "First to British Columbia" = "darkturquoise",
      "Railway" = "black"
    ),
    breaks = c("First to Canada", "First to British Columbia", "Other"),
    labels = c("First to Canada", 
               "First to British Columbia", 
               "Other British Columbian Collection")
  ) +
  scale_shape_manual(
    values = c(
      "Other" = 16,
      "First to British Columbia" = 15,
      "First to Canada" = 17
    ),
    labels = c("First to Canada", 
               "First to British Columbia", 
               "Other British Columbian Collection")
  ) +
  labs(title = "Eastham's British Columbian Collections",
       x = "Longitude", y = "Latitude",
       color = "Collection Type",
       shape = "Collection Type") +
  annotation_north_arrow(location = "tr",
                         which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  theme_minimal() +
  guides(
    color = guide_legend(override.aes = list(shape = c(17, 15, 16))),
    shape = guide_legend(override.aes = list(color = c("darkorange1", "darkturquoise", "darkslategray")))
  )

plot(simplerMap)

ggsave("EasthamCollectionsBC-simplerMap.png", plot = simplerMap, width = 10, height = 7.5)





# Simpler map

# Force collections to 48.4 for Vancouver Island ones written as 48 N?


# world map for plotting
world <- map_data("world")


ggplot() +
  # Plot world map
  geom_polygon(data = world, aes(x = long, y = lat, group = group), 
               fill = "lightgray", color = "black") +
  # Plot "Other" points (will be below the first to points)
  geom_point(data = eastham_records %>% filter(status == "Other"), 
             aes(x = decimalLongitude, y = decimalLatitude), 
             size = 1.25, color = "darkblue", alpha = 0.25,
             position = position_jitter(width = 0.1, height = 0.1)) +  # Add jitter
  # Plot "First to Canada" and "First to British Columbia" points first (on top)
  geom_point(data = eastham_records %>% filter(status %in% c("First to Canada", "First to British Columbia")), 
             aes(x = decimalLongitude, y = decimalLatitude, color = status), 
             size = 1.25, alpha = 0.7, 
             position = position_jitter(width = 0.1, height = 0.1)) +  # Add jitter
  # Customize colors for "First to Canada" and "First to British Columbia"
  scale_color_manual(values = c("First to Canada" = "hotpink", "First to British Columbia" = "red")) +
  labs(title = "Eastham's British Columbian Collections",
       x = "Longitude", y = "Latitude",
       color = "Collection Type") +
  theme_minimal() +
  theme(legend.position = "right") +
  # Crop the map to British Columbia's coordinates (Longitude: -140 to -110, Latitude: 48 to 62)
  coord_quickmap(
    xlim = c(-135, -113),  # Longitude bounds for BC
    ylim = c(48, 56))      # Latitude bounds for BC



















# Plot BC with a heatmap of collection density
# Convert sf points to a data frame with x/y for density plotting
points_df <- points_bc %>%
  st_coordinates() %>%
  as.data.frame() %>%
  rename(x = X, y = Y)

# Heatmap plot
ggplot() +
  geom_sf(data = bc, fill = "lightgray", color = "black") +  # BC boundary
  geom_sf(data = rail_shp, color = "black", size = 0.3, alpha = 0.7) +  # Railways
  stat_density_2d(
    data = points_df,
    aes(x = x, y = y, fill = after_stat(level), alpha = after_stat(level)),
    geom = "polygon",
    contour = TRUE
  ) +
  scale_fill_viridis_c(option = "plasma", name = "Collection Density") +
  scale_alpha(range = c(0.2, 0.7), guide = "none") +
  labs(title = "Heatmap of Eastham's Collections in British Columbia",
       x = "Longitude", y = "Latitude") +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_fancy_orienteering
  ) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  theme_minimal()



# Convert to data frame with long/lat
points_df <- points_bc %>%
  st_coordinates() %>%
  as.data.frame() %>%
  rename(long = X, lat = Y)

# Compute density as a grid
dens <- MASS::kde2d(points_df$long, points_df$lat, n = 200)  # n=grid resolution
dens_df <- expand.grid(long = dens$x, lat = dens$y)
dens_df$density <- as.vector(dens$z)

# Convert density grid to sf points for masking
dens_sf <- st_as_sf(dens_df, coords = c("long", "lat"), crs = st_crs(bc))
dens_sf <- st_intersection(dens_sf, bc)  # Keep only points inside BC

# Heatmap using geom_tile
ggplot() +
  geom_sf(data = bc, fill = "lightgray", color = "black") +
  geom_sf(data = rail_shp, color = "black", size = 0.3, alpha = 0.7) +
  geom_tile(data = dens_sf %>% st_coordinates() %>% as.data.frame() %>% 
              mutate(density = dens_sf$density),
            aes(x = X, y = Y, fill = density), alpha = 0.8) +
  scale_fill_viridis_c(option = "plasma", name = "Collection Density") +
  labs(title = "Heatmap of Eastham's Collections in British Columbia",
       x = "Longitude", y = "Latitude") +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_fancy_orienteering
  ) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  theme_minimal()



