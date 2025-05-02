# ============================================================
# School Proximity Analysis for Fast-Food Locations
# ============================================================

# Load required packages
library(readxl)
library(sf)
library(dplyr)
library(tmap)

# Set working directory
setwd("/Users/marcrisney/Projects/jhu/mas/Spring2025/SpatialApplications/Assignment2")

# 1) READ SCHOOL DATA -----------------------------------------
# Read school data from CSV file
schools_data <- read.csv("data/schools_data.csv", stringsAsFactors = FALSE)

# 2) CONVERT SCHOOLS TO SPATIAL POINTS ------------------------
# Convert to spatial points
schools_sf <- schools_data %>%
  filter(!is.na(Longitude) & !is.na(Latitude)) %>%  # Remove any schools with missing coordinates
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(26918)  # Transform to UTM Zone 18N

# 3) READ PREVIOUSLY PREPARED DATASETS ------------------------
# Read Baltimore City boundary
balt_city <- st_read("baltimore_city_boundary/baltimore_city_boundary.shp",
                     quiet=TRUE) %>%
  st_transform(26918)  # Transform to UTM Zone 18N

# Read and process fast-food locations
ff_df <- read_excel("data/fast-food-data-cleaned.xlsx")
ff_pts <- ff_df %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(26918) %>%
  # Filter to points within Baltimore City
  filter(lengths(st_intersects(., balt_city)) > 0)

# 4) CREATE SCHOOL BUFFERS -----------------------------------
# Create 500m buffers around schools
school_buffers <- st_buffer(schools_sf, 500)

# 5) IDENTIFY FAST-FOOD POINTS WITHIN SCHOOL BUFFERS ---------
# Spatial join to identify fast-food points within buffers
ff_with_buffer <- st_join(ff_pts, 
                          school_buffers %>% select(School.Number),
                          join = st_intersects, 
                          left = TRUE)

# Add buffer indicator (TRUE if within any buffer, FALSE otherwise)
ff_with_buffer$in_buffer <- !is.na(ff_with_buffer$School.Number)

# 6) CALCULATE STATISTICS ------------------------------------
# Count points inside and outside buffers
in_buffer_count <- sum(ff_with_buffer$in_buffer, na.rm = TRUE)
outside_buffer_count <- sum(!ff_with_buffer$in_buffer, na.rm = TRUE)

# Calculate total areas (accounting for buffer overlap)
united_buffers <- st_union(school_buffers)
buffer_area <- as.numeric(st_area(united_buffers)) / 1000000  # Convert to sq km
city_area <- as.numeric(st_area(st_union(balt_city))) / 1000000  # Convert to sq km
outside_area <- city_area - buffer_area

# Calculate densities (points per sq km)
in_density <- in_buffer_count / buffer_area
out_density <- outside_buffer_count / outside_area

# 7) CREATE SCHOOL PROXIMITY MAP -----------------------------
# Set tmap to plot mode
tmap_mode("plot")

# Define consistent compass style
compass_style <- tm_compass(
  type = "8star", 
  size = 1,
  position = c("left", "top"),
  color.dark = "black",
  color.light = "white"
)

# Create map
school_buffer_map <- tm_shape(balt_city, unit = "mi", unit.size = 1609.34) +
  tm_fill(col = "#ECECEC") +
  tm_borders(col = "black") +
  
  # Add 500m school buffers
  tm_shape(united_buffers) +  # Use united buffers to avoid overlap issues
  tm_fill(col = "lightblue", alpha = 0.3, title = "School 500m Buffer") +
  
  # Add fast-food points colored by buffer status
  tm_shape(ff_with_buffer) +
  tm_dots(
    col = "in_buffer", 
    palette = c("darkgray", "red"),
    labels = c("Outside buffer", "Within 500m of school"),
    title = "Fast-Food Location",
    size = 0.1
  ) +
  
  # Add schools
  tm_shape(schools_sf) +
  tm_dots(col = "blue", size = 0.2, title = "School") +
  
  # Add scale bar in miles
  tm_scalebar(
    breaks = c(0, 1, 2, 3, 4, 5),
    position = c("left", "bottom"),
    text.size = 0.6
  ) +
  
  # Add compass
  compass_style +
  
  # Layout elements
  tm_layout(
    main.title = "Fast-Food Locations Near Schools\nBaltimore City, MD",
    main.title.size = 1.2,
    legend.outside = TRUE,
    frame = FALSE
  )

# 8) PRINT AND SAVE MAP -------------------------------------
# Display map
print(school_buffer_map)

# Save map to file
tmap_save(school_buffer_map, 
          filename = "results/school_proximity_map.png",
          width = 8, height = 8, units = "in", dpi = 300)

# 9) CALCULATE DISTANCE-BASED STATISTICS --------------------
# Calculate distance from each fast-food point to the nearest school
nearest_dist <- st_distance(ff_pts, schools_sf)
ff_pts$dist_to_school <- apply(nearest_dist, 1, min)
ff_pts$dist_to_school_km <- as.numeric(ff_pts$dist_to_school) / 1000

# Create distance correlation plot
png("results/distance_correlation_plot.png", width=800, height=600, res=100)
# Count fast-food by distance bins
dist_bins <- cut(ff_pts$dist_to_school_km, 
                 breaks = seq(0, max(ff_pts$dist_to_school_km, na.rm=TRUE) + 0.1, by=0.1),
                 include.lowest = TRUE)
counts <- table(dist_bins)
plot(seq(0.05, max(ff_pts$dist_to_school_km, na.rm=TRUE), by=0.1)[1:length(counts)], 
     counts, 
     type="l", 
     xlab="Distance to Nearest School (km)",
     ylab="Number of Fast-Food Locations",
     main="Relationship Between Fast-Food Counts and Distance to Schools")
abline(v=0.5, col="red", lty=2)  # Mark 500m (0.5km) threshold
dev.off()

# 10) PRINT STATISTICS ---------------------------------------
# Print summary statistics
cat("Summary Statistics for School Proximity Analysis:\n")
cat("--------------------------------------------------\n")
cat("Number of schools analyzed:", nrow(schools_sf), "\n")
cat("Number of fast-food locations analyzed:", nrow(ff_pts), "\n\n")
cat("Fast-food locations within 500m of schools:", in_buffer_count, "\n")
cat("Fast-food locations outside school buffers:", outside_buffer_count, "\n")
cat("Percentage within school buffers:", round(in_buffer_count/(in_buffer_count+outside_buffer_count)*100, 1), "%\n\n")
cat("Area within 500m of schools:", round(buffer_area, 2), "sq km\n")
cat("Area outside school buffers:", round(outside_area, 2), "sq km\n")
cat("Percentage of city area within 500m of schools:", round(buffer_area/city_area*100, 1), "%\n\n")
cat("Density within buffers:", round(in_density, 2), "points/sq km\n")
cat("Density outside buffers:", round(out_density, 2), "points/sq km\n")
cat("Ratio of densities (in/out):", round(in_density/out_density, 2), "x\n\n")
cat("Average distance to nearest school:", round(mean(ff_pts$dist_to_school_km), 2), "km\n")
cat("Median distance to nearest school:", round(median(ff_pts$dist_to_school_km), 2), "km\n")