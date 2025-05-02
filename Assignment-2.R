# ============================================================
# Spatial Analysis of Fast-Food Distribution in Baltimore City
# Census Tract Level Analysis
# ============================================================

# 1) PACKAGES --------------------------------------------------
required <- c(
  "sf",         # For spatial data handling
  "readxl",     # For reading Excel files
  "dplyr",      # For data manipulation
  "tidyr",      # For data cleaning functions
  "tmap",       # For thematic mapping
  "spdep",      # For spatial dependence measures
  "geoR",       # For semivariogram analysis
  "smerc",      # For spatial scan statistics
  "tidycensus"  # For census data acquisition
)

# Load required packages
invisible(lapply(required, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg)
  library(pkg, character.only = TRUE)
}))

# 2) DEFINE PATHS ----------------------------------------------
# Set working directory
setwd("/Users/marcrisney/Projects/jhu/mas/Spring2025/SpatialApplications/Assignment2")

# Define input and output paths
tracts_shp  <- "baltimore_census_tracts/2020_Census_Tracts_(Census_TIGER).shp"
city_shp    <- "baltimore_city_boundary/baltimore_city_boundary.shp"
ff_path     <- "data/fast-food-data-cleaned.xlsx"
output_dir  <- "results"
# Ensure results directory exists
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
case_file   <- file.path(output_dir, "satscan_cases.txt")
pop_file    <- file.path(output_dir, "satscan_pop.txt")
coords_file <- file.path(output_dir, "satscan_coords.txt")

# 3) READ SPATIAL DATA -----------------------------------------
# Read census tracts and city boundary
tracts_all <- st_read(tracts_shp, quiet = TRUE) %>%
  rename(GEOID = GEOID20) %>%  # Rename GEOID20 to GEOID for consistency
  st_transform(26918)          # Transform to UTM Zone 18N

balt_city <- st_read(city_shp, quiet = TRUE) %>%
  st_transform(26918)          # Transform to UTM Zone 18N

# Filter tracts to only those within Baltimore City
tracts_city <- tracts_all %>%
  filter(lengths(st_intersects(., balt_city)) > 0)

# Read and process fast-food locations
ff_df <- read_excel(ff_path)
ff_pts <- ff_df %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(26918) %>%
  # Filter to points within Baltimore City
  filter(lengths(st_intersects(., balt_city)) > 0)

# 4) FETCH CENSUS DATA -----------------------------------------
# Set Census API key (replace with your own key)
census_api_key("bc11de0cba4b6116207e20a7174be44be30bb006", install = FALSE)

# Get total population from ACS 2020
pop_df <- get_acs(
  geography = "tract", 
  state = "24",         # Maryland
  county = "510",       # Baltimore City
  variables = "B01003_001",  # Total population variable
  year = 2020, 
  geometry = FALSE
) %>% 
  select(GEOID, total_pop = estimate)

# 5) AGGREGATE POINTS TO TRACTS --------------------------------
# Count fast-food locations in each tract
ff_counts <- st_join(ff_pts, tracts_city, join = st_within) %>%
  st_drop_geometry() %>%
  count(GEOID, name = "fastfood_n")

# Join counts to tracts and add population data
tracts_city <- tracts_city %>%
  left_join(ff_counts, by = "GEOID") %>%
  left_join(pop_df, by = "GEOID") %>%
  mutate(
    # Replace NA with 0 for tracts with no fast-food
    fastfood_n = ifelse(is.na(fastfood_n), 0, fastfood_n),
    # Calculate rate per 1000 population
    ff_per_1000 = (fastfood_n / total_pop) * 1000
  )

# Create a complete dataset for analysis (no NA values)
tracts_city_complete <- tracts_city %>% 
  filter(!is.na(total_pop))

# 6) SPATIAL AUTOCORRELATION ANALYSIS --------------------------
# Create neighbors list based on queen contiguity
nb <- poly2nb(tracts_city_complete, queen = TRUE)
# Create spatial weights matrix
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# Calculate Global Moran's I for fast-food counts
moran_i <- moran.test(tracts_city_complete$fastfood_n, lw, zero.policy = TRUE)

# Calculate Local Moran's I for fast-food counts
lisa <- localmoran(tracts_city_complete$fastfood_n, lw, zero.policy = TRUE)
lisa_mat <- as.matrix(lisa)
p_col <- "Pr(z != E(Ii))"  # Column name for p-value

# Add LISA results to tracts
tracts_city_complete <- tracts_city_complete %>%
  mutate(
    Ii      = lisa_mat[, "Ii"],
    p_value = lisa_mat[, p_col],
    cluster = case_when(
      fastfood_n > mean(fastfood_n, na.rm = TRUE) & 
        Ii > 0 & p_value < 0.05 ~ "High-High",
      fastfood_n < mean(fastfood_n, na.rm = TRUE) & 
        Ii > 0 & p_value < 0.05 ~ "Low-Low",
      TRUE ~ "Not significant"
    )
  )

# 7) SEMIVARIOGRAM ANALYSIS -----------------------------------
# Prepare data for semivariogram
coords_mat <- st_coordinates(st_centroid(tracts_city_complete))
counts <- tracts_city_complete$fastfood_n

# Create geodata object
geod <- as.geodata(cbind(coords_mat, counts))

# Calculate empirical semivariogram
vario_emp <- variog(geod, max.dist = max(dist(coords_mat))/2)

# Fit spherical model to semivariogram
fit_sph <- variofit(
  vario_emp, 
  cov.model = "spherical",
  ini.cov.pars = c(var(counts, na.rm = TRUE), max(dist(coords_mat))/3),
  nugget = var(counts, na.rm = TRUE) * 0.1,
  weights = "cressie"
)

# 8) PREPARE SATSCAN INPUT FILES ------------------------------
# Prepare and write case file (fast-food counts)
write.table(
  tracts_city_complete %>% 
    st_drop_geometry() %>% 
    select(GEOID, fastfood_n),
  case_file, 
  row.names = FALSE, 
  col.names = FALSE, 
  sep = " "
)

# Prepare and write population file
write.table(
  tracts_city_complete %>% 
    st_drop_geometry() %>% 
    select(GEOID, total_pop),
  pop_file, 
  row.names = FALSE, 
  col.names = FALSE, 
  sep = " "
)

# Prepare and write coordinates file (tract centroids)
coords_df <- st_centroid(tracts_city_complete) %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  mutate(GEOID = tracts_city_complete$GEOID) %>% 
  select(GEOID, X, Y)

write.table(
  coords_df, 
  coords_file, 
  row.names = FALSE, 
  col.names = FALSE, 
  sep = " "
)

# 9) READ SATSCAN RESULTS & PERFORM SCAN ANALYSIS -------------
# Read the SaTScan input files for the analysis
df <- read.table(coords_file,
                 col.names = c("GEOID", "x", "y"),
                 stringsAsFactors = FALSE) %>%
  left_join(read.table(case_file,
                       col.names = c("GEOID", "cases")),
            by = "GEOID") %>%
  left_join(read.table(pop_file,
                       col.names = c("GEOID", "pop")),
            by = "GEOID") %>%
  filter(!is.na(pop), pop > 0)

# Extract coordinates, cases, and population for scan test
coords    <- as.matrix(df[, c("x", "y")])
cases     <- df$cases
pop       <- df$pop
exp_cases <- sum(cases) * (pop / sum(pop))

# Run purely-spatial Poisson scan
res <- scan.test(
  coords  = coords,      # Spatial coordinates
  cases   = cases,       # Number of cases (fast-food counts)
  pop     = pop,         # Population at risk
  ex      = exp_cases,   # Expected cases
  nsim    = 999,         # Number of Monte Carlo simulations
  alpha   = 0.05,        # Significance level
  longlat = FALSE        # Coordinates are projected (not lat/long)
)

# Filter clusters with p-value < 0.05
sig <- Filter(function(cl) cl$pvalue < 0.05, res$clusters)

# Create polygons for each significant cluster using convex hull
cluster_polys <- lapply(seq_along(sig), function(i) {
  # Get locations in this cluster
  locs <- sig[[i]]$locids
  pts  <- df[locs, c("x", "y")]
  # Calculate convex hull
  h    <- chull(pts)
  # Close the polygon by repeating the first point
  hull <- pts[c(h, h[1]), , drop = FALSE]
  # Create sf polygon object
  st_sf(
    cluster  = paste0("Cluster ", i),
    pvalue   = sig[[i]]$pvalue,
    cases    = sig[[i]]$cases,
    expected = sig[[i]]$ex,
    geometry = st_sfc(st_polygon(list(as.matrix(hull))), crs = 26918)
  )
}) %>% bind_rows()

# 10) CREATE AND SAVE MAPS FOR VISUALIZATION ------------------
# Set tmap to plot mode for saving static maps
tmap_mode("plot")

# Define a consistent compass style to use across all maps
compass_style <- tm_compass(
  type = "8star", 
  size = 1,          # Consistent size 
  position = c("left", "top"),
  color.dark = "black",
  color.light = "white"
)

# 10a. Map of fast-food point locations
ff_points_map <- tm_shape(balt_city, unit = "mi", unit.size = 1609.34) +
  tm_fill(col = "#ECECEC") +
  tm_borders(col = "black") +
  
  tm_shape(ff_pts) +
  tm_dots(col = "red", size = 0.1, alpha = 0.6) +
  
  tm_scalebar(
    breaks = c(0, 1, 2, 3, 4, 5),
    position = c("left", "bottom"),
    text.size = 0.6
  ) +
  compass_style +  # Use consistent compass
  tm_layout(
    main.title = "Fast-Food Locations in Baltimore City",
    main.title.size = 1.2,
    frame = FALSE
  )

# Print and save the map
print(ff_points_map)
tmap_save(ff_points_map, 
          filename = file.path(output_dir, "fastfood_points.png"),
          width = 8, height = 8, units = "in", dpi = 300)

# 10b. Choropleth map of fast-food counts by tract
ff_count_map <- tm_shape(tracts_city_complete, unit = "mi", unit.size = 1609.34) +
  tm_fill(
    col = "fastfood_n",
    palette = "YlOrBr",  
    style = "jenks",  
    n = 5,
    title = "Fast-Food Count"
  ) +
  tm_borders(col = "black", lwd = 0.2) +
  
  tm_shape(balt_city) +
  tm_borders(col = "black", lwd = 1) +
  
  tm_scalebar(
    breaks = c(0, 1, 2, 3, 4, 5),
    position = c("left", "bottom"),
    text.size = 0.6
  ) +
  compass_style +  # Use consistent compass
  tm_layout(
    main.title = "Fast-Food Counts by Census Tract\nBaltimore City, MD",
    main.title.size = 1.2,
    legend.outside = TRUE,
    frame = FALSE
  )

# Print and save the map
print(ff_count_map)
tmap_save(ff_count_map, 
          filename = file.path(output_dir, "fastfood_counts.png"),
          width = 8, height = 8, units = "in", dpi = 300)

# 10c. Map of Local Moran's I clusters
lisa_map <- tm_shape(tracts_city_complete, unit = "mi", unit.size = 1609.34) +
  tm_fill(
    col = "cluster",
    palette = c("High-High" = "#D73027", "Low-Low" = "#4575B4", "Not significant" = "grey90"),
    title = "LISA Clusters"
  ) +
  tm_borders(col = "black", lwd = 0.2) +
  
  tm_shape(balt_city) +
  tm_borders(col = "black", lwd = 1) +
  
  tm_scalebar(
    breaks = c(0, 1, 2, 3, 4, 5),
    position = c("left", "bottom"),
    text.size = 0.6
  ) +
  compass_style +  # Use consistent compass
  tm_layout(
    main.title = "Local Moran's I Clusters of Fast-Food Counts\nBaltimore City, MD",
    main.title.size = 1.2,
    legend.outside = TRUE,
    frame = FALSE
  )

# Print and save the map
print(lisa_map)
tmap_save(lisa_map, 
          filename = file.path(output_dir, "lisa_clusters.png"),
          width = 8, height = 8, units = "in", dpi = 300)

# 10d. Map of SaTScan clusters
satscan_map <- tm_shape(balt_city, unit = "mi", unit.size = 1609.34) +
  tm_fill(col = "#ECECEC", border.col = "black") +
  
  tm_shape(ff_pts) +
  tm_dots(col = "grey40", size = 0.05, alpha = 0.6) +
  
  tm_shape(cluster_polys) +
  tm_polygons(
    "pvalue",
    palette = "brewer.reds",
    alpha = 0.3,
    border.col = "red",
    border.lwd = 1.2,
    title = "Cluster p-value"
  ) +
  
  tm_scalebar(
    breaks = c(0, 1, 2, 3, 4, 5),
    position = c("left", "bottom"),
    text.size = 0.6
  ) +
  compass_style +  # Use consistent compass (already had similar settings)
  tm_layout(
    main.title = "Purely-Spatial Poisson Scan Clusters\nBaltimore City, MD",
    main.title.size = 1.2,
    legend.outside = TRUE,
    legend.outside.size = 0.2,
    frame = FALSE
  )

# Print and save the map
print(satscan_map)
tmap_save(satscan_map, 
          filename = file.path(output_dir, "satscan_clusters.png"),
          width = 8, height = 8, units = "in", dpi = 300)

# 11) PRINT SUMMARY STATISTICS AND SAVE SEMIVARIOGRAM ---------
# Print Global Moran's I results
cat("\n=== Global Moran's I ===\n")
cat("Moran's I statistic:", round(moran_i$estimate[1], 4), "\n")
cat("Expected value:", round(moran_i$estimate[2], 4), "\n")
cat("Variance:", round(moran_i$estimate[3], 4), "\n")
cat("p-value:", round(moran_i$p.value, 4), "\n\n")

# Print semivariogram model parameters
cat("=== Semivariogram model (spherical) ===\n")
cat("Nugget: ", round(fit_sph$nugget, 2), "\n")
cat("Partial sill: ", round(fit_sph$cov.pars[1], 2), "\n")
cat("Range: ", round(fit_sph$cov.pars[2], 0), " meters\n\n")

# Print significant clusters information
cat("=== Significant SaTScan Clusters ===\n")
for (i in seq_along(sig)) {
  cat(sprintf("Cluster %d: p-value = %.4f, Cases = %d, Expected = %.2f\n", 
              i, sig[[i]]$pvalue, sig[[i]]$cases, sig[[i]]$ex))
}

# Save semivariogram to file
png(file.path(output_dir, "semivariogram_counts.png"), width=800, height=600, res=100)
plot(vario_emp, 
     main = "Experimental Semivariogram – Fast-Food Counts", 
     xlab = "Distance (m)", 
     ylab = "Semivariance", 
     pch = 16, 
     col = "darkgreen")
lines(fit_sph, col = "red", lwd = 2)
dev.off()

# Write statistical results to text file
sink(file.path(output_dir, "spatial_statistics_results.txt"))
cat("SPATIAL ANALYSIS OF FAST-FOOD DISTRIBUTION IN BALTIMORE CITY\n")
cat("==========================================================\n\n")

cat("Data Summary:\n")
cat("Number of census tracts analyzed:", nrow(tracts_city_complete), "\n")
cat("Total fast-food locations in Baltimore City:", sum(tracts_city_complete$fastfood_n), "\n")
cat("Mean fast-food locations per tract:", round(mean(tracts_city_complete$fastfood_n), 2), "\n")
cat("Maximum fast-food locations in a tract:", max(tracts_city_complete$fastfood_n), "\n\n")

cat("=== Global Moran's I ===\n")
cat("Moran's I statistic:", round(moran_i$estimate[1], 4), "\n")
cat("Expected value:", round(moran_i$estimate[2], 4), "\n")
cat("Variance:", round(moran_i$estimate[3], 4), "\n")
cat("p-value:", round(moran_i$p.value, 4), "\n\n")

cat("=== Semivariogram model (spherical) ===\n")
cat("Nugget: ", round(fit_sph$nugget, 2), "\n")
cat("Partial sill: ", round(fit_sph$cov.pars[1], 2), "\n")
cat("Range: ", round(fit_sph$cov.pars[2], 0), " meters\n\n")

cat("=== Significant SaTScan Clusters ===\n")
for (i in seq_along(sig)) {
  cat(sprintf("Cluster %d: p-value = %.4f, Cases = %d, Expected = %.2f\n", 
              i, sig[[i]]$pvalue, sig[[i]]$cases, sig[[i]]$ex))
}

cat("\n=== LISA Cluster Summary ===\n")
lisa_summary <- table(tracts_city_complete$cluster)
for (cluster_type in names(lisa_summary)) {
  cat(cluster_type, ":", lisa_summary[cluster_type], "tracts\n")
}
sink()

# 12) APPROACH OUTLINE FOR SCHOOL CLUSTERING HYPOTHESIS -------
cat("\n=== Approach for Analyzing Fast-Food Clustering Around Schools ===\n")
cat("The following approach could be used to test whether fast-food locations")
cat(" cluster around schools, and how school level affects this clustering:\n\n")

cat("1. DATA COLLECTION:\n")
cat("   - Obtain point data for all schools in Baltimore City\n")
cat("   - Classify schools by level (elementary, middle, high school, etc.)\n")
cat("   - Ensure school and fast-food data are in the same coordinate system\n\n")

cat("2. PROXIMITY ANALYSIS:\n")
cat("   - Create buffer zones around schools at various distances (e.g., 400m, 800m, 1200m)\n")
cat("   - Count fast-food establishments within each buffer zone\n")
cat("   - Stratify analysis by school level to determine differences\n\n")

cat("3. SPATIAL INTENSITY ANALYSIS:\n")
cat("   - Use kernel density estimation to visualize fast-food density around schools\n")
cat("   - Calculate K-function or cross K-function to quantify clustering\n")
cat("   - Compare observed distribution to random/expected distribution\n\n")

cat("4. STATISTICAL MODELING:\n")
cat("   - Build regression models with distance to nearest school as predictor\n")
cat("   - Use interaction terms to test if school level moderates the effect\n")
cat("   - Control for other factors like population density and income\n\n")

cat("5. BIVARIATE SPATIAL ASSOCIATION:\n")
cat("   - Calculate bivariate Moran's I between school locations and fast-food density\n")
cat("   - Run separate analyses for each school level\n")
cat("   - Test if spatial correlation differs by school type\n\n")

cat("This approach would determine whether fast-food locations cluster around schools,")
cat(" and specifically identify which school levels show stronger association with")
cat(" fast-food clustering, providing evidence for targeted policy interventions.\n")