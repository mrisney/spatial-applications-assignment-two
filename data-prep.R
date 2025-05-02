# ==============================
# fast-food-data-clean.R
# ==============================

# 1. Load required packages
library(readxl)    # to read .xlsx
library(dplyr)     # for distinct() and piping
install.packages("writexl")
library(writexl)   # to write .xlsx

# 2. Define file paths
infile  <- "/Users/marcrisney/Projects/jhu/mas/Spring2025/SpatialApplications/Assignment2/data/fast-food-data.xlsx"
outfile <- "/Users/marcrisney/Projects/jhu/mas/Spring2025/SpatialApplications/Assignment2/data/fast-food-data-cleaned.xlsx"

# 3. Read in the data
ff <- read_excel(infile)

# 4. Remove exact duplicates (identical across all columns)
ff_no_exact <- ff %>%
  distinct()

# 5. From that, keep only one record per unique location
#    (i.e. drop rows with duplicate Longitude+Latitude)
ff_clean <- ff_no_exact %>%
  distinct(Longitude, Latitude, .keep_all = TRUE)

# 6. Save the cleaned dataset to Excel
write_xlsx(ff_clean, outfile)

# 7. Report
cat("Original rows:", nrow(ff), "\n")
cat("After removing exact duplicates:", nrow(ff_no_exact), "\n")
cat("After keeping one per location:",     nrow(ff_clean), "\n")
cat("Cleaned file written to:", outfile, "\n")
