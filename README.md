# Spatial Analysis of Fast-Food Distribution in Baltimore City

## Overview
This repository contains spatial analysis scripts and data examining the distribution patterns of fast-food establishments in Baltimore City at the census tract level. The analysis focuses on spatial clustering, spatial scan statistics, and the relationship between fast-food locations and schools.

## Project Structure
- `Assignment-2.R`: Main analysis script that performs spatial autocorrelation, semivariogram analysis, and SaTScan cluster detection
- `SchoolProximity.R`: Script for analyzing the spatial relationship between schools and fast-food locations
- `data-prep.R`: Data cleaning and preparation script for fast-food location data
- `/data`: Directory containing input datasets
  - `fast-food-data-cleaned.xlsx`: Cleaned fast-food location data
  - `schools_data.csv`: Baltimore City school location data
- `/results`: Directory containing output maps and statistical results
  - `fastfood_points.png`: Map of all fast-food locations
  - `fastfood_counts.png`: Choropleth map of fast-food density by census tract
  - `lisa_clusters.png`: Map of Local Moran's I clusters
  - `satscan_clusters.png`: Map of significant clusters detected by SaTScan
  - `school_proximity_map.png`: Map showing relationship between schools and fast-food locations
  - `semivariogram_counts.png`: Semivariogram plot for spatial dependence
  - `spatial_statistics_results.txt`: Summary of statistical findings

## Key Findings
- Significant spatial clustering of fast-food establishments in Baltimore City (Moran's I = 0.2615, p < 0.001)
- Three statistically significant clusters identified through spatial scan statistics (p < 0.001)
- Fast-food density is 2.34 times higher within 500m of schools compared to areas outside school buffer zones
- 64.5% of fast-food locations are within 500m of schools, despite these areas comprising only 43.7% of the city's total area

## Methods
The analysis employs several spatial statistical methods:
1. Global and Local Spatial Autocorrelation (Moran's I)
2. Semivariogram analysis for spatial dependence
3. Purely Spatial Poisson Scan using SaTScan
4. School proximity analysis with 500m buffer zones
5. Distance-based correlation analysis

## Dependencies
- R packages: sf, readxl, dplyr, tidyr, tmap, spdep, geoR, smerc, tidycensus

## Usage
1. Clone the repository
2. Set working directory to the repository root
3. Run data-prep.R to clean the raw data
4. Run Assignment-2.R for the main spatial analysis
5. Run SchoolProximity.R for the school-focused analysis

## Author
Marc Risney  
Johns Hopkins University  
Spring 2025, Spatial Applications for Public Health GIS