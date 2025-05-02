# Spatial Analysis of Fast-Food Distribution in Baltimore City

## Overview
This repository contains the spatial analysis of fast-food locations in Baltimore City at the census tract level. The analysis investigates spatial clustering patterns, identifies statistically significant clusters, and examines the relationship between fast-food establishments and schools.

## Assignment
This project fulfills the requirements for Assignment 2 in the Spatial Applications IV (Public Health GIS) course, Spring 2025. The full assignment description is available [here](Assignment2.pdf).

## Project Structure
- `Assignment-2.R`: Main analysis script performing spatial autocorrelation, semivariogram analysis, and SaTScan cluster detection
- `SchoolProximity.R`: Script for analyzing spatial relationship between schools and fast-food locations
- `data-prep.R`: Data cleaning and preparation script for fast-food location data
- `/data`: Directory containing input datasets
  - `fast-food-data-cleaned.xlsx`: Cleaned fast-food location data
  - `schools_data.csv`: Baltimore City school location data
- `/results`: Directory containing output maps and statistical results
  - Maps: Point distribution, LISA clusters, SaTScan clusters, choropleth maps
  - `spatial_statistics_results.txt`: Summary of statistical findings

## Key Findings
- Significant spatial clustering of fast-food establishments (Moran's I = 0.2615, p < 0.001)
- Three statistically significant clusters identified through spatial scan statistics
- Fast-food density is 2.34 times higher within 500m of schools
- 64.5% of fast-food locations are within 500m of schools, despite these areas comprising only 43.7% of the city's total area
- Spatial dependence extends up to approximately 6856 meters (4.3 miles)

## Methods
The analysis employs several spatial statistical techniques:
1. Global and Local Spatial Autocorrelation (Moran's I)
2. Semivariogram analysis for spatial dependence assessment
3. Purely Spatial Poisson Scan using SaTScan
4. School proximity analysis with 500m buffer zones
5. Spatial visualization using tmap package

## Dependencies
- R packages: sf, readxl, dplyr, tidyr, tmap, spdep, geoR, smerc, tidycensus

## Usage
1. Clone the repository
2. Ensure all required R packages are installed
3. Run `data-prep.R` to clean the raw data
4. Run `Assignment-2.R` for the main spatial analysis
5. Run `SchoolProximity.R` for the school-focused analysis

## Author
Marc Risney  
Johns Hopkins University  
Spring 2025, Spatial Applications for Public Health GIS

## Repository
[https://github.com/mrisney/spatial-applications-assignment-two](https://github.com/mrisney/spatial-applications-assignment-two)