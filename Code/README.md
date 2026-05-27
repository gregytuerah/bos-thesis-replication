# Read this first before doing your code!

This directory contains the R scripts used to reproduce the empirical analysis
and manuscript figures for:

**Longer Exposure, Better Outcomes: Evidence from Indonesia's BOS Program**  
Gregy Tuerah  
Harris School of Public Policy, University of Chicago

The analysis uses Indonesia Family Life Survey (IFLS) data. Raw IFLS files are
not included in this repository and must be downloaded separately from RAND.
See [`../Data/README.md`](../Data/README.md) for access instructions and the
required folder structure.

## Script Guide

| Script | Purpose | Main Outputs |
| --- | --- | --- |
| `00_Project_Setup.R` | Defines the project setup and creates the required output directories. | Local folder structure |
| `01_Identification.R` | Harmonizes IFLS records, identifies siblings and origin households, constructs potential BOS exposure and schooling outcomes, and defines the analytic cohorts. | Processed analytic data in `Data/Processed/` |
| `01b_Controls.R` | Constructs household, parental education, location, and expenditure controls used in the analysis. | Controlled analytic dataset in `Data/Processed/` |
| `02_Balance.R` | Produces descriptive balance statistics for the exposure cohorts. | Balance table in `Output/Tables/` |
| `03_Regression.R` | Estimates the main origin-household fixed-effects models for senior-high attainment and completed years of schooling. | Main regression tables in `Output/Tables/` and `Paper/source/` |
| `04_HTE-Robustness.R` | Estimates heterogeneity and sensitivity specifications and generates the heterogeneity figure. | Robustness and HTE tables in `Output/Tables/`; heterogeneity figure in `Output/Figures/` and `Paper/source/` |
| `05_Manuscript_Figures.R` | Generates the analytic-sample origin map and BOS implementation-intensity map reported in the manuscript. | Map figures in `Output/Figures/` and `Paper/source/` |
