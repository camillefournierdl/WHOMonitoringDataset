# WHO Monitoring Dataset – Harmonized

## Authors
- [Andina Nabilla](https://github.com/andinazn) — Institute of Science, Technology and Policy (ISTP), ETH Zurich, Switzerland
  - Harmonization, City name matching, Geolocation matching
- [Camille Fournier de Lauriere](https://github.com/camillefournierdl) — Institute of Science, Technology and Policy (ISTP), ETH Zurich, Switzerland
  - Concept, First explorations and Supervision 

## Description
This repository is used by researchers at ETH Zurich to harmonize and combine information from different versions of the World Health Organization’s Air Quality Database. It provides a dataset of monitoring locations reported in the WHO dataset for 2011, 2014, 2016, 2018, 2022, and 2024 with a standardized format (id, region, country, city, year, version, pollutant, presence, concentration, longitude, latitude, nb_locations, reference, income_level).

## Methodological Remarks
- Region names have been standardised from the WHO area code into region and income level (if any)
- Country names have been standardised with the R library countrycode
- City names have been standardised using fuzzy matching (after being grouped by countries for better accuracy)
- Geolocation matching (longitude, latitude) was done using the R library tidygeocoder (openstreetmap), using the standardized city names, in 3 stages.
  - Stage 1: Automatic matching (country, city names)
  - Stage 2: Automatic matching (country, non-city names like landmarks), for example, Emu River, La Trobe Valley, etc
  - Stage 3: manually review & revise city names so they can be searchable

## Repository Structure
```
WHOMonitoringDataset/
├─ dataWHO/                         # Raw WHO input files (read-only)
│  ├─ datasetWHO2011.csv
│  ├─ datasetWHO2014.csv
│  ├─ datasetWHO2016.csv
│  ├─ datasetWHO2018.csv
│  ├─ datasetWHO2022.csv
│  └─ datasetWHO2024.csv
│
├─ data_processed/                  # Final harmonised outputs
│  ├─ datasetWHO-consolidated.csv
│  └─ datasetWHO-consolidated-geocoded.csv
│
├─ cache/                           # Caches & review sheets (intermediate)
│  ├─ cache_geocoded_city_country.csv
│  ├─ cache_geocoded_city_country_secondwave.csv
│  ├─ cache_geocoded_city_country_final.csv
│  ├─ geo_cache_failed_wave1.csv
│  ├─ geo_cache_failed_wave2.csv
│  ├─ geo_cache_success_wave1.csv
│  ├─ geo_cache_success_wave2.csv
│  ├─ geo_cache_manual_review.csv
│  ├─ geo_cache_manual_review_fixed.csv
│  ├─ manual_city_review_0.1_to_0.25.csv
│  └─ manual_city_review_confirmed.csv
│
├─ scripts/
│  └─ consolidateWHO-v2.Rmd         # Main processing & geocoding pipeline
│
└─ README.md
```

## Workflow Summary
| Step | Task                                          | Tools                                |
| ---- | --------------------------------------------- | ------------------------------------ |
| 1    | Import WHO releases (2011-2024)               | `readr`, `janitor`                   |
| 2    | Standardise column names & fields             | `dplyr`, `stringr`                   |
| 3    | Standardise countries                         | `countrycode`
| 4    | Standardise cities (automated fuzzy matching) | `stringdist`                         |
| 5    | Manual city name review                       | CSV review sheet                     |
| 6    | Geocoding — Stage 1                           | tidygeocoder (city, country)         |
| 7    | Geocoding — Stage 2                           | tidygeocoder (landmarks)             |
| 8    | Geocoding — Stage 3                           | Manual corrections + re-geocode      |
| 9    | Store coordinates in new fields               | `new_latitude`, `new_longitude`      |
| 10   | Export final harmonised dataset               | `.csv`                               |

## Notes
- The script uses batched geocoding with caching to comply with API rate limits
- The process is deterministic once caches are stored
- All manual decisions (manual city review) are stored in CSV inside cache folder for transparency

## Limitations: 
- Still some locations missing:
  - 8035/8315 unique (country, city) pairing found, 280 missing. Meaning, 5346/237958 (2.3%) records don’t have longitude & latitude information.
However, please keep in mind that a significant portion of missing locations originates from the United States, as city names from different states often share the same names. This is why OpenStreetMap can’t find the location when including the state code.
- Since there’s no record ID or exact monitoring in the original dataset, the consolidated data might contain duplicate records. Future update could focus on identifying duplicates and removing them.

## To Improve:
- More manual review
- Detect duplicate records
- Improve (country, city) matching especially for United States (they often have same city names in different states)
- Explore other geocoding library/API for better detection

