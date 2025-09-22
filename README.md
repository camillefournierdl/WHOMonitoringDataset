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

## Limitations: 
- Still some locations missing:
  - 6791/7011 unique (country, city) pairing found, 220 missing. Meaning, 2419/237958 (1%) records don’t have longitude & latitude information.
However, please keep in mind that a significant portion of missing locations originates from the United States, as city names from different states often share the same names. This is why OpenStreetMap can’t find the location when including the state code.

- Since there’s no record ID or exact monitoring in the original dataset, the consolidated data might contain duplicate records. Future update could focus on identifying duplicates and removing them.

