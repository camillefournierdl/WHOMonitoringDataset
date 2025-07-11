library(tidyverse)

whoDataset2011 <- read.csv("dataWHO/datasetWHO2011.csv", sep = ";")

whoDataset2011 <- whoDataset2011 %>%
  mutate(
    totalNumber = nbLocations %>%
      # 1. extract all digit‐runs as a list of character vectors
      str_extract_all("\\d+") %>%
      # 2. convert each to numeric
      map(~ as.numeric(.x)) %>%
      # 3. sum each vector
      map_dbl(sum)
  )

whoDataset2011row <- whoDataset2011 %>%
  # 1️⃣ extract numeric start/end
  mutate(
    start = str_extract(Year, "^[0-9]{4}") %>% as.integer(),
    end   = str_extract(Year, "(?<=-)[0-9]{4}$") %>% as.integer()
  ) %>%
  # 2️⃣ where there was no “-YYYY”, make end = start
  mutate(end = if_else(is.na(end), start, end)) %>%
  # 3️⃣ build a list of years for each row
  rowwise() %>%
  mutate(Year = list(seq(start, end))) %>%
  ungroup() %>%
  # 4️⃣ unnest so each year becomes its own row
  unnest(Year) %>%
  # 5️⃣ clean up
  select(-start, -end)

# 1. Aggregate stations per pollutant per year
df_agg <- whoDataset2011row %>%
  # ensure year is numeric
  mutate(year = as.integer(Year)) %>%
  group_by(Year, pollutant) %>%
  summarise(totalStations = sum(totalNumber, na.rm = TRUE), .groups = "drop")

# 2. Compute cumulative sum by pollutant
df_cum <- df_agg %>%
  arrange(pollutant, Year) %>%         # ensure correct order
  group_by(pollutant) %>%              
  mutate(cumStations = cumsum(totalStations)) %>%
  ungroup()

# 3a. Single‐panel cumulative lines
# ggplot(df_cum, aes(x = Year, y = cumStations, color = pollutant)) +
#   geom_line(size = 1) +
#   geom_point(size = 2) +
#   scale_x_continuous(breaks = sort(unique(df_cum$Year))) +
#   theme_minimal(base_size = 14) +
#   labs(
#     title = "Cumulative Number of Monitoring Stations by Pollutant",
#     x     = "Year",
#     y     = "Cumulative # of Stations",
#     color = "Pollutant"
#   )

#### counting cities instead
city_first <- whoDataset2011row %>%
  # just need one row per pollutant–city–year
  distinct(pollutant, City, Year) %>%
  # coerce to integer so min() works as expected
  mutate(year = as.integer(Year)) %>%
  # for each pollutant×city, find when it first shows up
  group_by(pollutant, City) %>%
  summarize(first_year = min(year), .groups = "drop")

years      <- sort(unique(as.integer(whoDataset2011row$Year)))
pollutants <- unique(whoDataset2011row$pollutant)

poll_years <- expand_grid(pollutant = pollutants, year = years)

cum_cities_2011 <- poll_years %>%
  # bring in each city’s first appearance
  left_join(city_first, by = "pollutant") %>%
  # for each pollutant×year, count how many first_year <= that year
  group_by(pollutant, year) %>%
  summarize(cumCities = sum(first_year <= year, na.rm = TRUE),
            .groups = "drop")

ggplot(cum_cities_2011, aes(x = year, y = cumCities, color = pollutant)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = years) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Cumulative Number of Cities Monitored per Pollutant",
    x     = "Year",
    y     = "Cumulative # of Cities",
    color = "Pollutant"
  )

### then for 2014 database

whoDataset2014 <- read.csv("dataWHO/datasetWHO2014.csv", sep = ";")

whoDataset2014_long <- whoDataset2014 %>%
  rename(concentrationpm25 = AnnualmeanPM25) %>% 
  pivot_longer(
    cols        = starts_with("concentration"),
    names_to    = "pollutant",
    names_prefix= "concentration",
    values_to   = "concentration"
  ) %>%
  # if you want nicer pollutant labels
  mutate(
    pollutant = case_when(
      pollutant == "pm10"  ~ "PM10",
      pollutant == "pm25"  ~ "PM2.5",
      TRUE                 ~ pollutant
    )
  )

whoDataset2014_long <- subset(whoDataset2014_long, (pollutant == "PM10" & convertedPM10 != TRUE) | (pollutant == "PM2.5" & convertedPM25 != TRUE)) %>% 
  rename(City = City.station)

whoDataset2014_long_row <- whoDataset2014_long %>%
  # 1️⃣ extract numeric start/end
  mutate(
    start = str_extract(yearMerged, "^[0-9]{4}") %>% as.integer(),
    end   = str_extract(yearMerged, "(?<=-)[0-9]{4}$") %>% as.integer()
  ) %>%
  # 2️⃣ where there was no “-YYYY”, make end = start
  mutate(end = if_else(is.na(end), start, end)) %>%
  # 3️⃣ build a list of years for each row
  rowwise() %>%
  mutate(yearMerged = list(seq(start, end))) %>%
  ungroup() %>%
  # 4️⃣ unnest so each year becomes its own row
  unnest(yearMerged) %>%
  # 5️⃣ clean up
  select(-start, -end)

city_first <- whoDataset2014_long_row %>%
  # just need one row per pollutant–city–year
  distinct(pollutant, City, yearMerged) %>%
  # coerce to integer so min() works as expected
  mutate(year = as.integer(yearMerged)) %>%
  # for each pollutant×city, find when it first shows up
  group_by(pollutant, City) %>%
  summarize(first_year = min(year), .groups = "drop")

years      <- sort(unique(as.integer(whoDataset2014_long_row$yearMerged)))
pollutants <- unique(whoDataset2014_long_row$pollutant)

poll_years <- expand_grid(pollutant = pollutants, year = years)

cum_cities_2014 <- poll_years %>%
  # bring in each city’s first appearance
  left_join(city_first, by = "pollutant") %>%
  # for each pollutant×year, count how many first_year <= that year
  group_by(pollutant, year) %>%
  summarize(cumCities = sum(first_year <= year, na.rm = TRUE),
            .groups = "drop")

ggplot(cum_cities_2014, aes(x = year, y = cumCities, color = pollutant)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = years) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Cumulative Number of Cities Monitored per Pollutant",
    x     = "Year",
    y     = "Cumulative # of Cities",
    color = "Pollutant"
  )

##### Now for 2016
whoDataset2016 <- read.csv("dataWHO/datasetWHO2016.csv", sep = ";")

whoDataset2016_long <- whoDataset2016 %>%
  rename(concentrationpm25 = AnnualmeanPM25) %>% 
  pivot_longer(
    cols        = starts_with("concentration"),
    names_to    = "pollutant",
    names_prefix= "concentration",
    values_to   = "concentration"
  ) %>%
  # if you want nicer pollutant labels
  mutate(
    pollutant = case_when(
      pollutant == "pm10"  ~ "PM10",
      pollutant == "pm25"  ~ "PM2.5",
      TRUE                 ~ pollutant
    )
  )

whoDataset2016_long <- subset(whoDataset2016_long, (pollutant == "PM10" & convertedpm10 != TRUE) | (pollutant == "PM2.5" & convertedpm25 != TRUE)) %>% 
  rename(City = City.Town)

city_first <- whoDataset2016_long %>%
  # just need one row per pollutant–city–year
  distinct(pollutant, City, Year) %>%
  # coerce to integer so min() works as expected
  mutate(year = as.integer(Year)) %>%
  # for each pollutant×city, find when it first shows up
  group_by(pollutant, City) %>%
  summarize(first_year = min(year), .groups = "drop")

years      <- sort(unique(as.integer(whoDataset2016_long$Year)))
pollutants <- unique(whoDataset2016_long$pollutant)

poll_years <- expand_grid(pollutant = pollutants, year = years)

cum_cities_2016 <- poll_years %>%
  # bring in each city’s first appearance
  left_join(city_first, by = "pollutant") %>%
  # for each pollutant×year, count how many first_year <= that year
  group_by(pollutant, year) %>%
  summarize(cumCities = sum(first_year <= year, na.rm = TRUE),
            .groups = "drop")

ggplot(cum_cities_2016, aes(x = year, y = cumCities, color = pollutant)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = years) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Cumulative Number of Cities Monitored per Pollutant",
    x     = "Year",
    y     = "Cumulative # of Cities",
    color = "Pollutant"
  )

##### Now for 2018
whoDataset2018 <- read.csv("dataWHO/datasetWHO2018.csv", sep = ";")

whoDataset2018_long <- whoDataset2018 %>%
  # rename(concentrationpm25 = AnnualmeanPM25) %>% 
  pivot_longer(
    cols        = starts_with("concentration"),
    names_to    = "pollutant",
    names_prefix= "concentration",
    values_to   = "concentration"
  ) %>%
  # if you want nicer pollutant labels
  mutate(
    pollutant = case_when(
      pollutant == "pm10"  ~ "PM10",
      pollutant == "pm25"  ~ "PM2.5",
      TRUE                 ~ pollutant
    )
  )

whoDataset2018_long <- subset(whoDataset2018_long, (pollutant == "PM10" & convertedpm10 != TRUE) | (pollutant == "PM2.5" & convertedpm25 != TRUE)) %>% 
  rename(City = City.Town)

city_first <- whoDataset2018_long %>%
  # just need one row per pollutant–city–year
  distinct(pollutant, City, Year) %>%
  # coerce to integer so min() works as expected
  mutate(year = as.integer(Year)) %>%
  # for each pollutant×city, find when it first shows up
  group_by(pollutant, City) %>%
  summarize(first_year = min(year), .groups = "drop")

years      <- sort(unique(as.integer(whoDataset2018_long$Year)))
pollutants <- unique(whoDataset2018_long$pollutant)

poll_years <- expand_grid(pollutant = pollutants, year = years)

cum_cities_2018 <- poll_years %>%
  # bring in each city’s first appearance
  left_join(city_first, by = "pollutant") %>%
  # for each pollutant×year, count how many first_year <= that year
  group_by(pollutant, year) %>%
  summarize(cumCities = sum(first_year <= year, na.rm = TRUE),
            .groups = "drop")

ggplot(cum_cities_2018, aes(x = year, y = cumCities, color = pollutant)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = years) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Cumulative Number of Cities Monitored per Pollutant",
    x     = "Year",
    y     = "Cumulative # of Cities",
    color = "Pollutant"
  )

##### Now for 2022
whoDataset2022 <- read.csv("dataWHO/datasetWHO2022.csv", sep = ";")

whoDataset2022_long <- whoDataset2022 %>%
  # rename(concentrationpm25 = AnnualmeanPM25) %>% 
  pivot_longer(
    cols        = starts_with("concentration"),
    names_to    = "pollutant",
    names_prefix= "concentration",
    values_to   = "concentration"
  ) %>%
  # if you want nicer pollutant labels
  mutate(
    pollutant = case_when(
      pollutant == "pm10"  ~ "PM10",
      pollutant == "pm25"  ~ "PM2.5",
      TRUE                 ~ pollutant
    )
  )

whoDataset2022_long <- subset(whoDataset2022_long,  !is.na(concentration)) %>% 
  rename(City = City.Town)

city_first <- whoDataset2022_long %>%
  # just need one row per pollutant–city–year
  distinct(pollutant, City, Year) %>%
  # coerce to integer so min() works as expected
  mutate(year = as.integer(Year)) %>%
  # for each pollutant×city, find when it first shows up
  group_by(pollutant, City) %>%
  summarize(first_year = min(year), .groups = "drop")

years      <- sort(unique(as.integer(whoDataset2022_long$Year)))
pollutants <- unique(whoDataset2022_long$pollutant)

poll_years <- expand_grid(pollutant = pollutants, year = years)

cum_cities_2022 <- poll_years %>%
  # bring in each city’s first appearance
  left_join(city_first, by = "pollutant") %>%
  # for each pollutant×year, count how many first_year <= that year
  group_by(pollutant, year) %>%
  summarize(cumCities = sum(first_year <= year, na.rm = TRUE),
            .groups = "drop")

ggplot(cum_cities_2022, aes(x = year, y = cumCities, color = pollutant)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = years) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Cumulative Number of Cities Monitored per Pollutant",
    x     = "Year",
    y     = "Cumulative # of Cities",
    color = "Pollutant"
  )

##### Now for 2024
whoDataset2024 <- read.csv("dataWHO/datasetWHO2024.csv", sep = ";")

whoDataset2024_long <- whoDataset2024 %>%
  # rename(concentrationpm25 = AnnualmeanPM25) %>% 
  pivot_longer(
    cols        = starts_with("concentration"),
    names_to    = "pollutant",
    names_prefix= "concentration",
    values_to   = "concentration"
  ) %>%
  # if you want nicer pollutant labels
  mutate(
    pollutant = case_when(
      pollutant == "pm10"  ~ "PM10",
      pollutant == "pm25"  ~ "PM2.5",
      TRUE                 ~ pollutant
    )
  )

whoDataset2024_long <- subset(whoDataset2024_long,  !is.na(concentration)) %>% 
  rename(City = City.Town)

city_first <- whoDataset2024_long %>%
  # just need one row per pollutant–city–year
  distinct(pollutant, City, Year) %>%
  # coerce to integer so min() works as expected
  mutate(year = as.integer(Year)) %>%
  # for each pollutant×city, find when it first shows up
  group_by(pollutant, City) %>%
  summarize(first_year = min(year), .groups = "drop")

years      <- sort(unique(as.integer(whoDataset2024_long$Year)))
pollutants <- unique(whoDataset2024_long$pollutant)

poll_years <- expand_grid(pollutant = pollutants, year = years)

cum_cities_2024 <- poll_years %>%
  # bring in each city’s first appearance
  left_join(city_first, by = "pollutant") %>%
  # for each pollutant×year, count how many first_year <= that year
  group_by(pollutant, year) %>%
  summarize(cumCities = sum(first_year <= year, na.rm = TRUE),
            .groups = "drop")

ggplot(cum_cities_2024, aes(x = year, y = cumCities, color = pollutant)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = years) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Cumulative Number of Cities Monitored per Pollutant",
    x     = "Year",
    y     = "Cumulative # of Cities",
    color = "Pollutant"
  )

#### then at the country level

# 1. For each pollutant–country, find the first year it appears
country_first2011 <- whoDataset2011row %>%
  distinct(pollutant, Country, Year) %>%              # (was city → now country)
  mutate(year = as.integer(Year)) %>%
  group_by(pollutant, Country) %>%
  summarize(first_year = min(year), .groups = "drop")

country_first2014 <- whoDataset2014_long_row %>%
  distinct(pollutant, Country, yearMerged) %>%              # (was city → now country)
  mutate(year = as.integer(yearMerged)) %>%
  group_by(pollutant, Country) %>%
  summarize(first_year = min(year), .groups = "drop")

country_first2016 <- whoDataset2016_long %>%
  distinct(pollutant, Country, Year) %>%              # (was city → now country)
  mutate(year = as.integer(Year)) %>%
  group_by(pollutant, Country) %>%
  summarize(first_year = min(year), .groups = "drop")

country_first2018 <- whoDataset2018_long %>%
  distinct(pollutant, Country, Year) %>%              # (was city → now country)
  mutate(year = as.integer(Year)) %>%
  group_by(pollutant, Country) %>%
  summarize(first_year = min(year), .groups = "drop")

country_first2022 <- whoDataset2022_long %>%
  distinct(pollutant, Country, Year) %>%              # (was city → now country)
  mutate(year = as.integer(Year)) %>%
  group_by(pollutant, Country) %>%
  summarize(first_year = min(year), .groups = "drop")

country_first2024 <- whoDataset2024_long %>%
  distinct(pollutant, Country, Year) %>%              # (was city → now country)
  mutate(year = as.integer(Year)) %>%
  group_by(pollutant, Country) %>%
  summarize(first_year = min(year), .groups = "drop")


allCountriesFirsts <- rbind(country_first2011, country_first2014, country_first2016, country_first2018, country_first2022, country_first2024)
# allCountriesFirsts <- country_first2024

#### harmonize pollutant
allCountriesFirsts$pollutant <- ifelse(allCountriesFirsts$pollutant == "no2", "NO2",
                                       ifelse(allCountriesFirsts$pollutant == "pm10", "PM10",
                                              ifelse(allCountriesFirsts$pollutant == "pm25", "PM2.5", allCountriesFirsts$pollutant)))
                                              
allCountriesFirsts <- allCountriesFirsts %>%
  group_by(Country, pollutant) %>%
  slice_min(first_year, with_ties = FALSE) %>%
  ungroup()
  
#### add income level? and check how different it is by removing high income

# add ISO from country name
library(countrycode)

allCountriesFirsts$Country <- ifelse(allCountriesFirsts$Country == "T?ºrkiye", "Turkey", allCountriesFirsts$Country)

allCountriesFirsts_ISO <- allCountriesFirsts %>% 
  mutate(country_iso3 = countrycode(
    sourcevar   = Country,
    origin      = "country.name",
    destination = "iso3c"
  ))

# merge income level
# load world bank data
dataWbk <- read.csv('data/worldBankIncomeGroups.csv', sep = ';', allowEscapes = T)

dataWbkToMerge <- dataWbk %>% select(Code, Income.group) %>% 
  rename(country_iso3 = Code)

# organize levels of the incomegroup variable
dataWbkToMerge$Income.group <- ifelse(dataWbkToMerge$Income.group %in% c("High income"), "1. High Income",
                                      ifelse(dataWbkToMerge$Income.group %in% c("Upper middle income"), "2. Upper middle income",
                                             ifelse(dataWbkToMerge$Income.group %in% c("Lower middle income"), "3. Lower middle income",
                                                    ifelse(dataWbkToMerge$Income.group %in% c("Low income"), "4. Low income", ""))))


# Numbers for different income groups: 
table(dataWbkToMerge$Income.group)

dataWbkToMerge$Income.group[dataWbkToMerge$CTR_MN_ISO == "VEN"] <- "2. Upper middle income"

allCountriesFirsts_ISO_merged <- merge(allCountriesFirsts_ISO, dataWbkToMerge, by = "country_iso3", all.x = T, all.y = F)

# 2. Build all pollutant × year combos
years      <- sort(unique(as.integer(allCountriesFirsts_ISO_merged$first_year)))
pollutants <- unique(allCountriesFirsts_ISO_merged$pollutant)
incomeGroups <- unique(allCountriesFirsts_ISO_merged$Income.group)
poll_years <- expand_grid(pollutant = pollutants, year = years, Income.group = incomeGroups)

# 3. Count how many countries have appeared up to each year
cum_countries <- poll_years %>%
  left_join(allCountriesFirsts_ISO_merged, by = c("pollutant", "Income.group")) %>%
  group_by(pollutant, year, Income.group) %>%
  summarize(cumCountries = sum(first_year <= year, na.rm = TRUE),
            .groups = "drop")

cum_countries <- subset(cum_countries, Income.group != "")
cum_countries <- subset(cum_countries, pollutant == "PM2.5")

# 4. Plot
ggplot(cum_countries, aes(x = year, y = cumCountries, color = pollutant)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = years) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Cumulative Number of Countries Monitored per Pollutant",
    x     = "Year",
    y     = "Cumulative # of Countries",
    color = "Pollutant"
  )+
  facet_wrap(~Income.group)

# Now do it for all non high income coountry, cumulatively
allCountriesFirsts_ISO_merged <- subset(allCountriesFirsts_ISO_merged, Income.group != "1. High Income")
# allCountriesFirsts_ISO_merged <- subset(allCountriesFirsts_ISO_merged, pollutant == "PM2.5")

years      <- sort(unique(as.integer(allCountriesFirsts_ISO_merged$first_year)))
pollutants <- unique(allCountriesFirsts_ISO_merged$pollutant)
poll_years <- expand_grid(pollutant = pollutants, year = years)

# 3. Count how many countries have appeared up to each year
cum_countries <- poll_years %>%
  left_join(allCountriesFirsts_ISO_merged, by = c("pollutant")) %>%
  group_by(pollutant, year) %>%
  summarize(cumCountries = sum(first_year <= year, na.rm = TRUE),
            .groups = "drop")

# cum_countries <- subset(cum_countries, Income.group != "")
# cum_countries <- subset(cum_countries, pollutant == "PM2.5")

# 4. Plot
ggplot(cum_countries, aes(x = year, y = cumCountries, color = pollutant)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = years) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Cumulative Number of Countries Monitored per Pollutant",
    x     = "Year",
    y     = "Cumulative # of Countries",
    color = "Pollutant"
  )

### Cumulative for all pollutants together
allCountriesFirsts_ISO_merged <- merge(allCountriesFirsts_ISO, dataWbkToMerge, by = "country_iso3", all.x = T, all.y = F)
allCountriesFirsts_ISO_merged <- subset(allCountriesFirsts_ISO_merged, Income.group != "1. High Income")

first_by_country <- allCountriesFirsts_ISO_merged %>% 
  group_by(Country) %>% 
  summarise(first_year = min(first_year, na.rm = TRUE)) %>% 
  ungroup()

cumul_df <- first_by_country %>% 
  count(first_year) %>% 
  # ensure every year in your span appears
  complete(first_year = seq(min(first_year), max(first_year)), fill = list(n = 0)) %>% 
  arrange(first_year) %>% 
  mutate(cumulative = cumsum(n))

ggplot(cumul_df, aes(x = first_year, y = cumulative)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(min(cumul_df$first_year), max(cumul_df$first_year), by = 1)) +
  labs(
    x = "Year",
    y = "Cumulative number of countries monitored",
    title = "Growth in Air Pollution Monitoring Coverage Over Time"
  ) +
  theme_minimal()
