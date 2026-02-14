# We attempted to find the original dataset behind the chart presented
# by KDHE Secretary Dr. Lee Norman on August 5, 2020
# However, we could not locate the exact dataset used to generate that chart
# through publicly available channels.
#
# But a subsequent CDC study published in MMWR — Van Dyke et al. (2020),
# "Trends in County-Level COVID-19 Incidence in Counties With and Without a
# Mask Mandate — Kansas, June 1–August 23, 2020" — appears to involve the
# same underlying data. This study was conducted in collaboration with the
# Kansas Department of Health and Environment (KDHE) and classified counties
# using data from the Kansas Health Institute (KHI).
#
# Regarding the source of case and population data, and 、
# the classification of mask-mandate counties,
# the paper all stated in thefootnot.
#
# So we tring to use the same dataset from USAfact webset(by the way it is also a good data visulization webset)
# DATA SOURCES:
#   Cases:      https://static.usafacts.org/public/data/covid-19/covid_confirmed_usafacts.csv
#   Population: https://static.usafacts.org/public/data/covid-19/covid_county_population_usafacts.csv
#   County classification: CDC MMWR, Van Dyke et al. (2020)

#   https://www.cdc.gov/mmwr/volumes/69/wr/mm6947e2.htm


library(tidyverse)
library(lubridate)

# cases data
cases_raw <- read_csv(
  "https://static.usafacts.org/public/data/covid-19/covid_confirmed_usafacts.csv",
  show_col_types = FALSE)

# population data
pop_raw <- read_csv(
  "https://static.usafacts.org/public/data/covid-19/covid_county_population_usafacts.csv",
  show_col_types = FALSE)

#  Filter Kansas counties
kansas_cases <- cases_raw %>%
  filter(State == "KS",countyFIPS != 0) %>%
  pivot_longer(
    cols = -c(countyFIPS, `County Name`, State, StateFIPS),
    names_to = "date",
    values_to = "cumulative_cases"
  ) %>%
  mutate(date = ymd(date)) %>%
  select(countyFIPS, `County Name`, date, cumulative_cases)

# Filter Kansas population data
kansas_pop <- pop_raw %>%
  filter(State == "KS",countyFIPS != 0) %>%
  select(countyFIPS, `County Name`, population)

# Calculate daily new cases
# Start from 7/05 (7 extra days before 7/12 to allow 7-day rolling average)
kansas_daily <- kansas_cases %>%
  filter(date >= as.Date("2020-07-05"),
         date <= as.Date("2020-08-03")) %>%
  arrange(countyFIPS, date) %>%
  group_by(countyFIPS, `County Name`) %>%
  mutate(daily_new_cases = cumulative_cases - lag(cumulative_cases)) %>%
  ungroup() %>%
  filter(!is.na(daily_new_cases))

# --- Step 6: Define mask mandate counties ---
# From CDC MMWR paper (Van Dyke et al., 2020), footnote we know the name of mandate countries
# Note: USAFacts county names have " County" suffix (e.g., "Allen County"),
# so we remove the suffix before matching.
mask_county_names <- c("Allen", "Atchison", "Bourbon", "Crawford", "Dickinson",
                       "Douglas", "Franklin", "Geary", "Gove", "Harvey",
                       "Jewell", "Johnson", "Mitchell", "Montgomery", "Morris",
                       "Pratt", "Reno", "Republic", "Saline", "Scott",
                       "Sedgwick", "Shawnee", "Stanton", "Wyandotte")

kansas_daily <- kansas_daily %>%
  mutate(
    county_base = str_remove(`County Name`, " County$"),
    mask_mandate = ifelse(county_base %in% mask_county_names, "Mask", "No Mask")
  )



kansas_daily <- kansas_daily %>%
  left_join(kansas_pop %>% select(countyFIPS, population), by = "countyFIPS")
write_csv(kansas_daily, "kansas_usafacts_jul05_aug03_2020.csv")

