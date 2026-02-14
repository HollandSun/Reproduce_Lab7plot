# Source:
# Van Dyke, M. E., et al. (2020). Trends in County-Level COVID-19 Incidence in Counties With and Without a Mask Mandate — Kansas, June 1–August 23, 2020. MMWR, 69(47), 1777–1781. https://www.cdc.gov/mmwr/volumes/69/wr/mm6947e2.htm
# USAFacts COVID-19 Data: https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/


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
    values_to = "cumulative_cases") %>%
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

# Define mask mandate counties
# From CDC MMWR paper (Van Dyke et al., 2020), footnote we know the name of mandate countries


mask_county_names <- c("Allen", "Atchison", "Bourbon", "Crawford", "Dickinson",
                       "Douglas", "Franklin", "Geary", "Gove", "Harvey",
                       "Jewell", "Johnson", "Mitchell", "Montgomery", "Morris",
                       "Pratt", "Reno", "Republic", "Saline", "Scott",
                       "Sedgwick", "Shawnee", "Stanton", "Wyandotte")

kansas_daily <- kansas_daily %>%
  mutate(
    county_base = str_remove(`County Name`, " County$"),
    mask_mandate = ifelse(county_base %in% mask_county_names, "Mask", "No Mask"))



kansas_daily <- kansas_daily %>%
  left_join(kansas_pop %>% select(countyFIPS, population), by = "countyFIPS")
write_csv(kansas_daily, "kansas_case_jul05_aug03_2020.csv")

