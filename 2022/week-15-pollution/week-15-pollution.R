
# 0 Packages --------------------------------------------------------------

library(tidyverse)
library(countrycode)

# 1 Global ----------------------------------------------------------------


# 2 Data ------------------------------------------------------------------

data <- tidytuesdayR::tt_load('2022-04-12')

fuel_access <- data$fuel_access %>% 
  janitor::clean_names()
death_source <- data$death_source %>% 
  janitor::clean_names()

poll <- left_join(
  death_source,
  fuel_access,
  by = c("entity", "code", "year")
) %>% 
  filter(
    year %in% c("2000", "2016")
  ) %>% 
  rename(
    deaths = deaths_cause_all_causes_risk_household_air_pollution_from_solid_fuels_sex_both_age_age_standardized_rate,
    access = access_to_clean_fuels_and_technologies_for_cooking_percent_of_population
  ) %>% 
  mutate(continent = countrycode(
    sourcevar = code, 
    origin = "iso3c",
    destination = "continent"
  )) 


# 3 Graph -----------------------------------------------------------------

poll %>% 
  filter(continent == "Africa") %>% 
  ggplot(aes(x = access, y = deaths)) +
  #geom_point() +
  geom_line(
    aes(group = entity), 
    arrow = arrow(length=unit(0.20,"cm"), type = "closed")
  )



