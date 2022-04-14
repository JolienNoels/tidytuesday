
# 0 Packages --------------------------------------------------------------

library(tidyverse)
library(countrycode)
library(ggrepel)

# 1 Data ------------------------------------------------------------------

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


# 2 Graph -----------------------------------------------------------------

poll %>% 
  filter(continent == "Africa") %>% 
  left_join(colored_dat, by = c("entity")) %>% 
  ggplot(aes(x = access, y = deaths)) +
  geom_path(
    aes(group = entity), colour = "#06042a", 
    arrow = arrow(length = unit(0.20,"cm"), type = "closed")
  ) +
  geom_text(
    data = . %>% filter(year == "2016")  %>% 
      filter(entity %in% 
               c("Equatorial Guina", "Angola", "Ethiopia",
                 "Rwanda", "Sudan", "Congo")), 
    aes(label = entity), colour = "#072ecf"
  ) +
  labs(
    x = "population share with access to clean fuel (%)",
    y = "Indoor air pollution death rate",
    title = "Change 2000 and 2016 in African countries\n2000->2016",
    caption = "\nData source: Our world in Data | @JolienNoels"
  ) +
  theme(
    plot.background = element_rect(colour = "#f1dfdb", fill = "#f1dfdb"),
    panel.background = element_rect(colour = "#f1dfdb", fill = "#f1dfdb"),
    panel.grid.minor.y = element_blank(),
    legend.position = "top",
    legend.title = element_text(size=13),
    axis.ticks = element_blank(),
    axis.title = element_text(hjust = 1)
  ) +
  ggeasy::easy_center_title()

ggsave(
  fs::path(here::here('2022/week-15-pollution'), "pollution.png"),
  height = 15, width = 30, units = "cm"
)



