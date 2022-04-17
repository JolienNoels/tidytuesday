
# 0 Packages --------------------------------------------------------------

library(tidyverse)
library(countrycode)
library(ggrepel)
library(ggtext)

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
  ggplot(aes(x = access, y = deaths)) +
  geom_path(
    aes(group = entity), colour = "#34293D", 
    arrow = arrow(length = unit(0.20,"cm"), type = "closed")
  ) +
  geom_path(
    data = . %>% filter(entity %in% 
                          c("Equatorial Guina", "Angola", "Ethiopia",
                                      "Rwanda", "Sudan", "Congo")),
    aes(group = entity), colour = "#F68989", size = 1.5,
    arrow = arrow(length = unit(0.20,"cm"), type = "closed")
  ) +
  geom_text_repel(
    data = . %>% filter(year == "2016")  %>% 
      filter(entity %in% 
               c("Equatorial Guina", "Angola", "Ethiopia",
                 "Rwanda", "Sudan", "Congo")), 
    aes(label = entity), colour = "#F68989", 
    position = position_nudge(x = 1, y = -5)
  ) +
  scale_x_continuous(labels=scales::percent_format(scale=1, accuracy=1)) +
  labs(
    x = "Population share with access to clean cooking fuels (%)",
    y = "Indoor air pollution death rate\n(number of deaths per 100,000 individuals)",
    title = "**Change in indoor air pollution death rates and acces to clean fuels in African countries** <br>2000→2016 <br> <span style = 'color: #F68989;'>Highest reductions</span>",
    subtitle = usefunc::str_wrap_break("In 2019, about 4% of global deaths were attributed to indoor air pollution. Death rates are higher in lower-income countries, particularly across Sub-Saharan Africa and Asia. However, annual deaths from indoor air pollution have declined globally. Indoor air pollution results from the burning of solid fuels such as crop waste, dung, charcoal and coal for cooking and heating in households.", break_limit = 120),
    caption = "\nData source: Our World in Data | @JolienNoels"
  ) +
  theme(
    plot.background = element_rect(colour = "#F3E5D6", fill = "#F3E5D6"),
    plot.title = element_markdown(hjust=.5, lineheight=1.5),
    panel.background = element_rect(colour = "#F3E5D6", fill = "#F3E5D6"),
    panel.grid.minor.y = element_blank(),
    legend.position = "top",
    legend.title = element_text(size=13),
    axis.ticks = element_blank(),
    axis.title = element_text(hjust = 1)
  ) 

ggsave(
  fs::path(here::here('2022/week-15-pollution'), "pollution.png"),
  height = 14, width = 22, units = "cm"
)
