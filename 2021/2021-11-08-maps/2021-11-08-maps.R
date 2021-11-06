
# 0 Packages --------------------------------------------------------------

library(tidyverse)
library(readxl)

# 1 Global ----------------------------------------------------------------

dir <- here::here("2021/2021-11-08-maps")


# 2 data ------------------------------------------------------------------

elec <- read_excel(path = fs::path(dir, "Data-Global-Electricity-Review-2021.xlsx"), skip = 1)
elec <- elec %>% 
  janitor::clean_names()
elec <- elec %>% 
  rename(country = area) %>% 
  mutate(share_of_production_percent = as.numeric(share_of_production_percent))
elec$category <- cut(
  elec$share_of_production_percent, 
  breaks = c(-1, 20, 40, 60, 80, 101), 
  labels = c("0-20", "20-40", "40-60", "60-80", "80-100") 
)

tiles <- read_excel(fs::path(dir, "tilemap_eu_jn.xlsx"))


data <- left_join(elec, tiles, by = c('country'))


# 3 Graph -----------------------------------------------------------------

data %>% 
  filter(
    region == "Europe",
    year == "2019",
    variable == "Fossil"
  ) %>% 
  ggplot(aes(xmin = column, ymin = row, xmax = column + 1, ymax = row + 1)) +
  geom_rect(aes(fill = category), colour = "white") +
  geom_text(
    aes(x = column, y = row + 1, label = iso3), 
    color = "white", nudge_x = 0.5, nudge_y = 0.5, size = 4
  ) + 
  scale_y_reverse() +
  scale_fill_manual(values = c("#F3E9DA","#dbcbbd","#c87941","#87431d","#290001")) +
  theme_void() +
  labs(
    fill = "% of total\nproduction",
    caption = "Data source: Ember. Graph: Jolien Noels.",
    title = "Electricity generation from fossil fuels, 2019"
  ) +
  theme(
    legend.position = "left",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    plot.title = element_text(size = 14, hjust = 0)
  ) 

ggsave(
  path = dir,
  "elec_eu.png", 
  width = 14, 
  height = 14, 
  units = "cm"
)
