

# 0 Packages --------------------------------------------------------------

library(tidyverse)

# 1 Global ----------------------------------------------------------------

theme_set(theme_void())

# 2 Data ------------------------------------------------------------------

data <- tidytuesdayR::tt_load('2022-03-01') 
stations <- data$stations %>% 
  janitor::clean_names()

elec <- stations %>% 
  filter(fuel_type_code == "ELEC", access_code == "public")

elec_geoms <- elec %>% 
  st_as_sf(coords = c("x", "y")) %>% 
  st_set_crs(4326) %>% 
  st_transform("ESRI:102003") 

states <- tigris::states(cb = TRUE) %>% 
  janitor::clean_names() %>% 
  filter(
    stusps != "AK" & stusps != "HI" & 
      stusps != "VI" & stusps != "MP" & 
      stusps != "PR" & stusps != "GU" & 
      stusps != "AS"
  ) %>% 
  st_transform("ESRI:102003") # ESRI:102003 # 4326

ggplot() +
  geom_sf(data = states, fill = NA, size = .1) +
  geom_sf(data = elec_geoms, , size = .1)






