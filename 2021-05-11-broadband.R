library(tidyverse)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)

data <- tidytuesdayR::tt_load(2021, week = 20)

data <- data$broadband %>% 
  janitor::clean_names() %>% 
  mutate(county_id = sprintf("%05d" , county_id))

data$county_id <- as.character(data$county_id)

counties <- counties(cb = TRUE) %>% 
  janitor::clean_names() %>% 
  rename(county_id = geoid) 

data <- inner_join(counties, data, by = c(county_id = "county_id"))

data$broadband_usage <- as.numeric(data$broadband_usage)

data %>% 
  filter(st != c("AK") & st != "HI") %>% 
  ggplot(aes(fill = broadband_usage)) +
  geom_sf() +
  coord_sf(crs = st_crs(2163)) +
  scale_fill_gradient(low = "#ffffe0", high = "#000f89", na.value="grey80", 
                      limits=c(0, .6)) +
  theme_void() +
  labs(title = "High-speed internet connection in the US" ,
       caption = "Data: Microsoft\n
       broadband speed defined as 25Mbps download speed (US Federal Communications Commission’s standard for “high-speed internet)")
