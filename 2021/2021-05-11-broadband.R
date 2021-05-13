library(tidyverse)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)
library(tidycensus)

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

data$broadband_usage <- cut(data$broadband_usage, 
                            breaks=c(0,0.2,0.4,0.6,0.8,1.01), 
                            labels=c("0-20%","20%-40%","40%-60%","60%-80%","80%-100%"))


data %>% 
  filter(st != c("AK") & st != "HI") %>% 
  ggplot(aes(fill = broadband_usage)) +
  geom_sf(size=0.2) +
  coord_sf(crs = st_crs(2163)) +
  scale_fill_manual(labels=c("0-20%","20%-40%","40%-60%","60%-80%","80%-100%","No data"),
                    values = c("#ffefa0","#FFB870","#EA484B","#963c2d","#FF99A8","#grey80"),
                    na.value = "grey80",
                    name ="Broadband usage\n", 
                    guide = guide_legend( keyheight = unit(3, units = "mm"), 
                                           keywidth=unit(20, units = "mm"), 
                                           label.position = "bottom", 
                                           title.position = 'left', nrow=1) ) +
  labs(title =   "High-speed internet connection in the US" ,
       subtitle = "by county, November 2019\n",
       caption = "Data: Microsoft\n
       broadband speed defined as 25Mbps download speed (US Federal\nCommunications Commission’s standard for high-speed internet)") +
  theme_void() +
  theme(plot.background = element_rect(colour = "#CBDAF6", fill = "#CBDAF6"),
        legend.position = "top",
        legend.title = element_text(size=13))


