# Date created: 13-05-2021
# Date updated: 16-05-2021
# Tidytuesday contribution for week 20 on US broadband data

## 0 Packages ----------------------------------------------------------------------------------------------------------
library(tidyverse)
library(sf)
library(albersusa)
library(janitor)
library(patchwork)

## 1 Global ----------------------------------------------------------------------------------------------------------
my_theme <- theme(plot.background = element_rect(colour = "#CBDAF6", fill = "#CBDAF6"),
                  panel.background = element_rect(colour = "#CBDAF6", fill = "#CBDAF6"),
                  panel.grid.major.x = element_blank(),
                  legend.position = "top",
                  legend.title = element_text(size=13),
                  axis.ticks = element_blank(),
                  axis.title.y = element_text(hjust = 1))



## 2 Data ----------------------------------------------------------------------------------------------------------
data <- tidytuesdayR::tt_load(2021, week = 20)
data <- data$broadband %>% 
  clean_names() %>% 
  mutate(fips = sprintf("%05d" , county_id)) 
data$fips <- as.character(data$fips)
data$broadband_usage <- as.numeric(data$broadband_usage)

states <- usa_sf()
counties <- counties_sf()

map <- inner_join(counties, data, by = c("fips"))

map$broadband_usage <- cut(map$broadband_usage, 
                           breaks=c(0,0.2,0.4,0.6,0.8,1.01), 
                           labels=c("0-20%","20%-40%","40%-60%","60%-80%","80%-100%"))

# rural urban classification from https://www.ers.usda.gov/data-products/rural-urban-continuum-codes.aspx
class <- read.csv("https://raw.githubusercontent.com/JolienNoels/tidytuesday/main/2021/2021-05-11-broadband/ruralurbancodes2013.csv") %>% 
  clean_names() %>% 
  mutate(fips = sprintf("%05d" , i_fips)) %>% 
  mutate(population_2010 = str_remove(population_2010, ",")) 
  
# population data from https://www.census.gov/data/datasets/time-series/demo/popest/2010s-counties-total.html
pop <- read.csv("https://raw.githubusercontent.com/JolienNoels/tidytuesday/main/2021/2021-05-11-broadband/co-est2019-annres.csv") %>% 
  clean_names() %>% 
  rename(population_2019 = x2019) %>% 
  rename(county_name = i_geographic_area) %>% 
  mutate(county_name = str_remove(county_name, "^\\.")) %>% 
  mutate(population_2019 = str_remove(population_2019, ",")) %>% 
  separate(county_name, c("county_name", "state_name"), sep = ", ")

data <- inner_join(data, class[c("fips", "rucc_2013", "description", "population_2010")], by = c("fips"))
data <- inner_join(data, pop[c("county_name", "population_2019")], by = c("county_name"))


data$broadband_usage <- as.numeric(data$broadband_usage)
data$population_2019 <- as.numeric(data$population_2019)
data$rucc_2013 <- as.character(data$rucc_2013)

data <- data %>% 
  mutate(rururb = ifelse((rucc_2013 == "1" | rucc_2013 == "2" | rucc_2013 == "3" ), "Urban",
                          ifelse((rucc_2013 == "4" | rucc_2013 == "5" | rucc_2013 == "6" ), "Intermediate",
                                 ifelse((rucc_2013 == "7" | rucc_2013 == "8" | rucc_2013 == "9" ), "Rural", "Other"))))

# t(t(sapply(data, class)))

## 3 Graph ----------------------------------------------------------------------------------------------------------
p2 <- data %>% 
  mutate(rururb = factor(rururb, levels=c("Urban", "Intermediate", "Rural"))) %>%
  group_by(rururb) %>% 
  drop_na() %>% # weighted.mean does not handle na values
  mutate(avg = weighted.mean(x = broadband_usage, w = population_2019)) %>% 
  ggplot(aes(x = rururb, y = avg)) +
  geom_bar(stat="unique", fill = "#8368BC") +
  labs(x = "",
       y = "Broadband usage") +
  scale_y_continuous(limits=c(0,0.45), expand=c(0,0), labels = scales::percent_format(accuracy = 1)) +
  my_theme  +
  theme(axis.text.y = element_text(size=13),
        axis.text.x = element_text(size=13, face="bold"),
        axis.title = element_text(size=13))


p1 <- map %>% 
  ggplot() +
  geom_sf(aes(fill = broadband_usage), size=0.125) +
  geom_sf(data = states, 
          color="black", fill=NA) +
  coord_sf(crs = st_crs(2163)) +
  scale_fill_manual(labels=c("0%-20%","20%-40%","40%-60%","60%-80%","80%-100%","No data"),
                    values = c("#FF99A8", "#A03C6D", "#58206A", "#491ABE","#0400FF","#grey80"),
                    na.value = "grey80",
                    name ="Broadband usage\n", 
                    guide = guide_legend( keyheight = unit(3, units = "mm"), 
                                          keywidth=unit(20, units = "mm"), 
                                          label.position = "bottom", 
                                          title.position = 'left', nrow=1) ) +
  theme_void() +
  my_theme

p1 + p2  +
  plot_layout(widths = c(2, 1)) +
  plot_annotation(
  title = 'US rural-urban divide in high-speed internet connection',
  subtitle = '% of residents using internet at broadband speeds, November 2019\n',
  caption = 'Broadband data: Microsoft via TidyTuesday. Population data: US Census. Rural-Urban Continuum: USDA & own aggregation. | graph: Jolien Noels\n
       Broadband speed defined as 25Mbps or above download speed (US Federal Communications Commission’s standard for high-speed internet)'
) &
  my_theme &
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5),
        plot.subtitle = element_text(size=14, hjust = 0.5),
        plot.caption = element_text(size=10, hjust = 1))

ggsave("2021-05-11-broadband.png", width = 32.385, height = 19.61, units = c("cm"))  

## other version With tigris -------------------------------------------------------------------------------
library(tidyverse)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)
library(tidycensus)
#theme_set(theme_void())

data <- tidytuesdayR::tt_load(2021, week = 20)

data <- data$broadband %>% 
  janitor::clean_names() %>% 
  mutate(county_id = sprintf("%05d" , county_id)) 

data$county_id <- as.character(data$county_id)

counties <- counties(cb = TRUE) %>% 
  janitor::clean_names() %>% 
  rename(county_id = geoid) 
states <- states(cb = TRUE) %>% 
  janitor::clean_names()

data <- inner_join(counties, data, by = c(county_id = "county_id"))

data$broadband_usage <- as.numeric(data$broadband_usage)

data$broadband_usage <- cut(data$broadband_usage, 
                            breaks=c(0,0.2,0.4,0.6,0.8,1.01), 
                            labels=c("0-20%","20%-40%","40%-60%","60%-80%","80%-100%"))




data %>% 
  filter(st != c("AK") & st != "HI") %>% 
  ggplot() +
  geom_sf(aes(fill = broadband_usage), size=0.2) +
  geom_sf(data = states %>% filter(stusps != c("AK") & stusps != "HI" & stusps != "VI" & stusps != "MP" & stusps != "PR" & stusps != "GU" & stusps != "AS"), 
          color="black", fill=NA) +
  coord_sf(crs = st_crs(2163)) +
  scale_fill_manual(labels=c("0%-20%","20%-40%","40%-60%","60%-80%","80%-100%","No data"),
                    values = c("#FF99A8", "#A03C6D", "#58206A", "#491ABE","#0400FF","#grey80"),
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






