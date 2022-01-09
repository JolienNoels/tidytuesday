
# 0 Packages --------------------------------------------------------------

library(tidyverse)
library(afrilearndata)
library(sf)
library(ourworldindata)

# 1 Global ----------------------------------------------------------------

dir <- here::here("2021/2021-11-08-maps")


# 2 data ------------------------------------------------------------------

mortality <- child_mortality %>% 
  filter(year == "2012")

africountries <- africountries %>% rename(country = name)

data <- left_join(
  africountries, 
  mortality,
  by = c("country")
)

data %>% 
  ggplot() +
  geom_sf(aes(fill = child_mort))
  


