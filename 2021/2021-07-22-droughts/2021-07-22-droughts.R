# Date created: 13-05-2021
# Date updated: 16-05-2021
# Tidytuesday contribution for week 20 on US broadband data

## 0 Packages ----------------------------------------------------------------------------------------------------------
library(tidyverse)
library(ggstream)

## 1 Global ----------------------------------------------------------------------------------------------------------
jn_theme <- function() {
  theme(
    plot.background = element_rect(colour = NA, fill = "transparent"),
    plot.title = element_text(size = 19),
    plot.caption = element_text(size = 13),
    panel.background = element_rect(colour = NA, fill = "transparent"),
    panel.grid.major.x = element_blank(),
    legend.position = "top",
    legend.text = element_text(size=15, colour = "black"),
    axis.ticks = element_blank(),
    axis.title.y = element_text(hjust = 1, size = 15),
    axis.text.y = element_text(size = 15, colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black")
  )
}

colours <- c("white", "#FEC196", "#FFA686", "#F52100", "#A31600","#660E00")

## 2 Data ----------------------------------------------------------------------------------------------------------
data <- tidytuesdayR::tt_load(2021, week = 30)
drought <- data$drought %>% 
  drop_na() 

drought <- data.frame(drought)


## 3 Graph ----------------------------------------------------------------------------------------------------------

drought %>% 
  filter(state_abb == "CA") %>% 
  mutate(drought_lvl = fct_relevel(drought_lvl, levels = c("None", "D0","D1","D2","D3","D4"))) %>%
  filter(drought_lvl != "None") %>% 
  ggplot(aes(x = valid_start, y  = area_pct, fill = drought_lvl)) +
  geom_area() +
  labs(y = "Share of area affected by drought level",
       title = "Droughts in California, July 2001 to July 2021\n",
       caption = "\nData source: U.S. Drought Monitor | @JolienNoels") +
  jn_theme()  +
  theme(axis.title.x = element_blank()) +
  scale_fill_manual(labels=c("abnormally dry","moderate","severe","extreme","exceptional drought"),
                    values= c("#FEC196", "#FFA686", "#F52100", "#A31600","#660E00"),
                    na.value = "grey80",
                    name ="", 
                    guide = guide_legend( keyheight = unit(3, units = "mm"), 
                                          keywidth=unit(50, units = "mm"), 
                                          label.position = "bottom", 
                                          title.position = 'left', nrow=1)) +
  scale_x_date(date_labels = "%Y", expand=c(0,0)) +
  scale_y_continuous(labels = scales::label_percent(suffix = "%", scale = 1),
                     limits=c(0,100))

ggsave(here::here("2021/2021-07-22-droughts", "2021-07-22-areaplot.png"), width = 17, height = 6)

drought %>% 
  filter(state_abb == "CA") %>% 
  drop_na() %>% 
  mutate(year = lubridate::year(valid_start)) %>% 
  mutate(drought_lvl = fct_relevel(drought_lvl, levels = c("None", "D0","D1","D2","D3","D4"))) %>%
  ggplot(aes(x = valid_start, y  = area_pct, fill = drought_lvl), colour = white, show_guide=FALSE) +
  geom_stream(n_grid=nrow(drought), type = "proportional", bw=0.15) +
  labs(y = "Share of area affected by drought level",
       title = "Droughts in California, July 2001 to July 2021\n",
       caption = "\nData source: U.S. Drought Monitor | @JolienNoels") +
  jn_theme() +
  theme(axis.title.x = element_blank()) +
  scale_fill_manual(labels=c("","abnormally dry","moderate","severe","extreme","exceptional drought"),
                    values=colours,
                    na.value = "grey80",
                    name ="", 
                    guide = guide_legend( keyheight = unit(3, units = "mm"), 
                                          keywidth=unit(50, units = "mm"), 
                                          label.position = "bottom", 
                                          title.position = 'left', nrow=1)) +
  scale_x_date(date_labels = "%Y", expand=c(0,0)) +
  scale_y_continuous(labels = scales::label_percent(suffix = "%"))

ggsave(here::here("2021/2021-07-22-droughts", "2021-07-22-streamplot.png"), width = 17, height = 6)

drought %>% 
  filter(state_abb == "CA") %>% 
  filter(drought_lvl == "D3") %>% 
  ggplot(aes(x = valid_start, y  = drought_lvl, fill = area_pct)) +
  geom_tile() +
  jn_theme()

ggsave(here::here("2021/2021-07-22-droughts", "tileplot.png"))



