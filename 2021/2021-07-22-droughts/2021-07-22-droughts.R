# Date created: 24-07-2021
# Date updated: 25-07-2021
# Tidytuesday contribution for week 30 on US drought data

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
    legend.justification = "left",
    legend.text = element_text(size=15, colour = "black"),
    legend.key.height = unit(3, units = "mm"),
    legend.key.width = unit(50, units = "mm"),
    legend.spacing.x = unit(3, "mm"),
    legend.title = element_blank(),
    legend.key = element_rect(fill = "white", colour = "white"),
    axis.ticks = element_blank(),
    axis.title.y = element_text(hjust = 1, size = 15),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 15, colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black")
  )
}

colours <- c("white", "#FEC196", "#FF9670", "#F52100", "#A31600","#660E00")

## 2 Data ----------------------------------------------------------------------------------------------------------
data <- tidytuesdayR::tt_load(2021, week = 30)
drought <- data$drought %>% 
  drop_na() 

drought <- data.frame(drought)


## 3 Graph ----------------------------------------------------------------------------------------------------------
drought %>% 
  filter(state_abb == "CA") %>% 
  drop_na() %>% 
  mutate(year = lubridate::year(valid_start)) %>% 
  mutate(drought_lvl = fct_relevel(drought_lvl, levels = c("None", "D0","D1","D2","D3","D4"))) %>%
  ggplot(aes(x = valid_start, y  = area_pct, fill = drought_lvl)) +
  geom_stream(n_grid=nrow(drought), type = "proportional", bw=0.15) +
  labs(y = "Share of area affected by drought level",
       title = "Droughts in California, July 2001 to July 2021\n",
       caption = "\nData source: U.S. Drought Monitor | @JolienNoels") +
  jn_theme() +
  scale_fill_manual(labels=c("","abnormally dry","moderate","severe","extreme","exceptional drought"),
                    values=colours,
                    na.value = "grey80",
                    name ="", 
                    guide = guide_legend(label.position = "bottom", ncol = 6)) +
  scale_x_date(date_labels = "%Y", expand=c(0,0)) +
  scale_y_continuous(labels = scales::label_percent(suffix = "%"))

ggsave(here::here("2021/2021-07-22-droughts", "2021-07-22-streamplot.png"), width = 17, height = 6)


## 4 Other ----------------------------------------------------------------------------------------------------------

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
  theme(legend.justification = "center") +
  scale_fill_manual(labels=c("abnormally dry","moderate","severe","extreme","exceptional drought"),
                    values= c("#FEC196", "#FF9670", "#F52100", "#A31600","#660E00"),
                    na.value = "grey80",
                    name ="", 
                    guide = guide_legend(label.position = "bottom")) +
  scale_x_date(date_labels = "%Y", expand=c(0,0)) +
  scale_y_continuous(labels = scales::label_percent(suffix = "%", scale = 1),
                     limits=c(0,100))

ggsave(here::here("2021/2021-07-22-droughts", "2021-07-22-areaplot.png"), width = 17, height = 6)



drought %>% 
  filter(state_abb == "CA") %>% 
  filter(drought_lvl == c("D3", "D4")) %>% 
  group_by(valid_start) %>% 
  mutate(extreme = sum(pop_pct)) %>% 
  ggplot(aes(x = valid_start, y  = drought_lvl, fill = extreme)) +
  geom_tile() +
  jn_theme() +
  scale_fill_viridis_c()

ggsave(here::here("2021/2021-07-22-droughts", "tileplot.png"))



