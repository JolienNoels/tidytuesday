# Date created: 09-10-2021
# Date updated: 
# Tidytuesday contribution for week 40 on NBER data


# 0 Packages --------------------------------------------------------------

library(tidyverse)


# 1 Global ----------------------------------------------------------------

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



# 2 Data ------------------------------------------------------------------

data <- tidytuesdayR::tt_load(2021, week = 40)
papers <- data$papers
paper_programs <- data$paper_programs
paper_authors <- data$paper_authors
programs <- data$programs

nber <- papers %>% 
  left_join(paper_programs, by = c("paper")) %>% 
  left_join(paper_authors, by = c("paper")) %>% 
  left_join(programs, by = c("program")) %>% 
  mutate(year = as.character(year))


# 3 Graph -----------------------------------------------------------------

nber %>% 
  drop_na(program) %>% 
  filter(year != "2021") %>% 
  group_by(program, year) %>% 
  mutate(nr_ofpapers = length(paper)) %>% 
  ungroup() %>% 
  ggplot(aes(x = year, y = program_desc, size = nr_ofpapers, colour = nr_ofpapers)) +
  geom_point(alpha = 0.7) +
  scale_size_continuous(range = c(0, 7)) +
  theme_void()

library(ggbump)



library(ggbeeswarm)

nber %>% 
  drop_na(program) %>% 
  filter(year != "2021") %>% 
  group_by(program, year) %>% 
  mutate(nr_ofpapers = length(paper)) %>% 
  ungroup() %>% 
  ggplot(aes(x = program, y = year)) + 
  geom_quasirandom() +
  coord_flip()



library(waffle)

