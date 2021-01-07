# title: "Tidy Tuesday - 	San Francisco Trees"
# author: "Christophe Nicault"
# date: "30 january 2020"

library(tidyverse)
library(ggmap)
library(osmdata)
library(extrafont)
library(sf)
library(lubridate)
library(viridis)
library(patchwork)
library(ggtext)

loadfonts(device = "win")

sf_trees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')

roads <- getbb("San Francisco") %>% 
  opq() %>% 
  add_osm_feature("highway") %>% 
  osmdata_sf()

coast <- getbb("San Francisco") %>% 
  opq() %>% 
  add_osm_feature(key = "natural", value = "coastline") %>%
  osmdata_sf()

sf_trees %>%
  summarise(nb_species = length(unique(species)))

# Vector of years to fill the gap in the dataset

years <- tibble(year = seq(1954,2019,1), nb = 0)

# Number of different species per year

p1 <- sf_trees %>%
  mutate(year = year(date)) %>%
  filter(year != 2020) %>%
  group_by(year) %>%
  summarise(nb_species = length(unique(species))) %>%
  right_join(years) %>%
  mutate(nb_species = ifelse(is.na(nb_species), 0, nb_species)) %>% 
  ggplot(aes(year, nb_species)) +
  geom_col(fill = "green", color = "darkgreen", size = 1) +
  scale_x_continuous(limits=c(1955,2020))+
  labs(title= "Number of different specie planted each year",
       y = "# different specie") +
  theme(panel.background = element_rect(fill = "#555555"),
        plot.background = element_rect(fill = "#555555", color = NA),
        legend.background = element_rect(fill = "#555555"),
        plot.title = element_text(color = "white", hjust = 0.5),
        panel.grid.major =  element_line(color = "white", linetype = "dashed"),
        panel.grid.minor =  element_line(color = "white", linetype = "dotted"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        axis.title.x = element_blank())


# Number of different species and number planted each year

p2 <- sf_trees %>%
  mutate(year = year(date)) %>%
  filter(year != 2020, year >= 1955) %>%
  group_by(year, species) %>%
  summarise(nb_species = n()) %>% 
  group_by(species) %>%
  mutate(tri = n())%>%
  ungroup() %>%
  mutate(species = fct_reorder(species, desc(tri))) %>%
  ggplot(aes(year, species, fill = nb_species)) +
  geom_tile() +
  scale_fill_viridis(option = "viridis", trans = "log10")+
  scale_x_continuous(limits=c(1955,2020))+
  labs(title= "Number of different species planted each year",
       fill = "Nb of Tree") +
  theme(panel.background = element_rect(fill = "#555555"),
        plot.background = element_rect(fill = "#555555", color = NA),
        legend.background = element_rect(fill = "#555555"),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        plot.title = element_text(color = "white", hjust = 0.5),
        panel.grid.major =  element_blank(),
        panel.grid.minor =  element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        axis.title.x = element_blank(),
        axis.ticks.x = element_line(color = "white"),
        axis.ticks.y = element_blank(),
        legend.position = c("bottom"))

# Number of trees planted each year

p3 <- sf_trees %>%
  mutate(year = year(date)) %>%
  filter(year != 2020 ) %>%
  filter(!is.na(year)) %>%
  group_by(year) %>%
  summarise(nb_trees = n()) %>% 
  right_join(years) %>%
  mutate(nb_trees = ifelse(is.na(nb_trees), 0, nb_trees)) %>%
  ggplot(aes(year, nb_trees)) +
  geom_line(color = "green", size = 1) +
  scale_x_continuous(limits=c(1955,2020))+
  labs(title= "Number of trees planted each year",
       y = "# of trees") +
  theme(panel.background = element_rect(fill = "#555555"),
        plot.background = element_rect(fill = "#555555", color = NA),
        legend.background = element_rect(fill = "#555555"),
        plot.title = element_text(color = "white", hjust = 0.5),
        panel.grid.major =  element_line(color = "white", linetype = "dashed"),
        panel.grid.minor =  element_line(color = "white", linetype = "dotted"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        axis.title.x = element_blank())

# Density map of the trees in San Francisco

p4 <- ggplot()+
  geom_sf(data = roads$osm_lines, color = "grey", size = 0.05) +
  geom_sf(data = coast$osm_lines, color = "grey", size = 0.5) +
  xlim(c(-122.53,-122.36)) +
  ylim(c(37.7,37.81)) +
  stat_density2d(aes(x = longitude, y = latitude, fill = ..level.., alpha = 0.05),
                 bins = 10, geom = "polygon",
                 data = sf_trees)+
  scale_fill_gradient(low = "green", high = "darkgreen") +
  labs(fill = "Density")+
  theme(panel.background = element_rect(fill = "#555555"),
        plot.background = element_rect(fill = "#555555", color = NA),
        legend.background = element_rect(fill = "#555555"),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())+
  guides(alpha = FALSE)
  
# Intro text, summary of the visualisation

text_intro <- ggplot(data.frame(x = 1:2, y = 1:2)) +
  labs(x = NULL, y = NULL,
       subtitle = "The number of trees planted in San Francisco kept increasing from the <span style='color:#feb24c'>70's</span> to the end of the <span style='color:#feb24c'>90's</span>.<br />
       This period showed an increase of the number of different  species planted each year, 
       with less than <span style='color:#00d400'>50</span> to about <span style='color:#00d400'>150</span>, 
       with the maximum of <span style='color:#00d400'>173 different species</span> in <span style='color:#feb24c'>2008</span>,<br /> and <span style='color:#00d400'>4388 trees planted</span>. <span style='color:#feb24c'>2008</span> was the second year with the most trees, after <span style='color:#feb24c'>1998</span><span style='color:#00d400'> with 4437 new trees</span>.
       <br />The top graph shows the evolution of the number of species planted each year, while the bottom one shows the evolution of the number of trees planted each year.
       <br />The middle visualisation summarises this and also shows that some species are consistantly planted accross the years while some appear less regularly (each line represent a specie).
       <br />San Francisco has now reached <span style='color:#00d400'>192987 trees of 571 different species</span>, with a higher density on the middle / east side of the city.") +
  theme(panel.background = element_rect(fill = "#555555"),
        plot.background = element_rect(fill = "#555555", color = NA),
        axis.text = element_blank(),
        axis.line.x = element_blank(),
        plot.title = element_text(color = "white", hjust = 0),
        plot.subtitle = element_textbox(color = "lightgrey", family = "Tahoma", hjust = 0, size = 12, lineheight = 1),
        plot.margin = margin(30, 100, 0, 100),
        strip.background = element_rect(fill = NA, color = NA)
       )


patchwork <- text_intro  / (p1 / p2 / p3 | p4 )

patchwork + plot_annotation(title = "San Francisco Trees",
                            subtitle = "Evolution of the number of trees & species.",
                            caption = "Visualisation: Christophe Nicault | Data: data.sfgov.org",
                            theme = theme(plot.background = element_rect(fill = "#555555"),
                                        plot.title = element_text(color = "white", size = 18, hjust = 0.5),
                                        plot.subtitle = element_text(color = "white", size = 14, hjust = 0.5),
                                        plot.caption = element_text(color = "white", hjust = 1),
                                        panel.background = element_rect(fill = "#555555")))+
            plot_layout(nrow = 2, heights = c(0.05, 5))



