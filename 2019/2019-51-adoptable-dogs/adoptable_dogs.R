# title: "Tidy Tuesday - Adoptable dogs"
# author: "Christophe Nicault"
# date: "18 december 2019"

library(tidyverse)
library(countrycode)
library(RColorBrewer)

dog_moves <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_moves.csv')
dog_travel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_travel.csv')
dog_descriptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_descriptions.csv')


require(maps)

theme_set(theme_bw())

theme_update(plot.title = element_text(hjust = 0.5),
             plot.subtitle = element_text(hjust = 0.5),
             plot.background = element_blank(),
             legend.background = element_blank(),
             panel.grid = element_blank(),
             panel.background = element_rect(fill  = "lightblue"),
             panel.border = element_blank(),
             axis.line = element_blank(),
             axis.ticks = element_blank(),
             axis.text = element_blank(),
             axis.title = element_blank(),
             plot.caption = element_text(size = 8))


worldmap <- map_data("world") %>% tbl_df %>% filter(region !="Antarctica")


# DF with countries and number of dogs
country <- dog_travel %>%
  filter(found %in% codelist$country.name.en) %>%
  rename(country = found) %>%
  group_by(country) %>%
  summarise(total = n())

# get the center coordinate for each country
centre <- map_data("world") %>% tbl_df %>% 
  group_by(region) %>%
  summarise(centx = mean(long),
            centy = mean(lat))

# add the countries coordinates and USA coordinates to df
country <- country %>%
  left_join(centre, by = c("country" = "region")) %>%
  mutate(xend = -100,
         yend = 42)

# Create a df with continent and region for each countries
countries <- worldmap %>% distinct(region) %>% pull()
country_code <- countrycode(countries,'country.name.en','iso3c')
countries_info <- as_tibble(list(country = countries, iso3c = country_code))

continent <- codelist %>% select(iso3c, continent, region)

countries_info <- countries_info %>%
  left_join(continent)

# barplot showing number of dogs per continent

nb_dogs <- countries_info %>% left_join(country)

p1 <- nb_dogs %>% 
  mutate(total = ifelse(is.na(total), 0, total)) %>%
  filter(!is.na(continent)) %>%
  group_by(continent) %>%
  summarise(total = sum(total)) %>%
  ungroup() %>%
  mutate(continent = fct_reorder(continent, total)) %>%
  ggplot(aes(continent, total, fill = continent, label = paste(continent, total, sep = " - "))) +
  geom_col() +
  geom_text(color = "black", hjust = 0, size = 4, nudge_y = 10) +  
  coord_flip() +
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position = "none",
        panel.background = element_rect(fill  = "lightblue"),
        plot.title = element_text(size = 12),
        panel.border =  element_rect(fill = NA, color = "gray", size = 2)) +
  scale_y_continuous(expand = expand_scale( mult = c(0,0.5),
                                           add = c(0, 30))) +
  labs(title = "# of dogs per continent")

# Add more colors to be able to display the 22 regions
# https://www.r-bloggers.com/how-to-expand-color-palette-with-ggplot-and-rcolorbrewer/

colourCount = length(unique(countries_info$region))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

# Plot the map
fullmap <- worldmap %>%
  left_join(countries_info, by = c("region" = "country")) %>%
  ggplot(aes(long, lat)) +
  geom_polygon(aes(group = group, fill = region.y), color = "grey") +
  scale_fill_manual(values = getPalette(colourCount))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey")

# Add the arrow from source to destination
p2 <- fullmap +
  geom_curve(data = country, aes(x = centx, y = centy, xend = xend, yend = yend, size = total, color = total), alpha = 0.4,
             curvature = 0.4, arrow = arrow(length = unit(0.07, "inch"))) +
  scale_color_viridis_c() +
  theme(plot.background = element_rect(fill  = "lightblue"),
        legend.background = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill  = "lightblue"))+
  labs(title = "Origin of adoptable dogs arriving to the USA",
       captions = "Visualisation: Christophe Nicault | Data: Petfinder / The Pudding")

# Add first plot to the map.
p2 + annotation_custom(ggplotGrob(p1), xmin = -170, xmax = -90, ymin = -30, ymax = 0) +
  guides(fill = FALSE, size = FALSE)

