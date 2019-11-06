# title: "Tidy Tuesday - Bike & Walk Commutes"
# author: "Christophe Nicault"
# date: "5 novembre 2019"

library(here)
library(tidyverse)
library(ggthemr)
ggthemr("flat")

#theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5),
             plot.subtitle = element_text(hjust = 0.5),
             plot.caption = element_text(size = 8))

commute_mode <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-05/commute.csv")

# Clean Cities  
city_split <- str_split(str_replace(commute_mode$city, " \\(balance\\)", ""), " ")
commute_mode$city_type <- sapply(city_split, function(x){rev(x)[1]})
commute_mode$city_name <- sapply(city_split, function(x){paste(x[1:(length(x)-1)], collapse = ' ')})

commute_mode$city_type[which(commute_mode$city_type %in% c("CDP", "City", "", "Bow"))] <- "city"
commute_mode$city_type[which(commute_mode$city_type == "Bow")] <- "Butte-Silver Bow"
commute_mode$city_type[which(commute_mode$city_type == "CDP")] <- "Urban Honolulu CDP"
commute_mode$city_name[which(commute_mode$city == "Carson City")] <- "Carson City"

  
# Odering a categorical variable with facet
library(devtools)
source_url("https://github.com/dgrtwo/drlib/blob/master/R/reorder_within.R?raw=TRUE")
  
commute_mode %>% 
  group_by(state, mode) %>%
  summarise(avg = mean(percent)) %>%
  ungroup() %>%
  group_by(mode) %>%
  top_n(n = 30, wt = avg) %>%
  ungroup() %>%
  mutate(state = reorder_within(state, avg, mode)) %>%
  ggplot(aes(state, avg, fill = avg)) +
  scale_x_reordered() +
  geom_col() +
  scale_fill_gradient(low = "#ddcc77", high = "#117733", trans = "log10") +  
  coord_flip() +
  facet_wrap(~mode, scales = "free") +
  labs(title = "30 first states for cycling and walking to work",
       x = "State",
       y = "Percent",
       fill = "Percent",
       caption = "Visualisation: Christophe Nicault | Data: Bike & Walk Commutes")


commute_mode %>% 
  group_by(mode) %>%
  top_n(n = 30, wt = percent) %>%
  ungroup() %>%
  mutate(city_name = paste(city_name, state, sep = ", "),
    city_name = reorder_within(city_name, percent, mode)) %>%
  ggplot(aes(city_name, percent, fill = city_size)) +
  scale_x_reordered() +
  geom_col() +
  coord_flip() +
  facet_wrap(~mode, scales = "free") +
  labs(title = "30 first cities for cycling and walking to work",
       subtitle = "by city size",
       x = "State",
       y = "Percent",
       fill = "City size",
       caption = "Visualisation: Christophe Nicault | Data: Bike & Walk Commutes")

commute_mode %>% 
  group_by(city_type, mode) %>%
  summarise(avg = mean(percent)) %>%
  ungroup() %>%
  mutate(city_type = fct_reorder(city_type, desc(avg))) %>%
  ggplot(aes(city_type, avg, fill = mode)) +
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Percentage of cycling and walking to work",
       subtitle = "by city type",
       x = "City Type",
       y = "Percent",
       fill = "Mode",
       caption = "Visualisation: Christophe Nicault | Data: Bike & Walk Commutes")

commute_mode %>% 
  group_by(city_size, mode) %>%
  summarise(avg = mean(percent)) %>%
  ungroup() %>%
  mutate(city_size = fct_reorder(city_size, desc(avg))) %>%
  ggplot(aes(city_size, avg, fill = mode)) +
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Percentage of cycling and walking to work",
       subtitle = "by city size",
       x = "City Size",
       y = "Percent",
       fill = "Mode",
       caption = "Visualisation: Christophe Nicault | Data: Bike & Walk Commutes")


# Get location -> execute once, then load the file

# devtools::install_github(repo = 'rCarto/photon')
# library(photon)
# loc <- unique(paste(commute_mode$city_name, commute_mode$state, "US", sep = ", "))
# locgeo <- geocode(loc, limit = 1, key = "place")
# 
# wrong_loc <- locgeo %>% 
#   filter(is.na(lat)) %>% 
#   mutate(location = str_replace(location, " Town", ""),
#          location = str_replace(location, "Urban ", ""),
#          location = str_replace(location, " urban", ""),
#          location = str_replace(location, " city and", "")) %>%
#   select(location) %>%
#   pull()
# 
# correct_loc <- geocode(wrong_loc, limit = 1, key = "place")
# 
# locgeo <- locgeo %>% 
#   filter(!is.na(lat)) %>% 
#   bind_rows(correct_loc)
# 
#write_csv(locgeo, here::here("2019-45-bike-walk-commutes", "loc_geo.csv"))
city_loc <- read_csv(here::here("2019-45-bike-walk-commutes", "loc_geo.csv"))


require(maps)

theme_set(theme_bw())

theme_update(plot.title = element_text(hjust = 0.5),
             plot.subtitle = element_text(hjust = 0.5),
             plot.background = element_blank(),
             legend.background = element_blank(),
             panel.grid = element_blank(),
             panel.background = element_rect(fill  = "lightblue"),
             panel.border = element_rect(color = "grey", size = 2),
             axis.line = element_blank(),
             axis.ticks = element_blank(),
             axis.text = element_blank(),
             axis.title = element_blank(),
             plot.caption = element_text(size = 8))


worldmap <- map_data("world") %>% tbl_df %>% filter(region == "USA")

plot_map <- function(commute_stat, activity, display_stat){

  # First character in Capital for country but lower case for state
  # Alaska
  commute_map <- worldmap %>%
    filter(subregion == "Alaska") %>%
    left_join(commute_stat, by = c("subregion" = "state"))

  city_loc_AK <- city_loc %>%
    filter(state == "Alaska")  
    
  p_alaska <- commute_map %>% 
    filter(long < 0) %>%
    ggplot(aes(long, lat, group = group)) +
    geom_polygon(aes(fill = calc_stat), color = "grey") +
    geom_point(data = city_loc_AK, mapping = aes(lon, lat), inherit.aes = FALSE, color = "darkred") +
    scale_fill_gradient(low = "#ddcc77", high = "#117733", trans = "log10") +
    scale_x_continuous(limits = c(-175, -140)) +
    theme(legend.position = "none")
  
  
  # Hawaii
  commute_map <- worldmap %>%
    filter(subregion == "Hawaii") %>%
    left_join(commute_stat, by = c("subregion" = "state"))
  
  city_loc_HA <- city_loc %>%
    filter(state == "Hawaii")
  
  p_hawaii <- commute_map %>% 
    ggplot(aes(long, lat, group = group)) +
    geom_polygon(aes(fill = calc_stat), color = "grey") +
    geom_point(data = city_loc_HA, mapping = aes(lon, lat), inherit.aes = FALSE, color = "darkred") +
    scale_fill_gradient(low = "#ddcc77", high = "#117733", trans = "log10") +
    theme(legend.position = "none")
  
  usamap <- map_data("state") %>% tbl_df
  
  commute_stat <- commute_stat %>% 
    mutate(state = tolower(state))
  
  commute_map <- usamap %>%
    left_join(commute_stat, by = c("region" = "state"))

  city_loc_US <- city_loc %>%
    filter(state != "Hawaii", state != "Alaska")

  p_usa <- commute_map %>%
    ggplot(aes(long, lat, group = group)) +
    geom_polygon(aes(fill = calc_stat), color = "grey") +
    geom_point(data = city_loc_US, mapping = aes(lon, lat), inherit.aes = FALSE, color = "darkred") +
    scale_fill_gradient(low = "#ddcc77", high = "#117733", trans = "log10") +
    scale_x_continuous(limits = c(-145, -65)) +
    theme(plot.background = element_rect(fill  = "lightblue"),
          legend.background = element_blank(),
          panel.grid = element_blank(),
          panel.border =  element_blank(),
          panel.background = element_rect(fill  = "lightblue")) +
    labs(title = paste0("Percentage of ",activity," to commute to work"),
         subtitle = display_stat,
         fill = "Percentage",
         caption = "Visualisation: Christophe Nicault | Data: Bike & Walk Commutes")
  
  full_plot <- p_usa +
    annotation_custom(ggplotGrob(p_alaska), xmin = -143, xmax = -125, ymin = 35, ymax = 45) +
    annotation_custom(ggplotGrob(p_hawaii), xmin = -143, xmax = -125, ymin = 25, ymax = 35)

  return(full_plot)
}  

commute_stat <- commute_mode %>%
  filter(mode == "Bike") %>%
  group_by(state) %>%
  summarise(calc_stat = mean(percent, na.rm = TRUE)) %>%
  ungroup()

plot_map(commute_stat, "cycling", "Mean per State")

commute_stat <- commute_mode %>%
  filter(mode == "Walk") %>%
  group_by(state) %>%
  summarise(calc_stat = mean(percent, na.rm = TRUE)) %>%
  ungroup()

plot_map(commute_stat, "walking", "Mean per State")

commute_stat <- commute_mode %>%
  filter(mode == "Walk") %>%
  group_by(state) %>%
  summarise(calc_stat = max(percent, na.rm = TRUE)) %>%
  ungroup()

plot_map(commute_stat, "walking", "Highest percent of the state ( max(percent) )")

commute_stat <- commute_mode %>%
  filter(mode == "Bike") %>%
  group_by(state) %>%
  summarise(calc_stat = max(percent, na.rm = TRUE)) %>%
  ungroup()

plot_map(commute_stat, "cycling", "Highest percent of the state ( max(percent) )")
