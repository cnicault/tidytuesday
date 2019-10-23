# title: "Tidy Tuesday - Horror Movie Ratings"
# author: "Christophe Nicault"
# date: "23 octobre 2019"

library(tidyverse)
library(ggthemr)
library(lubridate)

theme_update(plot.title = element_text(hjust = 0.5),
             plot.subtitle = element_text(hjust = 0.5))

library(extrafont) 
# font_import(pattern = "Exquisite Corpse") # to do once
loadfonts(device = "win")
library(ggimage)


horror_movie <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")

date_year_f <- str_detect(horror_movie$release_date, pattern = "^\\d+$")
date_full_f <- str_detect(horror_movie$release_date, pattern = "^\\d+[-A-Za-z]+\\d+$")
horror_movie$year <- NA
horror_movie$release_date <- dmy(horror_movie$release_date)
horror_movie$year[date_year_f] <- as.numeric(horror_movie$release_date[date_year_f])
horror_movie$year[date_full_f] <- year(horror_movie$release_date[date_full_f])
year_from_title <-str_extract(str_extract(horror_movie$title, pattern = "\\(\\d+\\)"), "\\d+")
horror_movie$year <- ifelse(is.na(horror_movie$year), year_from_title, horror_movie$year)
sum(is.na(horror_movie$year))
horror_movie$year <- as.numeric(horror_movie$year)


genres_extract <- str_split(str_replace_all(horror_movie$genres, " ", ""), "\\|", n = 8)
genres_sorted <- lapply(genres_extract, sort)
genres_clean <- unlist(sapply(genres_sorted, paste, collapse = " | "))

horror_movie$genres_clean <- genres_clean


require(maps)
library(countrycode)
library(png)
library(grid)

worldmap <- map_data("world") %>% tbl_df %>% filter(region !="Antarctica")
background <- png::readPNG("ghostface.png")


horror_movie %>%
  group_by(release_country) %>%
  summarise(total = n()) %>%
  right_join(worldmap, by = c("release_country" = "region")) %>%
  ggplot(aes(long, lat, group = group, fill = total)) +
  annotation_custom(rasterGrob(background, 
                               width = unit(1,"npc"), 
                               height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf)+
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "orange", high = "red", trans = "log10") +
  coord_fixed(1.5)+
  theme(plot.title =  element_text(size = 14, family = "Exquisite Corpse", color = "red"),
        plot.subtitle =  element_text(size = 12, family = "Exquisite Corpse", color = "red"),
        plot.background = element_rect(fill  = "black"),
        legend.background = element_rect(fill  = "black"),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.text =  element_text(size = 10, family = "Exquisite Corpse", color = "red"),
        legend.title =  element_text(size = 10, family = "Exquisite Corpse", color = "red"))+
  labs(title = "Number of movies produced by country from 2012 to 2017",
       subtitle = "")
  

ggthemr("earth", type = "outer")

horror_movie %>%
  group_by(genres_clean) %>%
  mutate(total = n(),
         avg_rating = mean(review_rating, na.rm = TRUE)) %>%
  filter(total > 20) %>%
  ungroup() %>%
  mutate(genres_clean = fct_reorder(genres_clean, total)) %>%
  ggplot(aes(genres_clean, review_rating, color = review_rating))+
  geom_point(position = "jitter") +
  stat_summary(fun.y=mean, colour="red", fill= "black", geom="line", aes(group = 1), size = 3, alpha = 0.8) +
  scale_color_gradient(name = "Ratings", low="yellow", high = "red") +
  annotate("text", x = 1.5, y = 6, size = 3, color = "red", label = "Mean", alpha = 0.7, fontface = "bold", family = "Exquisite Corpse") +
  coord_flip() +
  theme(plot.title =  element_text(size = 14, family = "Exquisite Corpse", color = "red"),
        plot.subtitle =  element_text(size = 12, family = "Exquisite Corpse", color = "red"),
        axis.title =  element_text(size = 12, family = "Exquisite Corpse", color = "orange"),
        axis.text =  element_text(size = 10, family = "Exquisite Corpse", color = "orange"),
        legend.text =  element_text(size = 10, family = "Exquisite Corpse", color = "red"),
        legend.title =  element_text(size = 10, family = "Exquisite Corpse", color = "red"))+
  labs(x = "Rating",
       y = "Genre combination",
       title = "Review Rating by genre combination",
       subtitle = "First 20 genre combination")


horror_movie %>%
  group_by(release_country) %>%
  mutate(continent = countrycode(sourcevar = release_country, origin = "country.name", destination = "continent"),
         region = countrycode(sourcevar = release_country, origin = "country.name", destination = "region")) %>%
  group_by(continent) %>%
  mutate(avg_rating = median(review_rating, na.rm = TRUE),
         min_rating = min(review_rating, na.rm = TRUE),
         max_rating = max(review_rating, na.rm = TRUE)) %>%
  filter(!is.na(continent)) %>%
  ungroup() %>%
  ggplot(aes(y = continent)) +
  geom_errorbarh(aes(xmin = min_rating, xmax = max_rating), color = "darkgrey", height = 0, size = 2, alpha = 0.1) +
  geom_point(aes(x = review_rating, color = review_rating), position = "jitter", alpha = 0.6) +
  scale_color_gradient(low = "yellow", high = "red") +
  geom_image(aes(avg_rating, continent), image = sample("pumpkin.png"), size = .1)+
  theme(plot.title =  element_text(size = 14, family = "Exquisite Corpse", color = "red"),
        plot.subtitle =  element_text(size = 12, family = "Exquisite Corpse", color = "red"),
        axis.title =  element_text(size = 12, family = "Exquisite Corpse", color = "orange"),
        axis.text =  element_text(size = 10, family = "Exquisite Corpse", color = "orange"),
        legend.text =  element_text(size = 10, family = "Exquisite Corpse", color = "red"),
        legend.title =  element_text(size = 10, family = "Exquisite Corpse", color = "red"))+
  labs(x = "Rating",
       y = "Continent",
       title = "Review Rating for each continent",
       subtitle = "Pumpkin on the mean")
  

horror_movie %>%
  group_by(release_country) %>%
  mutate(continent = countrycode(sourcevar = release_country, origin = "country.name", destination = "continent"),
         region = countrycode(sourcevar = release_country, origin = "country.name", destination = "region"),
         sorted = paste(continent, region)) %>%
  group_by(region) %>%
  mutate(avg_rating = median(review_rating, na.rm = TRUE),
         min_rating = min(review_rating, na.rm = TRUE),
         max_rating = max(review_rating, na.rm = TRUE)) %>%
  filter(!is.na(continent)) %>%
  ungroup() %>%
  arrange(sorted) %>%
  mutate(region = fct_reorder(region, as.numeric(as.factor(sort(sorted))))) %>%
  ggplot(aes(y = region)) +
  geom_errorbarh(aes(xmin = min_rating, xmax = max_rating), color = "darkgrey", height = 0, size = 2, alpha = 0.1) +
  geom_point(aes(x = review_rating, color = review_rating), position = "jitter", alpha = 0.6) +
  scale_color_gradient(low = "yellow", high = "red") +
  geom_image(aes(avg_rating, region), image = sample("pumpkin.png"), size = .1)+
  theme(plot.title =  element_text(size = 14, family = "Exquisite Corpse", color = "red"),
        plot.subtitle =  element_text(size = 12, family = "Exquisite Corpse", color = "red"),
        axis.title =  element_text(size = 12, family = "Exquisite Corpse", color = "orange"),
        axis.text =  element_text(size = 10, family = "Exquisite Corpse", color = "orange"),
        legend.text =  element_text(size = 10, family = "Exquisite Corpse", color = "red"),
        legend.title =  element_text(size = 10, family = "Exquisite Corpse", color = "red"))+
  labs(x = "Rating",
       y = "Region",
       title = "Review Rating for each region",
       subtitle = "Pumpkin on the mean")




# Is it a bug ? 
# str_detect with apply seems to detect only if the genres start with the pattern
# grepl work fine, and return true even if the pattern is in the middle 

sum(sapply("Horror", str_detect, horror_movie$genres), na.rm = TRUE)
sum(sapply("Horror", grepl, horror_movie$genres), na.rm = TRUE)
head(sapply("Horror", str_detect, horror_movie$genres))
head(horror_movie$genres)

