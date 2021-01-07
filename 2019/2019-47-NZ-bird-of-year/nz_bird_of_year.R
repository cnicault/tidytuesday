# title: "Tidy Tuesday - New Zealand Bird of the Year"
# author: "Christophe Nicault"
# date: "20 novembre 2019"

library(tidyverse)
library(ggthemr)
library(patchwork)
library(lubridate)
library(png)
library(ggimage)

Sys.setlocale("LC_TIME", "C")

nz_bird <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-19/nz_bird.csv")

nz_bird <- nz_bird %>%
  separate(vote_rank, c("vote", "rank"), sep = "_") %>%
  mutate(rank = as.integer(rank),
         bird_breed = ifelse(is.na(bird_breed), "unidentify", bird_breed)) %>%
  select(-vote)


ggthemr("flat")


theme_update(plot.title = element_text(hjust = 0.5),
             plot.subtitle = element_text(hjust = 0.5),
             plot.caption = element_text(size = 8),
             axis.line = element_blank(),
             axis.ticks = element_blank(),
             axis.text.y = element_blank(),
             axis.title = element_blank())

p1 <- nz_bird %>%
  mutate(day = wday(date, label = TRUE, abbr = FALSE)) %>%
  group_by(day) %>%
  summarise(total = n()) %>%
  ggplot(aes(day, total, fill = total)) +
  geom_col() +
  coord_polar() +
  geom_image(aes(0,0), image = sample(file.path("2019-47-NZ-bird-of-year","kiwi.png")), size = .2, alpha = 0.2)+
  labs(title = "Number of votes per day",
       subtitle= "People needs distraction at the beginning of the week. \nIs the focus shiffting to the week-end preparation from thursday ?",
       fill = "Number of votes") +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10))

p2 <- nz_bird %>%
  group_by(hour) %>%
  summarise(total = n()) %>% 
  ggplot(aes(hour, total, fill = total)) +
  geom_col() +
  coord_polar()+
  scale_x_continuous(breaks = seq(0, 23)) +
  labs(title = "Number of votes per hour",
       subtitle= "Most of the votes happens during office hours, than in the evening. \nSome people are night owls ... or on a night shift ?",
       fill = "Number of votes") +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10))


p1 + p2 + 
  plot_annotation(title = "Number of votes per hour and per day",
                  subtitle = "Apparently people like to vote for birds from the office :)",
                  caption = "Visualisation: Christophe Nicault | Data: New Zealand Forest and Bird Org")

