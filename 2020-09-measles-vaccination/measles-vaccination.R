# title: "Tidy Tuesday - 	Measles Vaccination"
# author: "Christophe Nicault"
# date: "27 February 2020"

library(tidyverse)
library(patchwork)
library(scales)
library(ggtext)
library(showtext)

font_add_google("Oswald", "oswald")
font_add_google("Cabin", "cabin")

measles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv')

p1 <- measles %>% 
  mutate(mmr = ifelse(mmr == -1, NA, mmr),
         year = ifelse(year == "null", "Other", year)) %>%
  ggplot(aes(state, mmr, color = year))+
  geom_point(position = "jitter", alpha = 0.5, size = 2) +
  stat_summary(fun=mean, colour="#FF6D17", geom="line", aes(group = 1), size = 2, alpha = 0.8) +
  stat_summary(fun=mean, colour="#FF6D17", geom="point", size = 3, alpha = 0.8) +
  stat_summary(fun=min, colour="gray", geom="line", aes(group = 1), size = 1, alpha = 0.8) +
  scale_y_continuous(label = percent_format(scale = 1), limits = c(-5, 100), expand = c(0, 0)) +
  scale_color_manual(values = c("2017" = "#C8CF5C", "2017-18" = "#87C36D", "2018-19" = "#50B283", "Other" = "#2F9C92")) +
  coord_flip()+
  labs(y = "School's Measles, Mumps, and Rubella (MMR) vaccination rate",
       color = "Year")+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(hjust = 1, color = "white"),
        axis.title.x = element_text(color = "white", margin = margin(10, 0, 0, 0)),
        legend.position = "none",
        panel.grid.major = element_line(color = "#6f6f6f", linetype = "dotted"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#3f3f3f", color = NA),
        panel.background = element_rect(fill = "#3f3f3f", color = NA),
        legend.background = element_rect(fill = "#3f3f3f", color = "white"),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        legend.key = element_rect(fill = "#3f3f3f"))

p2 <- measles %>%
  mutate(overall = ifelse(overall == -1, NA, overall),
         year = ifelse(year == "null", "Other", year)) %>%
  ggplot(aes(state, overall, color = year))+
  geom_point(position = "jitter", alpha = 0.5, size = 2) +
  stat_summary(fun=mean, colour="#FF6D17", geom="line", aes(group = 1), size = 2, alpha = 0.8) +
  stat_summary(fun=mean, colour="#FF6D17", geom="point", size = 3, alpha = 0.8) +
  stat_summary(fun=max, colour="gray", geom="line", aes(group = 1), size = 1, alpha = 0.8) +
  scale_y_reverse(label = percent_format(scale = 1), limits = c(100, -5), expand = c(0, 0))+
  scale_color_manual(values = c("2017" = "#C8CF5C", "2017-18" = "#87C36D", "2018-19" = "#50B283", "Other" = "#2F9C92")) +
  coord_flip() +
  labs(y = "School's overall vaccination rate",
       color = "Year")+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(color = "white", margin = margin(10, 0, 0, 0)),
        axis.text.y = element_text(hjust = 0.5, color = "#c1c1c1", family = "cabin", size = 12),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(hjust = 0, color = "white"),
        panel.grid.major = element_line(color = "#6f6f6f", linetype = "dotted"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#3f3f3f", color = NA),
        panel.background = element_rect(fill = "#3f3f3f", color = NA),
        legend.background = element_rect(fill = "#3f3f3f", color = NA),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        legend.key = element_rect(fill = "#3f3f3f")
        )

x11(type = "cairo")
showtext_auto()
p1 + p2 + plot_annotation(title = "MMR and overall vaccination rate in US schools",
                         caption = "Visualisation: Christophe Nicault | Data: The Wallstreet Journal",
                         subtitle = "<span style = 'color:\"#FF6D17\"'>Orange line represents the mean</span> / <span style = 'color:\"grey\"'>Grey line represents the minimum rate</span>",
                         theme = theme(
                            plot.title = element_textbox(color = "white", hjust = 0.5, size = 16, lineheight = 1, family = "oswald"),
                            plot.subtitle = element_textbox(color = "white", hjust = 0.5, size = 14,
                                                            lineheight = 1, family = "oswald", margin = margin(0, 0, 20, 0)),
                            plot.background = element_rect(fill = "#3f3f3f", color = NA),
                            plot.caption = element_text(hjust = 1, color = "white"))
                         )
