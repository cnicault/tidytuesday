# title: "Tidy Tuesday - 	Food Consumption"
# author: "Christophe Nicault"
# date: "20 February 2020"

# Get the Data
library(tidyverse)
library(scales)
library(ggtext)
library(showtext)

font_add_google("Roboto", "roboto")
font_add_google("Oswald", "oswald")
font_add_google("Cabin", "cabin")

food_consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')

food_consumption <- food_consumption %>%
  rename(co2_emission = co2_emmission)

fd_wide <- food_consumption %>%
  pivot_wider(names_from = "food_category", values_from = c("consumption", "co2_emission"))


# totals for consumption
cons <- fd_wide %>%
  select(starts_with("consumption")) %>%
  mutate(cons_animal = rowSums(select(., 1:7)),
         cons_nonanimal = rowSums(select(., 8:11)),
         cons_total = rowSums(select(., 1:11))) %>% 
  select(cons_animal, cons_nonanimal, cons_total)

# totol for co2 emission
co2 <- fd_wide %>%
  select(starts_with("co2")) %>%
  mutate(co2_animal = rowSums(select(., 1:7)),
         co2_nonanimal = rowSums(select(., 8:11)),
         co2_total = rowSums(select(., 1:11))) %>% 
  select(co2_animal, co2_nonanimal, co2_total)

fd_wide <- fd_wide %>% 
  bind_cols(cons) %>%
  bind_cols(co2)


# Separate in 3 groups, highest emitters, lowest and all the others
highest <- fd_wide %>%
  top_n(5, co2_total)

lowest <- fd_wide %>%
  top_n(-5, co2_total)

other <- fd_wide[6:nrow(fd_wide)-5,]


# Focus on the 2 outliers
outliers <- tibble(x = c(220, 550),
                   y = c(210, 115),
                   label = c("Tunisia (Wheat)", "Finland (Milk)"))
arrows <- tibble(x = c(200, 550),
                 y = c(206, 110),
                 xend = c(164, 550),
                 yend = c(206, 90))

# Coordinates for the labels (with datapasta)
#lowest %>%select(country, cons_animal, cons_nonanimal) %>% dpasta()
lowest_lab <- tibble::tribble(
  ~country, ~cons_animal, ~cons_nonanimal,
  "Malawi",        32,           26,
  "Rwanda",        10,           17,
  "Togo",          37,           45,
  "Mozambique",    10,           36,
  "Liberia",       33,          107.64
)


#highest %>%select(country, cons_animal, cons_nonanimal) %>% dpasta()
highest_lab <- tibble::tribble(
  ~country,  ~cons_animal, ~cons_nonanimal,
  "Argentina",     317,          100,
  "Australia",     340,           90,
  "Albania",       410,          160,
  "Iceland",       423,           80,
  "New Zealand",   266,           80
)

# Color selection
col_total <- "#e11e46"
col_nonanimal <- "#1ee1b9"


x11(type = "cairo")
showtext_auto()
other %>%
  ggplot() +
  geom_point(aes(cons_animal, cons_nonanimal, size = co2_total), color = "white", fill = col_total, shape = 21, alpha = 0.2)+
  geom_point(aes(cons_animal, cons_nonanimal, size = co2_nonanimal), color = col_nonanimal, fill = col_nonanimal, shape = 21, alpha = 0.2)+
  geom_point(data = highest, aes(cons_animal, cons_nonanimal, size = co2_total), color = "white", fill = col_total, shape = 21)+
  geom_point(data = highest, aes(cons_animal, cons_nonanimal, size = co2_nonanimal), color = col_nonanimal, fill = col_nonanimal, shape = 21)+
  geom_text(data = highest_lab, aes(cons_animal, cons_nonanimal, label = country), family = "cabin")+
  geom_point(data = lowest, aes(cons_animal, cons_nonanimal, size = co2_total), color = "white", fill = col_total, shape = 21)+
  geom_point(data = lowest, aes(cons_animal, cons_nonanimal, size = co2_nonanimal), color = col_nonanimal, fill = col_nonanimal, shape = 21)+
  geom_text(data = lowest_lab, aes(cons_animal, cons_nonanimal, label = country), family = "cabin", hjust = 0)+
  geom_text(data = outliers, aes(x, y, label = label), family = "cabin")+
  geom_curve(data = arrows, aes(x = x, y = y, xend = xend, yend = yend), arrow = arrow(length = unit(0.07, "inch")),
             size = 1, color = "gray20", curvature = -0.3)+
  scale_size_continuous(range = c(1,20)) +
  labs(title = "Food consumption & carbon footprint per capita",
       subtitle = "5 highest & lowest CO2 emitters, <span style = 'color:\"#e11e46\"'>Total emission</span> / <span style = 'color:\"#1ee1b9\"'>Part of non animal</span>",
       x = "Animal product (kg/pers)",
       y = "Non animal product (kg/pers)",
       size = "CO2 emission\n(kg CO2/pers)",
       caption = "Visualisation: Christophe Nicault | Data: nu3 / r-tastic by Kasia Kulma") +
  theme(panel.background = element_blank(),
        plot.title = element_textbox(color = "black", hjust = 0.5, size = 16, lineheight = 1, family = "oswald"),
        plot.subtitle = element_textbox(color = "black", hjust = 0.5, size = 14, lineheight = 1, family = "oswald"),
        plot.caption = element_textbox(color = "black", size = 10, lineheight = 1, family = "roboto"),
        axis.line = element_line(color = "grey"),
        panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        legend.key = element_blank()) +
  guides(fill = NA, size = guide_legend(override.aes = list(alpha = 1, color = NA, fill = "grey")))
  
