# title: "Tidy Tuesday - Traumatic Brain Injury"
# author: "Christophe Nicault"
# date: "29 March 2020"


library(tidyverse)
library(scales)
library(ggforce)
library(here)
library(patchwork)
library(ggtext)
library(showtext)


font_add_google("Roboto", "roboto")
font_add_google("Oswald", "oswald")

tbi_age <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_age.csv')
tbi_year <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_year.csv')
tbi_military <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_military.csv')

showtext_auto()

p1 <- tbi_year %>% 
  filter(injury_mechanism == "Total") %>%
  mutate(year = as.factor(year),
         type = fct_reorder(type, rate_est)) %>%
  ggplot(aes(year, rate_est, fill = type)) +
  geom_col(position = "dodge", width = 1) +
  scale_fill_manual(values = c("Deaths" = "#7a0177", "Hospitalizations" = "#f768a1" ,"Emergency Department Visit" = "#fcc5c0"))+
  labs(title = "The visits to emergency department have increased <br>by 54% in 8 years",
       y = "Rate per 100,000",
       fill = "") +
  theme(panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_line(color = "grey"),
        panel.grid.major.y = element_line(color = "grey", linetype = "dotted"),
        legend.key.height = unit(0.5, "mm"),
        panel.spacing.y = unit(20, "mm"),
        legend.title=element_text(size = 2),
        legend.position = "bottom",
        legend.direction = "vertical",
        plot.title = element_markdown(color = "black", hjust = 0.5, size = 14, family = "roboto")
  )

falls <- tibble(x = c("2006", "2014"),
                y = c(209, 375),
                nudge = c(-0.1, 0.08),
                label = c("209", "375"))


object <- tibble(x = c("2006", "2014"),
                 y = c(90.8, 144),
                 nudge = c(-0.1, 0.08),
                 label = c("90.8", "144"))

p2 <- tbi_year %>% 
  filter(injury_mechanism != "Total", type == "Emergency Department Visit") %>%
  filter(year == 2006 | year == 2014) %>%
  mutate(year = as.factor(year),
         highlight = ifelse(str_detect(injury_mechanism, "Unintentional"), injury_mechanism, "Other")) %>%
  ggplot(aes(year, rate_est, color = highlight)) +
  geom_point(size = 2) +
  geom_line(aes(group = injury_mechanism), size = 1.5) +
  geom_vline(xintercept = "2006", color = "grey") +
  geom_vline(xintercept = "2014", color = "grey") +
  geom_text(data = falls, aes(x, y, label = label), nudge_x = falls$nudge, family = "cabin", color = "#7a0177")+
  geom_text(data = object, aes(x, y, label = label), nudge_x = object$nudge, family = "cabin", color = "#f768a1")+
  scale_color_manual(values = c("Other" = "grey", "Unintentional falls" = "#7a0177" ,"Unintentionally struck by or against an object" = "#f768a1"))+
  scale_x_discrete(expand = c(0.5,0.5))+
  labs(title = "The increase is due mainly to Unintentionnally falls<br> or struck by/against on object",
       color = "")+
  theme(panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "bottom",
        legend.direction = "vertical",
        legend.key.height = unit(0.5, "mm"),
        plot.margin = margin(20, 0, 0, 0),
        plot.title = element_markdown(color = "black", hjust = 0.5, size = 14, family = "roboto"))

p3 <- tbi_age %>%
  filter(type == "Emergency Department Visit", injury_mechanism == "Unintentional Falls") %>%
  filter(age_group != "Total", age_group != "0-17") %>%
  mutate(age_group = fct_reorder(age_group, number_est),
         radius = sqrt(number_est / pi)) %>%
  arrange(radius) %>%
  mutate(radlag = ifelse(is.na(lag(radius)), 0, lag(radius)),
         x = cumsum(radius + radlag)) %>%
  ggplot() +
  geom_circle(aes(x0 = x, y0 = radius, r = radius, fill = number_est, color = number_est), alpha = 0.6, size = 1) +
  geom_text(aes(x, radius, label = age_group), size = 5)+
  scale_y_continuous(expand = c(0.1, 0.1))+
  scale_fill_gradient2(low = "#fcc5c0", high = "#7a0177")+
  scale_color_gradient2(low = "#fcc5c0", high = "#7a0177")+
  coord_fixed()+
  labs(title = "The \"unintentional falls\" affect mainly the elderly and babies (group of 75+ and 0-4)") +
  theme(panel.background = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        plot.margin = margin(30, 0, 0, 0),
        plot.title = element_textbox(color = "black", hjust = 0.5, size = 14, family = "roboto"))

p4 <- tbi_age %>%
  filter(type == "Emergency Department Visit", injury_mechanism == "Unintentionally struck by or against an object") %>%
  filter(age_group != "Total", age_group != "0-17") %>%
  mutate(age_group = fct_reorder(age_group, number_est),
         radius = sqrt(number_est / pi)) %>%
  arrange(radius) %>%
  mutate(radlag = ifelse(is.na(lag(radius)), 0, lag(radius)),
         x = cumsum(radius + radlag)) %>%
  ggplot() +
  geom_circle(aes(x0 = x, y0 = radius, r = radius, fill = number_est, color = number_est), alpha = 0.6, size = 1) +
  geom_text(aes(x, radius, label = age_group), size = 5)+
  scale_y_continuous(expand = c(0.1, 0.1))+
  scale_fill_gradient2(low = "#fcc5c0", high = "#7a0177")+
  scale_color_gradient2(low = "#fcc5c0", high = "#7a0177")+
  coord_fixed()+
  labs(title = "While \"unintentionally struck by or against an object\" affect mainly the children and teenagers (group of 15-24 and 5-14)") +
  theme(panel.background = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        plot.margin = margin(15, 0, 0, 0),
        plot.title = element_textbox(color = "black", hjust = 0.5, size = 14, family = "roboto"))


x11(type = "cairo", antialias = "subpixel")

(p1 + p2) / p3 / p4 + 
  plot_layout(widths = c(2, 1), heights = c(1, 1, 1)) +
  plot_annotation(title = "Main causes and their group age repartition in the increase of emergency visits<br> for traumatic brain injuries",
                  caption = "Visualisation: Christophe Nicault | Data: Traumatic Brain Injury / CDC",
                  theme = theme(
                    plot.title = element_markdown(hjust = 0.5, size = 20, family = "oswald"),
                    plot.margin = margin(20, 5, 5, 5)
                  )
  )


ggsave(here::here("2020-13-traumatic-brain-injury", "emergency_visit_brain_injury.png"))

showtext_auto(FALSE)
