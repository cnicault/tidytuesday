# title: "Tidy Tuesday - 	Passwords"
# author: "Christophe Nicault"
# date: "14 january 2020"

library(tidyverse)
library(ggthemr)
library(RColorBrewer)

ggthemr("grape")

theme_update(plot.title = element_text(hjust = 0.5),
             plot.subtitle = element_text(hjust = 0.5),
             plot.caption = element_text(size = 8))

passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')

# passwords %>%
#   mutate(online = case_when(time_unit == "seconds" ~ value,
#                             time_unit == "minutes" ~ value * 60,
#                             time_unit == "hours" ~ value * 60 * 60,
#                             time_unit == "days" ~ value * 60 * 60 * 24,
#                             time_unit == "weeks" ~ value * 60 * 60 * 24 * 7,
#                             time_unit == "months" ~ value * 60 * 60 * 24 * 365/12,
#                             time_unit == "years" ~ value * 60 * 60 * 24 * 365)) %>%
#   mutate(ratio = online / offline_crack_sec)


passwords <- passwords %>%
  filter(!is.na(password)) %>%
  mutate(nb_lower = str_count(password, pattern = "[a-z]"),
         nb_upper = str_count(password, pattern = "[A-Z]"),
         nb_num = str_count(password, pattern = "[0-9]"),
         nb_spec = str_count(password, pattern = "![a-zA-Z0-9]"),
         nb_tot = nchar(password)) %>%
  mutate(strength_disc = case_when(strength <= 10 ~ "super weak",
                                   strength > 10 & strength <= 30 ~ "very weak",
                                   strength > 30 ~ "weak"))


passwords %>%
  ggplot(aes(nb_lower, nb_num, color = strength_disc, size = strength)) +
  geom_point(position = "jitter", alpha = 0.3) +
  coord_fixed() +
  scale_y_continuous(breaks = c(0,2,4,6,8)) +
  scale_color_manual(values = c("super weak" = "darkred", "very weak" = "darkblue", "weak" = "darkgreen")) +  
  labs(title = "Password strength",
       subtitle = "depending on number of characters and digits",
       x = "Number of characters",
       y = "Number of digits",
       color = "Weakness",
       size = "Strength",
       caption = "Visualisation: Christophe Nicault | Data: Knowledge is Beautiful")


colourCount = length(unique(passwords$category))
getPalette = colorRampPalette(brewer.pal(10, "Set3"))

passwords %>%
  mutate(category = str_to_title(category)) %>%
  ggplot(aes(category, offline_crack_sec)) +
  geom_violin(aes(group = category, fill = category), alpha = 0.3, color = NA) +
  scale_y_log10() + 
  geom_point(aes(color = strength_disc, size = strength), position = "jitter", alpha = 0.5)+
  scale_fill_manual(values = getPalette(colourCount))+
  scale_color_manual(values = c("super weak" = "darkred", "very weak" = "darkblue", "weak" = "darkgreen")) +  
  theme(axis.text.x = element_text(angle=60, hjust=1),
        axis.title.x = element_blank()) +
  labs(title = "Password time to crack and strength",
       subtitle = "Distribution of time to crack by category \n Point for each password, with its strength",
       y = "Time to crack in seconds",
       color = "Weakness",
       size = "Strength",
       fill = "Category",
       caption = "Visualisation: Christophe Nicault | Data: Knowledge is Beautiful")


