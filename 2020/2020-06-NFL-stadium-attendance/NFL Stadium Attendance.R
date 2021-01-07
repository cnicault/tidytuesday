# title: "Tidy Tuesday - 	NFL Stadium Attendance"
# author: "Christophe Nicault"
# date: "05 Februart 2020"


library(tidyverse)
library(scales)
library(RColorBrewer)

attendance <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv')
standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv')
games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/games.csv')

# Number of attendee in the past 3 years
attendance %>%
  group_by(team, year) %>%
  summarise(total = mean(total),
            tot = sum(total)) %>%
  top_n(3, year) %>% 
  ungroup() %>%
  group_by(team) %>%
  mutate(tot3yr = sum(total)) %>%
  ungroup() %>%
  mutate(team = fct_reorder(team, tot3yr)) %>%
  ggplot(aes(team, total, fill = as.factor(year))) +
  geom_col() +
  scale_y_continuous(labels = comma) +
  coord_flip() +
  labs(title = "Total number of attendee in the past 3 years",
       x = "# of attendee",
       y = "Team city",
       fill = "Years")

# Winners of the Superbowl
standings %>%
  mutate(winner = ifelse(sb_winner == "Won Superbowl", 1, 0)) %>%
  group_by(team) %>%
  summarise(nb_sb = sum(winner)) %>%
  ungroup() %>%
  filter(nb_sb != 0) %>%
  mutate(team = fct_reorder(team, nb_sb)) %>%
  ggplot(aes(team, nb_sb, fill = nb_sb)) +
  geom_col() +
  coord_flip() +
  labs(title = "Winners of the Superbowl",
       x = "# of victory",
       Y = "Team",
       fill = "# of victory")

winners <- standings %>%
  mutate(winner = ifelse(sb_winner == "Won Superbowl", 1, 0)) %>%
  group_by(team) %>%
  summarise(nb_sb = sum(winner)) %>%
  filter(nb_sb > 1) %>%
  select(team) %>%
  pull
  
# Data preparation for final visualisation
stand_df <- standings %>%
  mutate(winner = ifelse(sb_winner == "Won Superbowl", 1, 0)) %>%
  mutate_at(.vars = c("wins", "loss", "points_differential", "margin_of_victory", "offensive_ranking", "defensive_ranking"), rescale) %>%
  pivot_longer(cols = c("wins", "loss", "points_differential", "margin_of_victory", "offensive_ranking", "defensive_ranking"), names_to = "measure", values_to = "value") %>%
  mutate(ty = paste(team, year, sep = "-"),
         tc = ifelse(team %in% winners, team, "Others")) %>%
  mutate(measure = str_to_title(str_replace_all(measure, "_", " ")))


win_df <- stand_df %>% 
  filter(tc != "Others", winner == 1)

win_uni <- stand_df %>% 
  filter(tc == "Others", winner == 1)


# Mix the 3 groups for the final plot
final_plot <- stand_df %>%
  filter(tc == "Others") %>%
  filter(winner == 0) %>%
  ggplot(aes(measure, value))+
  geom_jitter(color = "grey", width = 0.15, alpha = 0.3)+
  geom_line(aes(group = ty), color = "grey", alpha = 0.2) +
  geom_point(data = win_uni, aes(measure, value, group = ty, color = "Other"), size = 2, alpha = 0.3)+
  geom_line(data = win_uni, aes(measure, value, group = ty, color = "Other"), size = 1, alpha = 0.3)+
  geom_point(data = win_df, aes(measure, value, color = tc, group = ty), size = 2)+
  geom_line(data = win_df, aes(measure, value, color = tc, group = ty), size = 1)+
  scale_color_brewer(palette = "Set1")+
  labs(title = "Comparison of team performance",
       subtitle = "Highlighting the teams that won the superbowl, \n with a focus on the teams who won more than once",
       x = "Metric",
       y = "Measure (scaled 0 to 1)",
       color = "Team",
       caption = "Visualization : Christophe Nicault | Data : Pro Football Reference")+
  theme(plot.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 0.5, size = 12),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank())

final_plot

ggsave("2020-06-NFL-stadium-attendance\\NFL_team_perf.png", final_plot, width = 210, height = 150, units = "mm")


