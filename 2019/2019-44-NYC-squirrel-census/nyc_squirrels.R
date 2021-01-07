# title: "Tidy Tuesday - 	NYC Squirrel Census"
# author: "Christophe Nicault"
# date: "30 octobre 2019"

library(tidyverse)
library(ggalluvial)
library(janitor)
library(scales)
library(ggthemr)
theme_update(plot.title = element_text(hjust = 0.5),
             plot.subtitle = element_text(hjust = 0.5))

nyc_squirrels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

ggthemr("light", type = "outer")


# Creation of an activity columns by merging columns from running to foraging
squirrel_activity <- nyc_squirrels %>%
  select(running:foraging)


logical_to_name <- function(index, df){
  column <- df[[index]]
  column[which(column == TRUE)] <- colnames(df)[[index]]
  return(column)
}

merge_names <- function(x, y){
  ifelse(x == FALSE, y, x)
}

squirrel_activity <- map(seq_along(squirrel_activity), logical_to_name, squirrel_activity)
nyc_squirrels$activity <- purrr::reduce(squirrel_activity, merge_names)

# See if activity is different between adults and juveniles

# chisq to study if distribution is identical between adults & juvenile
age_act <- nyc_squirrels %>%
  filter(age != "?") %>%
  select(activity, age) %>%
  table()

res <- chisq.test(age_act)
# The null hypothesis is that juvenil and adult behave the same way
# the p-value is under 0.05, we can reject the null hypothesis and assume
# that juvenil and adult behave in a different way.
result_method <- res$method
result_stat <- paste0(attributes(res$statistic), ": ", round(res$statistic, 2))
result_pvalue <- paste0("p-value: ", scientific(res$p.value,5))
result_df <- paste0(attributes(res$parameter), ": ", res$parameter)

nyc_squirrels %>%
  filter(age != "?", !is.na(age), activity != FALSE) %>%
  ggplot(aes(age, fill = activity)) +
  geom_bar(position = "fill")+
  annotate("text", x = 1.5, y = 0.6, size = 3, color = "black", label = result_method, fontface = "bold")+
  annotate("text", x = 1.5, y = 0.55, size = 3, color = "black", label = result_stat, fontface = "bold")+
  annotate("text", x = 1.5, y = 0.5, size = 3, color = "black", label = result_pvalue, fontface = "bold")+
  annotate("text", x = 1.5, y = 0.45, size = 3, color = "black", label = result_df, fontface = "bold")+
  annotate("text", x = 1.5, y = 0.4, size = 3, color = "black", label = "The test shows a significant association between age and activity", fontface = "bold")+  
  theme(axis.title.y = element_blank())+
  labs(title = "Squirrel activity by age",
       subtitle = "More juvenile squirrels are seen climbing and more adults are seen foraging",
       x= "Age",
       fill = "Activity",
       caption = "Visualisation: Christophe Nicault | Data: Squirrel Census")


# See if activity is different between morning and afternoon

shift_act <- nyc_squirrels %>%
  filter(activity != "FALSE") %>%
  select(activity, shift) %>%
  table()

res <-chisq.test(shift_act)

result_method <- res$method
result_stat <- paste0(attributes(res$statistic), ": ", round(res$statistic, 2))
result_pvalue <- paste0("p-value: ", scientific(res$p.value,5))
result_df <- paste0(attributes(res$parameter), ": ", res$parameter)

nyc_squirrels %>%
  filter(activity != FALSE) %>%
  ggplot(aes(shift, fill = activity)) +
  geom_bar(position = "fill")+
  annotate("text", x = 1.5, y = 0.6, size = 3, color = "black", label = result_method, fontface = "bold")+
  annotate("text", x = 1.5, y = 0.55, size = 3, color = "black", label = result_stat, fontface = "bold")+
  annotate("text", x = 1.5, y = 0.5, size = 3, color = "black", label = result_pvalue, fontface = "bold")+
  annotate("text", x = 1.5, y = 0.45, size = 3, color = "black", label = result_df, fontface = "bold")+
  annotate("text", x = 1.5, y = 0.4, size = 3, color = "black", label = "The test shows a significant association between shift and activity", fontface = "bold")+
  theme(axis.title.y = element_blank())+
  labs(title = "Squirrel activity by shift",
       subtitle = "Squirrels are more seen climbing in the morning and eating in the afternoon",
       x = "Shift",
       fill = "Activity",
       caption = "Visualisation: Christophe Nicault | Data: Squirrel Census")

# Location of squirrels by fur color


dist_color <- nyc_squirrels %>%
  filter(primary_fur_color != FALSE) %>%
  group_by(primary_fur_color) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  mutate(primary_fur_color = fct_reorder(primary_fur_color, desc(total))) %>%
  ggplot(aes(primary_fur_color, total, fill = as.factor(primary_fur_color))) +
  geom_col(alpha = 0.8) +
  scale_fill_manual(values = c("Black" = "black", "Gray" = "Grey", "Cinnamon" = "brown")) +
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 8),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 8),
        axis.text.x = element_blank(),
        legend.position = "none")+
  labs(title = "Count by color",
       fill = "Fur colors",
       x = "Primary fur color",
       y = "Number of squirrels")

location <- nyc_squirrels %>%
  filter(primary_fur_color != FALSE) %>%
  ggplot(aes(long, lat, color = as.factor(primary_fur_color))) +
  geom_point(alpha = 0.8) +
  scale_color_manual(values = c("Black" = "black", "Gray" = "Grey", "Cinnamon" = "brown")) +
  theme(panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())+
  labs(title = "Location of squirrels by fur color",
       color = "Fur colors",
       caption = "Visualisation: Christophe Nicault | Data: Squirrel Census")


location + annotation_custom(ggplotGrob(dist_color), xmin = -73.96, xmax = -73.95, ymin = 40.765, ymax = 40.78)

# Combination of fur colors

nyc_squirrels <- nyc_squirrels %>% anti_join(get_dupes(nyc_squirrels, unique_squirrel_id), by = "unique_squirrel_id")

nyc_squirrels %>%
  filter(!is.na(primary_fur_color)) %>%
  rename(primary = primary_fur_color) %>%
  separate(highlight_fur_color, c("second", "third", sep = ", ")) %>%
  mutate(second = ifelse(is.na(second), "None", second),
         third = ifelse(is.na(third), "None", third)) %>%
  select(unique_squirrel_id, primary, second, third) %>%
  group_by(primary, second, third) %>%
  mutate(total = 1) %>%
  gather(type_color, color, c(primary, second, third)) %>%
  ggplot(aes(x = type_color, stratum = color, alluvium = unique_squirrel_id,  fill = color)) +
  geom_flow(alpha = 0.8)+
  geom_stratum(alpha = 0.8, linetype = "blank")+
  geom_text(stat = "stratum", label.strata = TRUE, size = 2, color = "#0a0a0a")+
  scale_fill_manual(values = c("Black" = "black", "Gray" = "Grey", "Cinnamon" = "#b76343", "White" = "white", "None" = "#ffcccc"))+
  scale_x_discrete(labels = c("Primary fur color", "Highlight fur color", "Third fur color"), expand = c(0,0))+
  theme(panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())+
  labs(title = "Combination of fur colors",
       fill = "Fur colors",
       caption = "Visualisation: Christophe Nicault | Data: Squirrel Census")


