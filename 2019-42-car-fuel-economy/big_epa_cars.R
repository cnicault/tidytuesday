# title: "Tidy Tuesday - Big EPA Cars"
# author: "Christophe Nicault"
# date: "15 octobre 2019"

library(tidyverse)
library(ggthemr)
ggthemr("earth", type = "outer")
theme_update(plot.title = element_text(hjust = 0.5),
             plot.subtitle = element_text(hjust = 0.5))

big_epa_cars <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-15/big_epa_cars.csv")

# co2 and co2TailpipeGpm have the same definition in the data description.
#  Let's check the differences
big_epa_cars %>%
  select(co2, co2TailpipeGpm) %>%
  ggplot(aes(co2, co2TailpipeGpm)) + 
  geom_point() +
  coord_fixed(ratio = 1) +
  labs(title = "co2TailpipeGpm vs co2",
       subtitle = "Can we use co2TailpipeGpm instead of co2?",
       x = "co2 in grams / mile",
       y = "co2TailpipeGpm in grams / mile") 

summary(big_epa_cars$co2)
summary(big_epa_cars$co2TailpipeGpm)
# It appears that -1 is used for missing values in co2 and there's no missing in co2TailpipeGpm

# Lets confirm that the values are identical between co2 and co2TailpipeGpm and that the only difference 
# is the missing value in co2
big_epa_cars %>%
  select(make, model, co2, co2TailpipeGpm) %>%
  filter(co2 != floor(co2TailpipeGpm)) %>%
  distinct(co2)
# I can use co2TailpipeGpm which appears to be the decimal value of the co2. 

# Biggest co2 emission
big_co2 <- big_epa_cars %>%
  filter(co2TailpipeGpm == max(co2TailpipeGpm)) %>%
  select(make, model, co2TailpipeGpm, year) %>%
  filter(year == max(year)) %>%
  distinct() %>%
  mutate(car = paste(make, model))

# first zero  emission
first_0_co2 <- big_epa_cars %>%
  filter(co2TailpipeGpm == 0) %>%
  select(make, model, year) %>%
  distinct() %>% 
  arrange(year) %>%
  filter(year == min(year)) %>%
  mutate(car = paste(make, model))


big_epa_cars %>%
  mutate(co2ana = co2TailpipeGpm) %>% 
  select(make, model, year, co2ana) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(year, co2ana, color = co2ana)) +
  geom_point(alpha = 0.5, position = "jitter")+
  stat_summary(fun.y=max, colour="red", geom="line", aes(group = 1), size = 2, alpha = 0.5) +
  stat_summary(fun.y=min, colour="green", geom="line", aes(group = 1), size = 2, alpha = 0.5) +
  stat_summary(fun.y=mean, colour="orange", geom="line", aes(group = 1), size = 2, alpha = 0.8) +
  scale_x_discrete(breaks = c(1985,1990,1995,2000,2005,2010,2015,2020)) +
  scale_color_gradient2(name = "CO2 Emission", low="green", mid = "grey", high = "red", midpoint = 500) +
  annotate("label", x =2, y = 540, size = 3, color = "orange", label = "Mean", fill = "#36312C", fontface = "bold") +
  annotate("text", x =2, y = 1320, size = 3, color = "red", label = "Max", alpha = 0.7, fontface = "bold") +
  annotate("text", x =2, y = 160, size = 3, color = "green", label = "Min", alpha = 0.7, fontface = "bold") +
  annotate("text", x = (big_co2$year - min(big_epa_cars$year)+6), y = big_co2$co2TailpipeGpm, size = 3, 
           color = "white", label = big_co2$car) +
  annotate("text", x = (first_0_co2$year - min(big_epa_cars$year)-5), y = 0, size = 3, 
           color = "white", label = first_0_co2$car[1]) +
  annotate("text", x = (first_0_co2$year - min(big_epa_cars$year)-5), y = 40, size = 3, 
           color = "white", label = first_0_co2$car[2]) +
  annotate("text", x = (2011 - min(big_epa_cars$year)+5), y = 40, size = 3, 
           color = "white", label = "1st Tesla") +
  geom_curve(aes(x = (big_co2$year - min(big_epa_cars$year)+3), y = big_co2$co2TailpipeGpm+10, xend = (big_co2$year - min(big_epa_cars$year)+1), yend = big_co2$co2TailpipeGpm + 10), 
             arrow = arrow(length = unit(0.1, "inch")), size = 0.3, color = "grey85", curvature = 0.5) +
  geom_curve(aes(x = (unique(first_0_co2$year) - min(big_epa_cars$year) - 3), y = 0, xend = (unique(first_0_co2$year) - min(big_epa_cars$year)+1), yend = 0), 
             arrow = arrow(length = unit(0.1, "inch")), size = 0.3, color = "grey85", curvature = 0.3) +
  geom_curve(aes(x = (2011 - min(big_epa_cars$year) + 3), y = 40, xend = (2011 - min(big_epa_cars$year)+1), yend = 0), 
             arrow = arrow(length = unit(0.1, "inch")), size = 0.3, color = "grey85", curvature = 0.3) +
  labs(title = "Evolution in co2 emission over year",
       x = "Years",
       y = "Co2 in grams / mile",
       caption = "Visualisation: Christophe Nicault | Data: www.fueleconomy.gov")
