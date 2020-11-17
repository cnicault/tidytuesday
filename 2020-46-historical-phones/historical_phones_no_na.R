# title: "Tidy Tuesday - 	Historical Phones"
# author: "Christophe Nicault"
# date: "16 November 2020"

library(tidyverse)
library(glue)
library(ggforce)
library(janitor)
library(ggtext)
library(showtext)

library(paletteer)
library(patchwork)
library(ragg)


font_add_google("Roboto", "roboto")
font_add_google("Oswald", "oswald")
font_add_google("Fira Sans", "fira")


showtext_auto(enable = TRUE)

library(emojifont)
load.fontawesome()


mobile <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/mobile.csv')
landline <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/landline.csv')


# Keep only 30 highest and 30 lowest gdp (for each year, some countries might come and go)
# and calculate statistics for each year / gdp group
mobile_gdp <- mobile %>%
  filter(!is.na(gdp_per_cap)) %>%
  group_by(year) %>%
  arrange(year, gdp_per_cap) %>%
  mutate(index = seq(1,n())) %>% 
  mutate(income = case_when(
    index <= 30 ~ "low GDP",
    index > n() - 30 ~ "high GDP"
  )) %>%
  filter(!is.na(income)) %>%
  group_by(year, income) %>%
  summarise(avg_subs = mean(mobile_subs, na.rm = T),
            med_subs = median(mobile_subs, na.rm = T),
            sd_subs = sd(mobile_subs, na.rm = T),
            sdhigh = avg_subs + sd_subs,
            sdlow = avg_subs - sd_subs) %>%
  ungroup()



# Create lags to be able to calculate the coefficient of the regression line
time_prep <- mobile_gdp %>%
  group_by(income) %>%
  mutate(subs_lag = lag(avg_subs)) %>%
  ungroup()




# Find the exact point in time where the threshold is crossed for low income
# by finding the first year after it cross that point, and doing an extrapolation with the value of
# the previous year (considering that between two years, we can approximate the curve by a line, and use
# the equation of a line to find the real x value knowing y = 50)
gap_time <- time_prep %>%
  filter(income == "low GDP" & avg_subs > 50 | income == "high GDP" & avg_subs > 50) %>%
  group_by(income) %>%
  mutate(first_subs_above_50 = min(avg_subs)) %>%
  filter(avg_subs == first_subs_above_50) %>%
  mutate(threshold = 50) %>%
  mutate(
    a = (avg_subs - subs_lag) / (year - (year-1)),
    b = avg_subs - a * year,
    year_low_50 = (threshold - b) / a
  ) %>%
  pivot_wider(id_cols = threshold, names_from = income, values_from = year_low_50) %>%
  clean_names() %>%
  rename(year_high_50 = high_gdp, year_low_50 = low_gdp)



year_low_50 <- gap_time$year_low_50

# Find the value of high income for the exact point of time where the low income crossed the thresholds
# Same method as above, approx a line between two years, knowing the values for the two years and the value of 
# x which is the value year_low_50 calculated above.
gap_value <- time_prep %>%
  filter(year == floor(year_low_50), income == "high GDP") %>%
  mutate(
    a = (avg_subs - subs_lag),
    b = avg_subs - a * year,
    value_high_50 = a * year_low_50 + b,
    year_low_50 = year_low_50,
    threshold = 50
  ) %>%
  select(year_low_50, avg_subs, threshold)


highlight_seg <-  tibble(y = c(gap_value$avg_subs, gap_time$threshold, 0, 0),
                         x = c(2000, 1993, gap_time$year_high_50, gap_time$year_low_50),
                         yend = c(gap_value$avg_subs, gap_time$threshold, gap_time$threshold, gap_time$threshold),
                         xend = c(gap_value$year_low_50, gap_time$year_high_50, gap_time$year_high_50, gap_time$year_low_50))


main <- mobile_gdp %>%
  # line and ribbon
  ggplot(aes(year, avg_subs)) +
  geom_ribbon(aes(x = year, ymin = sdlow, ymax = sdhigh, fill = income), alpha = 0.3) +
  geom_line(aes(color = income), size = 1) +
  # time gap 
  geom_segment(data = gap_time, aes(x= year_high_50, xend = year_low_50, y = threshold, yend = threshold),
               size = 1, linetype = "solid", color = "#3D98D3FF",
               arrow = arrow(ends = "both", type = "closed", length = unit(0.08, "inches")) )+
  geom_ellipse(data = gap_time, aes(x0 = (year_low_50 - year_high_50) / 2 + year_high_50, y0 = threshold, a = 2, b = 15, angle = 0), fill = "grey30", size = 3, color = "#3D98D3FF", inherit.aes = FALSE  ) +
  geom_text(data = gap_time, aes(x = (year_low_50 - year_high_50) / 2 + year_high_50, y = threshold, label = glue("{round(year_low_50 - year_high_50, 0)} years")),
            family = "oswald", fontface = "bold", size = 12, color = "#3D98D3FF") +
  # value gap
  geom_segment(data = gap_value, aes(x= year_low_50, xend = year_low_50, y = avg_subs, yend = threshold),
               size = 1, linetype = "solid", color = "#3D98D3FF",
               arrow = arrow(ends = "both", type = "closed", length = unit(0.08, "inches")) )+
  geom_ellipse(data = gap_value, aes(x0 = year_low_50, y0 = (avg_subs - threshold)/2 + threshold, a = 2, b = 15, angle = 0), fill = "grey30", size = 3, color = "#3D98D3FF", inherit.aes = FALSE  ) +
  geom_text(data = gap_value, aes(x = year_low_50, y = (avg_subs - threshold)/2 + threshold, label = glue("{round(avg_subs / threshold, 1)} X")),
            family = "oswald", fontface = "bold", size = 12, color = "#3D98D3FF") +
  # axis and labels for the threshold and max value
  geom_text(data = gap_time, aes(x = 1994, y = 55, label = "50 %"),
            family = "oswald", fontface = "bold", size = 12, color = "#3D98D3FF") +
  geom_text(data = gap_time, aes(x = 2001, y = 143, label = "140 %"),
            family = "oswald", fontface = "bold", size = 12, color = "#3D98D3FF") +
  geom_segment(data = highlight_seg, aes(x=x, y=y, xend = xend, yend = yend), color= "lightgrey", linetype = "dotted", inherit.aes = FALSE) +
  # scales 
  scale_color_manual(values = c("high GDP" = "#FF363CFF", "low GDP" = "#FFCE4EFF")) +
  scale_fill_manual(values = c("high GDP" = "#FF363CFF", "low GDP" = "#FFCE4EFF")) +
  scale_x_continuous(breaks = c(1990,2000,2012)) +
  guides(color = FALSE, fill = FALSE) +
  # theme
  theme_void() +
  theme(panel.background = element_rect(fill = "grey30", color = "grey30"),
        plot.background = element_rect(fill = "grey30", color = "grey30"),
        plot.margin = margin(10,5,0,0),
        # panel.grid = element_blank(),
        # axis.line.y = element_blank(),
        # axis.line = element_blank(),
        # axis.ticks = element_blank(),
        axis.text.x = element_text(color = "lightgrey", size = 20, face = "bold"),
        axis.title = element_blank())

# Keep only 30 highest and 30 lowest gdp for the year 2017 and mobile
mobile_2017 <- mobile %>%
  filter(!is.na(gdp_per_cap), !is.na(mobile_subs), year == 2017) %>%
  group_by(year) %>%
  arrange(year, gdp_per_cap) %>%
  mutate(index = seq(1,n())) %>% 
  mutate(income = case_when(
              index <= 30 ~ "low GDP",
              index > n() - 30 ~ "high GDP"
            )
  ) %>%
  ungroup() %>%
  filter(!is.na(income)) %>%
  mutate(subs = mobile_subs,
         type = "mobile") %>%
  select(-mobile_subs)

# Keep only 30 highest and 30 lowest gdp for the year 2017 and landline
landline_2017 <- landline %>%
  filter(!is.na(gdp_per_cap), !is.na(landline_subs),year == 2017) %>%
  group_by(year) %>%
  arrange(year, gdp_per_cap) %>%
  mutate(index = seq(1,n())) %>% 
  mutate(income = case_when(
    index <= 30 ~ "low GDP",
    index > n() - 30 ~ "high GDP"
  )
  ) %>%
  ungroup() %>%
  filter(!is.na(income)) %>%
  mutate(subs = landline_subs,
         type = "landline") %>%
  select(-landline_subs)

# Calculate statistic once to make plot annotation
# mobile_2017 %>%
#   bind_rows(landline_2017) %>% 
# group_by(income, type) %>%
#   summarise(med = median(subs),
#             max = max(subs),
#             min = min(subs))


median_seg <- tibble(y = c(0, 34.1, 37.8, 76.6, 85.9, 126, 249),
                     x = c(0, 0, 0, 0, 0, 0, 0),
                     yend = c(0, 34.1, 37.8, 76.6, 85.9, 126, 249),
                     xend = c(1, 2, 1, 2, 2, 2, 2))


stats <- mobile_2017 %>%
  bind_rows(landline_2017) %>%
  # boxplot and point
  ggplot(aes(type, subs, color = income)) +
  geom_boxplot(width = 0.4, aes(fill = income), color = "lightgrey", alpha = 0.3, position = "identity", outlier.size = 0.2, outlier.stroke = 0.2) +
  geom_jitter(width = 0.1, size = 1.5, alpha = 0.7) +
  # display mobile and phone icons
  geom_text(aes(1,-20,label=fontawesome("fa-phone")), family='fontawesome-webfont', size = 40, color = "#3D98D3FF")+
  geom_text(aes(2,-20,label=fontawesome("fa-mobile")), family='fontawesome-webfont', size = 40, color = "#3D98D3FF")+
  # dotted line to each text axis
  geom_segment(data = median_seg, aes(x=x, y=y, xend = xend, yend = yend), color= "lightgrey", linetype = "dotted", inherit.aes = FALSE) +
  # scales
  scale_color_manual(values = c("high GDP" = "#FF363CFF", "low GDP" = "#FFCE4EFF")) +
  scale_fill_manual(values = c("high GDP" = "#FF363CFF", "low GDP" = "#FFCE4EFF")) +
  scale_y_continuous(breaks = c(0.47, 34.1, 37.8, 76.6, 85.9, 126, 249), labels = scales::number_format(accuracy = 0.01))+
  coord_flip() +
  guides(color = FALSE, fill = FALSE) +
  # theme
  theme_void() +
  theme(plot.margin = margin(50,0,10,0),
        panel.background = element_rect(fill = "grey30", color = "grey30"),
        plot.background = element_rect(fill = "grey30", color = "grey30"),
        # panel.grid = element_blank(),
        # axis.title = element_blank(),
        # axis.line = element_blank(),
        # axis.ticks = element_blank(),
        axis.text.x = element_text(color = "lightgrey", size = 20, face = "bold", angle = 45))


top_text <- "The <span style='color:#FF363CFF'>30 countries with the highest GDP per capita</span> reached an average of 50% of mobile<br>
              subscriptions in 2000 (50 subscriptions per 100 people).
         <br> It took <b>12 years</b> for <span style='color:#FFCE4EFF'>the 30 countries with the lowest GPD per capita</span> to reach that number.<br>
              During that time, the other countries <b>multiplied by 2.7</b> their mobile subscription to reach<br>
              on average of 140 % (1.4 subscription per person)."

bottom_text <- "The countries with the lowest GPD never developped landline, and  in 2012 they have only a<br>
                median of 0.47 subscriptions per 100 person, while the countries with the highest GDP have <br>
                 37.8 % of landline subscriptions, which is <b>80 times more</b>. 
                <br>Madagascar has the lowest number of mobile subscription with 34.1 per 100 persons, while<br>
                Hong Kong has the highest with 249 subscription per 100 persons, which is <b>7.3 times more</b>."

# create space for top right panel
space <- ggplot() +
  theme_void()+
  theme(
    panel.background = element_rect(fill = "grey30", color = "grey30"),
    plot.background = element_rect(fill = "grey30", color = "grey30"),
  )

# text to plot on top right panel with an inset_element to put it closer to the two graphs
text <- ggplot() +
  geom_segment(aes(x = 0, y = 0, xend = 10, yend = 10), color = "grey30") +
  geom_segment(aes(x = 2, y = 5, xend = 0, yend = 5), color = "grey50", size = 1, arrow = arrow(ends = "last", type = "closed", length = unit(0.08, "inches"))) +
  geom_segment(aes(x = 0.8, y = 4, xend = 0.8, yend = 0), color = "grey50", size = 1, arrow = arrow(ends = "last", type = "closed", length = unit(0.08, "inches"))) +
  geom_richtext(aes(1,9),label = top_text, family = "fira", size = 9, label.colour = NA, fill = NA, color = "lightgrey", lineheight = 0.4, hjust = 0, vjust = 1) +
  geom_richtext(aes(1,4),label = bottom_text, family = "fira", size = 9, label.colour = NA, fill = NA, color = "lightgrey", lineheight = 0.4, hjust = 0, vjust = 1) +
  labs(title = "Phone inequality between countries with <span style='color:#FFCE4EFF'>low</span> and <span style='color:#FF363CFF'>high</span> GPD per capita")+
  theme_void() +
  theme(
    panel.background = element_rect(fill = "grey30", color = "grey30"),
    plot.background = element_rect(fill = "grey30", color = "grey30"),
    plot.title = element_textbox(size = 35, color = "lightgrey", family = "oswald", face = "bold", hjust = 0.5)
  )


# put everything together
final <- (main + (space / stats)) +
  inset_element(text, left = -0.05, bottom = 0.45, top = 0.95, right = 0.9, clip = TRUE) +
  plot_annotation(caption = "Visualisation: Christophe Nicault | Data: OurWorldInData.org",
                  theme = theme(plot.background = element_rect(fill = "grey30", color = "grey30"),
                                panel.background = element_rect(fill = "grey30", color = "grey30"),
                                plot.caption = element_text(size = 20, color = "lightgrey"))
  )

# save the final plot
ggsave(here::here("render", paste0("historical-phones-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")),
       plot = final, device = agg_png(width = 12, height = 6, units = "in", res = 300))

