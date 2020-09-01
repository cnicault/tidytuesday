# title: "Tidy Tuesday - Global Crop Yields"
# author: "Christophe Nicault"
# date: "01 September 2020"


library(tidyverse)
library(ggtext)
library(showtext)
library(ggforce)
library(ragg)

font_add_google("Fira Sans", "Fira Sans")
font_add_google("Oswald", "Oswald")

showtext_auto()

theme_set(theme_minimal(base_family = "Oswald"))

key_crop_yields <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv')
fertilizer <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/cereal_crop_yield_vs_fertilizer_application.csv')
tractors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/cereal_yields_vs_tractor_inputs_in_agriculture.csv')
land_use <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/land_use_vs_yield_change_in_cereal_production.csv')
arable_land <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/arable_land_pin.csv')


colnames(land_use) <- c('country', 'code', 'year', 'yield_index', 'land_area', 'population')
colnames(fertilizer) <- c('country', 'code', 'year', 'cereal_yield', 'nitrogen')
colnames(arable_land) <- c('country', 'code', 'year', 'arable_land')
land_use$year <- as.numeric(land_use$year)

min_land_use <- land_use %>%
  filter(year == 1961) %>%
  select(-country, -year) %>%
  rename_all(function(x){paste0("min_", x)})

land_prep <- land_use %>%
  left_join(min_land_use, by = c("code" = "min_code")) %>%
  mutate(yield_index = yield_index / min_yield_index,
         land_area = land_area / min_land_area,
         population = round(population / min_population, 2)) %>%
  select(1:6)


france_nb <- land_prep %>%
  filter(year == 2014, code == "FRA") %>%
  select(-code, -year) %>%
  pivot_longer(-country, names_to = "name", values_to = "label") %>%
  mutate(x = c(200, 200, 200),
         y = c(300, 100, 200))

others_nb <- land_prep %>%
  filter(year == 2014, code %in% c("DEU", "GBR", "ITA", "ESP")) %>%
  select(-code, -year)%>%
  pivot_longer(-country, names_to = "name", values_to = "label")

countries_stats <- others_nb %>%
  mutate(name = factor(name, levels = c("land_area", "population", "yield_index"), ordered = T)) %>%
  ggplot(aes(x = name, y = label, label = paste0(label, "x"), fill = name)) +
  geom_col() +
  scale_fill_manual(values = c("yield_index" = "#fee391",  "land_area" = "#66c2a4", "population" = "#d4b9da")) +
  geom_text(size = 20, position = position_stack(vjust = 0.5), color = "black", fontface = "bold",) +
  coord_flip() +
  facet_wrap(~country, ncol = 1) +
  theme_void() +
  guides(fill = F) +
  theme(panel.spacing = unit(8, "lines"),
        strip.text = element_blank())

countries <- map_data("world") %>% filter(region %in% c("Germany", "UK", "Italy", "Spain"))

countries_map <- countries %>%
  ggplot(aes(long, lat)) +
  geom_polygon(aes(group = group), fill = "lightgrey") +
  facet_wrap(~region, ncol = 1, scales = "free") +
  theme_void() +
  theme(panel.spacing = unit(2, "lines")) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        strip.text = element_blank())

france <- map_data("world") %>% filter(region == "France")
fra_map <- france %>%
  ggplot(aes(long, lat)) +
  geom_polygon(aes(group = group), fill = "lightgrey") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank())

p1 <- ggplot() +
  scale_x_continuous(limits = c(0,600)) +
  scale_y_continuous(limits = c(0,400)) +
  theme(plot.background = element_rect(fill = "darkgray"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p2 <- france_nb %>%
  ggplot() +
  geom_circle(aes(x0 = x, y0 = y - 10 , r = 30, fill = name, color = name), size = 8) +
  geom_text(aes(x = x, y = y - 20, label = str_to_title(str_replace(name, "_", " "))),
            family = "Oswald",
            fontface = "bold",
            size = 25
  ) +
  geom_text(aes(x = x, y = y, label = paste0(label, "x")),
                family = "Oswald",
                fontface = "bold",
                size = 45
            ) +
  scale_x_continuous(limits = c(0,600)) +
  scale_y_continuous(limits = c(0,400)) +
  scale_color_manual(values = c("yield_index" = "#ec7014",  "land_area" = "#238b45", "population" = "#ce1256")) +
  scale_fill_manual(values = c("yield_index" = "#fee391",  "land_area" = "#66c2a4", "population" = "#d4b9da")) +
  theme_void() +
  guides(fill = F, color = F)


final <- p1 + annotation_custom(ggplotGrob(fra_map), xmin = 0, ymin = -50, xmax = 400, ymax = 400) +
  annotation_custom(ggplotGrob(p2), xmin = 0, ymin = 0, xmax = 600, ymax = 400) +
  annotation_custom(ggplotGrob(countries_map), xmin = 400, ymin = 0, xmax = 500, ymax = 400) + 
  annotation_custom(ggplotGrob(countries_stats), xmin = 520, ymin = 0, xmax = 620, ymax = 400) +
  labs(title = "Evolution of crop yield index, land area and population in FRance since 1961",
       subtitle = "Comparison with its nearest biggest neighbours",
       caption = "Visualization: Christophe Nicault | Data: Our World in Data") +
  theme(plot.title = element_text(size = 120, hjust = 0.5, family = "Fira Sans", margin = margin(10, 0, 0, 0)),
        plot.subtitle = element_text(size = 90, hjust = 0.5, family = "Fira Sans", margin = margin(10, 0, 0, 0)),
        plot.caption = element_text(size = 70, family = "Fira Sans", hjust = 0, margin = margin(0, 0, 20, 20)))

ggsave(here::here("render", paste0("crop_yield-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")),
       plot = final, device = agg_png(width = 1920 , height = 1280, units = "px"))
