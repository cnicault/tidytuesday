# title: "Tidy Tuesday - 	Hotel Bookings"
# author: "Christophe Nicault"
# date: "12 February 2020"


library(tidyverse)
library(scales)
library(showtext)
library(lubridate)
library(countrycode)
library(xts)
library(ggthemr)
library(patchwork)
library(ggtext)

ggthemr("flat")

font_add_google("Lato", "Lato")

Sys.setlocale("LC_TIME", "English")

hotels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv')


showtext_auto()

# Cleaning
hotel_df <- hotels %>%
  filter(stays_in_week_nights != 0 | stays_in_weekend_nights != 0) 

# Country with the highest cancellation rate for countries having at least 100 booking
# to be significant ( some countries have 1 booking, 1 cancellation, so 100% cancellation rate)
p1 <- hotel_df %>%
  filter(hotel == "Resort Hotel") %>%
  mutate(country = fct_recode(country, CHN = "CN")) %>%
  group_by(country) %>%
  summarise(prop = sum(is_canceled) / n(),
            tot = n()) %>%
  ungroup() %>%
  mutate(country_name = countrycode(sourcevar = country, origin = "iso3c", destination = "un.name.en"),
         country_name = ifelse(str_detect(country_name, "United Kingdom"), "United Kingdom", country_name)) %>%
  filter(tot > 100) %>%
  arrange(desc(prop)) %>% 
  head(20) %>% 
  mutate(country_name = fct_reorder(country_name, prop)) %>%
  ggplot(aes(country_name, prop, fill = prop)) +
  geom_col()+
  coord_flip() +
  scale_y_continuous(labels = percent) +
  labs(title = "% of cancellation per country",
       subtitle = "20 first countries with at least 100 bookings",
       y = "% of cancellation",
       fill = "% of cancellation")+
  theme(legend.position = c("bottom"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.y = element_blank())

# Calcul of the delay between booking and cancellation
cancel_delay <- hotel_df %>%
  filter(hotel == "Resort Hotel") %>%
  mutate(date = glue::glue("{arrival_date_year}-{arrival_date_month}-{arrival_date_day_of_month}"),
         date = parse_date(date, format = "%Y-%B-%d"),
         booking_date = date - lead_time) %>%
  count(booking_date, country, reservation_status_date, sort = TRUE) %>%
  mutate(cancel_delay = reservation_status_date - booking_date)

# Plot the cancellations for the biggest wave of cancellation
p2 <- cancel_delay %>%
  arrange(desc(n)) %>%
  head(60) %>%
  mutate(country_name = countrycode(sourcevar = country, origin = "iso3c", destination = "un.name.en"),
         country_name = ifelse(str_detect(country_name, "United Kingdom"), "United Kingdom", country_name)) %>%
  ggplot() +
  geom_point(aes("Booking", booking_date,  fill = country_name, size = n), shape = 21, color = "black")+
  geom_point(aes("Cancellation", reservation_status_date, fill = country_name, size = n), shape = 21, color = "black")+
  geom_segment(aes(x ="Booking", xend = "Cancellation", y = booking_date, yend = reservation_status_date, size = n, color = country_name), alpha = 0.3) +
  scale_size_continuous(range = c(1,10)) + 
  scale_x_discrete(expand = c(0.1,0.1))+
  labs(title = "Delay for wave of cancellations",
       subtitle = "50 biggest cancellation (same booking date & cancellation date per country)",
       y = "Date",
       fill = "Country",
       size = "# cancellation")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.line.x = element_blank(),
        axis.title.x = element_blank())+
  guides(color = FALSE)

# The goal is to establish how many people are staying each night.
# We just have the date of arrival, and the number of days, so we need 
# some calculation.
# I first create a df with the date, the number of nights, and the departure calculated with the 2 previous variables
stay_duration <- hotels %>% 
  filter(hotel == "Resort Hotel", is_canceled == 0) %>%
  mutate(date = glue::glue("{arrival_date_year}-{arrival_date_month}-{arrival_date_day_of_month}"),
         date = parse_date(date, format = "%Y-%B-%d")) %>%
  mutate(total_stay = stays_in_week_nights + stays_in_weekend_nights,
         index = row_number(),
         departure = date + total_stay) %>%
  arrange(date) %>%
  select(date, total_stay, departure)

# Creation of a vector of date with all the dates of the time series
date_vec <- min(stay_duration$date) + 0:(max(stay_duration$departure)-min(stay_duration$date))
# create a tibble to match with the number of stay per night
filling_rate <- tibble(date = date_vec)

# for each date, check for each booking if the date is between arrival and departure, which return a boolean,
# and sum the result, so for each match, we add 1 
day_filling_rate <- sapply(date_vec, function(x){sum(x >= stay_duration$date & x < stay_duration$departure)})

# bind the result with the df
filling_rate <- filling_rate %>% bind_cols(nb_stay = day_filling_rate)

# cancelation rate and 20 days moving average
cancel_rate <- hotels %>% 
  filter(hotel == "Resort Hotel", is_canceled == 1) %>%
  mutate(date = glue::glue("{arrival_date_year}-{arrival_date_month}-{arrival_date_day_of_month}"),
         date = parse_date(date, format = "%Y-%B-%d")) %>%
  group_by(date) %>%
  summarise(nb_cancel = sum(is_canceled)) %>%
  ungroup() %>%
  mutate(maca = rollmean(nb_cancel, 20, fill = NA))

# df of labels for textbox
df_labels <- data.frame(
  label = c(
    "<span style='color:blue'>Number of stay per nights</span><br><span style='color:red'>20-days moving average</span>",
    "<span style='color:orange'>Number of cancellation per day</span><br><span style='color:red'>20-days moving average</span>"
  ),
  x = c(ymd("2017-06-01"), ymd("2016-07-01")),
  y = c(125, 60),
  hjust = c(0.5, 0.5),
  vjust = c(0.5, 0.5)
)

# Time series of the total occupancy per day with the number
# of cancellation per day
p3 <- filling_rate %>%
  filter(date < ymd("2017-09-01"), date > ymd("2015-09-01")) %>%
  mutate(ma = rollmean(nb_stay, 20, fill = NA)) %>%
  ggplot(aes(date, nb_stay)) +
  geom_line(aes(group = 1), color = "blue", alpha = 0.4) +
  geom_line(aes(date, ma), color = "red", size = 1) +
  geom_line(data = cancel_rate, aes(date, nb_cancel), color = "orange", alpha = 0.5)+
  geom_line(data = cancel_rate, aes(date, maca), color = "red", alpha = 0.5)+
  geom_textbox(data = df_labels, aes(x, y, label = label), size = 3, color = NA, fill = "white") +
  labs(title = "Number of room occupied per day") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_blank())

# Link between number of room occupied and number of cancellation
p4 <-filling_rate %>%
  filter(date < ymd("2017-09-01"), date > ymd("2015-09-01")) %>%
  mutate(ma = rollmean(nb_stay, 20, fill = NA)) %>%
  left_join(cancel_rate) %>%
  filter(!is.na(ma)) %>%
  ggplot(aes(nb_stay, nb_cancel)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  labs(x = "Number of room occupied",
       y = "Number of cancellation")


text_intro <- ggplot(data.frame(x = 1:2, y = 1:2)) +
  labs(x = NULL, y = NULL,
       subtitle = "The cancellation rate is high, and Portugal, the country where the hotel is located, has the highest cancellation rate,<br>
       with <span style='color:#00d400'><b>42.9% of cancellation in Resort Hotel</b></span>,<span style='color:darkorange'><b>57.3% of cancellation in both hotels</b></span> (left).<br>
       Many booking and cancellation seem to be made in batch, with the same booking date and all cancelled the same day. <br>
       The graph shows the delay between booking and cancellation.") +
  theme(panel.background = element_rect(fill = "#ffffff"),
        plot.background = element_rect(fill = "#ffffff", color = NA),
        axis.text = element_blank(),
        axis.line.x = element_blank(),
        plot.title = element_text(color = "black", hjust = 0),
        plot.subtitle = element_textbox(color = "black", hjust = 0, size = 12, lineheight = 1, family = "Lato"),
        plot.margin = margin(30, 100, 0, 100),
        strip.background = element_rect(fill = NA, color = NA)
  )

text_middle <- ggplot(data.frame(x = 1:2, y = 1:2)) +
  geom_textbox(aes(x =1, y = 0.5,label = "<span style='color:black'>Does the high cancellation rate has an impact on the filling rate ?<br>
                   Apparently not, looking at the number of room occupied each night, and the number of cancellations for the same day (bottom)<br>
                   it seems that there are more cancellation on the period with the most activity, which is also shown in the graph on the right.</span>"),
               width = unit(1, "npc"),  height =  unit(1, "npc"), color = "white", family = "Lato")+
  theme_void()+
  xlim(0, 2) + ylim(0, 2)

patchwork <- text_intro / (p1 + p2 ) / (text_middle + p4) / p3 +
  plot_layout(nrow = 4, heights = c(0.05, 10,7, 10))+
  plot_annotation(title = "Occupancy & cancellation in Resort Hotel",
                  caption = "Visualisation: Christophe Nicault | Data: Antonio, Almeida, and Nunes, 2019",
                  theme = theme(plot.title = element_text(hjust = 0.5)))
patchwork

ggsave("hotel_booking.png", width = 1200, height = 1600, units = "px", scale = 5)
