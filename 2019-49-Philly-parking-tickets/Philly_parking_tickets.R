# title: "Tidy Tuesday - Philly Parking Tickets"
# author: "Christophe Nicault"
# date: "04 december 2019"


library(tidyverse)
library(scales)
library(lubridate)
library(viridis)
library(patchwork)

theme_update(plot.title = element_text(hjust = 0.5),
             plot.subtitle = element_text(hjust = 0.5),
             plot.caption = element_text(size = 8))

tickets <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-03/tickets.csv")


p1 <- tickets %>%
  mutate(violation_desc = fct_lump(violation_desc, n = 30)) %>%
  group_by(violation_desc) %>%
  summarise(total = sum(fine)) %>%
  ungroup() %>%
  mutate(violation_desc = str_to_title(violation_desc),
         violation_desc = fct_reorder(violation_desc, total)) %>%
  ggplot(aes(violation_desc, total, fill = violation_desc)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(label = dollar_format()) +
  scale_fill_viridis_d(option = "plasma") +
  labs(title = "Total amount of fine for the first 30 violations type",
       x = "Violation",
       y = "Total amount",
       caption = "Visualisation: Christophe Nicault | Data: Philly Parking Tickets / Open Data Philly") +
  theme(panel.background = element_blank(),
        plot.background = element_rect(fill = "lightyellow"),
        legend.background = element_rect(fill = "khaki2"),
        panel.grid.major.y  = element_line(color = "lightgrey", linetype = "dotted"),
        panel.grid.major.x  = element_line(color = "lightgrey", linetype = "dotted"),
        legend.position  = "none")




p2 <- tickets %>%
  filter(!is.na(issuing_agency)) %>%
  mutate(zip_code = fct_explicit_na(as.factor(zip_code))) %>%
  group_by(issuing_agency, zip_code) %>%
  summarise(amount = sum(fine)) %>%
  ungroup() %>%
  mutate(issuing_agency = str_to_title(issuing_agency)) %>%
  ggplot(aes(zip_code, issuing_agency, fill = amount)) +
  geom_tile() +
  scale_fill_viridis(option = "plasma", label = dollar_format()) +
  coord_equal()+
  labs(title = "Agency presence by zip code",
       y = "Zip Code",
       x = "Agency",
       fill = "Amount",
       caption = "Visualisation: Christophe Nicault | Data: Philly Parking Tickets / Open Data Philly") +
  theme(axis.text.x = element_text(angle = 80, vjust = 0.5),
      panel.background = element_blank(),
      plot.background = element_rect(fill = "lightyellow"),
      legend.background = element_rect(fill = "khaki2"),
      panel.grid.major.y  = element_line(color = "lightgrey", linetype = "dotted"),
      panel.grid.major.x  = element_line(color = "lightgrey", linetype = "dotted"))


Sys.setlocale("LC_TIME", "C")

tickets_date <- tickets %>%
  mutate(lday = as.factor(wday(issue_datetime, label = TRUE, abbr = FALSE)),
         lmonth = month(issue_datetime, label = TRUE, abbr = FALSE),
         lhour = as.factor(hour(issue_datetime)),
         ldate = date(issue_datetime))


p3 <- tickets_date %>%
  group_by(lhour, lday) %>%
  summarise(total = sum(fine)) %>%
  ungroup() %>%
  ggplot(aes(lhour, lday, fill = total)) +
  geom_tile() +
  #scale_fill_gradient(low = "white", high = "red", trans = "log10", label = dollar_format())
  scale_fill_viridis(option = "plasma", label = dollar_format()) +
  coord_equal() +
  labs(title = "Value of fines by day and time",
       x = "Hour",
       y = "Day",
       fill = "Amount",
       caption = "Visualisation: Christophe Nicault | Data: Philly Parking Tickets / Open Data Philly") +
  theme(panel.background = element_blank(),
        plot.background = element_rect(fill = "lightyellow"),
        legend.background = element_rect(fill = "khaki2"),
        panel.grid.major.y  = element_line(color = "lightgrey", linetype = "dotted"),
        panel.grid.major.x  = element_line(color = "lightgrey", linetype = "dotted"),
        legend.key  = element_rect(color = "khaki2"))



abs_comma <- function (x, ...) {
  format(abs(x), ..., big.mark = ",", scientific = FALSE, trim = TRUE)
}

abs_dollar <- function (x, ...) {
  dollar(abs(x), big.mark = ",", prefix = "$", suffix = "", trim = TRUE)
}


order <- tickets %>%
  mutate(violation_desc = fct_lump(violation_desc, n = 15)) %>%
  filter(issuing_agency == "PPA") %>%
  group_by(violation_desc) %>%
  summarise(total = sum(fine)) %>%
  arrange(desc(total))  %>%
  select(violation_desc, order = total)
  

p4 <- tickets %>%
  mutate(violation_desc = fct_lump(violation_desc, n = 15)) %>%
  group_by(issuing_agency, violation_desc) %>%
  summarise(total = n(),
            amount = sum(fine)) %>%
  ungroup() %>%
  left_join(order) %>%
  arrange(desc(order)) %>%
  mutate(violation_desc = str_to_title(violation_desc),
         violation_desc = fct_reorder(violation_desc, order),
         total = ifelse(issuing_agency == "PPA", total, -total),
         amount = ifelse(issuing_agency == "PPA", amount, -amount)) %>% 
  ggplot(aes(violation_desc, amount, fill = issuing_agency)) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis_d(option = "plasma")+
  scale_y_continuous(labels = abs_dollar) +
  annotate(geom = "text", label ="Most of the fines are issued by the PPA,\n then the Police.\n All other agencies are inconsequencial\n compared to the Police and PPA", x = 5, y = 5500000) +
  labs(title = "Comparing PPA to all other agencies by amount and type of fine",
       subtitle = "PPA on the right side, all other agencies on the left side. 15 first violations.",
       x = "Violation",
       y = "Amount",
       fill = "Issuing Agency",
       caption = "Visualisation: Christophe Nicault | Data: Philly Parking Tickets / Open Data Philly") +
  theme(panel.background = element_blank(),
        plot.background = element_rect(fill = "lightyellow"),
        legend.background = element_rect(fill = "khaki2"),
        panel.grid.major.y  = element_line(color = "lightgrey", linetype = "dotted"),
        panel.grid.major.x  = element_line(color = "lightgrey", linetype = "dotted"),
        legend.key  = element_rect(color = "khaki2"))

patchwork <- (p2 / p3 / p4) | p1 

patchwork + plot_annotation(theme = theme(plot.background = element_rect(fill = "lightyellow")))+
plot_layout(nrow = 1, heights = c(1, 2))


