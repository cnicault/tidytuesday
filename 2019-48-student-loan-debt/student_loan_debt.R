# title: "Tidy Tuesday - Student Loan Debt"
# author: "Christophe Nicault"
# date: "27 novembre 2019"

library(tidyverse)
library(scales)
library(ggthemr)
library(patchwork)

loans <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-26/loans.csv")

ggthemr("flat")

theme_update(plot.title = element_text(hjust = 0.5),
             plot.subtitle = element_text(hjust = 0.5),
             plot.caption = element_text(size = 8))

loans <- map(loans, function(x){ifelse(is.na(x), 0, x)}) %>% as_tibble()


loans_prepared <- loans %>%
  group_by(year, quarter) %>%
  summarise(start = sum(starting, na.rm = TRUE),
            add = sum(added, na.rm = TRUE),
            total = start + add,
            repaid = sum(consolidation, rehabilitation, voluntary_payments, wage_garnishments)) %>%
  ungroup() %>%
  arrange(year, quarter) %>%
  mutate(quat = paste(paste0("20",year), paste0("Q",quarter), sep = "-"),
         repaid = repaid / total,
         lag1 = lag(total),
         loan = (total / lag1) - 1) %>%
  select(quat, repaid, loan) %>%
  gather(type, value, -quat)


p1 <- loans_prepared %>%
  mutate(type = str_to_title(type)) %>%
  ggplot(aes(quat, value, color = type, fill = type)) +
  geom_line(aes(group = type), size = 1.5)+
  geom_point(size = 3, shape = 21, color = "#f1f1f1", stroke = 1.5)+
  scale_y_continuous(label = percent_format()) +
  scale_color_manual(values = c("#d35400", "#1abc9c"))+
  scale_fill_manual(values = c("#d35400", "#1abc9c"))+
  labs(title = "Comparison of % of change in debt and % of repaid",
       subtitle = "Top graph % of change in loan from previous quarter\n(repaid displayed for comparison)\nBottom graph : zoom in the % of loan being repaid",
       y = "% of change in loan\nfrom previous quarter",
       color = 'Loan / Repaid')+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        axis.title.y = element_text(size = 8))+
  guides(fill = FALSE)


p2 <- loans_prepared %>%
  filter(type == "repaid") %>%
  ggplot(aes(quat, value),color = "#1abc9c") +
  geom_line(aes(group = type), size = 1.5, color = "#1abc9c")+
  geom_point(size = 2, shape = 21, color = "#f1f1f1", stroke = 1.5)+
  scale_y_continuous(label = percent_format(), limits = c(0.01,0.035)) +
  labs(y = "% of loan repaid")+
  theme(axis.text.x = element_text(angle = 80, vjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 8),
        panel.background = element_blank())


p3 <- loans %>%
  gather(type, amount, consolidation, rehabilitation, voluntary_payments, wage_garnishments) %>%
  group_by(year, quarter, type) %>%
  summarise(total = sum(amount)) %>%
  ungroup() %>%
  mutate(quat = paste(paste0("20",year), paste0("Q",quarter), sep = "-"),
         type = str_to_title(str_replace(type, "_", " "))) %>%
  ggplot(aes(quat, total, fill = type)) +
  geom_area(aes(group = type),position = "stack", alpha = 0.6)+
  scale_y_continuous(label = dollar_format()) +
  labs(title = "Amount of loan repaid each quarter",
       y = "Amount repaid",
       fill = "Type of payment")+
  theme(axis.text.x = element_text(angle = 80, vjust = 0.5),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10))

p4 <- loans %>%
  group_by(year, quarter) %>%
  summarise(start = sum(starting, na.rm = TRUE),
            add = sum(added, na.rm = TRUE),
            loan = start + add,
            repaid = sum(consolidation, rehabilitation, voluntary_payments, wage_garnishments)) %>%
  ungroup() %>%
  filter(quarter == 4) %>%
  select(-start, -add, -quarter) %>%
  gather(type, value, -year) %>%
  mutate(year = paste0("20", year),
         type = str_to_title(type),
         type = fct_reorder(type, value)) %>%
  ggplot(aes(year, value, fill = type)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = dollar_format()) +
  scale_fill_manual(values = c("#1abc9c", "#d35400"))+  
  coord_flip() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 12))+
  labs(title = "Amount of loan & repaid at the end of each year (Q4)",
       fill = "Loan / Repaid")


patchwork <- ((p1 / p2) | p3) / p4

patchwork + plot_annotation(title = "Evolution of students' capacity to repay loan",
                            subtitle = "The amount of loans increased by 40% in 2018, where the amount of payment is stable around $ 2 billions\n Therefore, the percentage of loan being repaid decreases and drop from 3.1% to 1.6%",
                            caption = "Visualisation: Christophe Nicault | Data: Department of Education / Dignity and Debt",
                            theme = theme(plot.background = element_rect(fill = "#f1f1f1")))+
                            plot_layout(nrow = 2, heights = c(3, 1))

