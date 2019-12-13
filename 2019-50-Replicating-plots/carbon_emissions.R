# title: "Tidy Tuesday - Replicating plot / impact of co2 emissions"
# author: "Christophe Nicault"
# date: "13 december 2019"

library(tidyverse)
library(scales)
library(ggforce)
library(patchwork)
library(dslabs)

data(historic_co2)
data(temp_carbon)

theme_set(theme_bw())
theme_update(panel.border = element_blank(),
      plot.background = element_rect(fill = "#edfbff"),
      legend.background = element_rect(fill = "#edfbff"),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(size = 8, hjust = 1),
      panel.grid.major = element_line(linetype = "dashed"),
      panel.grid.minor = element_line(linetype = "dotted"))

era_comma <- function (x, ...) {
  if(mean(x, na.rm = TRUE) < 0){
      format(x, ..., big.mark = ",", scientific = FALSE, trim = TRUE)
  }else{
    format(x, ..., big.mark = "", scientific = FALSE, trim = TRUE)
  }
}

historic_co2 %>%
  mutate(source = if_else(source == "Ice Cores", "Indirect", "Direct")) %>%
  ggplot(aes(year, co2), color = source) +
  geom_vline(xintercept = 1760, color = "gray") +
  geom_line(aes(color = source), size = 1) +
  scale_x_continuous(labels = era_comma) +
  scale_y_continuous(limits = c(150,450)) +
  facet_zoom(xlim = c(1800, 2014), ylim = c(150, 450), horizontal = FALSE, zoom.size = 1, show.area = TRUE) +
  scale_color_manual(values = c("Indirect" = "#5fbcd3", "Direct" = "#ff0000") )+
  labs(title = "CO2 concentration over time",
       subtitle = "Zoom on modern era",
       color = "Measurement",
       x = "Year",
       y = "CO2 in ppm by volume",
       caption = "Visualisation: Christophe Nicault | Data: dslabs - Data Science Labs")


temp_carbon %>%
  filter(year >= 1880) %>%
  gather(type, value, -year, -carbon_emissions) %>%
  mutate(type = str_to_title(str_replace(type, "_", " "))) %>%
  ggplot(aes(year, value, color = type)) +
  geom_line()+
  geom_smooth(se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")+
  geom_text(aes(min(year), 0, label = "Reference - mean of the 20th", hjust = 0, vjust = -1), size = 3, color = "black")+
  scale_color_manual(values = c("Land Anomaly" = "#5fd38d", "Ocean Anomaly" = "#5fbcd3", "Temp Anomaly" = "#ff0000") )+
  labs(title = "Average temperature anomalies from 1880 to 2014",
       y = "Temperature anomalies (°C)",
       x= "Year",
       color = "Anomaly",
       caption = "Visualisation: Christophe Nicault | Data: dslabs - Data Science Labs")


cor_co2 <- historic_co2 %>%
  spread(source, co2) %>%
  mutate(co2 = if_else(is.na(`Ice Cores`), `Mauna Loa`, `Ice Cores`),
         co2 = if_else(is.na(`Mauna Loa`), `Ice Cores`, `Mauna Loa`),
         co2 = if_else(!is.na(`Mauna Loa`) & !is.na(`Ice Cores`), (`Mauna Loa` + `Ice Cores`) /2 ,co2 )
         ) %>%
  left_join(temp_carbon, by = c("year")) %>%
  filter(year >= 1880, year <= 2014)

model <- lm(co2 ~ carbon_emissions, cor_co2)             

theme_set(theme_light())
theme_update(plot.title = element_text(hjust = 0.5, size = 12),
             plot.subtitle = element_text(hjust = 0.5),
             plot.caption = element_text(size = 8),
             panel.grid.major = element_line(linetype = "dashed"),
             panel.grid.minor = element_line(linetype = "dotted"))

p1 <- cor_co2 %>%
  ggplot(aes(carbon_emissions,co2)) +
  geom_line(size = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(limits = c(275,400), breaks = seq(250,400,by = 25))+
  annotate("text", hjust = 0, vjust = 0.5, size = 3.5, label = paste("Correlation :", round(cor(cor_co2$co2, cor_co2$carbon_emissions),2)), x = 3500, y = 310) +
  annotate("text", hjust = 0, vjust = 0.5, size = 3.5, label = paste("Slope :", format(model$coef[2], scientific = TRUE)), x = 3500, y = 305) +
  annotate("text", hjust = 0, vjust = 1, size = 3.5, 
           label = paste("(each increase of 1000 millions\n of metric tons of carbon emission\n increase the carbon concentration\n by ",round(model$coef[2]*1000, 2), "ppm per volume )"),
           x = 3500, y = 300) +
  annotate("rect", xmin = 3450, xmax = 9050, ymin = 275, ymax = 315, fill = "blue", alpha = 0.2) +
  labs(title = "Relation between carbon emissions\n and co2 concentration",
       x = "Carbon emissions (millions of metric tons)",
       y = "CO2 concentration\n(ppm per volume)")


p2 <- cor_co2 %>%
  ggplot(aes(year, carbon_emissions, size = co2, color = co2)) +
  geom_point(alpha = 0.6) +
  scale_size_continuous(limits=c(275, 400), breaks=seq(300,375, by=25), range = c(1,12))+
  scale_color_gradient(low="blue", high="red", limits=c(275, 400), breaks=seq(300,375, by=25))+
  labs(title = "Evolution of carbon emissions\n and its impact on co2 concentration",
       x = "Year",
       y = "Carbon emissions\n(millions of metric tons)",
       color = "CO2 concentration\n(ppm per volume)",
       size = "CO2 concentration\n(ppm per volume")+
  guides(size = FALSE)

patchwork <- p1 | p2
patchwork + plot_annotation(title = "Impact of carbon emissions on co2 concentration",
                           caption = "Visualisation: Christophe Nicault | Data: dslabs - Data Science Labs",
                           theme = theme(plot.background = element_rect(fill = "#edfbff")))
 
