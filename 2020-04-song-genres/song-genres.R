# title: "Tidy Tuesday - 	Song Genres"
# author: "Christophe Nicault"
# date: "23 january 2020"

library(tidyverse)
library(FactoMineR)
library(extrafont)
library(ggtext)

loadfonts(device = "win")

spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')

sps <- spotify_songs %>%
  select(track_popularity, danceability:duration_ms, -mode, -key)

spotify_songs <- spotify_songs %>%
  mutate(pop = case_when(track_popularity >=75 ~ 8,
                         track_popularity <=25 ~ 2,
                         track_popularity < 75 & track_popularity > 25  ~ 5))

pca <- PCA(sps)

variables<-data.frame(var = rownames(pca$var$coord),x = pca$var$coord[,1],y = pca$var$coord[,2], z = pca$var$coord[,3])
variables$x <- variables$x * 200
variables$y <- variables$y * 200
variables$z <- variables$z * 200

individu<-data.frame(x=scale(pca$ind$coord[,1]), y=scale(pca$ind$coord[,2]), z=scale(pca$ind$coord[,3]),color=sps$track_popularity)
individu$pop <- spotify_songs$pop
individu$x <- individu$x * 80
individu$y <- individu$y * 80
individu$z <- individu$z * 80
individu2 <- individu %>% filter(pop == 2, y != min(y))
individu5 <- individu %>% filter(pop == 5)
individu8 <- individu %>% filter(pop == 8)

pca_dim <- as_tibble(round(pca$var$coord,3))
pca_dim$dim_names <- attr(pca$var$coord, "dimnames")[[1]]

col_names <- pca_dim %>%
  mutate(dimname1 = paste(dim_names, Dim.1, sep = " : "),
         dimname2 = paste(dim_names, Dim.2, sep = " : "),
         dimname3 = paste(dim_names, Dim.3, sep = " : ")) %>%
  select(dimname1:dimname3)

text_dim <- as_tibble(map_chr(col_names, function(x){reduce(x, paste, sep = " \n ")}))

ggplot(variables, aes(x=0,y=0))+
  geom_point(data=individu2, aes(x=y, y=z), size = 2, color = "#feb24c", alpha = 0.5)+
  geom_point(data=individu8, aes(x=y, y=z), size = 2, color = "#f03b20", alpha = 0.5)+
  geom_segment(aes(xend = y, yend = z), color = "black",
               arrow=arrow(length = unit(0.3,"cm")), size = 2.5)+
  geom_segment(aes(xend = y, yend = z), color = "darkgrey",
               arrow=arrow(length = unit(0.3,"cm")), size = 2)+
  geom_text(aes(x = 1.2*y, y = 1.2*z, label = var), color = "white", size = 5)+
  geom_text(aes(x = 160, y = -85, label = "PC 2"), size = 5, color = "steelblue3") +
  geom_text(data = text_dim, aes(x = 150, y = -200, label = pull(text_dim[2,1])), size = 3.5, hjust = 0, color = "steelblue3") +
  geom_text(aes(x = -290, y = -85, label = "PC 3"), size = 5, color = "steelblue3") +
  geom_text(data = text_dim, aes(x = -300, y = -200, label = pull(text_dim[3,1])), size = 3.5, hjust = 0, color = "steelblue3") +
  geom_segment(aes(x = -300, y = -280, xend = -310, yend = -100), color = "steelblue3",
               arrow=arrow(length = unit(0.3,"cm")), size = 2)+
  scale_x_continuous(limits = c(-300, 200))+
  theme(panel.background = element_rect(fill = "#222222"),
        plot.background = element_rect(fill = "#222222"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_text(color = "steelblue2", size = 12),
        plot.title = element_text(color = "lightgrey", hjust = 0.5, family = "Cambria", size = 20),
        plot.subtitle = element_textbox(color = "lightgrey", family = "Tahoma", hjust = 0.5, size = 16, lineheight = 1),
        plot.caption = element_text(color = "white", hjust = 1, family = "Tahoma")) +
  labs(title = "Influence of audio features on song popularity (PCA results)",
       subtitle = "<span style='color:#feb24c'>popularity <= 25</span> / <span style='color:#f03b20'>popularity >= 75</span>",
       x = "Principal component 2",
       y = "Principal component 3",
       caption = "Visualisation: Christophe Nicault | Data: spotifyr")+
  guides(color = FALSE)



