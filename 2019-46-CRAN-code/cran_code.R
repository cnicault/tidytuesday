# title: "Tidy Tuesday - Cran Code"
# author: "Christophe Nicault"
# date: "12 novembre 2019"

library(tidyverse)
library(ggthemr)
library(scales)
library(patchwork)

cran_code <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-12/loc_cran_packages.csv")


# Interactive circles languages of ggplot package

tidyverse <- str_replace(tidyverse_packages(), "\\\n\\(>=", "")

tidyverse_pkg <- cran_code %>%
  filter(pkg_name %in% tidyverse) %>%
  group_by(language) %>%
  arrange(desc(code)) %>%
  select(language, pkg_name, code, comment, blank) %>%
  gather(type_line, total, c(code, comment, blank)) %>%
  arrange(pkg_name, language) %>%
  mutate(label = paste0(type_line, " : ", total))

library(data.tree)
tidyverse_pkg$pathString <- paste("Languages", tidyverse_pkg$pkg_name, tidyverse_pkg$language, tidyverse_pkg$label, sep = "/")
tidy_levels <- as.Node(tidyverse_pkg)

tidy_circle <- circlepackeR(tidy_levels, size = "total", color_min = "#06313E", color_max = "#BBCED3")
tidy_circle



# Interactive treemap languages of ggplot package

library(d3treeR)
library(treemap)

tidyverse_tree <- treemap(tidyverse_pkg,
             index=c("pkg_name","language", "label"),
             vSize="total",
             type="index",
             palette = "Set2",
             bg.labels=c("white"),
             align.labels=list(
               c("center", "center"), 
               c("right", "bottom")
             )
            )            

interactif <- d3tree2(tidyverse_tree, rootname = "Tidyverse Packages")
interactif



ggthemr("dust")

theme_update(plot.title = element_text(hjust = 0.5),
             plot.subtitle = element_text(hjust = 0.5),
             plot.caption = element_text(size = 8))

# # of packages per # of languages used

cran_code %>%
  count(pkg_name, name = "total_languages") %>%
  count(total_languages, name = "total_packages") %>%
  ggplot(aes(as.factor(total_languages), (total_packages+1), label = total_packages)) +
  geom_col() +
  geom_text(color = "black", position = position_stack(vjust = 0.5)) +
  scale_y_log10() +
  labs(title = "# of packages per # of languages used",
       subtitle = "Some packages use up to 15 different languages",
       x = "# of languages used",
       y = "# of packages (log scale)",
       caption = "Visualisation: Christophe Nicault | Data: CRAN / Phillip Massicotte")


# Number of package per language combination

language_mix <- cran_code %>%
  group_by(pkg_name) %>%
  mutate(total_languages = n()) %>%
  filter(total_languages <= 3) %>%
  arrange(pkg_name, language) %>%
  group_by(pkg_name, total_languages) %>%
  summarise(combination = reduce(unique(language), paste, sep = " - ")) %>%
  ungroup()

p2 <- language_mix %>%
  filter(total_languages == 2) %>%
  count(combination, name = "total") %>% 
  arrange(desc(total)) %>%
  head(20) %>%
  mutate(combination = fct_reorder(combination, total)) %>%
  ggplot(aes(combination, total, label = total)) +
  geom_col()+
  geom_text(color = "black", position = position_stack(vjust = 0.5), size = 3) +
  scale_y_log10() +
  coord_flip() +
  labs(title = "Packages using 2 languages",
       x = "Language combination",
       y = "Number of package (log scale)")+
    theme(axis.title = element_text(size = 10))

p3 <- language_mix %>%
  filter(total_languages == 3) %>%
  count(combination, name = "total") %>%
  arrange(desc(total)) %>%
  head(20) %>%
  mutate(combination = fct_reorder(combination, total)) %>%
  ggplot(aes(combination, total, label = total)) +
  geom_col()+
  geom_text(color = "black", position = position_stack(vjust = 0.5), size = 3) +
  scale_y_log10() +
  coord_flip() +
  labs(title = "Packages using 3 languages",
       x = "Language combination",
       y = "Number of package (log scale)")+
  theme(axis.title = element_text(size = 10))

p2 + p3 + 
  plot_annotation(title = "Number of package per language combination",
                  subtitle = "First 20 languages combination",
                  caption = "Visualisation: Christophe Nicault | Data: CRAN / Phillip Massicotte")

