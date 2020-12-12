library(png)
library(grid)
library(cowplot)
library(tidyverse)
library(stopwords)
library(ggimage)
library(tidytuesdayR)
library(wordcloud2)

set.seed(42)

tt_data <- tt_load(2020, "50")
df <- tt_data$women
stop_words <- stopwords("en")


remove <- c("@\\w+", "[[:punct:]]", "(",")", "[[:digit:]]+")
words <- df %>%
  select(description) %>%
  mutate(description = str_to_lower(description)) %>%
  mutate(description = str_remove_all(description, paste(remove, collapse = "|"))) %>%
  mutate(description = str_split(description, " ")) %>%
  unnest(description) %>%
  count(description) %>%
  arrange(-n) %>%
  filter(!description %in% stop_words)  %>%
  filter(!description %in% c("–","")) 
  

dat <- words %>%
  filter(!description %in% c( "which")) %>%
  mutate(description = str_to_title(description))

wordFreq <- dat %>%
  filter(n > 1) %>%
  filter(!description %in% c("2019", "90", "20", "22", "10", "2015", "90", "Dies")) %>%
  mutate(description = ifelse(description == 'Womens', "Women", description)) %>%
  rename(word = description, freq = n)

wordcloud2(data = wordFreq, size = 1.1, figPath = "BBC_top_100_women.jpg", backgroundColor = "black", color="random-light")


# Save image from wordcloud
ggplot(data = wordFreq) +
  draw_image("My Top 100.png") +
  labs(title = "BBC Top 100 Women 2020") +
  theme(
  plot.title = element_text(color='white', hjust=0.5, size=24, face = 'bold'),
  plot.background = element_rect(fill = "black"),
  panel.background = element_rect(fill = "black"),
  plot.margin=unit(c(1,1,1,1),"cm"),
  panel.border = element_rect(colour = "black", fill=NA, size=5)
  )
ggsave("Top 100 Women Word Cloud.png", dpi=500)





