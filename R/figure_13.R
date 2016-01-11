library(dplyr)
library(ggplot2)
library(grid)
library(Cairo)

figure_13_data <- read.csv("Data/figure_13.csv", stringsAsFactors=FALSE) %>%
  arrange(Value) %>%
  mutate(Country = factor(Country, levels=Country, ordered=TRUE))

p <- ggplot(figure_13_data, aes(x=Country, y=Value)) + 
  labs(x=NULL, y="Number of times country was mentioned as a partner in anti-TIP work") + 
  geom_bar(stat="identity", fill=bar.color.single) + 
  scale_y_continuous(breaks=seq(0, max(figure_13_data$Value), by=25)) + 
  coord_flip() + theme_clean()

ggsave(p, filename="figures/fig13.pdf", 
       width=5.5, height=4, units="in", device=cairo_pdf)
ggsave(p, filename="figures/fig13.png", 
       width=5.5, height=4, units="in")
p
