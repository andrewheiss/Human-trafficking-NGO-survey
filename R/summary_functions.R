#-----------------
# Load libraries
#-----------------
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)
library(grid)
# library(extrafont)
library(pander)


#----------------
# Set variables
#----------------
# Denominators
num.country.responses <- length(na.omit(responses.countries$work.country))
num.responses <- length(na.omit(responses.org.foreign$home.country))

bar.color.single <- "#243259"


#-------------------
# Output functions
#-------------------
# Return cleaned up factor levels
clean.text <- function(x) {
  str_trim(levels(factor(na.omit(x))))
}

# Return a data frame of summary statistics
numeric.summary <- function(x) {
  value <- c(min(x, na.rm=TRUE), max(x, na.rm=TRUE), mean(x, na.rm=TRUE),
             var(x, na.rm=TRUE), sd(x, na.rm=TRUE), length(x))
  label <- c("Minimum", "Maximum", "Mean", "Variance", 
             "Standard deviation", "Total responses")
  output <- cbind(label, round(value, 2))
  colnames(output) <- c("Statistic", "Value")
  output
}

# Return a data frame of counts and proportions for factor levels
factor.summary <- function(x, sort.me=FALSE, total=TRUE) {
  df <- data.frame(table(x)) %>%
    mutate(perc = round(Freq/sum(Freq) * 100, 2))
  
  if (sort.me == TRUE) {
    df <- df %>% arrange(desc(Freq)) %>%
      filter(Freq > 0)
  }
  
  colnames(df) <- c("Answer", "Responses", "%")
  
  if (total) {
    df <- rbind(as.matrix(df), c("Total", sum(df$Responses), "—")) 
  }
  
  df
}

# Return a data frame of counts and proportions for multiple responses
separate.answers.summary <- function(df, cols, labels, total=FALSE) {
  cols.to.select <- which(colnames(df) %in% cols)
  
  denominator <- df %>%
    select(cols.to.select) %>%
    mutate(num.answered = rowSums(., na.rm=TRUE)) %>%
    filter(num.answered > 0) %>%
    nrow()
  
  df <- df %>%
    select(survey.id, cols.to.select) %>%
    gather(question, value, -survey.id) %>%
    mutate(question = factor(question, labels=labels, ordered=TRUE)) %>%
    group_by(question) %>%
    summarize(response = sum(value, na.rm=TRUE), 
              pct = round(response / denominator * 100, 2),
              plot.pct = response / denominator)
  
  colnames(df) <- c("Answer", "Responses", "%", "plot.pct")
  
  if (total) {
    df <- df %>% select(1:3)
    df <- rbind(as.matrix(df), c("Total responses", denominator, "—"))
  }
  
  df
}


#-----------------
# Plot functions
#-----------------
# Custom themes
theme_bar <- theme_bw(9) + 
  theme(panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank())

theme_bar_flipped <- theme_bw(9) + 
  theme(panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank())


#text=element_text(family="Clear Sans"))

theme_line <- theme_bw(9) + 
  theme(panel.grid.minor.x = element_blank(),
        axis.ticks = element_blank())

theme_blank_map <- theme(panel.background = element_rect(fill="white"),
                         panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         axis.line=element_blank(),
                         axis.text.x=element_blank(),
                         axis.text.y=element_blank(),
                         axis.ticks=element_blank(),
                         axis.title.x=element_blank(),
                         axis.title.y=element_blank())#,
                         #text=element_text(size=9, family="Clear Sans"))

# Plot the summary of a factor
plot.single.question <- function(x, flipped=TRUE) {
  plot.data <- data.frame(var.to.plot = x) %>% na.omit() %>%
    group_by(var.to.plot) %>%
    summarize(n = n()) %>%
    mutate(y = n / sum(n))
  
  option.flip <- NULL
  option.theme <- theme_bar
  
  if (flipped) {
    plot.data <- plot.data %>% 
      mutate(var.to.plot = factor(var.to.plot, 
                                  levels=rev(levels(plot.data$var.to.plot)), 
                                  ordered=TRUE))
    option.flip <- coord_flip()
    option.theme <- theme_bar_flipped
  }
  
  # Do this to pass unquoted variables: 
  # aes <- eval(substitute(aes(x), list(x = substitute(x))))
  ggplot(plot.data, aes(x = var.to.plot, y = y)) + 
    geom_bar(stat="identity", fill=bar.color.single) +
    labs(x=NULL, y=NULL) + 
    scale_y_continuous(labels = percent, 
                       breaks = seq(0, max(round(plot.data$y, 1)), by=0.1)) + 
    option.flip + option.theme
}

# Plot the summary of a multiple-response question
plot.multiple.answers <- function(df, cols, labels, flipped=TRUE) {
  plot.data <- separate.answers.summary(df, cols, labels)
  
  option.flip <- NULL
  option.theme <- theme_bar
  
  if (flipped) {
    plot.data <- plot.data %>% 
      mutate(Answer = factor(Answer, levels=rev(labels), ordered=TRUE))
    option.flip <- coord_flip()
    option.theme <- theme_bar_flipped
  }
  
  p <- ggplot(plot.data, aes(x=Answer, y=Responses)) + 
    geom_bar(aes(y=plot.pct), stat="identity", fill=bar.color.single) + 
    labs(x=NULL, y=NULL) + 
    scale_y_continuous(labels = percent, 
                       breaks = seq(0, max(round(plot.data$plot.pct, 1)), by=0.1)) + 
    option.theme + option.flip
  p
}
