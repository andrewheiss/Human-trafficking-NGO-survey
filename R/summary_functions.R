library(dplyr)
library(ggplot2)
library(scales)
# library(extrafont)
library(stringr)
library(pander)
library(tidyr)

bar.color.single <- "#243259"

num.country.responses <- length(na.omit(responses.countries$work.country))
num.responses <- length(na.omit(responses.org.foreign$home.country))

theme_bar <- theme_bw(9) + 
  theme(panel.grid.major.x=element_blank())#, 
#         text=element_text(family="Clear Sans"))

clean.text <- function(x) {
  str_trim(levels(factor(na.omit(x))))
}

numeric.summary <- function(x) {
  value <- c(min(x, na.rm=TRUE), max(x, na.rm=TRUE), mean(x, na.rm=TRUE),
             var(x, na.rm=TRUE), sd(x, na.rm=TRUE), length(x))
  label <- c("Minimum", "Maximum", "Mean", "Variance", 
             "Standard deviation", "Total responses")
  output <- cbind(label, round(value, 2))
  colnames(output) <- c("Statistic", "Value")
  output
}

factor.summary <- function(x, sort_me=FALSE, total=TRUE) {
  df <- data.frame(table(x)) %>%
    mutate(perc = round(Freq/sum(Freq) * 100, 2))
  if (sort_me == TRUE) {
    df <- df %>% arrange(desc(Freq)) %>%
      filter(Freq > 0)
  }
  colnames(df) <- c("Answer", "Responses", "%")
  if (total) {
    df <- rbind(as.matrix(df), c("Total", sum(df$Responses), "—")) 
  }
  df
}

separate.answers.summary <- function(df, cols, labels, n=num.country.responses, total=FALSE) {
  cols.to.select <- which(colnames(df) %in% cols)
  df <- df %>%
    select(survey.id, cols.to.select) %>%
    gather(question, value, -survey.id) %>%
    mutate(question = factor(question, labels=labels, ordered=TRUE)) %>%
    group_by(question) %>%
    summarize(response = sum(value, na.rm=TRUE), 
              pct = round(response / n(), 2))
  colnames(df) <- c("Answer", "Responses", "%")
  if (total) {
    df <- rbind(as.matrix(df), c("Total responses", n, "—"))
  }
  df
}


plot.single.question <- function(x) {
  plot.data <- data.frame(var.to.plot = x) %>% na.omit()
  
  # TODO: This gives a warning:
  #  In if (class(plot.data$var.to.plot) != "factor") { :
  #    the condition has length > 1 and only the first element will be used
  if (class(plot.data$var.to.plot) != "factor") {
    plot.data$var.to.plot <- factor(plot.data$var.to.plot)
  }
  
  # Do this to pass unquoted variables: 
  # aes <- eval(substitute(aes(x), list(x = substitute(x))))
  ggplot(plot.data, aes(x = var.to.plot)) + 
    geom_bar(aes(y=(..count..)/sum(..count..)), fill=bar.color.single) + 
    labs(x=NULL, y="Proportion") + scale_y_continuous(labels = percent) + 
    theme_bar
}


plot.multiple.answers <- function(df, cols, labels, flipped=FALSE) {
  plot.data <- separate.answers.summary(df, cols, labels, n=num.responses)
  
  option.flip <- NULL
  
  if (flipped) {
    plot.data <- plot.data %>% 
      mutate(Answer = factor(Answer, levels=rev(labels), ordered=TRUE))
    option.flip <- coord_flip()
  }
  
  p <- ggplot(plot.data, aes(x=Answer, y=Responses)) + 
    geom_bar(aes(y=Responses / num.responses), stat="identity", fill=bar.color.single) + 
    labs(x=NULL, y="Proportion") + scale_y_continuous(labels = percent) + 
    theme_bar + option.flip
  p
}

