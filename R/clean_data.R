#-----------------
# Load libraries
#-----------------
# Hi, Hadley...
library(tidyr)
library(dplyr)
library(lubridate)

# Map stuff
# You must install geos (http://trac.osgeo.org/geos/) and 
# gdal (http://www.gdal.org/) first. 
# Easy to do on OS X: `brew install geos gdal`
# Then install these packages from source
# install.packages(c("rgeos", "rgdal"), type="source")
library(rgeos)
library(maptools)


#-------------------
# Helper functions
#-------------------
# Remove second line from Qualtrics CSV files since it contains 
# the actual questions and we don't need those.
load.qualtrics <- function(filename) {
  lines <- readLines(filename)
  lines <- gsub("\\\\", "-", lines)  # Fix unescaped \s
  
  ret <- read.csv(textConnection(lines[-2]),  # Remove 2nd line
                  header=TRUE, stringsAsFactors=FALSE, na.strings=c(""))
  return(ret)
}

# Running as.character() on a blank string ("") results in a blank string. 
# This function returns an NA instead.
fix.na <- function(x) {
  return(ifelse(x == "", NA, as.character(x)))
}


#------------
# Load data
#------------
# The phone and LinkedIn surveys need an empty Q67 added (the banner image) 
# so that the three can be merged correctly
raw.main <- load.qualtrics("../Data/Human_Trafficking_NGOs_survey.csv")

raw.phone <- load.qualtrics("../Data/Human_Trafficking_NGOs_survey_inline_for_phone.csv") %>%
  mutate(Q67 = NA) 

good.linkedin <- c("R_02iOgW9hAxLGdJr", "R_cVfWt7ooTCsCee9", "R_0oerAAwD33YRIPP", 
                   "R_6sdNfHkg9h8wKMZ", "R_8e8oOEOnHXOzxbv")
raw.linkedin <- load.qualtrics("../Data/Human_Trafficking_NGOs_survey_LinkedIn.csv") %>%
  mutate(Q67 = NA) %>% filter(V1 %in% good.linkedin)


#-------------
# Clean data
#-------------
# Load country list
countries <- read.csv("../Data/countries.csv", header=TRUE)

# These are separate for convenience, with columns for the primary key and
# country name. In theory I could make just one `countries` data frame, but
# there's no way to select which columns get merged in with left_join(), so too
# many columns come in. It's easier to just have two tiny data frames instead.
countries.home <- countries %>%
  mutate(home.country = country, Q1.4 = country_id) %>%
  select(Q1.4, home.country)

countries.work <- countries %>%
  mutate(work.country = country, Q3.2 = country_id) %>%
  select(Q3.2, work.country)


# Combine the three survey data frames, add an indicator for US-only work, and
# return a base data frame for future manipulation
responses <- rbind(rbind(raw.main, raw.phone), raw.linkedin) %>%
  # Calculate the number of countries they responded to. If it's just one, 
  # and it's the US (187), then mark as US-only
  mutate(num.responses = 4 - (is.na(Q3.2.1.) + is.na(Q3.2.2.) + is.na(Q3.2.3.) + is.na(Q3.2.4.))) %>%
  mutate(work.us = ifelse(Q3.2.1. == 187 | Q3.2.2. == 187 | 
                            Q3.2.3. == 187 | Q3.2.4. == 187, 1, 0)) %>%
  mutate(work.only.us = ifelse(num.responses == work.us, TRUE, FALSE),
         work.only.us = ifelse(is.na(work.only.us), FALSE, work.only.us)) %>%
  select(-work.us)


# Create a clean data frame with one row per organization. 
# All looped questions (Q3*) are omitted.
responses.org <- responses %>%
  # Clean up columns
  select(-starts_with("Q3"), -c(V2, V3, V4, V5, V7, Q67, Q1.1, 
                                Q2.0, Q4.6, LocationAccuracy, X)) %>%
  rename(survey.id = V1, ip.address = V6, 
         start.time = V8, end.time = V9, finished = V10,
         Q2.1 = Q2.1_1) %>%
  left_join(countries.home, by="Q1.4") %>%  # Join country names
  
  # Clean variables
  mutate(Q1.3 = factor(Q1.3, labels=c("No", "Yes")),
         Q1.5.factor = factor(Q1.5, labels=c("1", "2", "3", "4", "5+")),
         Q2.5 = factor(Q2.5, labels=c("No", "Yes")),
         Q2.6 = factor(Q2.6, labels=c("No", "Yes")),
         Q4.2 = factor(Q4.2, labels=c("No", "Yes")),
         Q4.3 = factor(Q4.3, labels=c("No", "Yes"))) %>%
  mutate(start.time = ymd_hms(start.time),
         end.time = ymd_hms(end.time))


# Filter organizations that only work in the US
responses.org.foreign <- responses.org %>% filter(work.only.us == FALSE)


# Create a clean data frame with one row per country response, indexed by
# survey ID. This *only* contains Q3* responses from the loop.
responses.countries <- responses %>%
  # Split all Q3* columns into a key and value column
  gather(key, value, starts_with("Q3")) %>%
  
  # Split the key column into two parts: question and loop number
  # The regex will match two formats (Q3.3.1. and Q3.5_1.1./Q3.5_5_TEXT.1.)
  # and split them into (Q3.3, 1) and (Q3.5_1, 1)/(Q3.5_5_TEXT, 1)
  extract(key, c("question", "loop.number"), 
          c("(Q3\\.[^\\.]*)\\.(\\d+)\\.")) %>%
  
  # Make columns for each of the questions
  spread(question, value) %>%
  
  # Get rid of extra columns
  select(survey.id=V1, loop.number, starts_with("Q3"), -Q3.0, -Q3.1) %>%
  
  # Join country names
  mutate(Q3.2 = as.numeric(Q3.2)) %>% filter(Q3.2 != 187) %>%
  left_join(countries.work, by="Q3.2") %>%
  
  # Create factors for scale questions
  mutate(loop.number = factor(loop.number, ordered=TRUE),
         Q3.3 = factor(Q3.3, labels=c("None", "Very little", "Little", 
                                      "Some", "A lot", "Don't know"), ordered=TRUE),
         Q3.4 = factor(Q3.4, labels=c("Never", "Rarely", "Sometimes", "Monthly", 
                                      "Weekly", "Daily", "Don't know"), ordered=TRUE),
         Q3.8 = factor(Q3.8, labels=c("No", "Yes", "Don't know"), ordered=TRUE),
         Q3.19 = factor(Q3.19, labels=c("Most important actor", 
                                        "Somewhat important actor", 
                                        "Not an important actor", "Don't know"), 
                        ordered=TRUE),
         Q3.20 = factor(Q3.20, labels=c("Not hard at all", "Not too hard", 
                                        "Somewhat hard", "Very hard", 
                                        "Extremely hard", "Don't know"), ordered=TRUE),
         Q3.22 = factor(Q3.22, labels=c("Tier 1", "Tier 2", "Watch list", 
                                        "Tier 3", "Don't know"), ordered=TRUE),
         Q3.23 = factor(Q3.23, labels=c("No", "Yes"), ordered=TRUE),
         Q3.25 = factor(Q3.25, labels=c("Negative", "Positive", "Mixed", 
                                        "Don't know"), ordered=TRUE),
         Q3.26 = factor(Q3.26, labels=c("Improved", "Remained constant", 
                                        "Slowed down", "Don't know"), ordered=TRUE),
         Q3.27 = factor(Q3.27, labels=c("No", "Yes", "Don't know"), ordered=TRUE),
         Q3.28 = factor(Q3.28, labels=c("No", "Yes", "Don't know"), ordered=TRUE),
         Q3.29 = factor(Q3.29, labels=c("Not restricted", "Very little restricted", 
                                        "A little restricted", "Somewhat restricted",
                                        "Very restricted", "Don't know"), 
                        ordered=TRUE)) %>%
  
  # Character-only fields
  mutate(Q3.10 = fix.na(Q3.10), Q3.11 = fix.na(Q3.11), Q3.12 = fix.na(Q3.12), 
         Q3.13 = fix.na(Q3.13), Q3.14 = fix.na(Q3.14), Q3.15 = fix.na(Q3.15),
         Q3.16 = fix.na(Q3.16), Q3.17 = fix.na(Q3.17), Q3.24 = fix.na(Q3.24),
         Q3.30 = fix.na(Q3.30), Q3.5_5_TEXT = fix.na(Q3.5_5_TEXT), 
         Q3.18_4_TEXT = fix.na(Q3.18_4_TEXT), Q3.21_4_TEXT = fix.na(Q3.21_4_TEXT)) %>%
  
  # Multiple response questions
  mutate(Q3.5_1 = as.numeric(Q3.5_1), Q3.5_2 = as.numeric(Q3.5_2),
         Q3.5_3 = as.numeric(Q3.5_3), Q3.5_4 = as.numeric(Q3.5_4),
         Q3.5_5 = as.numeric(Q3.5_5), Q3.9_1 = as.numeric(Q3.9_1),
         Q3.9_2 = as.numeric(Q3.9_2), Q3.9_3 = as.numeric(Q3.9_3),
         Q3.9_4 = as.numeric(Q3.9_4), Q3.9_5 = as.numeric(Q3.9_5),
         Q3.9_6 = as.numeric(Q3.9_6), Q3.9_7 = as.numeric(Q3.9_7),
         Q3.9_8 = as.numeric(Q3.9_8), Q3.9_9 = as.numeric(Q3.9_9),
         Q3.9_10 = as.numeric(Q3.9_10), Q3.18_1 = as.numeric(Q3.18_1),
         Q3.18_2 = as.numeric(Q3.18_2), Q3.18_3 = as.numeric(Q3.18_3),
         Q3.18_4 = as.numeric(Q3.18_4), Q3.18_5 = as.numeric(Q3.18_5),
         Q3.18_6 = as.numeric(Q3.18_6), Q3.21_1 = as.numeric(Q3.21_1),
         Q3.21_2 = as.numeric(Q3.21_2), Q3.21_3 = as.numeric(Q3.21_3),
         Q3.21_4 = as.numeric(Q3.21_4))


#---------------------
# Deal with map data
#---------------------
# Get frequency tables
home.countries.freq <- responses.org %>% 
  select(Q1.4, country=home.country) %>% 
  group_by(country) %>% 
  summarize(freq = n()) %>%
  arrange(desc(freq))

home.countries.plot <- countries %>%
  left_join(home.countries.freq, by="country") %>%
  select(id=ISO3, country, freq) %>%
  mutate(freq_ceiling = ifelse(freq > 10, 10, freq)) %>%
  arrange(desc(freq))

work.countries.freq <- responses.countries %>% 
  select(Q3.2, country=work.country) %>% 
  group_by(country) %>% 
  summarize(freq = n()) %>%
  arrange(desc(freq))

work.countries.plot <- countries %>%
  left_join(work.countries.freq, by="country") %>%
  select(id=ISO3, country, freq) %>%
  mutate(freq_ceiling = ifelse(freq > 10, 10, freq)) %>%
  arrange(desc(freq))

# Load map information
world.map <- readShapeSpatial("../Data/maps/TM_WORLD_BORDERS_SIMPL-0.3.shp")
world.ggmap <- ggplot2::fortify(world.map, region = "ISO3")


#------------------
# Save everything
#------------------
save.image(file="../Data/responses.RData", compress="gzip")
