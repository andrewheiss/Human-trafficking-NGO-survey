#----------------------------------------
# Libraries and useful helper functions
#----------------------------------------
library(dplyr)
library(magrittr)
library(lubridate)
library(foreign)

# Shrink long strings
truncate <- function(x) {
  if (nchar(x) > 80) {
    paste0(strtrim(x, 80), "...")
  } else {
    x
  }
}

# Change a cowcode to 1
binarize <- function(x) {
  ifelse(!is.na(x), 1, 0)
}

# Check if value in reference column (added previously with cbind) is missing; 
# if so, mark all columns in row as missing
fix.missing <- function(x) {
  if (is.na(x[1])) {
    x[1:length(x)] <- NA
  }
  
  x[-1]
}


#-----------------------------------
# Export organization-country data
#-----------------------------------
# Load data and handle missing values
responses.countries.raw <- read.csv("Original/responses_countries.csv",
                                    stringsAsFactors = FALSE, na.strings=c("NA", ""))
responses.countries.orphans <- read.csv("Original/orphans_clean.csv",
                                        stringsAsFactors=FALSE, na.strings=c("NA", ""))
responses.countries.raw.all <- bind_rows(responses.countries.raw, responses.countries.orphans)

# Clean up the data
responses.countries <- responses.countries.raw.all %>%
  mutate(work.country = factor(work.country),
         Q3.3 = factor(Q3.3, levels=c("None", "Very little", "Little", 
                                      "Some", "A lot", "Don't know"), ordered=TRUE),
         Q3.4 = factor(Q3.4, levels=c("Never", "Rarely", "Sometimes", "Monthly", 
                                      "Weekly", "Daily", "Don't know"), ordered=TRUE),
         Q3.8 = factor(Q3.8, levels=c("No", "Yes", "Don't know"), ordered=TRUE),
         Q3.19 = factor(Q3.19, levels=c("Most important actor", 
                                        "Somewhat important actor", 
                                        "Not an important actor", "Don't know"), 
                        ordered=TRUE),
         Q3.20 = factor(Q3.20, levels=c("Not hard at all", "Not too hard", 
                                        "Somewhat hard", "Very hard", 
                                        "Extremely hard", "Don't know"), ordered=TRUE),
         Q3.22 = factor(Q3.22, levels=c("Tier 1", "Tier 2", "Watch list", 
                                        "Tier 3", "Don't know"), ordered=TRUE),
         Q3.23 = factor(Q3.23, levels=c("No", "Yes"), ordered=TRUE),
         Q3.25 = factor(Q3.25, levels=c("Negative", "Positive", "Mixed", 
                                        "Don't know"), ordered=TRUE),
         Q3.26 = factor(Q3.26, levels=c("Improved", "Remained constant", 
                                        "Slowed down", "Don't know"), ordered=TRUE),
         Q3.27 = factor(Q3.27, levels=c("No", "Yes", "Don't know"), ordered=TRUE),
         Q3.28 = factor(Q3.28, levels=c("No", "Yes", "Don't know"), ordered=TRUE),
         Q3.29 = factor(Q3.29, levels=c("Not restricted", "Very little restricted", 
                                        "A little restricted", "Somewhat restricted",
                                        "Very restricted", "Don't know"), 
                        ordered=TRUE))


#--------------------------------------------------------
# Convert multiple-coded responses to binary indicators
#--------------------------------------------------------
# Fix Q3.5
Q3.5.responses <- responses.countries %>% select(13:24)

Q3.5.responses.bin <- apply(Q3.5.responses, MARGIN=2, FUN=binarize) %>%
  set_colnames(paste0("Q3.5_5_", gsub("_.*", "", colnames(Q3.5.responses)))) %>%
  data.frame()


Q3.5.responses.bin <- data.frame(t(apply(cbind(responses.countries$Q3.5_5_TEXT, 
                                               Q3.5.responses.bin), 1, fix.missing)))


# Fix Q3.6
# Extract just the country columns
Q3.6.countries <- responses.countries %>% select(26:89)

# Get the cowcode for use in new column names
ccodes <- t(na.omit(apply(Q3.6.countries, MARGIN=2, FUN=unique)))[,1]

# Convert cowcodes to binary response with cowcode column name
Q3.6.countries.bin <- apply(Q3.6.countries, MARGIN=2, FUN=binarize) %>%
  set_colnames(paste0("Q3.6_c", ccodes)) %>%
  data.frame()

Q3.6.countries.bin <- data.frame(t(apply(cbind(responses.countries$Q3.6, 
                                               Q3.6.countries.bin), 1, fix.missing)))


# Fix Q3.21
Q3.21.responses <- responses.countries %>% select(125:141)

Q3.21.responses.bin <- apply(Q3.21.responses, MARGIN=2, FUN=binarize) %>%
  set_colnames(paste0("Q3.21_4_", gsub("_.*", "", colnames(Q3.21.responses)))) %>%
  data.frame()

Q3.21.responses.bin <- data.frame(t(apply(cbind(responses.countries$Q3.21_4_TEXT, 
                                                Q3.21.responses.bin), 1, fix.missing)))


# Fix Q3.24
Q3.24.responses <- responses.countries %>% select(145:158)

Q3.24.responses.bin <- apply(Q3.24.responses, MARGIN=2, FUN=binarize) %>%
  set_colnames(paste0("Q3.24_", gsub("_.*", "", colnames(Q3.24.responses)))) %>%
  data.frame()

Q3.24.responses.bin <- data.frame(t(apply(cbind(responses.countries$Q3.24.Text, 
                                                Q3.24.responses.bin), 1, fix.missing)))


#--------------------------
# Write final clean files
#--------------------------
# Combine all the columns
final <- cbind(select(responses.countries, 1:12), 
               Q3.5.responses.bin,  # 13:24
               select(responses.countries, 25),
               Q3.6.countries.bin,  # 26:89
               select(responses.countries, 90:124),
               Q3.21.responses,  # 125:141
               select(responses.countries, 142:144),
               Q3.24.responses.bin,  # 145:158
               select(responses.countries, 159:ncol(responses.countries)))

# Truncate long cells
final.truncated <- final %>%
  rowwise() %>%  # Needed for truncate() to work
  mutate(Q3.6 = truncate(Q3.6),
         Q3.10 = truncate(Q3.10),
         Q3.11 = truncate(Q3.11),
         Q3.12 = truncate(Q3.12),
         Q3.13 = truncate(Q3.13),
         Q3.14 = truncate(Q3.14),
         Q3.15 = truncate(Q3.15),
         Q3.16 = truncate(Q3.16),
         Q3.17 = truncate(Q3.17),
         Q3.18_4_TEXT = truncate(Q3.18_4_TEXT),
         Q3.24.Text = truncate(Q3.24.Text),
         Q3.30 = truncate(Q3.30)) %>%
  ungroup()

# Add fancy Stata labels
labs <- read.csv("Labels/response_countries_labels.csv", 
                 stringsAsFactors=FALSE)$varlabel
attr(final.truncated, "var.labels") <- labs
# attr(final.truncated, "labels") <- labs  # For haven and dplyr someday
# for (i in 1:ncol(df)) {
#   attr(df[[i]], "label") <- NULL
# }

# Write (finally!)
# Stata
write.dta(final.truncated, file="Final/responses_countries.dta", version=11)

# CSV
write.csv(final.truncated, file="Final/responses_countries_truncated.csv",
          row.names=FALSE)
write.csv(final, file="Final/responses_countries.csv", row.names=FALSE)

# R
responses.countries <- final
responses.countries.truncated <- final.truncated
save(responses.countries, responses.countries.truncated, 
     file="Final/responses_countries.RData", compress="gzip")


#---------------------------
# Export organization data
#---------------------------
# Load data and handle missing values
responses.orgs.raw <- read.csv("Original/responses_orgs.csv",
                               stringsAsFactors = FALSE, na.strings=c("NA", ""),
                               encoding="UTF-8")

# Clean up the data
responses.orgs <- responses.orgs.raw %>%
  mutate(Q1.3 = factor(Q1.3, levels=c("No", "Yes"), ordered=TRUE),
         home.country = factor(home.country),
         work.only.us = factor(work.only.us, labels=c("No", "Yes"), ordered=TRUE),
         Q1.5.factor = factor(Q1.5.factor, levels=c("1", "2", "3", "4", "5+"),
                              ordered=TRUE),
         Q2.5 = factor(Q2.5, levels=c("No", "Yes"), ordered=TRUE),
         Q2.6 = factor(Q2.6, levels=c("No", "Yes"), ordered=TRUE),
         Q4.2 = factor(Q4.2, levels=c("No", "Yes"), ordered=TRUE),
         Q4.3 = factor(Q4.3, levels=c("No", "Yes"), ordered=TRUE))

responses.orgs.truncated <- responses.orgs %>%
  rowwise() %>%
  mutate(Q2.2_4_TEXT = truncate(Q2.2_4_TEXT),
         Q2.3_3_TEXT = truncate(Q2.3_3_TEXT),
         Q2.4_5_TEXT = truncate(Q2.4_5_TEXT),
         Q4.1 = truncate(Q4.1)) %>%
  ungroup()


# Add fancy Stata labels
labs <- read.csv("Labels/response_orgs_labels.csv",
                 stringsAsFactors=FALSE)$varlabel
attr(responses.orgs.truncated, "var.labels") <- labs

# Write!
# Stata
write.dta(responses.orgs.truncated, file="Final/responses_orgs.dta", version=11)


# Format time correctly (but not for Stata, since Stata's handling of time is voodoo)
responses.orgs %<>%
  mutate(start.time = mdy_hm(start.time),
         end.time = mdy_hm(end.time))

responses.orgs.truncated %<>%
  mutate(start.time = mdy_hm(start.time),
         end.time = mdy_hm(end.time))

# CSV
write.csv(responses.orgs.truncated, file="Final/responses_orgs_truncated.csv",
          row.names=FALSE)
write.csv(responses.orgs, file="Final/responses_orgs.csv", row.names=FALSE)

# R
save(responses.orgs.truncated, responses.orgs, 
     file="Final/responses_orgs.RData", compress="gzip")


#-------------------------------------------
# Run final cleaning script on Stata files
#-------------------------------------------
system("stata-se -b do clean_dta.do")
system("rm clean_dta.log")

# Merging
full <- final %>% left_join(responses.orgs)
example <- full %>% 
  select(survey.id, loop.number, home.country, Q1.4, Q1.2, Q3.2, work.country)

# write.dta(example, file="Final/example.dta", version=11)
# table(full$home.country)

# full1 <- responses.orgs %>% left_join(final)
# table(full1$home.country)
