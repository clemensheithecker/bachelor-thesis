# Set working directory to source file location in RStudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Clear environment
rm(list = ls())


# Load libraries ----------------------------------------------------------

# Import foreign statistical formats
library(haven)

# Manipulate metadata as variable and value labels
library(labelled)

library(tidyverse)


# Source scripts ----------------------------------------------------------

# A script that creates two custom ggplot2 themes
source("ggplot2-themes.R")


# Select variables of interest --------------------------------------------

gss_variables <-
  c(
    "year",
    "id",
    "age",
    "educ",
    "degree",
    "sex",
    "race",
    "region",
    "partyid",
    "polviews",
    "attend",
    "consci",
    "realinc",
    "cohort"
  )


# Load GSS data -----------------------------------------------------------

# Extract data files
unzip(zipfile = "../data/gss-2021.zip", exdir = "../data")

gss_raw <-
  read_dta("../data/gss-2021.dta", col_select = all_of(gss_variables))

glimpse(gss_raw)


# Convert variable labels to factors --------------------------------------

gss_raw <- gss_raw %>%
  # Convert all labeled columns to factors. Replace all numeric values with
  # label values using 'levels = "l"'.
  # Alternatively, 'levels = "v"' only keeps the values and 'levels = "p"' keeps
  # the labels prefixed with values.
  mutate_if(is.labelled, ~ to_factor(., levels = "l"))

glimpse(gss_raw)


# Clean variables ---------------------------------------------------------

# Transformed data with missing values
gss_with_na <- gss_raw

# Suppress printed messages
invisible(capture.output(
  # A script that cleans and creates relevant variables
  source("data-transformation.R")))


# Reorder variables in data set -------------------------------------------

gss_with_na <- gss_with_na %>%
  select(
    "year",
    "id",
    "consci",
    "female",
    "nonwhite",
    "educ",
    "highschool",
    "bachelor",
    "graduate",
    "south",
    "attend",
    "realinc",
    "age",
    "independent",
    "republican",
    "moderate",
    "conservative",
    "postreagan",
    "bush",
    "posttrump",
    "covid19",
    "cohort"
  )

glimpse(gss_with_na)


# Remove records containing missing values --------------------------------

gss <- gss_with_na

# Number of missing values in each column
sapply(gss, function(x) sum(is.na(x)))

# Total number of observations in data set (with missing values)
nrow(gss)

# number of observations = 67895

# Omit observations with missing values in all years but 2021
# Note: The GSS was conducted online in 2021 thus data on race and region is not
# available. If I drop all records containing NA values, I would drop all 2021
# records
gss_pre_2021 <- gss %>%
  # Select all observations before 2021
  filter(year < 2021) %>%
  # Drop all NA values over all columns
  drop_na()

gss_2021 <- gss %>%
  # Select all observations of 2021
  filter(year == 2021) %>%
  # Drop all NA values over all columns but nonwhite and south
  drop_na(
    # Select all columns but nonwhite and south
    -one_of(c("nonwhite", "south")))

# Number of missing values in each column for gss_pre_2021
sapply(gss_pre_2021, function(x) sum(is.na(x)))

# Number of missing values in each column for gss_2021
sapply(gss_2021, function(x) sum(is.na(x)))

# Concatenate or combine gss_pre_2021 and gss_2021 dataframes
gss <- rbind(gss_pre_2021, gss_2021)

# Total number of observations in data set (without missing values)
nrow(gss)

# number of observations = 35940

# Remove temporary data frames
rm(gss_pre_2021, gss_2021)


# Save cleaned GSS data ---------------------------------------------------

save(gss, file = "../data/gss.RData")
write.csv(gss, file = "../data/gss.csv")


# Save transformed GSS data with missing values ---------------------------

save(gss_with_na, file = "../data/gss-with-na.RData")
write.csv(gss_with_na, file = "../data/gss-with-na.csv")


# Save raw GSS data -------------------------------------------------------

save(gss_raw, file = "../data/gss-raw.RData")
write.csv(gss_raw, file = "../data/gss-raw.csv")
