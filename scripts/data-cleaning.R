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
    "realinc"
  )


# Load GSS data -----------------------------------------------------------

# Extract data files
unzip(zipfile = "../data/gss-2021.zip", exdir = "../data")

gss_raw <- read_dta(
  "../data/gss-2021.dta",
  col_select = all_of(gss_variables)
)

glimpse(gss_raw)


# Clean variables ---------------------------------------------------------

# Create copy of data frame to become transformed data with missing values
gss_with_na <- gss_raw

# Convert variable labels to factors
gss_with_na <- gss_with_na %>%
  # Convert all labeled columns to factors. Replace all numeric values with
  # label values using 'levels = "l"'.
  # Alternatively, 'levels = "v"' only keeps the values and 'levels = "p"' keeps
  # the labels prefixed with values.
  mutate_if(is.labelled, ~ to_factor(., levels = "l"))

glimpse(gss_with_na)

# Suppress printed messages
invisible(capture.output(
  # Run script that that cleans data set and creates relevant variables
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
    "socialmedia",
    "posttrump",
    "covid19"
  )

glimpse(gss_with_na)


# Remove records containing missing values --------------------------------

# Create copy of data frame to become transformed data without missing values
gss <- gss_with_na

# Number of missing values in each column
sapply(gss, function(x) sum(is.na(x)))

# Total number of observations in data set (with missing values)
nrow(gss)

# Number of observations (with missing values) = 67,210

# Omit observations with missing values in all years but 2021
# Note: The GSS was conducted online in 2021. Thus, data on race and region is
# not available. If I dropped all records containing NA values, I would drop all
# 2021 records
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

# Concatenate or combine gss_pre_2021 and gss_2021 data frames
gss <- rbind(gss_pre_2021, gss_2021)

# Clean up
rm(gss_pre_2021, gss_2021)

# Total number of observations in data set (without missing values)
nrow(gss)

# Number of observations (without missing values) = 35,427


# Save cleaned GSS data ---------------------------------------------------

save(gss, file = "../data/gss.RData")
write.csv(gss, file = "../data/gss.csv")


# Save transformed GSS data with missing values ---------------------------

save(gss_with_na, file = "../data/gss-with-na.RData")
write.csv(gss_with_na, file = "../data/gss-with-na.csv")


# Save raw GSS data -------------------------------------------------------

save(gss_raw, file = "../data/gss-raw.RData")
write.csv(gss_raw, file = "../data/gss-raw.csv")
