# Set working directory to source file location
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

gss <- gss_raw %>%
  # Convert all labeled columns to factors. Replace all numeric values with
  # label values using 'levels = "l"'.
  # Alternatively, 'levels = "v"' only keeps the values and 'levels = "p"' keeps
  # the labels prefixed with values.
  mutate_if(is.labelled, ~ to_factor(., levels = "l"))

glimpse(gss)


# Clean variables ---------------------------------------------------------

# Suppress printed messages
invisible(capture.output(
  # A script that cleans and creates relevant variables
  source("data-transformation.R")))


# Reorder variables in data set -------------------------------------------

gss <- gss %>%
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

glimpse(gss)


# Clean "polviews" variable -----------------------------------------------

# For my analysis I simplify political views into three categories: "liberal",
# "moderate" and "conservative".

# levels(gss$polviews)
# unique(gss$polviews)
# 
# gss$polviews <-
#   # Change factor levels
#   recode_factor(
#     gss$polviews,
#     `extremely liberal` = "liberal",
#     `slightly liberal` = "liberal",
#     `moderate, middle of the road` = "moderate",
#     `slightly conservative` = "conservative",
#     `extremely conservative` = "conservative"
#   )
# 
# levels(gss$polviews)
# unique(gss$polviews)


# Save cleaned GSS data ---------------------------------------------------

save(gss, file = "../data/gss.RData")

write.csv(gss, file = "../data/gss.csv")
