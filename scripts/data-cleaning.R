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
    "ballot",
    "consci",
    "polviews",
    "partyid",
    "sex",
    "ethnic",
    "educ",
    "degree",
    "realinc",
    "region",
    "attend",
    "age"
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


# Clean "consci" variable -------------------------------------------------

# I only consider the two cases for the "confidence in science" variable namely
# whether an individual shares "a great deal" of trust or otherwise.

levels(gss$consci)
unique(gss$consci)

gss$consci <-
  # Change factor levels
  recode_factor(
    gss$consci,
    `a great deal` = 1,
    `only some` = 0,
    `hardly any` = 0
  )

# Convert factor level to numeric
gss <- gss %>%
  mutate(consci = as.numeric(as.character(consci)))

glimpse(gss)


# Clean "polviews" variable -----------------------------------------------

# For my analysis I simplify political views into three categories: "liberal",
# "moderate" and "conservative".

levels(gss$polviews)
unique(gss$polviews)

gss$polviews <-
  # Change factor levels
  recode_factor(
    gss$polviews,
    `extremely liberal` = "liberal",
    `slightly liberal` = "liberal",
    `moderate, middle of the road` = "moderate",
    `slightly conservative` = "conservative",
    `extremely conservative` = "conservative"
  )

levels(gss$polviews)
unique(gss$polviews)


# Save cleaned GSS data ---------------------------------------------------

save(gss, file = "../data/gss.RData")

write.csv(gss, file = "../data/gss.csv")
