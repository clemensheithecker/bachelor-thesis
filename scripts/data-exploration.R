# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Clear environment
rm(list = ls())


# Load libraries ----------------------------------------------------------

# Package has useful function to generate descriptive statistics
library(psych)

# Create LaTeX code for well-formatted tables
library(stargazer)

library(tidyverse)

# Output variable documentation
library(vtable)


# Load data ---------------------------------------------------------------

load("../data/gss.RData")


# Create summary statistics -----------------------------------------------

gss_stats <- gss %>%
  # Select all columns from "consci" to "cohort"
  select(consci:cohort) %>%
  # Calculate summary statistics
  summarise_all(funs(
    Mean = mean(., na.rm = TRUE),
    SD = sd(., na.rm = TRUE),
    Min = min(., na.rm = TRUE),
    Max = max(., na.rm = TRUE)
  )) %>%
  # Reshape data frame from wide to long format
  gather(key = "Variable", value = "Value") %>%
  # Separate "Variable" column into two separate columns "Variable" and
  # "Statistic"
  separate(Variable,
           into = c("Variable", "Statistic"),
           sep = "_") %>%
  # Convert "Variable" to a factor to preserve the ordering of the variables
  mutate(Variable = factor(Variable, levels = unique(Variable))) %>%
  # Reshape "Statistic" and "Value" columns from long to wide format
  spread(Statistic, Value) %>%
  # Reorder columns
  select(Variable, Mean, SD, Min, Max) %>%
  # Round all values to three decimal places
  mutate(across(Mean:Max, round, 3))

# Convert "Variable" column to character
gss_stats$Variable <- as.character(gss_stats$Variable)

# Remove standard deviation (SD) values for dummy variables
gss_stats$SD[gss_stats$Variable == "consci"] <- NA
gss_stats$SD[gss_stats$Variable == "female"] <- NA
gss_stats$SD[gss_stats$Variable == "nonwhite"] <- NA
gss_stats$SD[gss_stats$Variable == "highschool"] <- NA
gss_stats$SD[gss_stats$Variable == "bachelor"] <- NA
gss_stats$SD[gss_stats$Variable == "graduate"] <- NA
gss_stats$SD[gss_stats$Variable == "south"] <- NA
gss_stats$SD[gss_stats$Variable == "independent"] <- NA
gss_stats$SD[gss_stats$Variable == "republican"] <- NA
gss_stats$SD[gss_stats$Variable == "moderate"] <- NA
gss_stats$SD[gss_stats$Variable == "conservative"] <- NA
gss_stats$SD[gss_stats$Variable == "postreagan"] <- NA
gss_stats$SD[gss_stats$Variable == "bush"] <- NA
gss_stats$SD[gss_stats$Variable == "posttrump"] <- NA
gss_stats$SD[gss_stats$Variable == "covid19"] <- NA

# Rename variables
gss_stats$Variable[gss_stats$Variable == "consci"] <-
  "Confidence in Science"
gss_stats$Variable[gss_stats$Variable == "female"] <- "Female"
gss_stats$Variable[gss_stats$Variable == "nonwhite"] <- "Non-White"
gss_stats$Variable[gss_stats$Variable == "educ"] <-
  "Education (yrs)"
gss_stats$Variable[gss_stats$Variable == "highschool"] <-
  "High School"
gss_stats$Variable[gss_stats$Variable == "bachelor"] <- "Bachelor"
gss_stats$Variable[gss_stats$Variable == "graduate"] <- "Graduate"
gss_stats$Variable[gss_stats$Variable == "south"] <- "South"
gss_stats$Variable[gss_stats$Variable == "attend"] <-
  "Church Attendance"
gss_stats$Variable[gss_stats$Variable == "realinc"] <-
  "Family Income"
gss_stats$Variable[gss_stats$Variable == "age"] <- "Age"
gss_stats$Variable[gss_stats$Variable == "independent"] <-
  "Independent"
gss_stats$Variable[gss_stats$Variable == "republican"] <-
  "Republican"
gss_stats$Variable[gss_stats$Variable == "moderate"] <- "Moderate"
gss_stats$Variable[gss_stats$Variable == "conservative"] <-
  "Conservative"
gss_stats$Variable[gss_stats$Variable == "postreagan"] <-
  "Post-Reagan (1981-2021)"
gss_stats$Variable[gss_stats$Variable == "bush"] <-
  "Bush (2001-2008)"
gss_stats$Variable[gss_stats$Variable == "posttrump"] <-
  "Post-Trump (2017-2021)"
gss_stats$Variable[gss_stats$Variable == "covid19"] <- "COVID-19"
gss_stats$Variable[gss_stats$Variable == "cohort"] <- "Cohort"



# Convert data frame to LaTeX table ---------------------------------------

# Number of observations
n_observations = nrow(gss)

# Generate LaTeX output

stargazer(
  gss_stats,
  type = "latex",
  title = paste0(
    "Descriptive Statistics, General Social Survey 1974 to 2021 ($N=",
    n_observations,
    "$)"
  ),
  out = "../reports/gss-stats.tex",
  out.header = TRUE,
  summary = FALSE,
  align = TRUE,
  rownames = FALSE
)
