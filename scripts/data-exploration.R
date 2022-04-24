# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Clear environment
rm(list = ls())


# Load libraries ----------------------------------------------------------

library(tidyr)

library(tidyverse)

# Export data frame as LaTeX table
library(xtable)


# Load data ---------------------------------------------------------------

load("../data/gss.RData")
load("../data/gss_with_na.RData")


# Create summary statistics -----------------------------------------------

summary_stats <- function(df, binary_dummies, variable_names) {
  df_stats <- df %>%
    # Calculate summary statistics
    summarise_all(list(
      Mean = ~ mean(., na.rm = TRUE),
      SD = ~ sd(., na.rm = TRUE),
      Min = ~ min(., na.rm = TRUE),
      Max = ~ max(., na.rm = TRUE)
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
  df_stats$Variable <- as.character(df_stats$Variable)
  
  # Remove standard deviation (SD) values for binary dummy variables
  for (variable in binary_dummies) {
    df_stats$SD[df_stats$Variable == variable] <- NA
  }
  
  # Rename variables
  
  i = 1
  
  for (variable in variable_names) {
    variable_code <- variable
    variable_name <- names(variable_names)[i]
    
    df_stats$Variable[df_stats$Variable == variable_code] <- variable_name
    
    i <- i + 1
  }
  
  return(df_stats)
}


# Summary statistics for 1974-2021 (without missing values)
gss_stats <- gss %>%
  select(consci:cohort) %>%
  summary_stats(
    df = .,
    binary_dummies = c(
      "consci",
      "female",
      "nonwhite",
      "highschool",
      "bachelor",
      "graduate",
      "south",
      "independent",
      "republican",
      "moderate",
      "conservative",
      "postreagan",
      "bush",
      "posttrump",
      "covid19"
    ),
    variable_names = c(
      "Confidence in Science" = "consci",
      "Female" = "female",
      "Non-White" = "nonwhite",
      "Education (yrs)" = "educ",
      "High School" = "highschool",
      "Bachelor" = "bachelor",
      "Graduate" = "graduate",
      "South" = "south",
      "Church Attendance" = "attend",
      "Family Income" = "realinc",
      "Age" = "age",
      "Independent" = "independent",
      "Republican" = "republican",
      "Moderate" = "moderate",
      "Conservative" = "conservative",
      "Post-Reagan (1981-2021)" = "postreagan",
      "Bush (2001-2008)" = "bush",
      "Post-Trump (2017-2021)" = "posttrump",
      "COVID-19" = "covid19",
      "Cohort" = "cohort"
    )
  )

# Summary statistics for 1974-2010 (without missing values)
gss_stats_2010 <- gss %>%
  filter(year <= 2010) %>%
  select(consci:cohort) %>%
  summary_stats(
    df = .,
    binary_dummies = c(
      "consci",
      "female",
      "nonwhite",
      "highschool",
      "bachelor",
      "graduate",
      "south",
      "independent",
      "republican",
      "moderate",
      "conservative",
      "postreagan",
      "bush",
      "posttrump",
      "covid19"
    ),
    variable_names = c(
      "Confidence in Science" = "consci",
      "Female" = "female",
      "Non-White" = "nonwhite",
      "Education (yrs)" = "educ",
      "High School" = "highschool",
      "Bachelor" = "bachelor",
      "Graduate" = "graduate",
      "South" = "south",
      "Church Attendance" = "attend",
      "Family Income" = "realinc",
      "Age" = "age",
      "Independent" = "independent",
      "Republican" = "republican",
      "Moderate" = "moderate",
      "Conservative" = "conservative",
      "Post-Reagan (1981-2021)" = "postreagan",
      "Bush (2001-2008)" = "bush",
      "Post-Trump (2017-2021)" = "posttrump",
      "COVID-19" = "covid19",
      "Cohort" = "cohort"
    )
  )

# Summary statistics for 1974-2010 (only missing values for income)
gss_stats_2010_na <- gss_with_na %>%
  # Select all rows containing missing values for income up to and including
  # the year 2010
  filter(year <= 2010, is.na(realinc)) %>%
  # Drop all rows containing NA values except for income
  drop_na(-realinc) %>%
  select(consci:cohort, -realinc) %>%
  summary_stats(
    df = .,
    binary_dummies = c(
      "consci",
      "female",
      "nonwhite",
      "highschool",
      "bachelor",
      "graduate",
      "south",
      "independent",
      "republican",
      "moderate",
      "conservative",
      "postreagan",
      "bush",
      "posttrump",
      "covid19"
    ),
    variable_names = c(
      "Confidence in Science" = "consci",
      "Female" = "female",
      "Non-White" = "nonwhite",
      "Education (yrs)" = "educ",
      "High School" = "highschool",
      "Bachelor" = "bachelor",
      "Graduate" = "graduate",
      "South" = "south",
      "Church Attendance" = "attend",
      "Age" = "age",
      "Independent" = "independent",
      "Republican" = "republican",
      "Moderate" = "moderate",
      "Conservative" = "conservative",
      "Post-Reagan (1981-2021)" = "postreagan",
      "Bush (2001-2008)" = "bush",
      "Post-Trump (2017-2021)" = "posttrump",
      "COVID-19" = "covid19",
      "Cohort" = "cohort"
    )
  )


# Convert data frame to LaTeX table ---------------------------------------

summary_stats_to_latex <- function(df, file, title, footnote = NA) {
  # Capture output from xtable
  output <- capture.output(print(
    xtable(
      df,
      type = "latex",
      caption = title,
      # Set the alignment of the columns
      align = c("l", "X", "r", "r", "r", "r"),
      # Set the number of digits
      digits = c(0, 0, 3, 3, 3, 3),
      # Set the format of the columns
      display = c("s", "s", "f", "f", "f", "f")
    ),
    type = "latex",
    include.rownames = FALSE,
    caption.placement = "top",
    booktabs = TRUE,
    tabular.environment = "tabularx",
    width = "\\textwidth"
  ))
  
  # Remove unnecessary decimal points
  output <- gsub(".000", "", output)
  
  if (!is.na(footnote)) {
    # Add footnote
    output[length(output) + 1] <- output[length(output)]
    output[length(output) - 1] <- paste0("\\floatfoot*{", footnote, "}")
  }
  
  # Write to LaTeX file
  write(output, file = file)
}


# Summary statistics for 1974-2021 (without missing values)

# Number of observations
n_obs = nrow(gss)

summary_stats_to_latex(
  df = gss_stats,
  file = "../reports/figures/gss-stats.tex",
  title = paste0(
    "Descriptive Statistics, General Social Survey 1974 to 2021 ($N=",
    # Add thousands-separator comma mark
    formatC(n_obs, format = "d", big.mark = ","),
    "$)"
  ),
  footnote = "{\\it Note:} Family income is measured in constant dollars ($\\text{base}=1986$) and was z-score standardized. The original age variable was decreased by a factor of 10."
)

# Summary statistics for 1974-2010 (without missing values)

# Number of observations
n_obs_2010 = nrow(gss %>% filter(year <= 2010))

summary_stats_to_latex(
  df = gss_stats_2010,
  file = "../reports/figures/gss-stats-2010.tex",
  title = paste0(
    "Descriptive Statistics, General Social Survey 1974 to 2010 ($N=",
    # Add thousands-separator comma mark
    formatC(n_obs_2010, format = "d", big.mark = ","),
    "$)"
  ),
  footnote = "{\\it Note:} Family income is measured in constant dollars ($\\text{base}=1986$) and was z-score standardized. The original age variable was decreased by a factor of 10."
)

# Summary statistics for 1974-2010 (only missing values for income)

# Number of observations
n_obs_2010_na = nrow(gss_with_na %>%
                       filter(year <= 2010, is.na(realinc)) %>%
                       drop_na(-realinc))

summary_stats_to_latex(
  df = gss_stats_2010_na,
  file = "../reports/figures/gss-stats-2010-na.tex",
  title = paste0(
    "Descriptive Statistics, General Social Survey 1974 to 2010 (Only Missing Values for Income, $N=",
    # Add thousands-separator comma mark
    formatC(n_obs_2010_na, format = "d", big.mark = ","),
    "$)"
  ),
  footnote = "{\\it Note:} Family income is measured in constant dollars ($\\text{base}=1986$) and was z-score standardized. The original age variable was decreased by a factor of 10."
)
