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
load("../data/gss-with-na.RData")
load("../data/gss-raw.RData")


# Summary statistics: confidence in scientific community ------------------

# Check for tagged missing values
unique(print_tagged_na(gss_raw$consci))

# Label tagged missing values
gss_raw$consci <- labelled(
  gss_raw$consci,
  append(
    val_labels(gss_raw$consci),
    c(
      "Don't know (DK)" = tagged_na("d"),
      "Inapplicable (IAP)" = tagged_na("i"),
      "No Answer (NA)" = tagged_na("n"),
      "Skipped on Web" = tagged_na("s")
    ) 
  )
)

val_labels(gss_raw$consci)

# Convert labelled vector to factor
gss_raw$consci <- as_factor(gss_raw$consci)

stats_consci <- gss_raw %>%
  filter(
    consci != "Inapplicable (IAP)" &
      consci != "Skipped on Web" &
      !year %in% c(1972, 1973, 1985)
  ) %>%
  select(consci) %>%
  mutate(n = n()) %>%
  group_by(consci) %>%
  summarise(
    consci_elements_count = n(),
    n = unique(n)
  ) %>%
  ungroup() %>%
  mutate(consci_elements_fraction = consci_elements_count / n)

stats_consci

stats_consci_until_2010 <- gss_raw %>%
  filter(
    consci != "Inapplicable (IAP)" &
      !year %in% c(1972, 1973, 1985) &
      year <= 2010
  ) %>%
  select(consci) %>%
  mutate(n = n()) %>%
  group_by(consci) %>%
  summarise(
    consci_elements_count = n(),
    n = unique(n)
  ) %>%
  ungroup() %>%
  mutate(consci_elements_fraction = consci_elements_count / n)

stats_consci_until_2010


# Summary statistics: political ideology ----------------------------------

# Check for tagged missing values
unique(print_tagged_na(gss_raw$polviews))

# Label tagged missing values
gss_raw$polviews <- labelled(
  gss_raw$polviews,
  append(
    val_labels(gss_raw$polviews),
    c(
      "Don't know (DK)" = tagged_na("d"),
      "Inapplicable (IAP)" = tagged_na("i"),
      "No Answer (NA)" = tagged_na("n"),
      "Skipped on Web" = tagged_na("s")
    ) 
  )
)

val_labels(gss_raw$polviews)

# Convert labelled vector to factor
gss_raw$polviews <- as_factor(gss_raw$polviews)


stats_polviews <- gss_raw %>%
  filter(
    polviews != "Inapplicable (IAP)" &
      polviews != "Skipped on Web" &
      !year %in% c(1972, 1973, 1985)
  ) %>%
  select(polviews) %>%
  mutate(n = n()) %>%
  group_by(polviews) %>%
  summarise(
    polviews_elements_count = n(),
    n = unique(n)
  ) %>%
  ungroup() %>%
  mutate(polviews_elements_fraction = polviews_elements_count / n)

stats_polviews

stats_polviews_grouped <- gss_raw %>%
  filter(
    polviews != "Inapplicable (IAP)" &
      polviews != "Skipped on Web" &
      !year %in% c(1972, 1973, 1985)
  ) %>%
  select(polviews) %>%
  mutate(polviews = factor(
    case_when(
      polviews == "extremely liberal" ~ "liberal",
      polviews == "liberal" ~ "liberal",
      polviews == "slightly liberal" ~ "liberal",
      polviews == "moderate, middle of the road" ~ "moderate",
      polviews == "slightly conservative" ~ "conservative",
      polviews == "conservative" ~ "conservative",
      polviews == "extremely conservative" ~ "conservative",
      polviews == "Don't know (DK)" ~ "Don't know (DK)",
      polviews == "No Answer (NA)" ~ "No Answer (NA)"
    ),
    levels = c(
      "liberal",
      "moderate",
      "conservative",
      "Don't know (DK)",
      "No Answer (NA)"
    )
  )) %>%
  mutate(n = n()) %>%
  group_by(polviews) %>%
  summarise(
    polviews_elements_count = n(),
    n = unique(n)
  ) %>%
  ungroup() %>%
  mutate(polviews_elements_fraction = polviews_elements_count / n)

stats_polviews_grouped


# Summary statistics: political party affiliation -------------------------

# Check for tagged missing values
unique(print_tagged_na(gss_raw$partyid))

# Label tagged missing values
gss_raw$partyid <- labelled(
  gss_raw$partyid,
  append(
    val_labels(gss_raw$partyid),
    c(
      "Don't know (DK)" = tagged_na("d"),
      "No Answer (NA)" = tagged_na("n")
    ) 
  )
)

val_labels(gss_raw$polviews)

# Convert labelled vector to factor
gss_raw$partyid <- as_factor(gss_raw$partyid)


stats_partyid <- gss_raw %>%
  filter(!year %in% c(1972, 1973, 1985)) %>%
  select(partyid) %>%
  mutate(n = n()) %>%
  group_by(partyid) %>%
  summarise(
    partyid_elements_count = n(),
    n = unique(n)
  ) %>%
  ungroup() %>%
  mutate(partyid_elements_fraction = partyid_elements_count / n)

stats_partyid

stats_partyid_grouped <- gss_raw %>%
  filter(!year %in% c(1972, 1973, 1985)) %>%
  select(partyid) %>%
  mutate(partyid = factor(
    case_when(
      partyid == "strong democrat" ~ "democrat",
      partyid == "not very strong democrat" ~ "democrat",
      partyid == "independent, close to democrat" ~ "independent",
      partyid == "independent (neither, no response)" ~ "independent",
      partyid == "independent, close to republican" ~ "independent",
      partyid == "not very strong republican" ~ "republican",
      partyid == "strong republican" ~ "republican",
      partyid == "other party" ~ "other party",
      partyid == "Don't know (DK)" ~ "Don't know (DK)",
      partyid == "No Answer (NA)" ~ "No Answer (NA)"
    ),
    levels = c(
      "democrat",
      "independent",
      "republican",
      "other party",
      "Don't know (DK)",
      "No Answer (NA)"
    )
  )) %>%
  mutate(n = n()) %>%
  group_by(partyid) %>%
  summarise(
    partyid_elements_count = n(),
    n = unique(n)
  ) %>%
  ungroup() %>%
  mutate(partyid_elements_fraction = partyid_elements_count / n)

stats_partyid_grouped


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
      "Education (years)" = "educ",
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
      "Post-Reagan (1981--2021)" = "postreagan",
      "Bush (2001--2008)" = "bush",
      "Post-Trump (2017--2021)" = "posttrump",
      "COVID-19 (2020--2021)" = "covid19",
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
      "Education (years)" = "educ",
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
      "Post-Reagan (1981--2021)" = "postreagan",
      "Bush (2001--2008)" = "bush",
      "Post-Trump (2017--2021)" = "posttrump",
      "COVID-19 (2020--2021)" = "covid19",
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
      "Education (years)" = "educ",
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
      "Post-Reagan (1981--2021)" = "postreagan",
      "Bush (2001--2008)" = "bush",
      "Post-Trump (2017--2021)" = "posttrump",
      "COVID-19 (2020--2021)" = "covid19",
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
    width = "\\textwidth",
    comment = FALSE
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
  footnote = "{\\it Note:} Family income is measured in constant dollars ($\\text{base}=1986$) and is $z$-score standardized. The original age variable is decreased by a factor of 10."
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
  footnote = "{\\it Note:} Family income is measured in constant dollars ($\\text{base}=1986$) and is $z$-score standardized. The original age variable is decreased by a factor of 10."
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
  footnote = "{\\it Note:} Family income is measured in constant dollars ($\\text{base}=1986$) and is $z$-score standardized. The original age variable is decreased by a factor of 10."
)
