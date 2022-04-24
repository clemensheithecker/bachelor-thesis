# Set working directory to source file location in RStudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Clear environment
rm(list = ls())


# Load libraries ----------------------------------------------------------

# Import foreign statistical formats
library(haven)

# Manipulate metadata as variable and value labels
library(labelled)

# A graphic device which supports custom fonts
library(ragg)

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


# Understanding missing values --------------------------------------------

missing_values_summary <- gss_raw %>%
  group_by(year) %>%
  summarise(
    across(everything(), ~ sum(is.na(.x))),
    n = n()
  ) %>%
  ungroup() %>%
  # Divide all columns but `year` and `n` by `n`
  mutate(across(-c(year, n), ~ .x / n)) %>%
  # Add "_fraction" suffix to all columns but `year` and `n`
  rename_with(~ paste0(., "_fraction_na"), - c(year, n))

head(missing_values_summary)
glimpse(missing_values_summary)


missing_values_summary_long <- missing_values_summary %>%
  select(-n) %>%
  rename_all( ~ stringr::str_replace(., "_fraction_na", "")) %>%
  pivot_longer(!year, names_to = "variable", values_to = "fraction_missing") %>%
  mutate(fraction_present = 1 - fraction_missing) %>%
  pivot_longer(
    !c(year, variable),
    names_to = "measure",
    values_to = "fraction"
  )


missing_values_summary_plot <- function(df, filename, title) {
  plot <- df %>%
    ggplot(mapping = aes(x = fraction, y = variable)) +
    geom_col(
      mapping = aes(fill = measure),
      position = position_stack(reverse = TRUE)
    ) +
    geom_text(
      mapping = aes(label = ifelse(measure == "fraction_missing",
                                   scales::percent(fraction, accuracy = 0.1),
                                   ""),
                    hjust = ifelse(measure == "fraction_missing" &
                                     fraction > 0.7,
                                   1.125,
                                   -0.125),
                    color = ifelse(measure == "fraction_missing" &
                                     fraction > 0.7,
                                   "white",
                                   "black")
      ),
    ) +
    facet_wrap(~ year, ncol = 4) +
    scale_y_discrete(limits = rev) +
    scale_x_continuous(labels = scales::percent) +
    scale_color_manual(values = c("white" = "#ffffff",
                                  "black" = "#000000"),
                       guide = "none") +
    scale_fill_manual(labels = c("fraction_missing" = "Missing",
                                 "fraction_present" = "Present"),
                      values = c("fraction_missing" = "#262626",
                                 "fraction_present" = "#E5E5E5")) +
    labs(
      x = "",
      y = "",
      title = title
    ) +
    theme_thesis() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major.y = element_blank()
    )
  
  ggsave(
    filename = filename,
    plot = plot,
    device = agg_png,
    res = 300,
    width = 16 * 1.5,
    height = 3 / 4 * 16 * 1.5,
    units = "cm"
  )
}


missing_values_summary_long %>%
  filter(year <= 1985) %>%
  missing_values_summary_plot(
    filename = "../figures/missing-values-summary-plot-1.png",
    title = "Fraction of Missing Values (1)"
  )

missing_values_summary_long %>%
  filter(year > 1985 & year <= 2002) %>%
  missing_values_summary_plot(
    filename = "../figures/missing-values-summary-plot-2.png",
    title = "Fraction of Missing Values (2)"
  )

missing_values_summary_long %>%
  filter(year > 2002) %>%
  missing_values_summary_plot(
    filename = "../figures/missing-values-summary-plot-3.png",
    title = "Fraction of Missing Values (3)"
  )


# Clean variables ---------------------------------------------------------

# Suppress printed messages
invisible(capture.output(
  # A script that cleans and creates relevant variables
  source("data-transformation.R")))


# Reorder variables in data set -------------------------------------------

gss_raw <- gss_raw %>%
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

glimpse(gss_raw)


# Remove records containing missing values --------------------------------

gss <- gss_raw

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


# Save raw GSS data -------------------------------------------------------

save(gss_raw, file = "../data/gss-raw.RData")
write.csv(gss_raw, file = "../data/gss-raw.csv")
