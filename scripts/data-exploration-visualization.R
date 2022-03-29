# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Clear environment
rm(list = ls())


# Load libraries ----------------------------------------------------------

# Manipulate metadata as variable and value labels
library(labelled)

library(tidyverse)
options(dplyr.summarise.inform = FALSE)

# A graphic device which supports custom fonts
library(ragg)

# R Graphics Output in LaTeX Format
library(tikzDevice)


# Source scripts ----------------------------------------------------------

# A script that creates two custom ggplot2 themes
source("ggplot2-themes.R")


# Load data ---------------------------------------------------------------

load("../data/gss.RData")
load("../data/gss-raw.RData")

glimpse(gss_raw)


# Missing values ----------------------------------------------------------

# Count number of missing values for each attribute as a list
missing_values <- as.list(colSums(is.na(gss_raw))) %>%
  # Convert the list to a dataframe
  stack(.) %>%
  # Set the column names
  setNames(., c("n_missing_values", "variable")) %>%
  # Reorder columns
  select(variable, n_missing_values) %>%
  # Add fraction column
  mutate(pct_missing_values = round(n_missing_values / nrow(gss_raw),
                                         digits = 3))

n_obs_2021 <- nrow(gss_raw %>% filter(year == 2021))


## Missing values count ---------------------------------------------------

plot_missing_values_n <- missing_values %>%
  filter(n_missing_values > 0) %>%
  ggplot(mapping = aes(
    x = reorder(variable, desc(n_missing_values)),
    y = n_missing_values)) +
  geom_col(fill = "#262626") +
  geom_text(mapping = aes(label = formatC(
    n_missing_values, format = "d", big.mark = ",")), hjust = -0.125) +
  # Reverse the order of the y-column
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels = scales::comma) +
  expand_limits(y = 28000) +
  coord_flip() +
  labs(
    x = "",
    y = "",
    title = "Number of Missing Values",
    subtitle = paste(
      "Plot only shows variables with missing values\n(Total number of observations:",
      formatC(nrow(gss_raw), format = "d", big.mark = ","),
      ")"
    ),
    caption = paste(
      "GSS 2021 (",
      formatC(n_obs_2021, format = "d", big.mark = ","),
      ' observations) did not include "race" and "region" variables.',
      sep = ""
    )
  ) +
  theme_thesis() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    # Remove horizontal grid lines
    panel.grid.major.y = element_blank()
  )

plot_missing_values_n

ggsave(
  filename = "../figures/missing-values-n.png",
  plot = plot_missing_values_n,
  device = agg_png,
  res = 300,
  width = 16,
  height = 3 / 4 * 16,
  units = "cm"
)

# Close the AGG PNG graphic device
dev.off()


## Missing values proportion ----------------------------------------------

plot_missing_values_pct <- missing_values %>%
  filter(n_missing_values > 0) %>%
  # Change order of columns (not necessary for plotting)
  arrange(desc(pct_missing_values)) %>%
  # Change order of factor levels
  mutate(variable = fct_reorder(variable,
                                pct_missing_values,
                                .desc = TRUE)) %>%
  mutate(pct_present_values = 1 - pct_missing_values) %>%
  gather(
    key = measure,
    value = value,
    pct_missing_values:pct_present_values,
    factor_key = TRUE
  ) %>%
  ggplot(mapping = aes(x = value, y = variable)) +
  geom_col(
    mapping = aes(fill = measure),
    # Reverse the fill order so that missing values go first
    position = position_stack(reverse = TRUE)
  ) +
  geom_text(mapping = aes(label = ifelse(measure == "pct_missing_values",
                                         scales::percent(value, accuracy = 0.1),
                                         "")),
            hjust = -0.125) +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(labels = scales::percent) +
  scale_fill_manual(labels = c("pct_missing_values" = "Missing",
                               "pct_present_values" = "Present"),
                    values = c("pct_missing_values" = "#262626",
                               "pct_present_values" = "#D4D4D4")) +
  labs(
    x = "",
    y = "",
    title = "Fraction of Missing Values",
    subtitle = "Plot only shows variables with missing values",
    caption = paste(
      "GSS 2021 (",
      formatC(n_obs_2021, format = "d", big.mark = ","),
      ' observations) did not include "race" and "region" variables.',
      sep = ""
    )
  ) +
  theme_thesis() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    # Remove horizontal grid lines
    panel.grid.major.y = element_blank()
  )

plot_missing_values_pct

ggsave(
  filename = "../figures/missing-values-pct.png",
  plot = plot_missing_values_pct,
  device = agg_png,
  res = 300,
  width = 16,
  height = 3 / 4 * 16,
  units = "cm"
)

# Close the AGG PNG graphic device
dev.off()


# Trust in science by political ideology ----------------------------------

# I want to visualize the mean "trust in science" for each of the three
# political ideology groups over time.


## Data selection from gss ------------------------------------------------

consci_by_polviews <- gss %>%
  # Select relevant columns
  select(year, consci, moderate, conservative) %>%
  # Create "polviews" variable
  mutate(polviews = as.factor(
    case_when(
      conservative == 1 ~ "Conservative",
      conservative == 0 & moderate == 0 ~ "Liberal",
      moderate == 1 ~ "Moderate"))) %>%
  # Select relevant columns
  select(year, consci, polviews) %>%
  # Group data by year and political ideology
  group_by(year, polviews) %>%
  # Summarize grouped data using the mean function
  summarize(consci_mean = mean(consci)) %>%
  # Group data by political ideology
  group_by(polviews) %>%
  # Calculate the three-period moving average for the group-specific means of
  # confidence in science using the "zoo" package
  mutate(consci_moving_mean = zoo::rollmean(consci_mean, k = 3, fill = NA))

# Add variable labels
var_label(consci_by_polviews$polviews) <-
  "think of self as liberal or conservative"


## Data selection from gss_raw --------------------------------------------

# Change factor levels of consci
gss_raw$consci <-
  recode_factor(
    gss_raw$consci,
    `a great deal` = 1,
    `only some` = 0,
    `hardly any` = 0
  )

# Convert factor level to numeric
gss_raw <- gss_raw %>%
  # Convert factor level to numeric
  mutate(consci = as.numeric(as.character(consci)))

gss_raw$polviews <-
  # Change factor levels
  recode_factor(
    gss_raw$polviews,
    `slightly conservative` = "Conservative",
    `conservative` = "Conservative",
    `extremely conservative` = "Conservative",
    `extremely liberal` = "Liberal",
    `liberal` = "Liberal",
    `slightly liberal` = "Liberal",
    `moderate, middle of the road` = "Moderate"
  )

consci_by_polviews_raw <- gss_raw %>%
  # Select relevant columns
  select(year, consci, polviews) %>%
  # Drop all records containing null values
  drop_na() %>%
  # Group data by year and political ideology
  group_by(year, polviews) %>%
  # Summarize grouped data using the mean function
  summarize(consci_mean = mean(consci)) %>%
  # Group data by political ideology
  group_by(polviews) %>%
  # Calculate the three-period moving average for the group-specific means of
  # confidence in science using the "zoo" package
  mutate(consci_moving_mean = zoo::rollmean(consci_mean, k = 3, fill = NA))


### Figure 1: Trust in science by political ideology ----------------------

consci_polviews <- function(data) {
  ggplot(
    data = data,
    mapping = aes(
      x = year,
      y  = consci_moving_mean,
      group = polviews,
      color = polviews
    )
  ) +
    # Add line plot
    geom_line(size = 1.1) +
    # Change the spacing of the x-axis tick marks
    scale_x_continuous(breaks = seq(from = 1974, to = 2022, by = 4)) +
    # Cahgne the y-axis scale to percent
    scale_y_continuous(label = scales::percent_format(accuracy = 1)) +
    # Change the scale limits of the y-axis
    coord_cartesian(ylim = c(0.3, 0.7)) +
    # Set the colors for the different groups
    scale_color_manual(values = c("#E63946", "#1D3557", "#A8DADC")) +
    # Set the axis labels
    labs(
      x = "Year",
      y = "Trust in Science (Unadjusted Means)",
      title = "Public Trust in Science for Each Survey Year by Political Ideology",
      subtitle = "Three-period moving averages for each group to smooth the patterns overtime"
    ) +
    # Make the legend keys wider
    guides(color = guide_legend(keywidth = 3)) +
    theme_thesis()
}

consci_by_polviews %>%
  consci_polviews()

ggsave(
  "../figures/consci-by-polviews.png",
  device = agg_png,
  res = 300,
  width = 16,
  height = 3 / 4 * 16,
  units = "cm"
)

# Close the AGG PNG graphic device
dev.off()

consci_by_polviews_raw %>%
  consci_polviews()

ggsave(
  "../figures/consci-by-polviews-raw.png",
  device = agg_png,
  res = 300,
  width = 16,
  height = 3 / 4 * 16,
  units = "cm"
)

# Close the AGG PNG graphic device
dev.off()


### Figure 1 (LaTeX): Trust in science by political ideology --------------

consci_polviews_latex <- function(data) {
  ggplot(
    data = data,
    mapping = aes(
      x = year,
      y  = consci_moving_mean,
      group = polviews,
      color = polviews
    )
  ) +
    # Add line plot
    geom_line(mapping = aes(linetype = polviews)) +
    # Add data points
    geom_point(mapping = aes(shape = polviews)) +
    # Change the spacing of the x-axis tick marks
    scale_x_continuous(breaks = seq(from = 1974, to = 2022, by = 4)) +
    # Cahgne the y-axis scale to percent
    scale_y_continuous(label = scales::percent_format(accuracy = 1)) +
    # Change the scale limits of the y-axis
    coord_cartesian(ylim = c(0.3, 0.7)) +
    # Set the colors for the different groups using a gray scale
    scale_color_grey(start = 0, end = 0.4) +
    # Set the axis labels
    labs(x = "Year",
         y = "Trust in Science (Unadjusted Means)") +
    # Make the legend keys wider
    guides(color = guide_legend(keywidth = 3)) +
    theme_thesis_latex()
}

tikz(
  "../reports/figures/consci-by-polviews.tex",
  width = 0.95 * 16 / 2.54,
  height = 0.95 * 3 / 4 * 16 / 2.54,
  sanitize = TRUE)

consci_by_polviews %>%
  consci_polviews_latex()

# Close the tikz graphics device
dev.off()

tikz(
  "../reports/figures/consci-by-polviews-raw.tex",
  width = 0.95 * 16 / 2.54,
  height = 0.95 * 3 / 4 * 16 / 2.54,
  sanitize = TRUE)

consci_by_polviews_raw %>%
  consci_polviews_latex()

# Close the tikz graphics device
dev.off()
