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
load("../data/gss-with-na.RData")


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


## Data selection from gss_with_na ----------------------------------------

consci_by_polviews_with_na <- gss_with_na %>%
  # Select relevant columns
  select(year, consci, conservative, moderate) %>%
  # Derive "polviews" variable from "conservative" and "moderate"
  mutate(polviews = as.factor(
    case_when(
      conservative == 1 ~ "Conservative",
      conservative == 0 & moderate == 0 ~ "Liberal",
      moderate == 1 ~ "Moderate"
    )
  )) %>%
  select(year, consci, polviews) %>%
  # Omit all records containing NA values
  na.omit() %>%
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
var_label(consci_by_polviews_with_na$polviews) <-
  "think of self as liberal or conservative"


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
    coord_cartesian(ylim = c(0.2, 0.8)) +
    # Set the colors for the different groups
    scale_color_manual(
      values = c(
        theme_thesis_colors$ideology$red,
        theme_thesis_colors$ideology$blue,
        theme_thesis_colors$ideology$gray
      )
    ) +
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

consci_by_polviews_with_na %>%
  consci_polviews()

ggsave(
  "../figures/consci-by-polviews-with-na.png",
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
    geom_point(mapping = aes(shape = polviews), size = 2) +
    # Change the spacing of the x-axis tick marks
    scale_x_continuous(breaks = seq(from = 1974, to = 2022, by = 4)) +
    # Cahgne the y-axis scale to percent
    scale_y_continuous(label = scales::percent_format(accuracy = 1)) +
    # Change the scale limits of the y-axis
    coord_cartesian(ylim = c(0.2, 0.8)) +
    # Set the colors for the different groups
    scale_color_manual(
      values = c(
        theme_thesis_colors$ideology$red,
        theme_thesis_colors$ideology$blue,
        theme_thesis_colors$ideology$gray
      )
    ) +
    # Set the axis labels
    labs(x = "Year",
         y = "Trust in Science (Unadjusted Means)") +
    # Make the legend keys wider
    guides(color = guide_legend(keywidth = 3)) +
    theme_thesis_latex()
}

tikz(
  "../reports/figures/figure-consci-by-polviews.tex",
  width = 0.95 * 16 / 2.54,
  height = 0.95 * 3 / 4 * 16 / 2.54,
  sanitize = TRUE,
  timestamp = FALSE
)

consci_by_polviews %>%
  consci_polviews_latex()

# Close the tikz graphics device
dev.off()

tikz(
  "../reports/figures/figure-consci-by-polviews-with-na.tex",
  width = 0.95 * 16 / 2.54,
  height = 0.95 * 3 / 4 * 16 / 2.54,
  sanitize = TRUE,
  timestamp = FALSE
)

consci_by_polviews_with_na %>%
  consci_polviews_latex()

# Close the tikz graphics device
dev.off()
