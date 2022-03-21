# Set working directory to source file location in RStudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Clear environment
rm(list = ls())


# Load libraries ----------------------------------------------------------

library(tidyverse)

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


# Data selection ----------------------------------------------------------

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


# Figure 1: Trust in science by political ideology ------------------------

plot_consci_polviews <- function(
  data,
  # Set default output_name to the variable name of data
  output_name = gsub("_", "-", deparse(substitute(data)))) {
  
  ggplot(
    data = data,
    aes(
      x = year,
      y = consci_moving_mean,
      color = polviews
    )
  ) +
    # Add line plot
    geom_line(size = 1.2) +
    # Change the spacing of the x-axis tick marks
    scale_x_continuous(breaks = seq(from = 1974, to = 2022, by = 4)) +
    # Change the y-axis scale to percent
    scale_y_continuous(label = scales::percent_format(accuracy = 1)) +
    # Change the scale limits of the y-axis
    coord_cartesian(expand = TRUE,
                    clip = "off",
                    ylim = c(0.3, 0.7)) +
    # Set the colors for the different groups
    scale_color_manual(values = c("#E63946", "#1D3557", "#A8DADC")) +
    # Set the titles and axis labels
    labs(
      title = "Public Trust in Science for Each Survey Year by Political Ideology",
      subtitle = "Three-period moving averages for each group to smooth the patterns overtime",
      x = "Year",
      y = "Trust in Science (Unadjusted Means)"
    ) +
    # Make the legend keys wider
    guides(color = guide_legend(keywidth = 3)) +
    # Use my custom thesis theme
    theme_thesis()
  
  # Save figure as PNG file
  ggsave(
    paste0("../figures/", output_name, ".png"),
    device = agg_png,
    res = 300,
    width = 24,
    height = 16,
    units = "cm"
  )
  
  # Close the current plotting device
  dev.off()
  
}

plot_consci_polviews(consci_by_polviews)
plot_consci_polviews(consci_by_polviews_raw)


# Figure 1 (LaTeX): Trust in science by political ideology ----------------

plot_consci_polviews_latex <- function(
  data,
  # Set default output_name to the variable name of data
  output_name = gsub("_", "-", deparse(substitute(data)))) {
  
  tikz(
    paste0("../figures/", output_name, ".tex"),
    width = 8.75,
    height = 5)
  
  ggplot(
    data = data,
    aes(
      x = year,
      y = consci_moving_mean,
      color = polviews,
      linetype = polviews,
      shape = polviews
    )
  ) +
    # Add line plot
    geom_line(size = 0.8) +
    # Add data points
    geom_point(size = 2.4) +
    # Change the spacing of the x-axis tick marks
    scale_x_continuous(breaks = seq(from = 1974, to = 2022, by = 4)) +
    # Change the scale limits of the y-axis
    coord_cartesian(expand = TRUE,
                    clip = "off",
                    ylim = c(0.3, 0.7)) +
    # Set the colors for the different groups using a gray scale
    scale_color_grey(start = 0, end = 0.4) +
    # Set the axis labels
    labs(x = "Year", y = "Trust in Science (Unadjusted Means)") +
    # Make the legend keys wider
    guides(color = guide_legend(keywidth = 3)) +
    # Use my custom thesis LaTeX theme
    theme_thesis_latex()
  
  # Close the current plotting device
  dev.off()
  
}

plot_consci_polviews_latex(consci_by_polviews)
plot_consci_polviews_latex(consci_by_polviews_raw)
