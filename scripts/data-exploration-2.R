# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Clear environment
rm(list = ls())


# Load libraries ----------------------------------------------------------

library(tidyverse)

# A graphic device which supports custom fonts
library(ragg)


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

plot_missing_values_n <- missing_values %>%
  filter(n_missing_values > 0) %>%
  ggplot(mapping = aes(
    x = reorder(variable, desc(n_missing_values)),
    y = n_missing_values)) +
  geom_col() +
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

plot_missing_values_pct <- missing_values %>%
  filter(n_missing_values > 0) %>%
  ggplot(mapping = aes(
    x = pct_missing_values,
    y = reorder(variable, desc(pct_missing_values)))) +
  geom_col() +
  geom_text(mapping = aes(label = scales::percent(pct_missing_values)),
            hjust = -0.125) +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(labels = scales::percent) +
  expand_limits(x = 0.43) +
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


# Summary statistics ------------------------------------------------------


