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


# Source scripts ----------------------------------------------------------

# A script that creates two custom ggplot2 themes
source("ggplot2-themes.R")


# Load data ---------------------------------------------------------------

load("../data/gss.RData")
load("../data/gss-with-na.RData")
load("../data/gss-raw.RData")

glimpse(gss_with_na)


# Missing values of every variable by year --------------------------------

na_variable_year <- gss_raw %>%
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

head(na_variable_year)
glimpse(na_variable_year)


na_variable_year_long <- na_variable_year %>%
  select(-n) %>%
  rename_all( ~ stringr::str_replace(., "_fraction_na", "")) %>%
  pivot_longer(!year, names_to = "variable", values_to = "fraction_missing") %>%
  mutate(fraction_present = 1 - fraction_missing) %>%
  pivot_longer(
    !c(year, variable),
    names_to = "measure",
    values_to = "value"
  )

head(na_variable_year_long)


plot_na_variable_year <- function(df, filename, title) {
  plot <- df %>%
    ggplot(mapping = aes(x = value, y = variable)) +
    geom_col(
      mapping = aes(fill = measure),
      position = position_stack(reverse = TRUE)
    ) +
    geom_text(
      mapping = aes(label = ifelse(measure == "fraction_missing",
                                   scales::percent(value, accuracy = 0.1),
                                   ""),
                    hjust = ifelse(measure == "fraction_missing" &
                                     value > 0.7,
                                   1.125,
                                   -0.125),
                    color = ifelse(measure == "fraction_missing" &
                                     value > 0.7,
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
    scale_fill_manual(
      labels = c(
        "fraction_missing" = "Missing",
        "fraction_present" = "Present"
      ),
      values = c(
        "fraction_missing" = theme_thesis_colors$basic$primary,
        "fraction_present" = theme_thesis_colors$basic$tertiary
      )
    ) +
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


na_variable_year_long %>%
  filter(year <= 1985) %>%
  plot_na_variable_year(
    filename = "../figures/missing-values-variable-year-1.png",
    title = "Fraction of Missing Values (1)"
  )

na_variable_year_long %>%
  filter(year > 1985 & year <= 2002) %>%
  plot_na_variable_year(
    filename = "../figures/missing-values-variable-year-2.png",
    title = "Fraction of Missing Values (2)"
  )

na_variable_year_long %>%
  filter(year > 2002) %>%
  plot_na_variable_year(
    filename = "../figures/missing-values-variable-year-3.png",
    title = "Fraction of Missing Values (3)"
  )


# Missing values of every variable ----------------------------------------

# Count number of missing values for each attribute as a list
na_variable <- as.list(colSums(is.na(gss_with_na))) %>%
  # Convert the list to a dataframe
  stack(.) %>%
  # Set the column names
  setNames(., c("na_count", "variable")) %>%
  # Reorder columns
  select(variable, na_count) %>%
  # Add fraction column
  mutate(na_fraction = round(na_count / nrow(gss_with_na),
                                         digits = 3))

n_obs_2021 <- nrow(gss_with_na %>% filter(year == 2021))


## Missing values count ---------------------------------------------------

plot_na_count <- na_variable %>%
  filter(na_count > 0) %>%
  ggplot(mapping = aes(
    x = reorder(variable, desc(na_count)),
    y = na_count)) +
  geom_col(fill = theme_thesis_colors$basic$primary) +
  geom_text(mapping = aes(label = formatC(
    na_count, format = "d", big.mark = ",")), hjust = -0.125) +
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
      formatC(nrow(gss_with_na), format = "d", big.mark = ","),
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

plot_na_count

ggsave(
  filename = "../figures/missing-values-count.png",
  plot = plot_na_count,
  device = agg_png,
  res = 300,
  width = 16,
  height = 3 / 4 * 16,
  units = "cm"
)

# Close the AGG PNG graphic device
dev.off()


## Missing values proportion ----------------------------------------------

plot_na_fraction <- na_variable %>%
  filter(na_count > 0) %>%
  # Change order of columns (not necessary for plotting)
  arrange(desc(na_fraction)) %>%
  # Change order of factor levels
  mutate(variable = fct_reorder(variable,
                                na_fraction,
                                .desc = TRUE)) %>%
  mutate(not_na_fraction = 1 - na_fraction) %>%
  gather(
    key = measure,
    value = value,
    na_fraction:not_na_fraction,
    factor_key = TRUE
  ) %>%
  ggplot(mapping = aes(x = value, y = variable)) +
  geom_col(
    mapping = aes(fill = measure),
    # Reverse the fill order so that missing values go first
    position = position_stack(reverse = TRUE)
  ) +
  geom_text(mapping = aes(label = ifelse(measure == "na_fraction",
                                         scales::percent(value, accuracy = 0.1),
                                         "")),
            hjust = -0.125) +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(labels = scales::percent) +
  scale_fill_manual(
    labels = c(
      "na_fraction" = "Missing",
      "not_na_fraction" = "Present"),
    values = c(
      "na_fraction" = theme_thesis_colors$basic$primary,
      "not_na_fraction" = theme_thesis_colors$basic$tertiary
    )
  ) +
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

plot_na_fraction

ggsave(
  filename = "../figures/missing-values-fraction.png",
  plot = plot_na_fraction,
  device = agg_png,
  res = 300,
  width = 16,
  height = 3 / 4 * 16,
  units = "cm"
)

# Close the AGG PNG graphic device
dev.off()


## Distributions of entries with missing income values --------------------

head(gss_with_na)

na_distribution <- gss_with_na %>%
  filter(is.na(realinc)) %>%
  select(-year,
         -id,
         -educ,
         -attend,
         -realinc,
         -age,
         -postreagan,
         -bush,
         -socialmedia,
         -posttrump,
         -covid19) %>%
  gather(
    key = variable,
    value = value,
    factor_key = TRUE
  )

not_na_distribution <- gss %>%
  select(-year,
         -id,
         -educ,
         -attend,
         -realinc,
         -age,
         -postreagan,
         -bush,
         -socialmedia,
         -posttrump,
         -covid19) %>%
  gather(
    key = variable,
    value = value,
    factor_key = TRUE
  )

plot_na_distribution <- ggplot() +
  geom_bar(
    data = not_na_distribution,
    mapping = aes(x = value,
                  y = (..count..) / sum(..count..),
                  color = "Data Without Missing Values",
                  linetype = "Data Without Missing Values",
                  fill = "Data Without Missing Values"),
    width = 1) +
  geom_bar(
    data = na_distribution,
    mapping = aes(x = value,
                  y = (..count..) / sum(..count..),
                  color = "Data With Missing Income Values",
                  linetype = "Data With Missing Income Values",
                  fill = "Data With Missing Income Values"),
    width = 1) +
  facet_wrap(~ variable, scales = "free", nrow = 2) +
  # facet_wrap(~ variable, scales = "free", nrow = 4) +
  scale_x_continuous(breaks = c(0, 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(
    values = c(
      "Data Without Missing Values" = theme_thesis_colors$basic$secondary,
      "Data With Missing Income Values" = theme_thesis_colors$basic$primary
    )
  ) +
  scale_linetype_manual(
    values = c("Data Without Missing Values" = "dashed",
               "Data With Missing Income Values" = "solid")
  ) +
  scale_fill_manual(
    values = alpha(
      c(
        "Data Without Missing Values" = theme_thesis_colors$basic$tertiary,
        "Data With Missing Income Values" = theme_thesis_colors$basic$secondary
      ),
      0.2)
  ) +
  labs(
    x = "Value",
    y = "Proportion in Sample",
    title = "Data Without Missing Values vs. Data with Missing Income Values",
    subtitle = "Plot only shows binary dummy variables",
    # Merge legends by giving them the same name
    color = "Legend",
    linetype = "Legend",
    fill = "Legend"
  ) +
  theme_thesis() +
  theme(
    # Remove horizontal grid lines
    panel.grid.major.y = element_blank()
  )

plot_na_distribution

ggsave(
  filename = "../figures/missing-values-distributions-wide.png",
  plot = plot_na_distribution,
  device = agg_png,
  res = 300,
  width = 16 * 4 / 3,
  # width = 16,
  height = 3 / 4 * 16,
  # height = 5 / 4 * 16,
  units = "cm"
)

# Close the AGG PNG graphic device
dev.off()
