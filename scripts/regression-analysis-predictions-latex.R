# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Clear environment
rm(list = ls())


# Load libraries ----------------------------------------------------------

library(tidyverse)

# R Graphics Output in LaTeX Format
library(tikzDevice)


# Load data ---------------------------------------------------------------

load("../data/gss.RData")
load("../data/models.RData")


gss_pre_2021 <- gss %>%
  filter(year < 2021)

gss_incl_2021 <- gss %>%
  select(-nonwhite, -south)


# Source scripts ----------------------------------------------------------

# A script that creates two custom ggplot2 themes
source("ggplot2-themes.R")


# Predicted probabilities -------------------------------------------------

# See https://druedin.com/2016/01/16/predicted-probabilities-in-r/

predict_df <- function(dataframe, regression, probability) {
  # 'probability' used for confidence interval, e.g. 95% confidence interval
  
  predicted <- predict(
    object = regression,
    newdata = dataframe,
    type = "response",
    se.fit = TRUE
  )
  
  new_dataframe <- dataframe %>%
    mutate(
      fit = predicted$fit,
      lower_bound = predicted$fit - qnorm(probability) * predicted$se.fit,
      upper_bound = predicted$fit + qnorm(probability) * predicted$se.fit
    )
  
  return(new_dataframe)
}


survey_years <- c(
  1974,
  1975,
  1976,
  1977,
  1978,
  1980,
  1982,
  1983,
  1984,
  1986,
  1987,
  1988,
  1989,
  1990,
  1991,
  1993,
  1994,
  1996,
  1998,
  2000,
  2002,
  2004,
  2006,
  2008,
  2010,
  2012,
  2014,
  2016,
  2018
)


# Predicted probability by political ideology over time -------------------

prediction_polview_year_plot <- function(data) {
  # Plotting figure (LaTeX)
  ggplot(
    data = data,
    mapping = aes(x = year)
  ) +
    geom_line(
      mapping = aes(
        y = fit, color = polview, linetype = polview
      )
    ) +
    geom_ribbon(
      mapping = aes(
        ymin = lower_bound,
        ymax = upper_bound,
        fill = polview
      ),
      alpha = 0.25
    ) +
    # Add data points
    geom_point(
      mapping = aes(y = fit, shape = polview, color = polview),
      size = 2
    ) +
    # Change the spacing of the x-axis tick marks
    scale_x_continuous(breaks = seq(from = 1974, to = 2022, by = 4)) +
    # Change the spacing of the y-axis tick marks and the axis' scale to percent
    scale_y_continuous(
      breaks = seq(from = 0, to = 1, by = 0.1),
      label = scales::percent_format(accuracy = 1)
    ) +
    # Change the scale limits of the y-axis
    coord_cartesian(ylim = c(0.2, 0.8)) +
    # Set the colors for the different groups
    scale_color_manual(
      values = c(
        "conservative" = theme_thesis_colors$ideology$red,
        "liberal" = theme_thesis_colors$ideology$blue,
        "moderate" = theme_thesis_colors$ideology$gray
      ),
      labels = c(
        "conservative" = "Conservative",
        "liberal" = "Liberal",
        "moderate" = "Moderate"
      )
    ) +
    scale_fill_manual(
      values = c(
        "conservative" = theme_thesis_colors$ideology$red,
        "liberal" = theme_thesis_colors$ideology$blue,
        "moderate" = theme_thesis_colors$ideology$gray
      ),
      labels = c(
        "conservative" = "Conservative",
        "liberal" = "Liberal",
        "moderate" = "Moderate"
      )
    ) +
    scale_linetype_discrete(
      labels = c(
        "conservative" = "Conservative",
        "liberal" = "Liberal",
        "moderate" = "Moderate"
      )
    ) +
    scale_shape_discrete(
      labels = c(
        "conservative" = "Conservative",
        "liberal" = "Liberal",
        "moderate" = "Moderate"
      )
    ) +
    labs(
      x = "Year",
      y = "Predicted Probability"
    ) +
    # Remove the legend for the fill aesthetic and increase the legend key width
    guides(fill = "none", color = guide_legend(keywidth = 3)) +
    theme_thesis_latex()
}


## Pre 2021 ---------------------------------------------------------------

mean_polview_year_pre_2021 <- gss_pre_2021 %>%
  group_by(moderate, conservative, year) %>%
  summarise(across(c(female:posttrump), ~ mean(.x))) %>%
  ungroup() %>%
  mutate(
    polview = as.factor(
      case_when(
        moderate == 0 & conservative == 0 ~ "liberal",
        moderate == 0 & conservative == 1 ~ "conservative",
        moderate == 1 & conservative == 0 ~ "moderate"
      )
    )
  )


for (i in c("1", "2", "2_variation", "3")) {
  model <- get(paste0("model_", i))
  
  predict_polview_year_pre_2021 <- predict_df(
    dataframe = mean_polview_year_pre_2021,
    regression = model,
    probability = 0.95
  ) %>%
    select(year, polview, fit, lower_bound, upper_bound) %>%
    mutate(
      # Change order of factor level to change the line plotting order
      polview = factor(
        polview,
        levels = c("conservative", "liberal", "moderate")
      )
    )
  
  tikz(
    paste0(
      "../reports/figures/figure-predict-polview-year-model-",
      gsub(
        pattern = "_",
        replacement = "-",
        x = i
      ),
      ".tex"
    ),
    width = 0.95 * 16 / 2.54,
    height = 0.95 * 3 / 4 * 16 / 2.54,
    sanitize = TRUE,
    timestamp = FALSE
  )
  
  prediction_polview_year_plot(
    data = predict_polview_year_pre_2021
  )
  
  plot(last_plot())
  
  dev.off()
  
  rm(model, predict_polview_year_pre_2021)
}


## Including 2021 ---------------------------------------------------------

mean_polview_year_incl_2021 <- gss_incl_2021 %>%
  group_by(moderate, conservative, year) %>%
  summarise(across(c(female:covid19), ~ mean(.x))) %>%
  ungroup() %>%
  mutate(
    polview = as.factor(
      case_when(
        moderate == 0 & conservative == 0 ~ "liberal",
        moderate == 0 & conservative == 1 ~ "conservative",
        moderate == 1 & conservative == 0 ~ "moderate"
      )
    )
  )


for (i in c("4", "5", "5_variation", "6")) {
  model <- get(paste0("model_", i))
  
  predict_polview_year_incl_2021 <- predict_df(
    dataframe = mean_polview_year_incl_2021,
    regression = model,
    probability = 0.95
  ) %>%
    select(year, polview, fit, lower_bound, upper_bound) %>%
    mutate(
      # Change order of factor level to change the line plotting order
      polview = factor(
        polview,
        levels = c("conservative", "liberal", "moderate")
      )
    )
  
  tikz(
    paste0(
      "../reports/figures/figure-predict-polview-year-model-",
      gsub(
        pattern = "_",
        replacement = "-",
        x = i
      ),
      ".tex"
    ),
    width = 0.95 * 16 / 2.54,
    height = 0.95 * 3 / 4 * 16 / 2.54,
    sanitize = TRUE,
    timestamp = FALSE
  )
  
  prediction_polview_year_plot(
    data = predict_polview_year_incl_2021
  )
  
  plot(last_plot())
  
  dev.off()
  
  rm(model, predict_polview_year_incl_2021)
}
