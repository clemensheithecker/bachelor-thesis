# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Clear environment
rm(list = ls())


# Load libraries ----------------------------------------------------------

# A collection of functions for regression analysis, e.g. 'export_summs()'
library(jtools)

# Linear mixed effects models
# library(lme4)

# Provides marginal effects summaries of models via 'margins()'
library(margins)

# A graphic device which supports custom fonts
library(ragg)

# Create LaTeX code for well-formatted tables
library(stargazer)

library(tidyverse)

# R Graphics Output in LaTeX Format
library(tikzDevice)

# Export data frame as LaTeX table
library(xtable)


# Source scripts ----------------------------------------------------------

# A script that includes custom stargazer table exporting functions
source("custom-tables.R")

# A script that creates two custom ggplot2 themes
source("ggplot2-themes.R")


# Load data ---------------------------------------------------------------

load("../data/gss.RData")

glimpse(gss)


gss_pre_2021 <- gss %>%
  filter(year < 2021)

gss_incl_2021 <- gss %>%
  select(-nonwhite, -south)

sum(is.na(gss_pre_2021))
sum(is.na(gss_incl_2021))


# Logistic regression models (pre 2021) -----------------------------------

model_1 <- glm(
  consci ~ year + female + nonwhite + educ + highschool + bachelor + graduate +
    south + attend + realinc + age + I(age ^ 2) + independent + republican +
    moderate + conservative + postreagan + bush + posttrump,
  family = binomial(link = "logit"),
  data = gss_pre_2021
)

summary(model_1)


model_2 <- glm(
  consci ~ year + female + nonwhite + educ + highschool + bachelor + graduate +
    south + attend + realinc + age + I(age ^ 2) + independent + republican +
    moderate + conservative + postreagan + bush + posttrump + moderate * year +
    conservative * year,
  family = binomial(link = "logit"),
  data = gss_pre_2021
)

summary(model_2)


model_3 <- glm(
  consci ~ year + female + nonwhite + educ + highschool + bachelor + graduate +
    south + attend + realinc + age + I(age ^ 2) + independent + republican +
    moderate + conservative + postreagan + bush + posttrump +
    postreagan * moderate + bush * moderate + posttrump * moderate +
    postreagan * conservative + bush * conservative + posttrump * conservative,
  family = binomial(link = "logit"),
  data = gss_pre_2021
)

summary(model_3)


regression_table_thesis(
  model_1,
  model_2,
  model_3,
  file = "../reports/figures/logit-pre-2021.tex",
  title = "Logit Models Predicting Public Confidence in Science",
  column.labels = c("Model 1", "Model 2", "Model 3"),
  covariate.labels = c(
    "Constant",
    "Year",
    "Female",
    "Non-White",
    "Education (years)",
    "High School",
    "Bachelor",
    "Graduate",
    "South",
    "Church Attendance",
    "Family Income",
    "Age",
    "Age\\textsuperscript{2}",
    "Independent",
    "Republican",
    "Moderate",
    "Conservative",
    "Post-Reagan (1981--2021)",
    "Bush (2001--2008)",
    "Post-Trump (2017--2021)",
    "Year $\\times$ Moderate",
    "Year $\\times$ Conservative",
    "Moderate $\\times$ Post-Reagan",
    "Moderate $\\times$ Bush",
    "Moderate $\\times$ Post-Trump",
    "Conservative $\\times$ Post-Reagan",
    "Conservative $\\times$ Bush",
    "Conservative $\\times$ Post-Trump"
  ),
  add.lines = list(c("Including Year 2021", "No", "No", "No")),
  label = "table:LogitPre2021",
  star.cutoffs = c(0.05, 0.01, 0.001),
  footnote = "{\\it Note:} Numbers in parentheses represent standard errors. The reference category for political party affiliation is Democrat. The reference category for political ideology is liberal. Age\\textsuperscript{2} is a squared term. \\newline $^{*}p<0.1$; $^{**}p<0.05$; $^{***}p<0.01$"
)


# Logistic regression models (including 2021) -----------------------------

model_4 <- glm(
  consci ~ year + female + educ + highschool + bachelor + graduate + attend +
    realinc + age + I(age ^ 2) + independent + republican + moderate +
    conservative + postreagan + bush + posttrump + covid19,
  family = binomial(link = "logit"),
  data = gss_incl_2021
)

summary(model_4)

model_5 <- glm(
  consci ~ year + female + educ + highschool + bachelor + graduate + attend +
    realinc + age + I(age ^ 2) + independent + republican + moderate +
    conservative + postreagan + bush + posttrump + covid19 + moderate * year +
    conservative * year,
  family = binomial(link = "logit"),
  data = gss_incl_2021
)

summary(model_5)

model_6 <- glm(
  consci ~ year + female + educ + highschool + bachelor + graduate + attend +
    realinc + age + I(age ^ 2) + independent + republican + moderate +
    conservative + postreagan + bush + posttrump + covid19 +
    postreagan * moderate + bush * moderate + posttrump * moderate +
    covid19 * moderate + postreagan * conservative + bush * conservative +
    posttrump * conservative + covid19 * conservative,
  family = binomial(link = "logit"),
  data = gss_incl_2021
)

summary(model_6)


regression_table_thesis(
  model_4,
  model_5,
  model_6,
  file = "../reports/figures/logit-incl-2021.tex",
  title = "Logit Models Predicting Public Confidence in Science",
  column.labels = c("Model 1", "Model 2", "Model 3"),
  covariate.labels = c(
    "Constant",
    "Year",
    "Female",
    "Education (years)",
    "High School",
    "Bachelor",
    "Graduate",
    "Church Attendance",
    "Family Income",
    "Age",
    "Age\\textsuperscript{2}",
    "Independent",
    "Republican",
    "Moderate",
    "Conservative",
    "Post-Reagan (1981--2021)",
    "Bush (2001--2008)",
    "Post-Trump (2017--2021)",
    "COVID-19 (2020--2021)",
    "Year $\\times$ Moderate",
    "Year $\\times$ Conservative",
    "Moderate $\\times$ Post-Reagan",
    "Moderate $\\times$ Bush",
    "Moderate $\\times$ Post-Trump",
    "Moderate $\\times$ COVID-19",
    "Conservative $\\times$ Post-Reagan",
    "Conservative $\\times$ Bush",
    "Conservative $\\times$ Post-Trump",
    "Conservative $\\times$ COVID-19"
  ),
  add.lines = list(c("Including Year 2021", "Yes", "Yes", "Yes")),
  label = "table:LogitPre2021",
  star.cutoffs = c(0.05, 0.01, 0.001),
  footnote = "{\\it Note:} Numbers in parentheses represent standard errors. The reference category for political party affiliation is Democrat. The reference category for political ideology is liberal. Age\\textsuperscript{2} is a squared term. \\newline $^{*}p<0.1$; $^{**}p<0.05$; $^{***}p<0.01$"
)


# Marginal effects --------------------------------------------------------

margins_model_1 <- margins(model_1)
margins_model_2 <- margins(model_2)
margins_model_3 <- margins(model_3)

export_summs(
  margins_model_1,
  margins_model_2,
  margins_model_3,
  coefs = c(
    "Year" = "year",
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
    "Post-Trump (2017--2021)" = "posttrump"
  ),
  to.file = "html",
  file.name = "../figures/logit-pre-2021-margins.html",
  number_format = "%.3f",
  statistics = c(
    AIC = "AIC",
    BIC = "BIC"
  )
)


margins_model_4 <- margins(model_4)
margins_model_5 <- margins(model_5)
margins_model_6 <- margins(model_6)

export_summs(
  margins_model_4,
  margins_model_5,
  margins_model_6,
  coefs = c(
    "Year" = "year",
    "Female" = "female",
    "Education (years)" = "educ",
    "High School" = "highschool",
    "Bachelor" = "bachelor",
    "Graduate" = "graduate",
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
    "COVID-19 (2020--2021)" = "covid19"
  ),
  to.file = "html",
  file.name = "../figures/logit-incl-2021-margins.html",
  number_format = "%.3f",
  statistics = c(
    AIC = "AIC",
    BIC = "BIC"
  )
)


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


## Predicted probability by political ideology ----------------------------

### Pre 2021 --------------------------------------------------------------

mean_polview_pre_2021 <- gss_pre_2021 %>%
  group_by(moderate, conservative) %>%
  summarise(across(c(year, female:posttrump), ~ mean(.x))) %>%
  ungroup() %>%
  mutate(
    polview = case_when(
      moderate == 0 & conservative == 0 ~ "liberal",
      moderate == 0 & conservative == 1 ~ "conservative",
      moderate == 1 & conservative == 0 ~ "moderate"
    )
  )


predict_polview_pre_2021_model_1 <- predict_df(
  dataframe = mean_polview_pre_2021,
  regression = model_1,
  probability = 0.95
) %>%
  select(polview, fit, lower_bound, upper_bound)

predict_polview_pre_2021_model_1


predict_polview_pre_2021_list <- list()

for (model in c("model_1", "model_2", "model_3")) {
  predict_polview_pre_2021_list[[model]] <-
    predict_df(
      dataframe = mean_polview_pre_2021,
      regression = get(model),
      probability = 0.95
    ) %>%
    select(polview, fit, lower_bound, upper_bound) %>%
    rename_with(
      ~ paste0(., "_", model),
      c(fit, lower_bound, upper_bound)
    )
}

predict_polview_pre_2021 <- Reduce(
  full_join,
  predict_polview_pre_2021_list
) %>%
  select(polview, starts_with("fit"))

predict_polview_pre_2021


rm(predict_polview_pre_2021_list)


### Including 2021 --------------------------------------------------------

mean_polview_incl_2021 <- gss_incl_2021 %>%
  group_by(moderate, conservative) %>%
  summarise(across(c(year, female:covid19), ~ mean(.x))) %>%
  ungroup() %>%
  mutate(
    polview = case_when(
      moderate == 0 & conservative == 0 ~ "liberal",
      moderate == 0 & conservative == 1 ~ "conservative",
      moderate == 1 & conservative == 0 ~ "moderate"
    )
  )


predict_polview_incl_2021_model_4 <- predict_df(
  dataframe = mean_polview_incl_2021,
  regression = model_4,
  probability = 0.95
) %>%
  select(polview, fit, lower_bound, upper_bound)

predict_polview_incl_2021_model_4


predict_polview_incl_2021_list <- list()

for (model in c("model_4", "model_5", "model_6")) {
  predict_polview_incl_2021_list[[model]] <-
    predict_df(
      dataframe = mean_polview_incl_2021,
      regression = get(model),
      probability = 0.95
    ) %>%
    select(polview, fit, lower_bound, upper_bound) %>%
    rename_with(
      ~ paste0(., "_", model),
      c(fit, lower_bound, upper_bound)
    )
}

predict_polview_incl_2021 <- Reduce(
  full_join,
  predict_polview_incl_2021_list
) %>%
  select(polview, starts_with("fit"))

predict_polview_incl_2021


rm(predict_polview_incl_2021_list)


## Predicted probability by political ideology over time ------------------

### Pre 2021 --------------------------------------------------------------

mean_polview_year_pre_2021 <- gss_pre_2021 %>%
  group_by(moderate, conservative, year) %>%
  summarise(across(female:posttrump, ~ mean(.x))) %>%
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


for (model in c("model_1", "model_2", "model_3")) {
  predict_polview_year_pre_2021 <- data.frame()
  
  for (polview in c("liberal", "conservative", "moderate")) {
    predict_polview_year_pre_2021 <- bind_rows(
      predict_polview_year_pre_2021,
      predict_df(
        dataframe = mean_polview_year_pre_2021 %>% filter(polview == polview),
        regression = get(model),
        probability = 0.95
      )
    )
  }
  
  predict_polview_year_pre_2021 <-
    predict_polview_year_pre_2021 %>%
    select(year, polview, fit, lower_bound, upper_bound)
  
  head(predict_polview_year_pre_2021)
  
  # Plotting figure
  ggplot(
    data = predict_polview_year_pre_2021 %>% mutate(
      # Change order of factor level to change the line plotting order
      polview = factor(
        polview,
        levels = c("moderate", "liberal", "conservative")
      )
    ),
    mapping = aes(x = year)
  ) +
    geom_line(
      mapping = aes(y = fit, color = polview),
      size = 1.1
    ) +
    geom_ribbon(
      mapping = aes(
        ymin = lower_bound,
        ymax = upper_bound,
        fill = polview
      ),
      alpha = 0.25
    ) +
    # Change the spacing of the x-axis tick marks
    scale_x_continuous(breaks = seq(from = 1974, to = 2022, by = 4)) +
    # Change the spacing of the y-axis tick marks and the axis' scale to percent
    scale_y_continuous(
      breaks = seq(from = 0, to = 1, by = 0.1),
      label = scales::percent_format(accuracy = 1)
    ) +
    # Change the scale limits of the y-axis
    coord_cartesian(ylim = c(0, 1)) +
    # Set the colors for the different groups
    scale_color_manual(
      values = c(
        "conservative" = "#E63946",
        "liberal" = "#1D3557",
        "moderate" = "#A8DADC"
      ),
      labels = c(
        "conservative" = "Conservative",
        "liberal" = "Liberal",
        "moderate" = "Moderate"
      )
    ) +
    scale_fill_manual(
      values = c(
        "conservative" = "#E63946",
        "liberal" = "#1D3557",
        "moderate" = "#A8DADC"
      ),
      labels = c(
        "conservative" = "Conservative",
        "liberal" = "Liberal",
        "moderate" = "Moderate"
      )
    ) +
    labs(
      x = "Year",
      y = "Predicted Probability",
      title = 'Predicted Probability of Sharing "a great deal" of Confidence in Science',
      subtitle = paste0(
        gsub("model_", "Model ", model),
        " (excluding 2021)"
      )
    ) +
    # Remove the legend for the fill aesthetic and increase the legend key width
    guides(fill = "none", color = guide_legend(keywidth = 3)) +
    theme_thesis()
  
  ggsave(
    paste0(
      "../figures/predict-polview-year-pre-2021-",
      gsub(
        pattern = "_",
        replacement = "-",
        x = model,
        fixed = TRUE
      ),
      ".png"
    ),
    device = agg_png,
    res = 300,
    width = 16,
    height = 3 / 4 * 16,
    units = "cm"
  )
  
  tikz(
    paste0(
      "../reports/figures/predict-polview-year-pre-2021-",
      gsub(
        pattern = "_",
        replacement = "-",
        x = model,
        fixed = TRUE
      ),
      ".tex"
    ),
    width = 0.95 * 16 / 2.54,
    height = 0.95 * 3 / 4 * 16 / 2.54,
    sanitize = TRUE,
    timestamp = FALSE
  )
  
  # Plotting figure (LaTeX)
  ggplot(
    data = predict_polview_year_pre_2021 %>% mutate(
      # Change order of factor level to change the line plotting order
      polview = factor(
        polview,
        levels = c("moderate", "liberal", "conservative")
      )
    ),
    mapping = aes(x = year)
  ) +
    geom_line(
      mapping = aes(y = fit, color = polview, linetype = polview)
    ) +
    geom_ribbon(
      mapping = aes(
        ymin = lower_bound,
        ymax = upper_bound,
        fill = polview
      ),
      alpha = 0.1
    ) +
    # Add data points
    geom_point(mapping = aes(y = fit, shape = polview, color = polview), size = 2) +
    # Change the spacing of the x-axis tick marks
    scale_x_continuous(breaks = seq(from = 1974, to = 2022, by = 4)) +
    # Change the spacing of the y-axis tick marks and the axis' scale to percent
    scale_y_continuous(
      breaks = seq(from = 0, to = 1, by = 0.1),
      label = scales::percent_format(accuracy = 1)
    ) +
    # Change the scale limits of the y-axis
    coord_cartesian(ylim = c(0.3, 0.7)) +
    # Set the colors for the different groups using a gray scale
    scale_color_grey(
      start = 0,
      end = 0.4,
      labels = c(
        "conservative" = "Conservative",
        "liberal" = "Liberal",
        "moderate" = "Moderate"
      )
    ) +
    scale_fill_grey(
      start = 0,
      end = 0.4,
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
  
  plot(last_plot())
}


### Including 2021 --------------------------------------------------------

mean_polview_year_incl_2021 <- gss_incl_2021 %>%
  group_by(moderate, conservative, year) %>%
  summarise(across(female:covid19, ~ mean(.x))) %>%
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


for (model in c("model_4", "model_5", "model_6")) {
  predict_polview_year_incl_2021 <- data.frame()
  
  for (polview in c("liberal", "conservative", "moderate")) {
    predict_polview_year_incl_2021 <- bind_rows(
      predict_polview_year_incl_2021,
      predict_df(
        dataframe = mean_polview_year_incl_2021 %>% filter(polview == polview),
        regression = get(model),
        probability = 0.95
      )
    )
  }
  
  predict_polview_year_incl_2021 <-
    predict_polview_year_incl_2021 %>%
    select(year, polview, fit, lower_bound, upper_bound)
  
  head(predict_polview_year_incl_2021)
  
  # Plotting figure
  ggplot(
    data = predict_polview_year_incl_2021 %>% mutate(
      # Change order of factor level to change the line plotting order
      polview = factor(
        polview,
        levels = c("moderate", "liberal", "conservative")
      )
    ),
    mapping = aes(x = year)
  ) +
    geom_line(
      mapping = aes(y = fit, color = polview),
      size = 1.1
    ) +
    geom_ribbon(
      mapping = aes(
        ymin = lower_bound,
        ymax = upper_bound,
        fill = polview
      ),
      alpha = 0.25
    ) +
    # Change the spacing of the x-axis tick marks
    scale_x_continuous(breaks = seq(from = 1974, to = 2022, by = 4)) +
    # Change the spacing of the y-axis tick marks and the axis' scale to percent
    scale_y_continuous(
      breaks = seq(from = 0, to = 1, by = 0.1),
      label = scales::percent_format(accuracy = 1)
    ) +
    # Change the scale limits of the y-axis
    coord_cartesian(ylim = c(0, 1)) +
    # Set the colors for the different groups
    scale_color_manual(
      values = c(
        "conservative" = "#E63946",
        "liberal" = "#1D3557",
        "moderate" = "#A8DADC"
      ),
      labels = c(
        "conservative" = "Conservative",
        "liberal" = "Liberal",
        "moderate" = "Moderate"
      )
    ) +
    scale_fill_manual(
      values = c(
        "conservative" = "#E63946",
        "liberal" = "#1D3557",
        "moderate" = "#A8DADC"
      ),
      labels = c(
        "conservative" = "Conservative",
        "liberal" = "Liberal",
        "moderate" = "Moderate"
      )
    ) +
    labs(
      x = "Year",
      y = "Predicted Probability",
      title = 'Predicted Probability of Sharing "a great deal" of Confidence in Science',
      subtitle = paste0(
        gsub("model_", "Model ", model),
        " (including 2021)"
      )
    ) +
    # Remove the legend for the fill aesthetic and increase the legend key width
    guides(fill = "none", color = guide_legend(keywidth = 3)) +
    theme_thesis()
  
  ggsave(
    paste0(
      "../figures/predict-polview-year-incl-2021-",
      gsub(
        pattern = "_",
        replacement = "-",
        x = model,
        fixed = TRUE
      ),
      ".png"
    ),
    device = agg_png,
    res = 300,
    width = 16,
    height = 3 / 4 * 16,
    units = "cm"
  )
}
