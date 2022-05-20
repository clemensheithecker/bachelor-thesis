# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Clear environment
rm(list = ls())


# Load libraries ----------------------------------------------------------

library(tidyverse)


# Load data ---------------------------------------------------------------

load("../data/gss.RData")
load("../data/models.RData")


gss_pre_2021 <- gss %>%
  filter(year < 2021)

gss_incl_2021 <- gss %>%
  select(-nonwhite, -south)


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


# Predicted probability by political ideology -----------------------------

## Pre 2021 ---------------------------------------------------------------

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

rm(predict_polview_pre_2021_list)


predict_polview_pre_2021


# Predicted probability by political ideology (incl. post-Trump) ----------

## Pre 2021 ---------------------------------------------------------------

mean_polview_posttrump_pre_2021 <- gss_pre_2021 %>%
  group_by(moderate, conservative, posttrump) %>%
  summarise(across(c(year, female:bush), ~ mean(.x))) %>%
  ungroup() %>%
  mutate(
    polview = case_when(
      moderate == 0 & conservative == 0 ~ "liberal",
      moderate == 0 & conservative == 1 ~ "conservative",
      moderate == 1 & conservative == 0 ~ "moderate"
    )
  )


predict_polview_posttrump_pre_2021_list <- list()

for (model in c("model_1", "model_2", "model_3")) {
  predict_polview_posttrump_pre_2021_list[[model]] <-
    predict_df(
      dataframe = mean_polview_posttrump_pre_2021,
      regression = get(model),
      probability = 0.95
    ) %>%
    select(polview, posttrump, fit, lower_bound, upper_bound) %>%
    rename_with(
      ~ paste0(., "_", model),
      c(fit, lower_bound, upper_bound)
    )
}

predict_polview_posttrump_pre_2021 <- Reduce(
  full_join,
  predict_polview_posttrump_pre_2021_list
) %>%
  select(polview, posttrump, starts_with("fit"))

rm(predict_polview_posttrump_pre_2021_list)


predict_polview_posttrump_pre_2021




