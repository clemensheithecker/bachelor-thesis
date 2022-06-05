# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Clear environment
rm(list = ls())


# Load libraries ----------------------------------------------------------

# Provides marginal effects summaries of models via 'margins()'
library(margins)

library(texreg)

library(tidyverse)


# Source scripts ----------------------------------------------------------

# A script that includes custom table exporting functions
source("custom-tables.R")


# Load data ---------------------------------------------------------------

load("../data/gss.RData")

glimpse(gss)


gss_pre_2021_female <- gss %>%
  filter(year < 2021 & female == 1)

gss_incl_2021_female <- gss %>%
  select(-nonwhite, -south) %>%
  filter(female == 1)


n_obs_pre_2021_female <- nrow(gss_pre_2021_female)
n_obs_incl_2021_female <- nrow(gss_incl_2021_female)


# Logistic regression models (pre 2021, female only) ----------------------

model_1_female <- glm(
  consci ~ year + nonwhite + educ + highschool + bachelor + graduate + south +
    attend + realinc + age + I(age ^ 2) + independent + republican + moderate +
    conservative + postreagan + bush + socialmedia + posttrump,
  family = binomial(link = "logit"),
  data = gss_pre_2021_female
)

summary(model_1_female)


model_2_female <- glm(
  consci ~ year + nonwhite + educ + highschool + bachelor + graduate + south +
    attend + realinc + age + I(age ^ 2) + independent + republican + moderate +
    conservative + postreagan + bush + socialmedia + posttrump + moderate:year +
    conservative:year,
  family = binomial(link = "logit"),
  data = gss_pre_2021_female
)

summary(model_2_female)

model_2_variation_female <- glm(
  consci ~ year + nonwhite + educ + highschool + bachelor + graduate + south +
    attend + realinc + age + I(age^2) + independent + republican + moderate +
    conservative + postreagan + bush + socialmedia + posttrump + moderate:year +
    conservative:year + moderate:socialmedia + conservative:socialmedia +
    year:socialmedia + moderate:socialmedia:year +
    conservative:socialmedia:year,
  family = binomial(link = "logit"),
  data = gss_pre_2021_female
)

summary(model_2_variation_female)


model_3_female <- glm(
  consci ~ year + nonwhite + educ + highschool + bachelor + graduate + south +
    attend + realinc + age + I(age ^ 2) + independent + republican + moderate +
    conservative + postreagan + bush + socialmedia + posttrump +
    postreagan:moderate + bush:moderate + socialmedia:moderate +
    posttrump:moderate + postreagan:conservative + bush:conservative +
    socialmedia:conservative + posttrump:conservative,
  family = binomial(link = "logit"),
  data = gss_pre_2021_female
)

summary(model_3_female)


## Export regression table to LaTeX ---------------------------------------

# List of goodness of fit (GOF) statistics methods
# getMethod("extract", "glm")

texreg_custom(
  l = list(
    model_1_female,
    model_2_female,
    model_2_variation_female,
    model_3_female
  ),
  # Use "path" instead of "file" when calling texreg_custom()
  path = "../reports/figures/table-logit-pre-2021-female.tex",
  stars = c(0.01, 0.05, 0.1),
  custom.model.names = c("Model 1", "Model 2", "Model 2 Variation", "Model 3"),
  custom.coef.names = c(
    "Constant",
    "Year",
    "Non-White",
    "Education (Years)",
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
    "S.M.E. (2010--2021)",
    "Post-Trump (2017--2021)",
    "Year $\\times$ Moderate",
    "Year $\\times$ Conservative",
    "Year $\\times$ Social Media",
    "Moderate $\\times$ S.M.E.",
    "Conservative $\\times$ S.M.E.",
    "Year $\\times$ Mod. $\\times$ S.M.E.",
    "Year $\\times$ Cons. $\\times$ S.M.E.",
    "Moderate $\\times$ Post-Reagan",
    "Moderate $\\times$ Bush",
    "Moderate $\\times$ Post-Trump",
    "Conservative $\\times$ Post-Reagan",
    "Conservative $\\times$ Bush",
    "Conservative $\\times$ Post-Trump"
  ),
  custom.gof.rows = list(
    "Including Year 2021" = c("No", "No", "No", "No"),
    "Female Individuals Only" = c("Yes", "Yes", "Yes", "Yes"),
    "AIC" = c(
      formatC(round(AIC(model_1_female)), format = "d", big.mark = ","),
      formatC(round(AIC(model_2_female)), format = "d", big.mark = ","),
      formatC(round(AIC(model_2_variation_female)), format = "d", big.mark = ","),
      formatC(round(AIC(model_3_female)), format = "d", big.mark = ",")
    ),
    "BIC" = c(
      formatC(round(BIC(model_1_female)), format = "d", big.mark = ","),
      formatC(round(BIC(model_2_female)), format = "d", big.mark = ","),
      formatC(round(BIC(model_2_variation_female)), format = "d", big.mark = ","),
      formatC(round(BIC(model_3_female)), format = "d", big.mark = ",")
    ),
    "Log Likelihood" = c(
      formatC(round(logLik(model_1_female)[1]), format = "d", big.mark = ","),
      formatC(round(logLik(model_2_female)[1]), format = "d", big.mark = ","),
      formatC(round(logLik(model_2_variation_female)[1]), format = "d", big.mark = ","),
      formatC(round(logLik(model_3_female)[1]), format = "d", big.mark = ",")
    ),
    "Num. obs." = c(
      formatC(nobs(model_1_female), format = "d", big.mark = ","),
      formatC(nobs(model_2_female), format = "d", big.mark = ","),
      formatC(nobs(model_2_variation_female), format = "d", big.mark = ","),
      formatC(nobs(model_3_female), format = "d", big.mark = ",")
    )
  ),
  # Don't include \\item in custom.note
  custom.note = "%stars\\\\[0.6em]\n {\\it Note:} Mod., Cons., and S.M.E. are acronyms for moderate, conservative, and social media era, respectively. Numbers in parentheses represent standard errors. The reference category for political party affiliation is Democrat. The reference category for political ideology is liberal. Age\\textsuperscript{2} is a squared term.",
  digits = 3,
  caption = "Logit Models Predicting Public Confidence in Science (I)---Robustness Check: Sample Includes Female Individuals Only",
  caption.above = TRUE,
  label = "table:LogitPre2021Female",
  booktabs = TRUE,
  dcolumn = TRUE,
  longtable = TRUE,
  threeparttable = TRUE,
  use.packages = FALSE,
  include.aic = FALSE,
  include.bic = FALSE,
  include.loglik = FALSE,
  include.deviance = FALSE,
  include.nobs = FALSE
)


# Logistic regression models (including 2021, female only) ----------------

model_4_female <- glm(
  consci ~ year + educ + highschool + bachelor + graduate + attend + realinc +
    age + I(age ^ 2) + independent + republican + moderate + conservative +
    postreagan + bush + socialmedia + posttrump + covid19,
  family = binomial(link = "logit"),
  data = gss_incl_2021_female
)

summary(model_4_female)


model_5_female <- glm(
  consci ~ year + educ + highschool + bachelor + graduate + attend + realinc +
    age + I(age ^ 2) + independent + republican + moderate + conservative +
    postreagan + bush + socialmedia + posttrump + covid19 + moderate:year +
    conservative:year,
  family = binomial(link = "logit"),
  data = gss_incl_2021_female
)

summary(model_5_female)

model_5_variation_female <- glm(
  consci ~ year + educ + highschool + bachelor + graduate + attend + realinc +
    age + I(age^2) + independent + republican + moderate + conservative +
    postreagan + bush + socialmedia + posttrump + covid19 + moderate:year +
    conservative:year + moderate:socialmedia + conservative:socialmedia +
    year:socialmedia + moderate:socialmedia:year +
    conservative:socialmedia:year,
  family = binomial(link = "logit"),
  data = gss_incl_2021_female
)

summary(model_5_female_variation)


model_6_female <- glm(
  consci ~ year + educ + highschool + bachelor + graduate + attend + realinc +
    age + I(age ^ 2) + independent + republican + moderate + conservative +
    postreagan + bush + socialmedia + posttrump + covid19 +
    postreagan:moderate + bush:moderate + socialmedia:moderate +
    posttrump:moderate + covid19:moderate + postreagan:conservative +
    bush:conservative + socialmedia:conservative + posttrump:conservative +
    covid19:conservative,
  family = binomial(link = "logit"),
  data = gss_incl_2021_female
)

summary(model_6_female)


## Export regression table to LaTeX ---------------------------------------

# List of goodness of fit (GOF) statistics methods
# getMethod("extract", "glm")

texreg_custom(
  l = list(
    model_4_female,
    model_5_female,
    model_5_variation_female,
    model_6_female
  ),
  # Use "path" instead of "file" when calling texreg_custom()
  path = "../reports/figures/table-logit-incl-2021-female.tex",
  stars = c(0.01, 0.05, 0.1),
  custom.model.names = c("Model 4", "Model 5", "Model 5 Variation", "Model 6"),
  custom.coef.names = c(
    "Constant",
    "Year",
    "Education (Years)",
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
    "S.M.E. (2010--2021)",
    "Post-Trump (2017--2021)",
    "COVID-19 (2020--2021)",
    "Year $\\times$ Moderate",
    "Year $\\times$ Conservative",
    "Year $\\times$ Social Media",
    "Moderate $\\times$ S.M.E.",
    "Conservative $\\times$ S.M.E.",
    "Year $\\times$ Mod. $\\times$ S.M.E.",
    "Year $\\times$ Cons. $\\times$ S.M.E.",
    "Moderate $\\times$ Post-Reagan",
    "Moderate $\\times$ Bush",
    "Moderate $\\times$ Post-Trump",
    "Moderate $\\times$ COVID-19",
    "Conservative $\\times$ Post-Reagan",
    "Conservative $\\times$ Bush",
    "Conservative $\\times$ Post-Trump",
    "Conservative $\\times$ COVID-19"
  ),
  custom.gof.rows = list(
    "Including Year 2021" = c("Yes", "Yes", "Yes", "Yes"),
    "Female Individuals Only" = c("Yes", "Yes", "Yes", "Yes"),
    "AIC" = c(
      formatC(round(AIC(model_4_female)), format = "d", big.mark = ","),
      formatC(round(AIC(model_5_female)), format = "d", big.mark = ","),
      formatC(round(AIC(model_5_variation_female)), format = "d", big.mark = ","),
      formatC(round(AIC(model_6_female)), format = "d", big.mark = ",")
    ),
    "BIC" = c(
      formatC(round(BIC(model_4_female)), format = "d", big.mark = ","),
      formatC(round(BIC(model_5_female)), format = "d", big.mark = ","),
      formatC(round(BIC(model_5_variation_female)), format = "d", big.mark = ","),
      formatC(round(BIC(model_6_female)), format = "d", big.mark = ",")
    ),
    "Log Likelihood" = c(
      formatC(round(logLik(model_4_female)[1]), format = "d", big.mark = ","),
      formatC(round(logLik(model_5_female)[1]), format = "d", big.mark = ","),
      formatC(round(logLik(model_5_variation_female)[1]), format = "d", big.mark = ","),
      formatC(round(logLik(model_6_female)[1]), format = "d", big.mark = ",")
    ),
    "Num. obs." = c(
      formatC(nobs(model_4_female), format = "d", big.mark = ","),
      formatC(nobs(model_5_female), format = "d", big.mark = ","),
      formatC(nobs(model_5_variation_female), format = "d", big.mark = ","),
      formatC(nobs(model_6_female), format = "d", big.mark = ",")
    )
  ),
  # Don't include \\item in custom.note
  custom.note = '%stars\\\\[0.6em]\n {\\it Note:} Mod., Cons., and S.M.E. are acronyms for moderate, conservative, and social media era, respectively. The variables "Non-White" and "South" are excluded from Model 4, 5, and 6. Numbers in parentheses represent standard errors. The reference category for political party affiliation is Democrat. The reference category for political ideology is liberal. Age\\textsuperscript{2} is a squared term.',
  digits = 3,
  caption = "Logit Models Predicting Public Confidence in Science (II)---Robustness Check: Sample Includes Female Individuals Only",
  caption.above = TRUE,
  label = "table:LogitIncl2021Female",
  booktabs = TRUE,
  dcolumn = TRUE,
  longtable = TRUE,
  threeparttable = TRUE,
  use.packages = FALSE,
  include.aic = FALSE,
  include.bic = FALSE,
  include.loglik = FALSE,
  include.deviance = FALSE,
  include.nobs = FALSE
)
