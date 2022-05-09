# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Clear environment
rm(list = ls())


# Load libraries ----------------------------------------------------------

# Linear mixed effects models
# library(lme4)

# library(jtools)

library(margins)

# library(pscl)
# library(rstatix)
# library(lmtest)

# Create LaTeX code for well-formatted tables
library(stargazer)

library(tidyverse)

# Export data frame as LaTeX table
library(xtable)

# library(wooldridge)
# library(jtools)
# library(margins)
# library(tidyverse)
# library(lmtest)
# library(pscl)
# library(rstatix)
# library(stargazer)


# Source scripts ----------------------------------------------------------

# A script that includes custom stargazer table exporting functions
source("custom-tables.R")


# Load data ---------------------------------------------------------------

load("../data/gss.RData")

glimpse(gss)


# Simple logistic regression model ----------------------------------------

gss_pre_2021 <- gss %>%
  filter(year <= 2018)


reg_1 <- glm(
  consci ~ . - id - cohort - covid19,
  family = binomial(link = "logit"),
  data = gss_pre_2021
)

summary(reg_1)


reg_2 <- glm(
  consci ~ . - id - cohort - covid19 + moderate * year + conservative * year,
  family = binomial(link = "logit"),
  data = gss_pre_2021
)

summary(reg_2)


reg_3 <- glm(
  consci ~ . - id - cohort - covid19 + postreagan * moderate + bush * moderate +
    posttrump * moderate + postreagan * conservative + bush * conservative +
    posttrump * conservative,
  family = binomial(link = "logit"),
  data = gss_pre_2021
)

summary(reg_3)


regression_table_thesis(
  reg_1,
  reg_2,
  reg_3,
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
  footnote = "{\\it Note:} Numbers in parentheses represent standard errors. The reference category for political party affiliation is Democract. The reference category for political ideology is liberal. \\newline $^{*}p<0.1$; $^{**}p<0.05$; $^{***}p<0.01$"
)

gss_incl_2021 <- gss %>%
  select(-nonwhite, -south)


reg_4 <- glm(
  consci ~ . - id - cohort,
  family = binomial(link = "logit"),
  data = gss_incl_2021
)

summary(reg_4)

reg_5 <- glm(
  consci ~ . - id - cohort + moderate * year + conservative * year,
  family = binomial(link = "logit"),
  data = gss_incl_2021
)

summary(reg_5)

reg_6 <- glm(
  consci ~ . - id - cohort + postreagan * moderate + bush * moderate +
    posttrump * moderate + postreagan * conservative + bush * conservative +
    posttrump * conservative,
  family = binomial(link = "logit"),
  data = gss_incl_2021
)

summary(reg_6)


stargazer(
  reg_4,
  reg_5,
  reg_6,
  type = "html",
  out = "test-regressions-2.html",
  title = "Logit Models Predicting Public Confidence in Science"
)


# Marginal effects --------------------------------------------------------

margins_reg_1 <- margins(reg_1)
margins_reg_2 <- margins(reg_2)
margins_reg_3 <- margins(reg_3)

export_summs(
  margins_reg_1,
  margins_reg_2,
  margins_reg_3,
  to.file = "html",
  file.name = "margins.html"
)


margins_reg_4 <- margins(reg_4)
margins_reg_5 <- margins(reg_5)
margins_reg_6 <- margins(reg_6)

export_summs(
  margins_reg_4,
  margins_reg_5,
  margins_reg_6,
  to.file = "html",
  file.name = "margins-2.html"
)

