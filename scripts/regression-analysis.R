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

# A script that includes custom stargazer table exporting functions
source("custom-tables.R")


# Load data ---------------------------------------------------------------

load("../data/gss.RData")

glimpse(gss)


gss_pre_2021 <- gss %>%
  filter(year < 2021)

gss_incl_2021 <- gss %>%
  select(-nonwhite, -south)

sum(is.na(gss_pre_2021))
sum(is.na(gss_incl_2021))


n_obs_pre_2021 <- nrow(gss_pre_2021)
n_obs_incl_2021 <- nrow(gss_incl_2021)


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


## Export regression table to LaTeX ---------------------------------------

# List of goodness of fit (GOF) statistics methods
# getMethod("extract", "glm")

texreg_custom(
  l = list(model_1, model_2, model_3),
  # Use "path" instead of "file" when calling texreg_custom()
  path = "../reports/figures/logit-pre-2021.tex",
  stars = c(0.01, 0.05, 0.1),
  custom.model.names = c("Model 1", "Model 2", "Model 3"),
  custom.coef.names = c(
    "Constant",
    "Year",
    "Female",
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
  custom.gof.rows = list(
    "Including Year 2021" = c("No", "No", "No")
  ),
  # Don't incluce \\item in custom.note
  custom.note = "%stars\\\\[0.6em]\n {\\it Note:} Numbers in parentheses represent standard errors. The reference category for political party affiliation is Democrat. The reference category for political ideology is liberal. Age\\textsuperscript{2} is a squared term.",
  digits = 3,
  caption = "Logit Models Predicting Public Confidence in Science (I)",
  caption.above = TRUE,
  label = "table:LogitPre2021",
  booktabs = TRUE,
  dcolumn = TRUE,
  longtable = TRUE,
  threeparttable = TRUE,
  use.packages = FALSE,
  include.aic = TRUE,
  include.bic = TRUE,
  include.loglik = TRUE,
  include.deviance = FALSE,
  include.nobs = TRUE
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


## Export regression table to LaTeX ---------------------------------------

texreg_custom(
  l = list(model_4, model_5, model_6),
  # Use "path" instead of "file" when calling texreg_custom()
  path = "../reports/figures/logit-incl-2021.tex",
  stars = c(0.01, 0.05, 0.1),
  custom.model.names = c("Model 4", "Model 5", "Model 6"),
  custom.coef.names = c(
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
  custom.gof.rows = list(
    "Including Year 2021" = c("Yes", "Yes", "Yes")
  ),
  # Don't incluce \\item in custom.note
  custom.note = '%stars\\\\[0.6em]\n {\\it Note:} The variables "Non-White" and "South" are excluded from models 4, 5, and 6. Numbers in parentheses represent standard errors. The reference category for political party affiliation is Democrat. The reference category for political ideology is liberal. Age\\textsuperscript{2} is a squared term.',
  digits = 3,
  caption = "Logit Models Predicting Public Confidence in Science (II)",
  caption.above = TRUE,
  label = "table:LogitIncl2021",
  booktabs = TRUE,
  dcolumn = TRUE,
  longtable = TRUE,
  threeparttable = TRUE,
  use.packages = FALSE,
  include.aic = TRUE,
  include.bic = TRUE,
  include.loglik = TRUE,
  include.deviance = FALSE,
  include.nobs = TRUE
)


# Marginal effects (pre 2021) ---------------------------------------------

margins_model_1 <- margins(model_1)
margins_model_2 <- margins(model_2)
margins_model_3 <- margins(model_3)

margins_model_1_table <- summary(margins_model_1)
margins_model_2_table <- summary(margins_model_2)
margins_model_3_table <- summary(margins_model_3)


# Add "fake" rows of intercept and age squared and interaction terms

for (margins_table in c(
  "margins_model_1_table",
  "margins_model_2_table",
  "margins_model_3_table")
) {
  margins_table_new <- get(margins_table) %>%
    arrange(
      factor(
        x = factor,
        levels = c(
          "year",
          "female",
          "nonwhite",
          "educ",
          "highschool",
          "bachelor",
          "graduate",
          "south",
          "attend",
          "realinc",
          "age",
          "independent",
          "republican",
          "moderate",
          "conservative",
          "postreagan",
          "bush",
          "posttrump"
        )
      )
    ) %>%
    add_row(factor = "(Intercept)", .before = 1) %>%
    add_row(factor = "I(age ^ 2)", .after = 12)
  
  assign(margins_table, margins_table_new)
}

margins_model_2_table <- margins_model_2_table %>%
  add_row(factor = "moderate * year") %>%
  add_row(factor = "conservative * year")

margins_model_3_table <- margins_model_3_table %>%
  add_row(factor = "postreagan * moderate") %>%
  add_row(factor = "bush * moderate") %>%
  add_row(factor = "posttrump * moderate") %>%
  add_row(factor = "postreagan * conservative") %>%
  add_row(factor = "bush * conservative") %>%
  add_row(factor = "posttrump * conservative")


## Export marginal effects table to LaTeX ---------------------------------

texreg_custom(
  l = list(model_1, model_2, model_3),
  # Use "path" instead of "file" when calling texreg_custom()
  path = "../reports/figures/logit-pre-2021-margins.tex",
  stars = c(0.01, 0.05, 0.1),
  custom.model.names = c("Model 1", "Model 2", "Model 3"),
  custom.coef.names = c(
    "Constant",
    "Year",
    "Female",
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
  custom.gof.rows = list(
    "Including Year 2021" = c("No", "No", "No")
  ),
  # Don't include \\item in custom.note
  custom.note = "%stars",
  digits = 3,
  override.coef = list(
    margins_model_1_table$AME,
    margins_model_2_table$AME,
    margins_model_3_table$AME
  ),
  override.se = list(
    margins_model_1_table$SE,
    margins_model_2_table$SE,
    margins_model_3_table$SE
  ),
  override.pvalues = list(
    margins_model_1_table$p,
    margins_model_2_table$p,
    margins_model_3_table$p
  ),
  omit.coef = "Intercept|I.age|year.moderate|year.conservative|moderate.postreagan|moderate.bush|moderate.posttrump|conservative.postreagan|conservative.bush|conservative.posttrump",
  caption = "Average Marginal Effects of Logit Models (I)",
  caption.above = TRUE,
  label = "table:LogitPre2021Margins",
  booktabs = TRUE,
  dcolumn = TRUE,
  longtable = TRUE,
  threeparttable = TRUE,
  use.packages = FALSE,
  include.aic = TRUE,
  include.bic = TRUE,
  include.loglik = TRUE,
  include.deviance = FALSE,
  include.nobs = TRUE
)



# Marginal effects (including 2021) ---------------------------------------

margins_model_4 <- margins(model_4)
margins_model_5 <- margins(model_5)
margins_model_6 <- margins(model_6)

margins_model_4_table <- summary(margins_model_4)
margins_model_5_table <- summary(margins_model_5)
margins_model_6_table <- summary(margins_model_6)


# Add "fake" rows of intercept and age squared and interaction terms

for (margins_table in c(
  "margins_model_4_table",
  "margins_model_5_table",
  "margins_model_6_table")
) {
  margins_table_new <- get(margins_table) %>%
    arrange(
      factor(
        x = factor,
        levels = c(
          "year",
          "female",
          "nonwhite",
          "educ",
          "highschool",
          "bachelor",
          "graduate",
          "south",
          "attend",
          "realinc",
          "age",
          "independent",
          "republican",
          "moderate",
          "conservative",
          "postreagan",
          "bush",
          "posttrump",
          "covid19"
        )
      )
    ) %>%
    add_row(factor = "(Intercept)", .before = 1) %>%
    add_row(factor = "I(age ^ 2)", .after = 10)
  
  assign(margins_table, margins_table_new)
}

margins_model_5_table <- margins_model_5_table %>%
  add_row(factor = "moderate * year") %>%
  add_row(factor = "conservative * year")

margins_model_6_table <- margins_model_6_table %>%
  add_row(factor = "postreagan * moderate") %>%
  add_row(factor = "bush * moderate") %>%
  add_row(factor = "posttrump * moderate") %>%
  add_row(factor = "covid19 * moderate") %>%
  add_row(factor = "postreagan * conservative") %>%
  add_row(factor = "bush * conservative") %>%
  add_row(factor = "posttrump * conservative") %>%
  add_row(factor = "covid19 * conservative")


## Export marginal effects table to LaTeX ---------------------------------

texreg_custom(
  l = list(model_4, model_5, model_6),
  # Use "path" instead of "file" when calling texreg_custom()
  path = "../reports/figures/logit-incl-2021-margins.tex",
  stars = c(0.01, 0.05, 0.1),
  custom.model.names = c("Model 4", "Model 5", "Model 6"),
  custom.coef.names = c(
    "Constant",
    "Year",
    "Female",
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
  custom.gof.rows = list(
    "Including Year 2021" = c("Yes", "Yes", "Yes")
  ),
  # Don't include \\item in custom.note
  custom.note = '%stars\\\\[0.6em]\n {\\it Note:} The variables "Non-White" and "South" are excluded from models 4, 5, and 6.',
  digits = 3,
  override.coef = list(
    margins_model_4_table$AME,
    margins_model_5_table$AME,
    margins_model_6_table$AME
  ),
  override.se = list(
    margins_model_4_table$SE,
    margins_model_5_table$SE,
    margins_model_6_table$SE
  ),
  override.pvalues = list(
    margins_model_4_table$p,
    margins_model_5_table$p,
    margins_model_6_table$p
  ),
  omit.coef = "Intercept|I.age|year.moderate|year.conservative|moderate.postreagan|moderate.bush|moderate.posttrump|moderate.covid19|conservative.postreagan|conservative.bush|conservative.posttrump|conservative.covid19",
  caption = "Average Marginal Effects of Logit Models (II)",
  caption.above = TRUE,
  label = "table:LogitIncl2021Margins",
  booktabs = TRUE,
  dcolumn = TRUE,
  longtable = TRUE,
  threeparttable = TRUE,
  use.packages = FALSE,
  include.aic = TRUE,
  include.bic = TRUE,
  include.loglik = TRUE,
  include.deviance = FALSE,
  include.nobs = TRUE
)


# Export models -----------------------------------------------------------

save(
  model_1,
  model_2,
  model_3,
  model_4,
  model_5,
  model_6,
  margins_model_1,
  margins_model_2,
  margins_model_3,
  margins_model_4,
  margins_model_5,
  margins_model_6,
  file = "../data/models.RData"
)
