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


gss_pre_2010 <- gss %>%
  filter(year <= 2010)

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
    moderate + conservative + postreagan + bush + socialmedia + posttrump,
  family = binomial(link = "logit"),
  data = gss_pre_2021
)

summary(model_1)


model_2 <- glm(
  consci ~ year + female + nonwhite + educ + highschool + bachelor + graduate +
    south + attend + realinc + age + I(age^2) + independent + republican +
    moderate + conservative + postreagan + bush + socialmedia + posttrump +
    moderate:year + conservative:year,
  family = binomial(link = "logit"),
  data = gss_pre_2021
)

summary(model_2)

model_2_variation <- glm(
  consci ~ year + female + nonwhite + educ + highschool + bachelor + graduate +
    south + attend + realinc + age + I(age^2) + independent + republican +
    moderate + conservative + postreagan + bush + socialmedia + posttrump +
    moderate:year + conservative:year + year:socialmedia +
    moderate:socialmedia + conservative:socialmedia +
    moderate:socialmedia:year + conservative:socialmedia:year,
  family = binomial(link = "logit"),
  data = gss_pre_2021
)

summary(model_2_variation)


model_3 <- glm(
  consci ~ year + female + nonwhite + educ + highschool + bachelor + graduate +
    south + attend + realinc + age + I(age ^ 2) + independent + republican +
    moderate + conservative + postreagan + bush + socialmedia + posttrump +
    postreagan:moderate + bush:moderate + socialmedia:moderate +
    posttrump:moderate + postreagan:conservative + bush:conservative +
    socialmedia:conservative + posttrump:conservative,
  family = binomial(link = "logit"),
  data = gss_pre_2021
)

summary(model_3)


## Export regression table to LaTeX ---------------------------------------

# List of goodness of fit (GOF) statistics methods
# getMethod("extract", "glm")

texreg_custom(
  l = list(model_1, model_2, model_2_variation, model_3),
  # Use "path" instead of "file" when calling texreg_custom()
  path = "../reports/figures/table-logit-pre-2021.tex",
  stars = c(0.01, 0.05, 0.1),
  custom.model.names = c("Model 1", "Model 2", "Model 2 Var.", "Model 3"),
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
    "Post-Reagan (1981--2018)",
    "Bush (2001--2008)",
    "Social Media Era (2010--2018)",
    "Post-Trump (2017--2018)",
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
    "AIC" = c(
      formatC(round(AIC(model_1)), format = "d", big.mark = ","),
      formatC(round(AIC(model_2)), format = "d", big.mark = ","),
      formatC(round(AIC(model_2_variation)), format = "d", big.mark = ","),
      formatC(round(AIC(model_3)), format = "d", big.mark = ",")
    ),
    "BIC" = c(
      formatC(round(BIC(model_1)), format = "d", big.mark = ","),
      formatC(round(BIC(model_2)), format = "d", big.mark = ","),
      formatC(round(BIC(model_2_variation)), format = "d", big.mark = ","),
      formatC(round(BIC(model_3)), format = "d", big.mark = ",")
    ),
    "Log Likelihood" = c(
      formatC(round(logLik(model_1)[1]), format = "d", big.mark = ","),
      formatC(round(logLik(model_2)[1]), format = "d", big.mark = ","),
      formatC(round(logLik(model_2_variation)[1]), format = "d", big.mark = ","),
      formatC(round(logLik(model_3)[1]), format = "d", big.mark = ",")
    ),
    "Num. obs." = c(
      formatC(nobs(model_1), format = "d", big.mark = ","),
      formatC(nobs(model_2), format = "d", big.mark = ","),
      formatC(nobs(model_2_variation), format = "d", big.mark = ","),
      formatC(nobs(model_3), format = "d", big.mark = ",")
    )
  ),
  # Don't incluce \\item in custom.note
  custom.note = "%stars\\\\[0.6em]\n {\\it Note:} Var., Mod., Cons., and S.M.E. are acronyms for variation, moderate, conservative, and social media era, respectively. Numbers in parentheses represent standard errors. The reference category for political party affiliation is Democrat. The reference category for political ideology is liberal. Age\\textsuperscript{2} is a squared term.",
  digits = 3,
  caption = "Logit Models Predicting Public Confidence in Science (I)",
  caption.above = TRUE,
  label = "table:LogitPre2021",
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


# Logistic regression models (pre 2010) -----------------------------------

model_2_pre2010 <- glm(
  consci ~ year + female + nonwhite + educ + highschool + bachelor + graduate +
    south + attend + realinc + age + I(age^2) + independent + republican +
    moderate + conservative + postreagan + bush + moderate:year +
    conservative:year,
  family = binomial(link = "logit"),
  data = gss_pre_2010
)

summary(model_2_pre2010)


# Logistic regression models (including 2021) -----------------------------

model_4 <- glm(
  consci ~ year + female + educ + highschool + bachelor + graduate + attend +
    realinc + age + I(age ^ 2) + independent + republican + moderate +
    conservative + postreagan + bush + socialmedia + posttrump + covid19,
  family = binomial(link = "logit"),
  data = gss_incl_2021
)

summary(model_4)


model_5 <- glm(
  consci ~ year + female + educ + highschool + bachelor + graduate + attend +
    realinc + age + I(age^2) + independent + republican + moderate +
    conservative + postreagan + bush + socialmedia + posttrump + covid19 +
    moderate:year + conservative:year,
  family = binomial(link = "logit"),
  data = gss_incl_2021
)

summary(model_5)

model_5_variation <- glm(
  consci ~ year + female + educ + highschool + bachelor + graduate + attend +
    realinc + age + I(age^2) + independent + republican + moderate +
    conservative + postreagan + bush + socialmedia + posttrump + covid19 +
    moderate:year + conservative:year + year:socialmedia +
    moderate:socialmedia + conservative:socialmedia +
    moderate:socialmedia:year + conservative:socialmedia:year,
  family = binomial(link = "logit"),
  data = gss_incl_2021
)

summary(model_5_variation)


model_6 <- glm(
  consci ~ year + female + educ + highschool + bachelor + graduate + attend +
    realinc + age + I(age ^ 2) + independent + republican + moderate +
    conservative + postreagan + bush + socialmedia + posttrump + covid19 +
    postreagan:moderate + bush:moderate + socialmedia:moderate +
    posttrump:moderate + covid19:moderate + postreagan:conservative +
    bush:conservative + socialmedia:conservative + posttrump:conservative +
    covid19:conservative,
  family = binomial(link = "logit"),
  data = gss_incl_2021
)

summary(model_6)


## Export regression table to LaTeX ---------------------------------------

texreg_custom(
  l = list(model_4, model_5, model_5_variation, model_6),
  # Use "path" instead of "file" when calling texreg_custom()
  path = "../reports/figures/table-logit-incl-2021.tex",
  stars = c(0.01, 0.05, 0.1),
  custom.model.names = c("Model 4", "Model 5", "Model 5 Var.", "Model 6"),
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
    "Social Media Era (2010--2021)",
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
    "AIC" = c(
      formatC(round(AIC(model_4)), format = "d", big.mark = ","),
      formatC(round(AIC(model_5)), format = "d", big.mark = ","),
      formatC(round(AIC(model_5_variation)), format = "d", big.mark = ","),
      formatC(round(AIC(model_6)), format = "d", big.mark = ",")
    ),
    "BIC" = c(
      formatC(round(BIC(model_4)), format = "d", big.mark = ","),
      formatC(round(BIC(model_5)), format = "d", big.mark = ","),
      formatC(round(BIC(model_5_variation)), format = "d", big.mark = ","),
      formatC(round(BIC(model_6)), format = "d", big.mark = ",")
    ),
    "Log Likelihood" = c(
      formatC(round(logLik(model_4)[1]), format = "d", big.mark = ","),
      formatC(round(logLik(model_5)[1]), format = "d", big.mark = ","),
      formatC(round(logLik(model_5_variation)[1]), format = "d", big.mark = ","),
      formatC(round(logLik(model_6)[1]), format = "d", big.mark = ",")
    ),
    "Num. obs." = c(
      formatC(nobs(model_4), format = "d", big.mark = ","),
      formatC(nobs(model_5), format = "d", big.mark = ","),
      formatC(nobs(model_5_variation), format = "d", big.mark = ","),
      formatC(nobs(model_6), format = "d", big.mark = ",")
    )
  ),
  # Don't incluce \\item in custom.note
  custom.note = '%stars\\\\[0.6em]\n {\\it Note:} Mod., Cons., and S.M.E. are acronyms for moderate, conservative, and social media era, respectively. The variables "Non-White" and "South" are excluded from Model 4, 5, and 6. Numbers in parentheses represent standard errors. The reference category for political party affiliation is Democrat. The reference category for political ideology is liberal. Age\\textsuperscript{2} is a squared term.',
  digits = 3,
  caption = "Logit Models Predicting Public Confidence in Science (II)",
  caption.above = TRUE,
  label = "table:LogitIncl2021",
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


# Marginal effects (pre 2021) ---------------------------------------------

margins_model_1 <- margins(model_1)
margins_model_2 <- margins(model_2)
margins_model_2_variation <- margins(model_2_variation)
margins_model_3 <- margins(model_3)

margins_model_1_table <- summary(margins_model_1)
margins_model_2_table <- summary(margins_model_2)
margins_model_2_variation_table <- summary(margins_model_2_variation)
margins_model_3_table <- summary(margins_model_3)


# Add "fake" rows of intercept and age squared and interaction terms

for (margins_table in c(
  "margins_model_1_table",
  "margins_model_2_table",
  "margins_model_2_variation_table",
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
          "socialmedia",
          "posttrump"
        )
      )
    ) %>%
    add_row(factor = "(Intercept)", .before = 1) %>%
    add_row(factor = "I(age ^ 2)", .after = 12)
  
  assign(margins_table, margins_table_new)
  
  rm(margins_table_new)
}

margins_model_2_table <- margins_model_2_table %>%
  add_row(factor = "year:moderate") %>%
  add_row(factor = "year:conservative")

margins_model_2_variation_table <- margins_model_2_variation_table %>%
  add_row(factor = "year:moderate") %>%
  add_row(factor = "year:conservative") %>%
  add_row(factor = "year:socialmedia") %>%
  add_row(factor = "moderate:socialmedia") %>%
  add_row(factor = "conservative:socialmedia") %>%
  add_row(factor = "year:moderate:socialmedia") %>%
  add_row(factor = "year:conservative:socialmedia")

margins_model_3_table <- margins_model_3_table %>%
  add_row(factor = "moderate:postreagan") %>%
  add_row(factor = "moderate:bush") %>%
  add_row(factor = "moderate:socialmedia") %>%
  add_row(factor = "moderate:posttrump") %>%
  add_row(factor = "conservative:postreagan") %>%
  add_row(factor = "conservative:bush") %>%
  add_row(factor = "conservative:socialmedia") %>%
  add_row(factor = "conservative:posttrump")


## Export marginal effects table to LaTeX ---------------------------------

texreg_custom(
  l = list(model_1, model_2, model_2_variation, model_3),
  # Use "path" instead of "file" when calling texreg_custom()
  path = "../reports/figures/table-logit-pre-2021-margins.tex",
  stars = c(0.01, 0.05, 0.1),
  custom.model.names = c("Model 1", "Model 2", "Model 2 Var.", "Model 3"),
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
    "Post-Reagan (1981--2018)",
    "Bush (2001--2008)",
    "Social Media Era (2010--2018)",
    "Post-Trump (2017--2018)",
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
    "AIC" = c(
      formatC(round(AIC(model_1)), format = "d", big.mark = ","),
      formatC(round(AIC(model_2)), format = "d", big.mark = ","),
      formatC(round(AIC(model_2_variation)), format = "d", big.mark = ","),
      formatC(round(AIC(model_3)), format = "d", big.mark = ",")
    ),
    "BIC" = c(
      formatC(round(BIC(model_1)), format = "d", big.mark = ","),
      formatC(round(BIC(model_2)), format = "d", big.mark = ","),
      formatC(round(BIC(model_2_variation)), format = "d", big.mark = ","),
      formatC(round(BIC(model_3)), format = "d", big.mark = ",")
    ),
    "Log Likelihood" = c(
      formatC(round(logLik(model_1)[1]), format = "d", big.mark = ","),
      formatC(round(logLik(model_2)[1]), format = "d", big.mark = ","),
      formatC(round(logLik(model_2_variation)[1]), format = "d", big.mark = ","),
      formatC(round(logLik(model_3)[1]), format = "d", big.mark = ",")
    ),
    "Num. obs." = c(
      formatC(nobs(model_1), format = "d", big.mark = ","),
      formatC(nobs(model_2), format = "d", big.mark = ","),
      formatC(nobs(model_2_variation), format = "d", big.mark = ","),
      formatC(nobs(model_3), format = "d", big.mark = ",")
    )
  ),
  # Don't include \\item in custom.note
  custom.note = "%stars",
  digits = 3,
  override.coef = list(
    margins_model_1_table$AME,
    margins_model_2_table$AME,
    margins_model_2_variation_table$AME,
    margins_model_3_table$AME
  ),
  override.se = list(
    margins_model_1_table$SE,
    margins_model_2_table$SE,
    margins_model_2_variation_table$SE,
    margins_model_3_table$SE
  ),
  override.pvalues = list(
    margins_model_1_table$p,
    margins_model_2_table$p,
    margins_model_2_variation_table$p,
    margins_model_3_table$p
  ),
  omit.coef = "Intercept|I.age|year.moderate|year.conservative|year.socialmedia|moderate.postreagan|moderate.bush|moderate.posttrump|moderate.socialmedia|conservative.postreagan|conservative.bush|conservative.socialmedia|conservative.posttrump",
  caption = "Average Marginal Effects of Logit Models (I)",
  caption.above = TRUE,
  label = "table:LogitPre2021Margins",
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


# Marginal effects (including 2021) ---------------------------------------

margins_model_4 <- margins(model_4)
margins_model_5 <- margins(model_5)
margins_model_5_variation <- margins(model_5_variation)
margins_model_6 <- margins(model_6)

margins_model_4_table <- summary(margins_model_4)
margins_model_5_table <- summary(margins_model_5)
margins_model_5_variation_table <- summary(margins_model_5_variation)
margins_model_6_table <- summary(margins_model_6)


# Add "fake" rows of intercept and age squared and interaction terms

for (margins_table in c(
  "margins_model_4_table",
  "margins_model_5_table",
  "margins_model_5_variation_table",
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
          "socialmedia",
          "posttrump",
          "covid19"
        )
      )
    ) %>%
    add_row(factor = "(Intercept)", .before = 1) %>%
    add_row(factor = "I(age ^ 2)", .after = 10)
  
  assign(margins_table, margins_table_new)
  
  rm(margins_table_new)
}

margins_model_5_table <- margins_model_5_table %>%
  add_row(factor = "year:moderate") %>%
  add_row(factor = "year:conservative")

margins_model_5_variation_table <- margins_model_5_variation_table %>%
  add_row(factor = "year:moderate") %>%
  add_row(factor = "year:conservative") %>%
  add_row(factor = "year:socialmedia") %>%
  add_row(factor = "moderate:socialmedia") %>%
  add_row(factor = "conservative:socialmedia") %>%
  add_row(factor = "year:moderate:socialmedia") %>%
  add_row(factor = "year:conservative:socialmedia")

margins_model_6_table <- margins_model_6_table %>%
  add_row(factor = "moderate:postreagan") %>%
  add_row(factor = "moderate:bush") %>%
  add_row(factor = "moderate:socialmedia") %>%
  add_row(factor = "moderate:posttrump") %>%
  add_row(factor = "moderate:covid19") %>%
  add_row(factor = "conservative:postreagan") %>%
  add_row(factor = "conservative:bush") %>%
  add_row(factor = "conservative:socialmedia") %>%
  add_row(factor = "conservative:posttrump") %>%
  add_row(factor = "conservative:covid19")


## Export marginal effects table to LaTeX ---------------------------------

texreg_custom(
  l = list(model_4, model_5, model_5_variation, model_6),
  # Use "path" instead of "file" when calling texreg_custom()
  path = "../reports/figures/table-logit-incl-2021-margins.tex",
  stars = c(0.01, 0.05, 0.1),
  custom.model.names = c("Model 4", "Model 5", "Model 5 Var.", "Model 6"),
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
    "Social Media Era (2010--2021)",
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
    "AIC" = c(
      formatC(round(AIC(model_4)), format = "d", big.mark = ","),
      formatC(round(AIC(model_5)), format = "d", big.mark = ","),
      formatC(round(AIC(model_5_variation)), format = "d", big.mark = ","),
      formatC(round(AIC(model_6)), format = "d", big.mark = ",")
    ),
    "BIC" = c(
      formatC(round(BIC(model_4)), format = "d", big.mark = ","),
      formatC(round(BIC(model_5)), format = "d", big.mark = ","),
      formatC(round(BIC(model_5_variation)), format = "d", big.mark = ","),
      formatC(round(BIC(model_6)), format = "d", big.mark = ",")
    ),
    "Log Likelihood" = c(
      formatC(round(logLik(model_4)[1]), format = "d", big.mark = ","),
      formatC(round(logLik(model_5)[1]), format = "d", big.mark = ","),
      formatC(round(logLik(model_5_variation)[1]), format = "d", big.mark = ","),
      formatC(round(logLik(model_6)[1]), format = "d", big.mark = ",")
    ),
    "Num. obs." = c(
      formatC(nobs(model_4), format = "d", big.mark = ","),
      formatC(nobs(model_5), format = "d", big.mark = ","),
      formatC(nobs(model_5_variation), format = "d", big.mark = ","),
      formatC(nobs(model_6), format = "d", big.mark = ",")
    )
  ),
  # Don't include \\item in custom.note
  custom.note = '%stars\\\\[0.6em]\n {\\it Note:} Var., Mod., Cons., and S.M.E. are acronyms for variation, moderate, conservative, and social media era, respectively. The variables "Non-White" and "South" are excluded from Model 4, 5, and 6.',
  digits = 3,
  override.coef = list(
    margins_model_4_table$AME,
    margins_model_5_table$AME,
    margins_model_5_variation_table$AME,
    margins_model_6_table$AME
  ),
  override.se = list(
    margins_model_4_table$SE,
    margins_model_5_table$SE,
    margins_model_5_variation_table$SE,
    margins_model_6_table$SE
  ),
  override.pvalues = list(
    margins_model_4_table$p,
    margins_model_5_table$p,
    margins_model_5_variation_table$p,
    margins_model_6_table$p
  ),
  omit.coef = "Intercept|I.age|year.moderate|year.conservative|year.socialmedia|moderate.postreagan|moderate.bush|moderate.socialmedia|moderate.posttrump|moderate.covid19|conservative.postreagan|conservative.bush|conservative.socialmedia|conservative.posttrump|conservative.covid19",
  caption = "Average Marginal Effects of Logit Models (II)",
  caption.above = TRUE,
  label = "table:LogitIncl2021Margins",
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


# Export models -----------------------------------------------------------

save(
  model_1,
  model_2,
  model_2_variation,
  model_3,
  model_4,
  model_5,
  model_5_variation,
  model_6,
  margins_model_1,
  margins_model_2,
  margins_model_2_variation,
  margins_model_3,
  margins_model_4,
  margins_model_5,
  margins_model_5_variation,
  margins_model_6,
  file = "../data/models.RData"
)
