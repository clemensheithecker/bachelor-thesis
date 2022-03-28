# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Clear environment
rm(list = ls())


# Load libraries ----------------------------------------------------------

# Linear mixed effects models
library(lme4)

library(jtools)

library(margins)

# Create LaTeX code for well-formatted tables
library(stargazer)

library(tidyverse)

# Export data frame as LaTeX table
library(xtable)


# Load data ---------------------------------------------------------------

load("../data/gss.RData")

gss$realinc <- as.vector(gss$realinc)

glimpse(gss)
summary(gss)

df1 <- gss %>%
  select(-id, -covid19, -posttrump) %>%
  filter(year <= 2010)

df2 <- gss %>%
  select(-id, -covid19) %>%
  filter(year != 2021)


# Simple logistic regression model ----------------------------------------

reg1 <- glm(consci ~ ., data = df1, family = binomial(link = "logit"))

summary(reg1)

reg2 <- glm(consci ~ . + moderate * year + conservative * year +
              postreagan * moderate + bush * moderate +
              postreagan * conservative + bush * conservative - cohort,
            data = df1,
            family = binomial(link = "logit"))

summary(reg2)

reg3 <- glm(consci ~ . + moderate * year + conservative * year +
              postreagan * moderate + bush * moderate + posttrump * moderate +
              postreagan * conservative + bush * conservative +
              posttrump * conservative - cohort,
            data = df2,
            family = binomial(link = "logit"))

summary(reg3)

export_summs(reg2, reg3, to.file = "html")

reg2_margins <- margins(reg2)
reg3_margins <- margins(reg3)

export_summs(reg2_margins, reg3_margins, to.file = "html")

reg2_margins_liberal <- margins(reg2,
                                at = list(moderate = 0, conservative = 0))
reg3_margins_liberal <- margins(reg3,
                                at = list(moderate = 0, conservative = 0))

export_summs(reg2_margins_liberal, reg3_margins_liberal, to.file = "html")

reg4 <- glmer(consci ~ . + moderate * year + conservative * year +
                postreagan * moderate + bush * moderate +
                postreagan * conservative + bush * conservative - cohort +
                (1 | cohort),
              data = df1,
              family = binomial(link = "logit"),
              nAGQ = 0)
  
  glmer(consci ~ . - cohort + (1 | cohort), data = df1, family = "binomial")

summary(reg4)

reg5 <- glmer(consci ~ . + moderate * year + conservative * year +
                postreagan * moderate + bush * moderate + posttrump * moderate +
                postreagan * conservative + bush * conservative +
                posttrump * conservative - cohort + (1 | cohort),
            data = df2,
            family = binomial(link = "logit"))

summary(reg5)

export_summs(reg4, reg5, to.file = "html")

reg4_margins <- margins(reg4)
reg5_margins <- margins(reg5)

export_summs(reg4_margins, reg5_margins, to.file = "html")
