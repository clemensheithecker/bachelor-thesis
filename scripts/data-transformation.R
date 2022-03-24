# Load libraries ----------------------------------------------------------

# Manipulate metadata as variable and value labels
library(labelled)

library(tidyverse)


glimpse(gss)


# Variable "consci" transformation ----------------------------------------

# I only consider two cases for confidence in science namely whether an
# individual shares "a great deal" of trust or not

levels(gss$consci)
unique(gss$consci)

# Change factor levels
gss$consci <-
  recode_factor(
    gss$consci,
    `a great deal` = 1,
    `only some` = 0,
    `hardly any` = 0
  )

unique(gss$consci)

# Convert factor level to numeric
gss <- gss %>%
  # Convert factor level to numeric
  mutate(consci = as.numeric(as.character(consci)))

glimpse(gss)

# Add variable label
var_label(gss$consci) <- "confidence in scientific community"


# Variable "sex" transformation -------------------------------------------

# Transform "sex" into a binary female variable

# Change factor levels
gss$sex <- recode_factor(gss$sex, `female` = 1, `male` = 0)

gss <- gss %>%
  # Convert factor level to numeric
  mutate(sex = as.numeric(as.character(sex))) %>%
  # Rename variable
  rename(female = sex)

glimpse(gss)

# Add variable label
var_label(gss$female) <- "respondent's gender dummy"


# Variable "race" transformation ------------------------------------------

# I only consider two cases for race namely whether an individual is non-white
# or not

levels(gss$race)
unique(gss$race)

# Change factor levels
gss$race <-
  recode_factor(
    gss$race,
    `black` = 1,
    `other` = 1,
    `white` = 0
  )

unique(gss$race)

# Convert factor level to numeric
gss <- gss %>%
  # Convert factor level to numeric
  mutate(race = as.numeric(as.character(race))) %>%
  # Rename variable
  rename(nonwhite = race)

glimpse(gss)

# Add variable label
var_label(gss$nonwhite) <- "respondent's race dummy"


# Transform "educ" variable -----------------------------------------------

glimpse(gss)

unique(gss$educ)

# Change factor levels
gss$educ <- forcats::fct_recode(
  gss$educ,
  "0" = "no formal schooling")

unique(gss$educ)

gss <- gss %>%
  # Convert factor level to numeric
  mutate(educ = as.numeric(as.character(educ)))

glimpse(gss)

# Add variable label
var_label(gss$educ) <- "highest year of school completed"


# Transform "degree" variable ---------------------------------------------

# Transform "degree" into multiple binary variables ("highschool", "bachelor",
# and "graduate")

levels(gss$degree)

gss <- gss %>%
  # Create new columns conditional on degree column
  mutate(
    highschool = if_else(degree == "high school", 1, 0),
    bachelor = if_else(degree == "bachelor's", 1, 0),
    graduate = if_else(degree == "graduate", 1, 0)
  ) %>%
  # Remove "degree" column
  select(-degree)

glimpse(gss)

# Add variable labels
var_label(gss$highschool) <- "r's highest degree dummy"
var_label(gss$bachelor) <- "r's highest degree dummy"
var_label(gss$graduate) <- "r's highest degree dummy"


# Variable "region" transformation ----------------------------------------

# Transform "region" into a binary South variable

levels(gss$region)

# Change factor levels
gss$region <- recode_factor(
  gss$region,
  `south atlantic` = 1,
  `east south atlantic` = 1,
  `west south central` = 1,
  `new england` = 0,
  `middle atlantic` = 0,
  `east north central` = 0,
  `west north central` = 0,
  `mountain` = 0,
  `pacific` = 0)

levels(gss$region)

gss <- gss %>%
  # Convert factor level to numeric
  mutate(region = as.numeric(as.character(region))) %>%
  # Rename variable
  rename(south = region)

glimpse(gss)

# Add variable label
var_label(gss$south) <- "region of interview dummy"


# Transform "attend" variable ---------------------------------------------

# Transform "attend" into a binary variable representing regular church
# attendance ("nearly every week", "every week", and "several times a week")

levels(gss$attend)

# Change factor levels
gss$attend <- recode_factor(
  gss$attend,
  `never` = 0,
  `less than once a year` = 1,
  `about once or twice a year` = 2,
  `2-3 times a year` = 3,
  `several times a year` = 4,
  `about once a month` = 5,
  `nearly every week` = 6,
  `every week` = 7,
  `several times a week` = 8)

levels(gss$attend)

gss <- gss %>%
  # Convert factor level to numeric
  mutate(attend = as.numeric(as.character(attend)))

glimpse(gss)

# Add variable label
var_label(gss$attend) <- "how often r attends religious services dummy"


# Transform "age" variable ------------------------------------------------

glimpse(gss)

unique(gss$age)
length(unique(gss$age))

gss <- gss %>%
  # Remove individuals "89 or older"
  filter(age != "89 or older") %>%
  # Drop unused factor levels (namely "89 or older")
  mutate(age = droplevels(age)) %>%
  # Convert factor level to numeric
  mutate(age = as.numeric(as.character(age))) %>%
  # Divide "age" by 10
  mutate(age = age / 10)

length(unique(gss$age))

# Add variable label
var_label(gss$age) <- "respondent's age"


# Transform "partyid" variable --------------------------------------------

# Transform "partyid" into multiple binary variables ("independent" and
# "republican")

levels(gss$partyid)

gss <- gss %>%
  # Create new columns conditional on partyid column
  mutate(
    independent = if_else(partyid %in% c(
      "independent, close to democrat",
      "independent (neither, no response)",
      "independent, close to republican"
      ), 1, 0),
    republican = if_else(partyid %in% c(
      "not very strong republican",
      "strong republican"
      ), 1, 0)
  ) %>%
  # Remove "partyid" column
  select(-partyid)

glimpse(gss)

# Add variable labels
var_label(gss$independent) <- "political party affiliation dummy"
var_label(gss$republican) <- "political party affiliation dummy"


# Transform "polviews" variable -------------------------------------------

# Transform "polviews" into multiple binary variables ("moderate" and
# "conservative")

levels(gss$polviews)

gss <- gss %>%
  # Create new columns conditional on polviews column
  mutate(
    moderate = if_else(polviews == "moderate, middle of the road", 1, 0),
    conservative = if_else(polviews == "conservative", 1, 0)
  ) %>%
  # Remove "polviews" column
  select(-polviews)

glimpse(gss)

# Add variable labels
var_label(gss$moderate) <- "think of self as liberal or conservative dummy"
var_label(gss$conservative) <- "think of self as liberal or conservative dummy"


# Create "postreagan" variable --------------------------------------------

# Create a binary "postreagan" variable indicating the years of and after
# Ronald Reagan's presidency (years after 1980)

gss <- gss %>%
  # Create new column conditional on the year column
  mutate(
    postreagan = if_else(year > 1980, 1, 0)
  )

summary(gss$postreagan)

# Add variable label
var_label(gss$postreagan) <- "post-reagan (since 1981) dummy"


# Create "bush" variable --------------------------------------------------

# Create a binary "bush" variable indicating the years of George W. Bush's
# presidency (years between 2001 and 2008)

gss <- gss %>%
  # Create new column conditional on the year column
  mutate(
    bush = if_else(year >= 2001 & year <= 2008, 1, 0)
  )

summary(gss$bush)

# Add variable label
var_label(gss$bush) <- "bush (2001-2008) dummy"


# Create "posttrump" variable ---------------------------------------------

# Create a binary "posttrump" variable indicating the years of and after
# Donald Trump's presidency (years after 2017)

gss <- gss %>%
  # Create new column conditional on the year column
  mutate(
    posttrump = if_else(year >= 2017, 1, 0)
  )

summary(gss$posttrump)

# Add variable label
var_label(gss$posttrump) <- "post-trump (since 2017) dummy"


# Create "covid19" variable -----------------------------------------------

# Create a binary "covid19" variable indicating the years of the COVID-19
# pandemic (years after 2020 as of conducting this analysis)

gss <- gss %>%
  # Create new column conditional on the year column
  mutate(
    covid19 = if_else(year > 2020, 1, 0)
  )

summary(gss$covid19)

# Add variable label
var_label(gss$covid19) <- "covid-19 (2020-2022) dummy"
