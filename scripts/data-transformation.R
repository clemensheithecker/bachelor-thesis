# Load libraries ----------------------------------------------------------

# Manipulate metadata as variable and value labels
require(labelled)

require(tidyverse)


# Transform variables -----------------------------------------------------

glimpse(gss_with_na)


## consci variable --------------------------------------------------------

# I only consider two cases for confidence in science, namely whether an
# individual indicates "a great deal" of trust or not

levels(gss_with_na$consci)
unique(gss_with_na$consci)

# Get variable label
consci_label <- var_label(gss_with_na$consci)

# Change factor levels
gss_with_na$consci <-
  recode_factor(
    gss_with_na$consci,
    `a great deal` = 1,
    `only some` = 0,
    `hardly any` = 0
  )

unique(gss_with_na$consci)

# Convert factor level to numeric
gss_with_na <- gss_with_na %>%
  # Convert factor level to numeric
  mutate(consci = as.numeric(as.character(consci)))

glimpse(gss_with_na)

# Add variable label
var_label(gss_with_na$consci) <- consci_label

# Clean up
rm(consci_label)


## sex variable -----------------------------------------------------------

# Transform sex into a binary female variable

levels(gss_with_na$sex)
unique(gss_with_na$sex)

# Change factor levels
gss_with_na$sex <- recode_factor(gss_with_na$sex, `female` = 1, `male` = 0)

gss_with_na <- gss_with_na %>%
  # Convert factor level to numeric
  mutate(sex = as.numeric(as.character(sex))) %>%
  # Rename variable from sex to female
  rename(female = sex)

glimpse(gss_with_na)

# Add variable label
var_label(gss_with_na$female) <- "respondent's gender"


## race variable ----------------------------------------------------------

# I only consider two cases for race, namely whether an individual is non-white
# or otherwise (white)

levels(gss_with_na$race)
unique(gss_with_na$race)

# Change factor levels
gss_with_na$race <-
  recode_factor(
    gss_with_na$race,
    `black` = 1,
    `other` = 1,
    `white` = 0
  )

unique(gss_with_na$race)

# Convert factor level to numeric
gss_with_na <- gss_with_na %>%
  # Convert factor level to numeric
  mutate(race = as.numeric(as.character(race))) %>%
  # Rename variable from race to nonwhite
  rename(nonwhite = race)

glimpse(gss_with_na)

# Add variable label
var_label(gss_with_na$nonwhite) <- "respondent's race"


## educ variable ----------------------------------------------------------

glimpse(gss_with_na)
unique(gss_with_na$educ)

# Get variable label
educ_label <- var_label(gss_with_na$educ)

# Change factor levels
gss_with_na$educ <- forcats::fct_recode(
  gss_with_na$educ,
  "0" = "no formal schooling"
)

unique(gss_with_na$educ)

gss_with_na <- gss_with_na %>%
  # Convert factor level to numeric
  mutate(educ = as.numeric(as.character(educ)))

glimpse(gss_with_na)

# Add variable label
var_label(gss_with_na$educ) <- educ_label

# Clean up
rm(educ_label)


## degree variable --------------------------------------------------------


# Transform degree variable into multiple binary variables (highschool,
# bachelor, and graduate)

levels(gss_with_na$degree)

gss_with_na <- gss_with_na %>%
  # Create new columns conditional on degree column
  mutate(
    highschool = if_else(degree == "high school", 1, 0),
    bachelor = if_else(degree == "bachelor's", 1, 0),
    graduate = if_else(degree == "graduate", 1, 0)
  ) %>%
  # Remove degree column
  select(-degree)

glimpse(gss_with_na)

# Add variable labels
var_label(gss_with_na$highschool) <- "respondent's highest degree"
var_label(gss_with_na$bachelor) <- "respondent's highest degree"
var_label(gss_with_na$graduate) <- "respondent's highest degree"


## region variable --------------------------------------------------------

# Transform region into a binary south variable

levels(gss_with_na$region)

# Get variable label
region_label <- var_label(gss_with_na$region)

# Change factor levels
gss_with_na$region <- recode_factor(
  gss_with_na$region,
  `south atlantic` = 1,
  `east south atlantic` = 1,
  `west south central` = 1,
  `new england` = 0,
  `middle atlantic` = 0,
  `east north central` = 0,
  `west north central` = 0,
  `mountain` = 0,
  `pacific` = 0
)

levels(gss_with_na$region)

gss_with_na <- gss_with_na %>%
  # Convert factor level to numeric
  mutate(region = as.numeric(as.character(region))) %>%
  # Rename variable from region to south
  rename(south = region)

glimpse(gss_with_na)

# Add variable label
var_label(gss_with_na$south) <- region_label

# Clean up
rm(region_label)


## attend variable --------------------------------------------------------

# Transform attend into a continuous variable representing frequency of church
# attendance using the following order: ("never", "less than once a year",
# "about once or twice a year", "2-3 times a year", "several times a year",
# "about once a month", "nearly every week", "every week", and "several times a
# week")

levels(gss_with_na$attend)

# Get variable label
attend_label <- var_label(gss_with_na$attend)

# Change factor levels
gss_with_na$attend <- recode_factor(
  gss_with_na$attend,
  `never` = 0,
  `less than once a year` = 1,
  `about once or twice a year` = 2,
  `2-3 times a year` = 3,
  `several times a year` = 4,
  `about once a month` = 5,
  `nearly every week` = 6,
  `every week` = 7,
  `several times a week` = 8
)

levels(gss_with_na$attend)

gss_with_na <- gss_with_na %>%
  # Convert factor level to numeric
  mutate(attend = as.numeric(as.character(attend)))

glimpse(gss_with_na)

# Add variable label
var_label(gss_with_na$attend) <- attend_label

# Clean up
rm(attend_label)


## realinc variable -------------------------------------------------------

# Get variable label
realinc_label <- var_label(gss_with_na$realinc)

# Standardize the z-scores of family income.
gss_with_na <- gss_with_na %>%
  # Scale realinc to have mean = 0 and standard deviation = 1
  mutate(realinc = as.vector(scale(realinc)))

# Add variable label
var_label(gss_with_na$realinc) <- realinc_label

# Clean up
rm(realinc_label)


## age variable -----------------------------------------------------------

unique(gss_with_na$age)
length(unique(gss_with_na$age))

table(gss_with_na$age)

levels(gss_with_na$age)

# Change level "89 or older" to 89
levels(gss_with_na$age)[levels(gss_with_na$age) == "89 or older"] <- 89

levels(gss_with_na$age)

gss_with_na <- gss_with_na %>%
  # Convert factor level to numeric
  mutate(age = as.numeric(as.character(age))) %>%
  # Divide age by 10
  mutate(age = age / 10)

length(unique(gss_with_na$age))

# Add variable label
var_label(gss_with_na$age) <- "respondent's age"


## partyid variable -------------------------------------------------------

# Transform partyid into multiple binary variables (independent and republican)

levels(gss_with_na$partyid)

# Get variable label
partyid_label <- var_label(gss_with_na$partyid)

gss_with_na <- gss_with_na %>%
  # Create new columns conditional on partyid column
  mutate(
    independent = if_else(
      partyid == "independent, close to democrat" |
        partyid ==  "independent (neither, no response)" |
        partyid == "independent, close to republican",
      1, 0),
    republican = if_else(
      partyid == "not very strong republican" |
        partyid == "strong republican",
      1, 0)
  ) %>%
  # Remove observations where partyid is "other party"
  filter(partyid != "other party") %>%
  # Remove partyid column
  select(-partyid)

glimpse(gss_with_na)

# Add variable labels
var_label(gss_with_na$independent) <- partyid_label
var_label(gss_with_na$republican) <- partyid_label

# Clean up
rm(partyid_label)


## polviews variable ------------------------------------------------------

# Transform polviews into multiple binary variables (moderate and conservative)

levels(gss_with_na$polviews)

# Get variable label
polviews_label <- var_label(gss_with_na$polviews)

gss_with_na <- gss_with_na %>%
  # Create new columns conditional on polviews column
  mutate(
    moderate = if_else(polviews == "moderate, middle of the road", 1, 0),
    conservative = if_else(
      polviews == "extremely conservative" |
        polviews == "conservative" |
        polviews == "slightly conservative",
      1, 0)
  ) %>%
  # Remove polviews column
  select(-polviews)

glimpse(gss_with_na)

# Add variable labels
var_label(gss_with_na$moderate) <- polviews_label
var_label(gss_with_na$conservative) <- polviews_label

# Clean up
rm(polviews_label)


# postreagan variable -----------------------------------------------------

# Create a binary postreagan variable indicating the years of and after Ronald
# Reagan's presidency (years after 1980)

gss_with_na <- gss_with_na %>%
  # Create new column conditional on the year column
  mutate(
    postreagan = if_else(year >= 1981, 1, 0)
  )

summary(gss_with_na$postreagan)

# Add variable label
var_label(gss_with_na$postreagan) <- "post-reagan era (since 1981)"


## bush variable ----------------------------------------------------------


# Create a binary bush variable indicating the years of George W. Bush's
# presidency (years between 2001 and 2008)

gss_with_na <- gss_with_na %>%
  # Create new column conditional on the year column
  mutate(
    bush = if_else(year >= 2001 & year <= 2008, 1, 0)
  )

summary(gss_with_na$bush)

# Add variable label
var_label(gss_with_na$bush) <- "bush era (2001-2008)"


## socialmedia variable ---------------------------------------------------

# Create a binary socialmedia variable indicating the decade of 2010 and after,
# when social media significantly increased its popularity

gss_with_na <- gss_with_na %>%
  # Create new column conditional on the year column
  mutate(
    socialmedia = if_else(year >= 2010, 1, 0)
  )

summary(gss_with_na$socialmedia)

# Add variable label
var_label(gss_with_na$socialmedia) <- "social media era (since 2010)"


# posttrump variable ------------------------------------------------------

# Create a binary posttrump variable indicating the years of and after Donald
# Trump's presidency and election (years after 2016)

gss_with_na <- gss_with_na %>%
  # Create new column conditional on the year column
  mutate(
    posttrump = if_else(year >= 2016, 1, 0)
  )

summary(gss_with_na$posttrump)

# Add variable label
var_label(gss_with_na$posttrump) <- "post-trump era (since 2016)"


## covid19 variable -------------------------------------------------------

# Create a binary covid19 variable indicating the years of the COVID-19 pandemic
# (years including and after 2020, as of conducting this analysis)

gss_with_na <- gss_with_na %>%
  # Create new column conditional on the year column
  mutate(
    covid19 = if_else(year >= 2020, 1, 0)
  )

summary(gss_with_na$covid19)

# Add variable label
var_label(gss_with_na$covid19) <- "covid-19 era (2020-2022)"
