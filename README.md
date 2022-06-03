# Bachelor Thesis Clemens Heithecker

The effect of political ideology on the trust in science in the United States. The data used in this analysis comes from the General Social Survey (GSS) from 1972 to 2021 (Davern et al. 2021).

## Data Loading and Cleaning

To load the data, run the script [data-cleaning.R](scripts/data-cleaning.R) which extracts the relevant varaibles from the General Social Survey (GSS) 2021 data file. The script transforms the raw data and exports it into the [/data](data) directory, both as CSV and RData files.

I created three different datasets:

| Name          | Description                                        |
| ------------- | -------------------------------------------------- |
| `gss_raw`     | All relevant variables with data type adjustments. |
| `gss_with_na` | Transformed data _with_ missing values.            |
| `gss`         | Transformed data _without_ missing values.         |

## Analysis

I model the interaction of political ideology and public trust in science in the United States using four types of models. Similar to Gauchat (2012), the models of this bachelor thesis fall into the following categories: uniform continous-time effects, group-specific continous-time effects, and group-specific cultrual break effects. I extend Gauchat's (2012) analysis by including post-Trump and COVID-19 time effects. Furthermore, I run a variation of the group-specific continous-time models which includes a post-2010s (2010 and after) cultural break to capture trend changes. Models 1, 2, 2 Variation, and 3 are analogous to models 4, 5, 5 Variation, and 6. However, the first set of models uses data _before_ the survey year 2020/2021 and the second set data _including_ the year.

| Model               | Description                                                                           |
| ------------------- | ------------------------------------------------------------------------------------- |
| `Model 1`           | Uniform continuous-time effects, _excluding_ 2020/2021.                               |
| `Model 2`           | Group-specific continuous-time effects, _excluding_ 2020/2021.                        |
| `Model 2 Variation` | `Model 2` _with_ a post-2010s (2010 and after) cultural break, _excluding_ 2020/2021. |
| `Model 3`           | Group-specific cultural break effects, _excluding_ 2020/2021.                         |
| `Model 4`           | Uniform continuous-time effects, _including_ 2020/2021.                               |
| `Model 5`           | Group-specific continuous-time effects, _including_ 2020/2021.                        |
| `Model 5 Variation` | `Model 5` _with_ a post-2010s (2010 and after) cultural break, _including_ 2020/2021. |
| `Model 6`           | Group-specific cultural break effects, _including_ 2020/2021.                         |

Please refer to [figures-presentation.pdf](reports/figures-presentation.pdf) for a graphical representation of the data analysis, including data exploration and predictions. For an overview of the regression results, please look at [tables.pdf](reports/tables.pdf).

## References

Davern, Michael; Bautista, Rene; Freese, Jeremy; Morgan, Stephen L.; and Tom W. Smith. General Social Survey 2021 Cross-section. [Machine-readable data file]. Principal Investigator, Michael Davern; Co-Principal Investigators, Rene Bautista, Jeremy Freese, Stephen L. Morgan, and Tom W. Smith. NORC ed. Chicago, 2021. 1 datafile (68,846 cases) and 1 codebook (506 pages).

Gauchat, G. (2012). Politicization of science in the public sphere: A study of public trust in the United States, 1974 to 2010. _American sociological review_, 77(2), 167-187.
