# Bachelor Thesis Clemens Heithecker

The effect of political ideology on the trust in science in the United States.

## Data Loading and Cleaning

To load the data, run the script [data-cleaning.R](scripts/data-cleaning.R) which extracts the relevant varaibles from the General Social Survey (GSS) 2021 data file. In addition, the script transforms the raw data and creates relevant variables. Finally, it exports the raw dataset as `gss-raw` and the transformed dataset as `gss` in the [data](data) directory, both as CSV and RData files.

## References

Davern, Michael; Bautista, Rene; Freese, Jeremy; Morgan, Stephen L.; and Tom W. Smith. General Social Survey 2021 Cross-section. [Machine-readable data file]. Principal Investigator, Michael Davern; Co-Principal Investigators, Rene Bautista, Jeremy Freese, Stephen L. Morgan, and Tom W. Smith. NORC ed. Chicago, 2021. 1 datafile (68,846 cases) and 1 codebook (506 pages).

Gauchat, G. (2012). Politicization of science in the public sphere: A study of public trust in the United States, 1974 to 2010. _American sociological review_, 77(2), 167-187.
