Melanoma Data Import
================
Benjamin Goebel
11/23/2021

## Incidence of Melanoma by Year

#### New York

``` r
# Read in the data and set the column names
nys_melanoma_incidence <- read_csv(here("data",
                                        "incidence_melanoma_data",
                                        "nys_melanoma_incidence_mortality_year.csv"),
                                   skip = 3,
                                   col_names = c("year",
                                                 "mf_cases",
                                                 "mf_case_rate",
                                                 "mf_case_rate_ci",
                                                 "m_cases",
                                                 "m_case_rate",
                                                 "m_case_rate_ci",
                                                 "f_cases",
                                                 "f_case_rate",
                                                 "f_case_rate_ci",
                                                 "mf_deaths",
                                                 "mf_death_rate",
                                                 "mf_death_rate_ci",
                                                 "m_deaths",
                                                 "m_death_rate",
                                                 "m_death_rate_ci",
                                                 "f_deaths",
                                                 "f_death_rate",
                                                 "f_death_rate_ci"
                                                 )) %>%
  select(year, mf_case_rate)
```

#### Ohio

``` r
# Read in data set
ohio_cancer_incidence <- readxl::read_xls(here("data",
                                               "incidence_melanoma_data",
                                               "ohio_cancer_incidence_year.xls"),
                                          skip = 1)
###############################################################################
# Melanoma cases

# Get cases for each year
ohio_cancer_cases <- ohio_cancer_incidence %>%
  select("Site/Type", starts_with("Case"))

# Update column names for the Ohio cancer cases data frame
colnames(ohio_cancer_cases) <- c("type", 1996:2018, "total")

# Get melanoma of the skin cancer cases for Ohio
# Pivot longer year
# Select year and cases variables
ohio_melanoma_cases <- ohio_cancer_cases %>%
  filter(type == "Melanoma of Skin") %>%
  pivot_longer("1996":total,
               names_to = "year",
               values_to = "cases") %>%
  select(year, cases)

###############################################################################
# Melanoma age-adjusted

# Get age-adjusted for each year
ohio_cancer_age_adjusted <- ohio_cancer_incidence %>%
  select("Site/Type", starts_with("Age"))

# Update column names for Ohio cancer age-adjusted data frame
colnames(ohio_cancer_age_adjusted) <- c("type", 1996:2018, "total")

# Get melanoma of the skin cancer age-adjusted for Ohio
# Pivot longer year
# Select year and age_adjusted variables
ohio_melanoma_age_adjusted <- ohio_cancer_age_adjusted %>%
  filter(type == "Melanoma of Skin") %>%
  pivot_longer("1996":total,
               names_to = "year",
               values_to = "age_adjusted") %>%
  select(year, age_adjusted)

# Final data frames: ohio_melanoma_cases and ohio_melanoma_age_adjusted
```

#### Pennsylvania

``` r
# Read in data, skip first three lines
# Clean variable names
# Select relevant variables
pa_melanoma_incidence <- read_csv(here("data",
                                       "incidence_melanoma_data",
                                       "pa_melanoma_incidence_year.csv"),
                                  skip = 3) %>%
                          janitor::clean_names() %>%
  select(year, rate_ratio_result)
```

## Incidence of Melanoma 2014-2018 at the county level

We will use the following function to read in and clean the data.

``` r
# Purpose: Reads in the county-level data for a given state, cleans 
#          variable names and selects columns of interest.
# Arguments: A character, the file path name to read in
# Returns: A tibble, the data.
read_and_clean_county_data <- function(file_path_name) {
  read_csv(file_path_name,
           skip = 8) %>%
  janitor::clean_names() %>%
  select(county, age_adjusted_incidence_rate_rate_note_cases_per_100_000)
}
```

#### New York

``` r
nys_county_melanoma_incidence_2014_2018 <- 
read_and_clean_county_data(here("data",
                                "incidence_melanoma_data",
                                "nys_county_melanoma_incidence_2014_2018.csv"))
```

#### Ohio

``` r
ohio_county_melanoma_incidence_2014_2018 <- 
read_and_clean_county_data(here("data",
                                "incidence_melanoma_data",
                                "ohio_county_melanoma_incidence_2014_2018.csv"))
```

#### Pennsylvania

``` r
pa_county_melanoma_incidence_2014_2018 <- 
read_and_clean_county_data(here("data",
                                "incidence_melanoma_data",
                                "pa_county_melanoma_incidence_2014_2018.csv"))
```
