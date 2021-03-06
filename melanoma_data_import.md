Melanoma Data Import
================
Benjamin Goebel
11/30/2021

## Incidence of Melanoma by Year

#### New York

``` r
# Read in the data and set the column names
# Add a state column and make all rows = "NY"
# Make the year column of type integer
# Add a cancer type column and make all rows = "melanoma"
# Select the state, year, cancer_type and mf_case_rate columns
# Rename mf_case_rate column "age_adjusted_incidence_rate"
nys_melanoma_age_adjusted <- read_csv(here("data",
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
  mutate(state = "NY",
         year = as.integer(year),
         cancer_type = "melanoma") %>%
  select(state, year, cancer_type, mf_case_rate) %>%
  rename(age_adjusted_incidence_rate = mf_case_rate)
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
# Remove total row
# Add a state column and set all rows = "OH"
# Convert type of year column to be integer
# Add a cancer type column and set all rows = "melanoma"
# Convert type of age_adjusted_incidence_rate column to be double
# Reorder columns: state, year, cancer_type, age_adjusted_incidence_rate
ohio_melanoma_age_adjusted <- ohio_cancer_age_adjusted %>%
  filter(type == "Melanoma of Skin") %>%
  pivot_longer("1996":total,
               names_to = "year",
               values_to = "age_adjusted_incidence_rate") %>%
  select(year, age_adjusted_incidence_rate) %>%
  filter(year != "total") %>%
  mutate(state = "OH", 
         year = as.integer(year),
         cancer_type = "melanoma",
         age_adjusted_incidence_rate = as.double(age_adjusted_incidence_rate)) %>%
  select(state, year, cancer_type, age_adjusted_incidence_rate)

# Final data frames: ohio_melanoma_cases and ohio_melanoma_age_adjusted
```

#### Pennsylvania

``` r
# Read in data, skip first three lines
# Clean variable names
# Select relevant variables
# Add a state column and set all rows = "PA"
# Convert type of year column to be integer
# Add a cancer_type column and set all rows = "melanoma"
# Rename rate_ratio_result column to "age_adjusted_incidence_rate"
# Reorder columns: state, year, cancer_type, age_adjusted_incidence_rate
# Sort data frame by ascending year
pa_melanoma_age_adjusted <- read_csv(here("data",
                                          "incidence_melanoma_data",
                                          "pa_melanoma_incidence_year.csv"),
                                     skip = 3) %>%
                            janitor::clean_names() %>%
  select(year, rate_ratio_result) %>%
  mutate(state = "PA",
         year = as.integer(year),
         cancer_type = "melanoma") %>%
  rename(age_adjusted_incidence_rate = rate_ratio_result) %>%
  select(state, year, cancer_type, age_adjusted_incidence_rate) %>%
  arrange(year)
```

Now, we can merge the state melanoma incidence data together.

``` r
# Merge state-level data frames with bind_rows
state_melanoma_age_adjusted <- 
  bind_rows(nys_melanoma_age_adjusted,
            ohio_melanoma_age_adjusted,
            pa_melanoma_age_adjusted)
```

Let???s visualize the merged state melanoma incidence data.

``` r
# Visualize the merged state melanoma incidence data with line graph
state_melanoma_age_adjusted %>%
  ggplot(aes(x = year, y = age_adjusted_incidence_rate, color = state)) +
  geom_line() +
  labs(
    title = "Age-adjusted incidence rate of Melanoma by Year colored by State",
    x = "Year",
    y = "Age-adjusted incidence rate of Melanoma", 
    color = "State"
  ) +
  scale_x_continuous(breaks = seq(1976, 2018, by = 4)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
```

![](melanoma_data_import_files/figure-gfm/state_melanoma_incidence_line_graph-1.png)<!-- -->

## Incidence of Melanoma 2014-2018 at the county level

We will use the following function to read in and clean the data.

``` r
# Purpose: Reads in the county-level data for a given state, cleans 
#          variable names and selects columns of interest.
# Arguments: file_path_name: A character, the file path name to read in
#            state: A character, the state
#            cancer_type: A character, the type of cancer
# Returns: A tibble, the data.
read_and_clean_county_data <- function(file_path_name, state, cancer_type) {
  read_csv(file_path_name,
           skip = 8) %>%
  janitor::clean_names() %>%
  select(county, age_adjusted_incidence_rate_rate_note_cases_per_100_000) %>%
  mutate(county = stringr::str_remove(county, "\\(.\\)$"),
        state = state,
         cancer_type = cancer_type,
         age_adjusted_incidence_rate =
           as.numeric(age_adjusted_incidence_rate_rate_note_cases_per_100_000)) %>%
  select(state, county, cancer_type, age_adjusted_incidence_rate)
}
```

We can get the county-level melanoma average incidence data for New
York, Ohio and Pennsylvania from 2014 to 2018 as follows.

#### New York

``` r
# Read in New York melanoma incidence county-level data averaged from 2014 to 
# 2018
nys_county_melanoma_incidence_2014_2018 <- 
read_and_clean_county_data(here("data",
                                "incidence_melanoma_data",
                                "nys_county_melanoma_incidence_2014_2018.csv"),
                           "NY",
                           "melanoma")
```

#### Ohio

``` r
# Read in Ohio melanoma incidence county-level data averaged from 2014 to 2018
ohio_county_melanoma_incidence_2014_2018 <- 
read_and_clean_county_data(here("data",
                                "incidence_melanoma_data",
                                "ohio_county_melanoma_incidence_2014_2018.csv"),
                           "OH",
                           "melanoma")
```

    ## Warning: One or more parsing issues, see `problems()` for details

#### Pennsylvania

``` r
# Read in Pennsylvania melanoma incidence county-level data averaged from 
# 2014 to 2018
pa_county_melanoma_incidence_2014_2018 <- 
read_and_clean_county_data(here("data",
                                "incidence_melanoma_data",
                                "pa_county_melanoma_incidence_2014_2018.csv"),
                           "PA",
                           "melanoma")
```

Now, we can merge the county melanoma incidence data together.

``` r
# Merge county-level data frames with bind_rows
state_county_melanoma_incidence_2014_2018 <- 
  bind_rows(nys_county_melanoma_incidence_2014_2018,
            ohio_county_melanoma_incidence_2014_2018,
            pa_county_melanoma_incidence_2014_2018)
```

``` r
write_csv(state_county_melanoma_incidence_2014_2018, "./data/incidence_melanoma_data/state_county_melanoma_incidence_2014_2018.csv")

write_csv(state_melanoma_age_adjusted, "./data/incidence_melanoma_data/state_melanoma_age_adjusted.csv")
```
