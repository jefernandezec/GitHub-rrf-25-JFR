# Reproducible Research Fundamentals 
# 01. Data processing

### Libraries
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(labelled)

### Loading data ----

# Load the dataset
data_path <- "C:/Users/wb553773/OneDrive - WBG/Training/Credible Analytics/DataWork/Data/"
data      <- read_dta(file.path(data_path, "Raw/TZA_CCT_baseline.dta"))

nrow(data)

data %>%
    select(hhid) %>%
    n_distinct()

### Remove duplicates based on hhid
data_dedup <- data %>%
    distinct(hhid, .keep_all = TRUE)

### Household (HH) level data ----

#### Tidying data for HH level
data_tidy_hh <- data_dedup %>%
    select(vid,hhid,enid,floor:n_elder,food_cons:last_col())

### Data cleaning for Household-member (HH-member) level
data_clean_hh <- data_tidy_hh %>%
    # Convert submissionday to date
    mutate(submissiondate = as.Date(submissionday, format = "%Y-%m-%d %H:%M:%S")) %>%
    # Convert duration to numeric (if it is not already)
    mutate(duration=as.numeric(duration)) %>%
    # Convert ar_farm_unit to factor (categorical data)
    mutate(ar_farm_unit=as.factor(ar_farm_unit)) %>%
    # Replace values in the crop variable based on crop_other using regex for new crops
    mutate(crop = case_when(
        ......
    )) %>%
    # Recode negative numeric values (-88) as missing (NA)
    mutate(across(......)) %>%
    # Add variable labels
    set_variable_labels(
        ......
    )

# Save the household data
write_dta(data_clean_hh, file.path(data_path, "Intermediate/TZA_CCT_HH.dta"))

### Household member (HH-member) level data ----

#### Tidying data for HH-member level

data_tidy_hh_member <- data_dedup %>%
    select(vid, hhid, enid,
           starts_with("gender"),
           starts_with("age"),
           starts_with("read"),
           starts_with("clinic_visit"),
           starts_with("sick"),
           starts_with("days_sick"),
           starts_with("treat_fin"),
           starts_with("treat_cost"),
           starts_with("ill_impact"),
           starts_with("days_impact")) %>%
    pivot_longer(cols = -c(vid, hhid, enid),
                 names_to = c(".value", "member"),
                 names_pattern = "(.*) (\\d+)")

### Data cleaning for HH-member level
data_clean_mem <- data_tidy_mem %>%
    # Drop rows where gender is missing (NA)
    ...... %>%
    # Variable labels
    ......

# Save the tidy household-member data
write_dta(data_clean_mem, file.path(data_path, "Intermediate/TZA_CCT_HH_mem.dta"))

### Secondary data ----

# Load CSV data
secondary_data <- read.csv(file.path(data_path, "Raw/TZA_amenity.csv"))

# Tidying data
secondary_data <- secondary_data %>%
    pivot_wider(names_from = ......,
                values_from = ......,
                names_prefix = ......)

# Save the final tidy secondary data
write_dta(secondary_data, file.path(data_path, "Intermediate/TZA_amenity_tidy.dta"))
