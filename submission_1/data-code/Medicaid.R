if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, readr, readxl, scales, acs, tidyr)

kff.data <- read_csv('data/input/KFF_medicaid_expansion.csv')

# clean KFF data 
kff.final <- kff.data %>%
  mutate(
    expanded = (`Expansion Status` == 'Adopted and Implemented'),
    Description = str_replace_all(Description, c("\n" = "", '"' = "")),
    first_date_str = str_extract(Description, "\\d{1,2}/\\d{1,2}/\\d{4}"),
    date_adopted = mdy(first_date_str)
  ) %>%
  select(State, expanded, date_adopted)

write_tsv(kff.final,'data/output/medicaid_expansion.txt')
