library(purrr)

## see list of variable names and tables
vars <- load_variables(2019, "acs1", cache = TRUE)
view.vars <- vars %>% filter(str_detect(name, "B27010"))


# retrieve ACS data 
get.insurance.data <- function(year) {
  get_acs(
    geography = "state",
    table = "B27010",
    survey = "acs1",
    year = year,
    output = "wide"
  ) %>%
    transmute(
      State = NAME,
      year = year,
      all_18to34 = B27010_018E,
      employer_18to34 = B27010_020E,
      direct_18to34 = B27010_021E,
      medicare_18to34 = B27010_022E,
      medicaid_18to34 = B27010_023E,
      tricare_18to34 = B27010_024E,
      va_18to34 = B27010_025E,
      none_18to34 = B27010_033E,
      all_35to64 = B27010_034E,
      employer_35to64 = B27010_036E,
      direct_35to64 = B27010_037E,
      medicare_35to64 = B27010_038E,
      medicaid_35to64 = B27010_039E,
      tricare_35to64 = B27010_040E,
      va_35to64 = B27010_041E,
      none_35to64 = B27010_050E
    )
}


# run for all years 
years <- 2012:2019 
insurance.data <- map_dfr(years, get_insurance_data) 

# tidy the data 
final.insurance <- insurance.data %>%
  mutate(
    adult_pop = all_18to34 + all_35to64,
    ins_employer = employer_18to34 + employer_35to64,
    ins_direct = direct_18to34 + direct_35to64,
    ins_medicare = medicare_18to34 + medicare_35to64,
    ins_medicaid = medicaid_18to34 + medicaid_35to64,
    uninsured = none_18to34 + none_35to64
  ) %>%
  select(State, year, adult_pop, ins_employer, ins_direct,
         ins_medicare, ins_medicaid, uninsured)


# save the data 
write_tsv(final.insurance,'data/output/acs_insurance.txt')
