# preliminaries 
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, readr, readxl, scales, acs, tidyr)

# import the data 
final.data <- read_tsv("data/output/acs_medicaid.txt")



# 1. Plot the share of the adult population with direct purchase health insurance over time.
direct.share <- final.data %>%
  group_by(year) %>%
  summarize(
    total_direct = sum(ins_direct, na.rm = TRUE),
    total_adult_pop = sum(adult_pop, na.rm = TRUE),
    share_direct = total_direct / total_adult_pop
  )

### plot
direct.share.plt <- ggplot(direct.share, aes(x = year, y = share_direct)) +
  geom_line(size = 1.2, color = "steelblue") +
  geom_point(color = "steelblue") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Share of Adult Population with Direct Purchase Insurance (2012–2019)",
    x = "Year",
    y = "Share with Direct Purchase Insurance"
  ) +
  theme_minimal()

print(direct.share.plt)



# 3. Plot the share of the adult population with Medicaid over time.
medicaid.share <- final.data %>%
  group_by(year) %>%
  summarize(
    total_medicaid = sum(ins_medicaid, na.rm = TRUE),
    total_adult_pop = sum(adult_pop, na.rm = TRUE),
    share_medicaid = total_medicaid / total_adult_pop
  )

### plot
medicaid.share.plt <- ggplot(medicaid.share, aes(x = year, y = share_medicaid)) +
  geom_line(size = 1.2, color = "darkgreen") +
  geom_point(color = "darkgreen") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Share of Adult Population with Medicaid (2012–2019)",
    x = "Year",
    y = "Share with Medicaid"
  ) +
  theme_minimal()

print(medicaid.share.plt)



# 4. Plot the share of uninsured over time, separately by states that expanded Medicaid in 2014 versus those that did not. Drop all states that expanded after 2014.
### states that expanded in 2014 
expanded <- final.data %>%
  group_by(State) %>%
  summarize(first_expand_year = unique(year(date_adopted))) %>%
  mutate(
    expand_group = case_when(
      is.na(first_expand_year) ~ "Never Expanded",
      first_expand_year == 2014 ~ "Expanded in 2014",
      TRUE ~ NA_character_  # Drop states with other expansion years
    )
  ) %>%
  filter(!is.na(expand_group))

### join to main data 
final.data.exp <- final.data %>%
  inner_join(expanded, by = "State")

### calculate uninsured share by year and expansion group
uninsured.share <- final.data.exp %>%
  group_by(year, expand_group) %>%
  summarize(
    total_uninsured = sum(uninsured, na.rm = TRUE),
    total_adult_pop = sum(adult_pop, na.rm = TRUE),
    share_uninsured = total_uninsured / total_adult_pop,
    .groups = "drop"
  )

### plot
uninsured.plot <- ggplot(uninsured.share, aes(x = year, y = share_uninsured, color = expand_group)) +
  geom_line(size = 1.2) +
  geom_point() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Uninsured Rate by Medicaid Expansion Status (2012–2019)",
    x = "Year",
    y = "Share Uninsured",
    color = "Expansion Status"
  ) +
  theme_minimal()

print(uninsured.plot)
