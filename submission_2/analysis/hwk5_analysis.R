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



###### For the rest of the assignment, we’re going to apply the difference-in-differences estimator to the question of Medicaid expansion and uninsurance.

# 5. Calculate the average percent of uninsured individuals in 2012 and 2015, separately for expansion and non-expansion states. Present your results in a basic 2x2 DD table.
### expansion status as of 2014
expansion.status <- final.data.exp %>%
  group_by(State) %>%
  summarize(first_expand_year = unique(year(date_adopted))) %>%
  mutate(
    group = case_when(
      first_expand_year == 2014 ~ "Expansion",
      is.na(first_expand_year) ~ "Non-Expansion",
      TRUE ~ NA_character_  # drop late expanders
    )
  ) %>%
  filter(!is.na(group))

### merge with final data
dd.data <- final.data.exp %>%
  filter(year %in% c(2012, 2015)) %>%
  inner_join(expansion.status, by = "State")

### compute mean uninsured share by year & group
dd.table <- dd.data %>%
  group_by(group, year) %>%
  summarize(
    avg_uninsured = sum(uninsured, na.rm = TRUE) / sum(adult_pop, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = year, values_from = avg_uninsured)

### add DID manually 
dd.table <- dd.table %>%
  mutate(
    diff = `2015` - `2012`
  )

print(dd.table)



# 6. Estimate the effect of Medicaid expansion on the uninsurance rate using a standard DD regression estimator, again focusing only on states that expanded in 2014 versus those that never expanded
reg.data <- final.data.exp %>%
  mutate(
    expand_ever = if_else(expand_group == "Expanded in 2014", 1, 0),
    post = if_else(year >= 2014, 1, 0),
    treat = expand_ever * post,
    perc_unins = uninsured / adult_pop
  )


### DID regression
dd.model <- lm(perc_unins  ~ post + expand_ever + treat, data = reg.data)
summary(dd.model)



# 7. Include state and year fixed effects in your estimates. 
library(fixest) 
library(broom)

fe.model <- feols(perc_unins ~ treat | State + year, data = reg.data)
fe.model.tidy <- tidy(fe.model, conf.int = TRUE)
summary(fe.model.tidy)



# 8. Repeat the analysis in question 7 but include all states (even those that expanded after 2014). Are your results different? If so, why?
### expansion status 
reg.all <- final.data %>%
  mutate(
    expand_ever = if_else(!is.na(date_adopted), 1, 0),  # any state that ever expanded
    post = if_else(year >= 2014, 1, 0),
    treat = expand_ever * post,
    perc_unins = uninsured / adult_pop
  )

### run fixed effects regression using all states
fe.model.all <- feols(perc_unins ~ treat | State + year, data = reg.all)
fe.model.all.tidy <- tidy(fe.model.all, conf.int = TRUE)
summary(fe.model.all.tidy)



# 9. Provide an “event study” graph showing the effects of Medicaid expansion in each year. Use the specification that includes state and year fixed effects, limited to states that expanded in 2014 or never expanded.
event.data <- final.data.exp %>%
  mutate(
    uninsured_rate = uninsured / adult_pop,
    expansion = if_else(expand_group == "Expanded in 2014", 1, 0),
    year = as.factor(year)  
  )

### estimate model  
event.model <- feols(
  uninsured_rate ~ i(year, expansion, ref = "2013") | State + year,
  cluster = ~State,
  data = event.data
)

### plot 
iplot(event.model,
      xlab = "Year",
      main = "Event Study: Effect of Medicaid Expansion (2014 only)")


# 10. Repeat part 9 but again include states that expanded after 2014. Note: this is tricky…you need to put all states onto “event time” to create this graph.
event.data.all <- final.data %>%
  filter(!is.na(date_adopted)) %>%
  mutate(
    expand_year = year(date_adopted),
    event_time = year - expand_year,
    uninsured_rate = uninsured / adult_pop
  )

### trim event window 
event.data.all <- event.data.all %>%
  filter(event_time >= -5, event_time <= 5) %>%
  mutate(event_time = factor(event_time))

### estimate model
event.model.all <- feols(
  uninsured_rate ~ i(event_time, ref = "-1") | State + year,
  data = event.data.all
)

### extract and plot 
iplot(
  event.model.all,
  xlab = "Event Time (Years Since Expansion)",
  main = "Event Study: Medicaid Expansion (All States with Event Time)",
  ref.line = TRUE
)




###rm(list = setdiff(ls(), c("direct.share.plt", "medicaid.share.plt", "uninsured.plot", "dd.table", "dd.model", "fe.model.tidy", "fe.model.all.tidy", "event.study14", "event.model", "event.study.all", "event.model.all")))
###save.image("submission_2/results/hwk5_workspace.RData") 
