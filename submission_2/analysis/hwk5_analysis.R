options(modelsummary_factory_default = "kableExtra")

# preliminaries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggplot2, dplyr, lubridate, readr, readxl,
  hrbrthemes, fixest, scales, gganimate, gapminder,
  gifski, png, tufte, plotly, OECD, ggrepel,
  survey, foreign, devtools, pdftools,
  modelsummary, kableExtra, data.table, gdata
)



# import the data 
final.data <- read_tsv("data/output/acs_medicaid.txt")

final.data <- final.data %>%
  mutate(perc_private = (ins_employer + ins_direct)/adult_pop,
         perc_public = (ins_medicare + ins_medicaid)/adult_pop,
         perc_ins = (adult_pop - uninsured)/adult_pop,
         perc_unins = uninsured/adult_pop,
         perc_employer = ins_employer/adult_pop,
         perc_medicaid = ins_medicaid/adult_pop,
         perc_medicare = ins_medicare/adult_pop,
         perc_direct = ins_direct/adult_pop) %>%
  filter(! State %in% c("Puerto Rico", "District of Columbia"))



# 1. Plot the share of the adult population with direct purchase health insurance over time.
direct.share.plt <- final.data %>% group_by(year) %>% summarize(mean=mean(perc_direct)) %>% 
  ggplot(aes(x = year, y = mean)) +
  geom_line(linewidth = 1.2, color = "steelblue") +
  geom_point(color = "steelblue") +
  labs(
    x = "Year",
    y = "Share with Direct Purchase Insurance"
  ) +
  theme_minimal()

print(direct.share.plt)



# 3. Plot the share of the adult population with Medicaid over time.
medicaid.share.plt <- final.data %>% group_by(year) %>% summarize(mean=mean(perc_medicaid)) %>% 
  ggplot(aes(x = year, y = mean)) +
  geom_line(linewidth = 1.2, color = "darkgreen") +
  geom_point(color = "darkgreen") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Year",
    y = "Share with Medicaid"
  ) +
  theme_minimal()

print(medicaid.share.plt)



# 4. Plot the share of uninsured over time, separately by states that expanded Medicaid in 2014 versus those that did not. Drop all states that expanded after 2014.
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
  geom_line(linewidth = 1.2) +
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
library(fixest) 
library(broom)

reg.data <- final.data %>%
  mutate(
    expand_ever = if_else(!is.na(date_adopted) & year(date_adopted) == 2014, 1, 0),
    post = if_else(year >= 2014, 1, 0),
    treat = expand_ever * post,
    perc_unins = uninsured / adult_pop
  ) %>%
  filter(is.na(expand_year) | expand_year == 2014)

### DID regression
dd.est <- lm(perc_unins ~ post + expand_ever + treat, data = reg.data)
summary(dd.est)



# 7. Include state and year fixed effects in your estimates. 
fe.est <- feols(perc_unins ~ treat | State + year, data = reg.data, 
                notes = FALSE, fixef.rm = "none")
summary(fe.est) 

models <- list(
  "Standard DD" = dd.est,
  "TWFE" = fe.est
)



# 8. Repeat the analysis in question 7 but include all states (even those that expanded after 2014). Are your results different? If so, why?
### expansion status 
reg.data2 <- final.data %>% 
  mutate(
    treat = case_when(
      year >= expand_year & !is.na(expand_year) ~ 1,
      is.na(expand_year) ~ 0,
      year < expand_year & !is.na(expand_year) ~ 0
    )
  )

### run fixed effects regression using all states
fe.est2 <- feols(perc_unins ~ treat | State + year, data = reg.data2)
summary(fe.est2)

models2 <- list("Standard DD" = dd.est, 
               "TWFE" = fe.est,
               "Time-varying Treatment" = fe.est2)



# 9. Provide an “event study” graph showing the effects of Medicaid expansion in each year. Use the specification that includes state and year fixed effects, limited to states that expanded in 2014 or never expanded.
mod.twfe <- feols(
  perc_unins ~ i(year, expand_ever, ref = 2013) | State + year,
  cluster = ~State,
  data = reg.data  
)

### plot 
iplot(mod.twfe, xlab = "Year", main = "Event Study: Medicaid Expansion (2014 expanders only)", ref.line = TRUE)



# 10. Repeat part 9 but again include states that expanded after 2014. Note: this is tricky…you need to put all states onto “event time” to create this graph.
reg.data2 <- final.data %>%
  filter(!is.na(date_adopted)) %>%  # Only states that eventually expand
  mutate(
    expand_year = year(date_adopted),
    expand_ever = 1,  # By construction, they all expanded
    perc_unins = uninsured / adult_pop,
    time_to_treat = year - expand_year
  ) %>%
  mutate(
    time_to_treat = if_else(time_to_treat <= -4, -4L, time_to_treat),
    time_to_treat = as.factor(time_to_treat)
  )

### estimate model
mod.twfe2 <- feols(
  perc_unins ~ i(time_to_treat, expand_ever, ref = -1) | State + year,
  cluster = ~State,
  data = reg.data2
) 

### extract and plot 
iplot(mod.twfe2, xlab = "Event Time (Years Since Expansion)", main = "Event Study: Medicaid Expansion (All States, Event Time)", ref.line = TRUE)




rm(list=c("final.data","uninsured.share"))
save.image("submission_2/results/hwk5_workspace.RData") 
