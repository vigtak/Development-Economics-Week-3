library(survey)
library(readstata13) 
library(tidyr)
library(dplyr)
library(knitr)

Tanzania_2012 <- "C:/Users/viggo/Documents/Development Economics 1/Tanzania_2012.dta"
Tanzania_2018 <- "C:/Users/viggo/Documents/Development Economics 1/Tanzania_2018.dta"

tzdata2012 <- read.dta13(Tanzania_2012,nonint.factors = T)
tzdata2018 <- read.dta13(Tanzania_2018,nonint.factors = T)

keep_cases <- !with(tzdata2012,
                    is.na(cons) | is.na(STRATUM) | is.na(CLUSTER) |
                      is.na(hhsize) | is.na(hhweight))

tzdata2012 <- tzdata2012[keep_cases, ]
rm(keep_cases)


tzdata2012$popwt <- with(tzdata2012, hhsize * hhweight)

library(survey)

tzdesign <- svydesign(
  id = ~CLUSTER,
  strata = ~STRATUM,
  weights = ~popwt,
  data = tzdata2012
)

#Start of assignment


#1

#Calculations of the standard and the poverty line
tzdata <- bind_rows(tzdata2012, tzdata2018)

# National
national <- tzdata %>%
  group_by(year) %>%
  summarise(
    headcount_standard = weighted.mean(cons < povline, hhweight * hhsize) * 100,
    poverty_gap_standard = weighted.mean(pmax(0, (povline - cons) / povline), hhweight * hhsize) * 100,
    sq_poverty_gap_standard = weighted.mean(pmax(0, (povline - cons) / povline)^2, hhweight * hhsize) * 100,
    headcount_food = weighted.mean(cons < food_povline, hhweight * hhsize) * 100,
    poverty_gap_food = weighted.mean(pmax(0, (food_povline - cons) / food_povline), hhweight * hhsize) * 100,
    sq_poverty_gap_food = weighted.mean(pmax(0, (food_povline - cons) / food_povline)^2, hhweight * hhsize) * 100
  )

# Area
area <- tzdata %>%
  group_by(year, STRATUM) %>%
  summarise(
    headcount_standard = weighted.mean(cons < povline, hhweight * hhsize) * 100,
    poverty_gap_standard = weighted.mean(pmax(0, (povline - cons) / povline), hhweight * hhsize) * 100,
    sq_poverty_gap_standard = weighted.mean(pmax(0, (povline - cons) / povline)^2, hhweight * hhsize) * 100,
    headcount_food = weighted.mean(cons < food_povline, hhweight * hhsize) * 100,
    poverty_gap_food = weighted.mean(pmax(0, (food_povline - cons) / food_povline), hhweight * hhsize) * 100,
    sq_poverty_gap_food = weighted.mean(pmax(0, (food_povline - cons) / food_povline)^2, hhweight * hhsize) * 100
  )

# Region
region <- tzdata %>% 
  group_by(year, region) %>% 
  summarise(
    headcount_standard = weighted.mean(cons < povline, hhweight * hhsize) * 100,
    poverty_gap_standard = weighted.mean(pmax(0, (povline - cons) / povline), hhweight * hhsize) * 100,
    sq_poverty_gap_standard = weighted.mean(pmax(0, (povline - cons) / povline)^2, hhweight * hhsize) * 100,
    headcount_food = weighted.mean(cons < food_povline, hhweight * hhsize) * 100,
    poverty_gap_food = weighted.mean(pmax(0, (food_povline - cons) / food_povline), hhweight * hhsize) * 100,
    sq_poverty_gap_food = weighted.mean(pmax(0, (food_povline - cons) / food_povline)^2, hhweight * hhsize) * 100
  )


#Table
national_table <- national %>% mutate(Level = "National", Category = "All")
area_table <- area %>% mutate(Level = "Area", Category = STRATUM) %>% select(-STRATUM)
region_table <- region %>% mutate(Level = "Region", Category = region) %>% select(-region)

all_results <- bind_rows(national_table, area_table, region_table)

kable(all_results)

#3

#Stratum-level contribution to national headcount rate
stratum_contribution <- tzdata %>%
  group_by(year, STRATUM) %>%
  summarise(
    poor_standard = sum((cons < povline) * hhweight * hhsize),
    poor_food = sum((cons < food_povline) * hhweight * hhsize)
  ) %>%
  group_by(year) %>%
  mutate(
    contribution_standard = poor_standard / sum(poor_standard) * 100,
    contribution_food = poor_food / sum(poor_food) * 100
  ) %>%
  select(year, STRATUM, contribution_standard, contribution_food)

kable(stratum_contribution)
#Region-level contribution to national headcount rate
region_contribution <- tzdata %>%
  group_by(year, region) %>%
  summarise(
    poor_standard = sum((cons < povline) * hhweight * hhsize),
    poor_food = sum((cons < food_povline) * hhweight * hhsize)
  ) %>%
  group_by(year) %>%
  mutate(
    contribution_standard = poor_standard / sum(poor_standard) * 100,
    contribution_food = poor_food / sum(poor_food) * 100
  ) %>%
  select(year, region, contribution_standard, contribution_food)

kable(region_contribution)
  
