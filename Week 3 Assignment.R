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

#Start of assignments


#1

tzdata <- bind_rows(tzdata2012, tzdata2018)

national <- tzdata %>%
  group_by(year) %>%
  summarise(
    headcount_standard = weighted.mean(cons < povline, hhweight * hhsize) * 100,
    headcount_food = weighted.mean(cons < food_povline, hhweight * hhsize) * 100
  )

area <- tzdata %>%
  group_by(year, STRATUM) %>%
  summarise(
    headcount_standard = weighted.mean(cons < povline, hhweight * hhsize) * 100,
    headcount_food = weighted.mean(cons < food_povline, hhweight * hhsize) * 100
  )

region <- tzdata %>% 
  group_by(year, region) %>% 
  summarise(
    headcount_std = weighted.mean(cons < povline, hhweight * hhsize) * 100,
    headcount_food = weighted.mean(cons < food_povline, hhweight * hhsize) * 100
  )

print(national)
#2012 headcount standard: 28.2
#2012 headcount food: 9.74

#2018 headcount standard: 26.4
#2018 headcount food: 8.01

print(area)


print(region)
  


  
