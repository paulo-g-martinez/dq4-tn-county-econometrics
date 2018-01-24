library("tidyverse")
library("dplyr")
library("magrittr")
library("gdata")
library("ggplot2")

load("irs.Rda")

ach_profile <- read.csv(file="data/achievement_profile_data_with_CORE.csv", header = TRUE)

load("zip_code.Rda")

membership <- read.csv("./data/data_2015_membership_school.csv", header = TRUE)
membership %<>% filter(grade %in% c(9,10,11,12), race_or_ethnicity == "All Race/Ethnic Groups", gender == "All Genders") %>% 
  rename(grade_enrollment = enrollment)

crosswalk <- read.xls("./data/data_district_to_county_crosswalk.xls", header = TRUE)

districts <- merge(ach_profile, crosswalk, by.x = "system", by.y = "District.Number", type="left", all.x = TRUE)

View(districts)
districts %<>% merge(membership, by.x = 'system', by.y = 'district_id', type="inner", all.x = TRUE)

drops <- c("race_or_ethnicity", "gender")
districts <- districts[,!(names(districts) %in% drops)]
View(districts)

districts$total_expenditures <- districts$Per_Pupil_Expenditures * districts$enrollment

zipcodes <- merge(zip_code, irs11, by.x="zip", by.y="zipcode")
zipcodes <- zipcodes %>% 
  select("zip", "county")
zipcodes <- zipcodes[!duplicated(zipcodes),]

# template: irs_w_counties <- merge(zipcodes, irs11, by.y="zipcode", by.x="zip", all.y = T)

irs13_counties <- merge(zipcodes, irs13, by.y="zipcode", by.x="zip", all.y = T)

irs13_counties %>%
  group_by(zip, county) %>% 
  summarise(agi=sum(adjusted_gross_income, na.rm = T))

View(irs13_agi)

View(zip_code)