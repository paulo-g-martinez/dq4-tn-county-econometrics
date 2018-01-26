library("tidyverse")
library("dplyr")
library("magrittr")
library("gdata")
library("ggplot2")
library("plotly")
library("maps")
     


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

View(irs13)

irs13_counties <- merge(zipcodes, irs13, by.y="zipcode", by.x="zip", all.y = T)

irs13_counties %<>%
  group_by(county) %>% 
  summarise(agi=sum(adjusted_gross_income, na.rm = T)) %>% 
  na.omit()

districts$grade_expenditure <- districts$Per_Pupil_Expenditures * districts$grade_enrollment

View(districts)

county_expenditure <- districts %>% 
  group_by(system, County.Name) %>% 
  summarise(act_comp = mean(ACT_Composite, na.rm=T),
            per_pupil_exp = mean(Per_Pupil_Expenditures, na.rm=T))

county_expenditure %<>% 
  group_by(County.Name) %>% 
  summarise(act_avg = mean(act_comp, na.rm=T),
            avg_per_pupil_expenditure = mean(per_pupil_exp, na.rm=T)) %>% 
  na.omit()

county_df = map_data("county")

View(county_df)

county_df <- county_df[(county_df["region"] == "tennessee"),]

county_expenditure$County.Name <- tolower(gsub("(\\S+) (\\S+)", "\\1", county_expenditure$County.Name))

chloropleth <- merge(county_df, county_expenditure, by.x = "subregion", by.y = "County.Name")

ggplot(county_df, aes(long, lat, group=group)) + geom_polygon()





View(county_act)

ggplot(county_expenditure, aes(y=act_avg, x=avg_per_pupil_expenditure)) +
  geom_point(alpha=0.4) + geom_smooth(method='lm')

agi_pupil_expenditure <- merge(county_expenditure, irs13_counties, by.x = "County.Name", by.y = "county")

ggplot(agi_pupil_expenditure, aes(y=avg_per_pupil_expenditure, x=agi)) + 
  geom_point() + scale_x_log10() + geom_smooth(method="lm")

qplot(agi_pupil_expenditure$agi, geom="histogram") + ggtitle("Distribution of AGI") +
  labs(x = "AGI")

county_act = districts %>% 
  group_by(County.Name) %>% 
  summarise(act_avg = mean(ACT_Composite, na.rm=T),
            high_school_expenditure = sum(grade_expenditure, na.rm=T)) %>% 
  na.omit()




agi_act = merge(irs13_counties, county_act, by.x = "county", by.y = "County.Name", all.y = T, all.x = T)

View(irs13_counties)

ggplot(agi_act, aes(y=act_avg, x=agi)) + 
  geom_point() + geom_smooth(method='lm')
