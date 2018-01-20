library("tidyverse")
library("dplyr")
library("magrittr")

load("irs.Rda")

ach_profile <- read.csv(file="data/achievement_profile_data_with_CORE.csv", header = TRUE)

load("zip_code.Rda")

membership <- read.csv("./data/data_2015_membership_school.csv", header = TRUE)
membership %<>% filter(grade %in% c(9,10,11,12), race_or_ethnicity == "All Race/Ethnic Groups", gender == "All Genders")

crosswalk <- read.xls("./data/data_district_to_county_crosswalk.xls", header = TRUE)

districts <- merge(ach_profile, crosswalk, by.x = "system", by.y = "District.Number", type="left", all.x = TRUE)

View(districts)
districts %<>% merge(membership, by.x = 'system', by.y = 'district_id', type="inner", all.x = TRUE)

drops <- c("race_or_ethnicity", "gender")
districts <- districts[,!(names(districts) %in% drops)]
View(districts)

districts$total_expenditures <- districts$Per_Pupil_Expenditures * districts$enrollment



'''
districts %<>% group_by("Column.Name") %>% 
  summarise(AlgI=mean(AlgI),
            AlgII=mean(AlgII),
            BioI=mean(BioI),
            Chemistry=mean(Chemistry),
            ELA=mean(ELA),
            EngI=mean(EngI),
            EngII=mean(EngII),
            EngIII=mean(EngIII),
            Math=mean(Math),
            Science=mean(Science),
            Enrollment=sum(Enrollment),
            Pct_Black=mean(Pct_Black),
            Pct_Hispanic=mean(Pct_Hispanic),
            Pct_Native_American=mean(Pct_Native_American),
            Pct_EL=mean(Pct_EL),
            Pct_SWD=mean(Pct_SWD),
            Pct_ED=mean(Pct_ED),
            Per_Pupil_Expenditures=#mean(Per_Pupil_Expenditures),
            '''
            
            
            