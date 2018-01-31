library("tidyverse")
library("plyr")
library("dplyr")
library("magrittr")
library("gdata")
library("ggplot2")
library("plotly")
library("maps")
     

# reading in irs11, irs12, irs13, irs14, irs15
load("data/irs.Rda")


# reading in ach_profile
ach_profile <- read.csv(file="data/achievement_profile_data_with_CORE.csv", header = TRUE)

# reading in zip_code
load("data/zip_code.Rda")

# reading in membership
membership <- read.csv("./data/data_2015_membership_school.csv", header = TRUE)
membership %<>% filter(grade %in% c(9,10,11,12), race_or_ethnicity == "All Race/Ethnic Groups", gender == "All Genders") %>% 
  dplyr::rename(grade_enrollment = enrollment)

# reading in crosswalk
crosswalk <- read.xls("./data/data_district_to_county_crosswalk.xls", header = TRUE)

# combining ach_profile, membership, crosswalk -> districts
districts <- merge(ach_profile, crosswalk, by.x = "system", by.y = "District.Number", type="left", all.x = TRUE)
districts %<>% merge(membership, by.x = 'system', by.y = 'district_id', type="inner", all.x = TRUE)
drops <- c("race_or_ethnicity", "gender")
districts <- districts[,!(names(districts) %in% drops)]
# ----------------------------------------------------------

# defining zip_codes to map counties to zips
zipcodes <- merge(zip_code, irs11, by.x="zip", by.y="zipcode")
zipcodes <- zipcodes %>% 
  select("zip", "county")
zipcodes <- zipcodes[!duplicated(zipcodes),]

# merge irs13 with zipcodes -> irs13_counties
irs13_counties <- merge(zipcodes, irs13, by.y="zipcode", by.x="zip", all.y = T)
irs13_counties %<>%
  dplyr::group_by(county) %>% 
  dplyr::summarise(agi=sum(adjusted_gross_income, na.rm = T)) %>% 
  na.omit()
# -------------------------------------------

# CHLOROPLETH OF AGI BY COUNTY

# reading in county plot points for map
county_df = map_data("county")
county_df <- county_df[(county_df["region"] == "tennessee"),]

# replacing county names with single name lower case
irs13_counties$county <- tolower(gsub("(.*) (County)", "\\1", irs13_counties$county))
irs13_counties$county <- tolower(gsub("dekalb", "de kalb", irs13_counties$county))

# changing column name for join
colnames(county_df)[colnames(county_df)=="subregion"] <- "county"

# joining for map chloropleth
chloropleth <- join(county_df, irs13_counties, by="county")

rbPal <- colorRampPalette(c('light blue', 'dark blue'))
chloropleth$Col <- log10(chloropleth$agi) / log10(24046456000)

# plotting
ggplot(chloropleth, aes(long, lat, group = group, fill=Col)) +
  geom_polygon(color = "white") +
  labs(title = "Choropleth of AGI by County", subtitle = "overlaid with a plot of wealth bubbles") + 
  geom_point(data = irs13_zip_plotable, 
             aes(longitude, latitude, alpha = agi/pop, size = pop), 
             color = "yellow", inherit.aes = F) +
  coord_fixed(ratio = 1/1)
  
# ---------------------------------------------------------------


# % OF TN AGI SPENT ON EDUCATION

# grouping by system number and county
sysno_county <- districts %>% 
  dplyr::group_by(system, County.Name) %>% 
  dplyr::summarise(total_enrollment = mean(Enrollment, na.rm = TRUE),
            per_pupil_exp = mean(Per_Pupil_Expenditures, na.rm = TRUE)) %>% 
  na.omit()

# calculating total expenditures
sysno_county$total_expenditures <- sysno_county$per_pupil_exp * sysno_county$total_enrollment

# renaming counties in districts
sysno_county$County.Name <- tolower(gsub("(.*) (County)", "\\1", sysno_county$County.Name))
sysno_county$County.Name <- tolower(gsub("dekalb", "de kalb", sysno_county$County.Name))

# joining districts to irs13_counties
prc_agi_ed <- merge(sysno_county, irs13_counties, by.x="County.Name", by.y="county")

# grouping further by county alone and averaging/summing
prc_agi_ed %<>% 
  dplyr::group_by(County.Name) %>% 
  dplyr::summarise(total_enrollment = sum(total_enrollment, na.rm = TRUE),
            per_pupil_exp = mean(per_pupil_exp, na.rm = TRUE),
            total_expenditures = sum(total_expenditures, na.rm = TRUE),
            agi = mean(agi, na.rm = TRUE))

# calculating percentage of county expenditure to county agi
prc_agi_ed$agi_prc <- prc_agi_ed$total_expenditures / prc_agi_ed$agi

# calculating total spent on school
total_tn_school_expenditure <- sum(prc_agi_ed$total_expenditures)

# calculating county's expenditure as percentage of total
prc_agi_ed$county_prc <- prc_agi_ed$total_expenditures / total_tn_school_expenditure
View(prc_agi_ed)





# ------------------NEEDS CLEANING----------------------------------------------------

# template: irs_w_counties <- merge(zipcodes, irs11, by.y="zipcode", by.x="zip", all.y = T)

View(irs13)

irs13_counties <- merge(zipcodes, irs13, by.y="zipcode", by.x="zip", all.y = T)

irs13_counties %<>%
  dplyr::group_by(county) %>% 
  dplyr::summarise(agi=sum(adjusted_gross_income, na.rm = T)) %>% 
  na.omit()

View(irs13_counties)

districts$grade_expenditure <- districts$Per_Pupil_Expenditures * districts$grade_enrollment

View(districts)

county_expenditure <- districts %>% 
  dplyr::group_by(system, County.Name) %>% 
  dplyr::summarise(act_comp = mean(ACT_Composite, na.rm=T),
            per_pupil_exp = mean(Per_Pupil_Expenditures, na.rm=T))

county_expenditure %<>% 
  dplyr::group_by(County.Name) %>% 
  dplyr::summarise(act_avg = mean(act_comp, na.rm=T),
            avg_per_pupil_expenditure = mean(per_pupil_exp, na.rm=T)) %>% 
  na.omit()

county_df = map_data("county")



View(county_df)

county_df <- county_df[(county_df["region"] == "tennessee"),]

irs13_counties$county <- tolower(gsub("(.*) (County)", "\\1", irs13_counties$county))
irs13_counties$county <- tolower(gsub("dekalb", "de kalb", irs13_counties$county))
View(irs13_counties)
View(county_df)
colnames(county_df)[colnames(county_df)=="subregion"] <- "county"

chloropleth <- join(county_df, irs13_counties, by="county")

View(chloropleth)

ggplot(chloropleth, aes(long, lat, group = group)) +
  geom_polygon(alpha = log10(chloropleth$agi) / log10(24046456000), color = "white", fill = "dark green") +
  coord_fixed(ratio = 1/1)


View(county_act)

ggplot(county_expenditure, aes(y=act_avg, x=avg_per_pupil_expenditure)) +
  geom_point(alpha=0.4) + geom_smooth(method='lm')

agi_pupil_expenditure <- merge(county_expenditure, irs13_counties, by.x = "County.Name", by.y = "county")

ggplot(agi_pupil_expenditure, aes(y=avg_per_pupil_expenditure, x=agi)) + 
  geom_point() + scale_x_log10() + geom_smooth(method="lm")

qplot(agi_pupil_expenditure$agi, geom="histogram") + ggtitle("Distribution of AGI") +
  labs(x = "AGI")

county_act = districts %>% 
  dplyr::group_by(County.Name) %>% 
  dplyr::summarise(act_avg = mean(ACT_Composite, na.rm=T),
            high_school_expenditure = sum(grade_expenditure, na.rm=T)) %>% 
  na.omit()




agi_act = merge(irs13_counties, county_act, by.x = "county", by.y = "County.Name", all.y = T, all.x = T)

View(irs13_counties)

ggplot(agi_act, aes(y=act_avg, x=agi)) + 
  geom_point() + geom_smooth(method='lm')
