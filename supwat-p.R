library("tidyverse")
library("plyr")
library("dplyr")
library("magrittr")
library("gdata")
library("ggplot2")
library("plotly")
library("maps")
library("reshape2")
library("GGally")


# reading in IRS datasets: irs11, irs12, irs13, irs14, irs15
load("data/irs.Rda")

# reading in ach_profile
ach_profile <- read.csv(file="data/achievement_profile_data_with_CORE.csv", header = TRUE)

# reading in zip_code; to match counties to zips
load("data/zip_code.Rda")

# reading in membership: for education enrollment data
membership <- read.csv("./data/data_2015_membership_school.csv", header = TRUE)
#filter membership down to only highschool
membership %<>% filter(grade %in% c(9,10,11,12), race_or_ethnicity == "All Race/Ethnic Groups", gender == "All Genders") %>% 
  rename(grade_enrollment = enrollment)

# reading in crosswalk; to match districts to county
crosswalk <- read.xls("./data/data_district_to_county_crosswalk.xls", header = TRUE)

# combining ach_profile, membership, crosswalk -> districts
districts <- merge(ach_profile, crosswalk, by.x = "system", by.y = "District.Number", type="left", all.x = TRUE)

# ----------------------------------------------------------

# defining zip_codes to map counties to zips
zipcodes <- merge(zip_code, irs11, by.x="zip", by.y="zipcode")
zipcodes <- zipcodes %>% 
  select("zip", "county")
zipcodes <- zipcodes[!duplicated(zipcodes),]

# merge irs13 with zipcodes -> irs13_counties
irs13_counties <- merge(zipcodes, irs13, by.y="zipcode", by.x="zip", all.y = T)
irs13_counties %<>%
  group_by(county) %>% 
  summarise(agi=sum(adjusted_gross_income, na.rm = T),
            num_returns=sum(number_of_returns, na.rm = T)) %>% 
  na.omit()

# -------------------------------------------

# CORRELATION MATRIX

corr <- districts %>% 
  group_by(system, County.Name) %>% 
  summarise(per_pupil_exp=mean(Per_Pupil_Expenditures, na.rm=TRUE),
            enrollment=mean(Enrollment, na.rm=TRUE),
            act_comp=mean(ACT_Composite, na.rm=TRUE),
            graduation=mean(Graduation, na.rm=TRUE)) %>% 
  mutate(dist_expenditure = per_pupil_exp*enrollment)

load("data/irs_edu_13.Rda")

#Scatter Plot of County AGI vs ACT composite
act_agi_irs <- ggplot(irs_edu_13, aes(x=adjusted_gross_income, y=ACT_Composite, na.rm = T))+
                  geom_point(color = "green", alpha = 0.5) + 
                  geom_smooth(method = "lm") +
                  geom_text(aes(label = row.names(irs_edu_13), angle = 0), nudge_y = .2, check_overlap = T
                            ) +
                  geom_line(aes(x = 1*10^8.5), color = "red", alpha = .75) +
                  geom_line(aes(x = 1*10^9.5), color = "red", alpha = .75) +
                  #scale_y_log10() + 
                  scale_x_log10() + 
                  labs(x="County AGI", y="County ACT", 
                       title = "Scatter Plot of County ACT Composite as a Function of County AGI")

#shortening names for correlation matrix
irs_edu_13 <- irs_edu_13[,-c(2:12)]
irs_edu_13 <- irs_edu_13[,-c(5:15)]
col_names <- c(
  'agi',
  'sals',
  'unemp',
  'mort_int',
  'pct_blk',
  'pct_hisp',
  'pct_nata',
  'pct_el',
  'pct_swd',
  'pct_ed',
  'ppe',
  'pct_bhn',
  'ACT',
  'pct_abs',
  'pct_sus',
  'pct_exp',
  'grad',
  'drop',
  'g_enr',
  'g_exp',
  'agi_q'
)
names(irs_edu_13) <- col_names
corr_matrix <- ggcorr(irs_edu_13 ,geom = "circle", digits = 0, hjust=0.75, size=5) +
  labs(title = "Correlation Matrix of Key IRS and Education Indicators")
plot(corr_matrix)


# CHLOROPLETH OF AGI BY COUNTY

# reading and subset county plot points for map
county_df = map_data("county")
county_df <- county_df[(county_df["region"] == "tennessee"),]

# replacing county names with single name lower case
irs13_counties$county <- tolower(gsub("(.*) (County)", "\\1", irs13_counties$county))
irs13_counties$county <- tolower(gsub("dekalb", "de kalb", irs13_counties$county))

# changing column name for join
colnames(county_df)[colnames(county_df)=="subregion"] <- "county"

# joining for map chloropleth
chloropleth <- join(county_df, irs13_counties, by="county")

# rbPal <- colorRampPalette(c('light blue', 'dark blue'))
chloropleth$Cnty_AGI <- -1*log10(chloropleth$agi) / log10(24046456000)

# Create the getmode function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# create a plotable df that has zip, lat, lon, pop, and agi
irs13_zip_plotable <- merge(zip_code, irs13, by.y = "zipcode", by.x = "zip", all.y = T, all.x = T) %>% 
  dplyr::select("zip", "latitude", "longitude", "irs_estimated_population_2014", "adjusted_gross_income") %>% 
  dplyr::filter(zip != "00000", irs_estimated_population_2014 != 0) %>% 
  dplyr::group_by(zip) %>% 
  dplyr::summarise(agi = sum(adjusted_gross_income, na.rm = T),
                   latitude = getmode(latitude), longitude = getmode(longitude),
                   zip_pop = getmode(irs_estimated_population_2014))

# plotting
combo_chloro1 <- ggplot(chloropleth, aes(long, lat, group = group,
                                        #fill=Cnty_AGI
                                        )) +
  geom_polygon(color = "white") +
  labs(title = "Bubble plot of Zipcode AGI per capita") + 
  geom_point(data = irs13_zip_plotable, aes(longitude, latitude, alpha = agi/zip_pop, size = zip_pop), 
             inherit.aes = F, color = "green") +
  coord_fixed(ratio = 1/1)

combo_chloro2 <- ggplot(chloropleth, aes(long, lat, group = group, 
                                        fill=Cnty_AGI)) +
  geom_polygon(color = "white") +
  labs(title = "Choropleth of AGI by County", subtitle = "County AGI shade of blue scaled logarithmically; darker indicates higher AGI") + 
  geom_point(data = irs13_zip_plotable, aes(longitude, latitude, alpha = agi/zip_pop, size = zip_pop), 
             inherit.aes = F, color = "green") +
  coord_fixed(ratio = 1/1)

# TN Counties
agi_by_county <- ggplot(chloropleth, aes(long, lat, group = group, 
                                         #fill="Cnty_AGI"
                                         )) +
  geom_polygon(color = "white") + 
  theme(line = element_blank(), rect = element_blank(), axis.title = element_blank(), axis.text = element_blank()
        ) + #reomve grid and axis and ticks
  coord_fixed(ratio = 1/1)

# ---------------------------------------------------------------


# % OF TN AGI SPENT ON EDUCATION

# grouping by system number and county
sysno_county <- districts %>% 
  group_by(system, County.Name) %>% 
  summarise(total_enrollment = mean(Enrollment, na.rm = TRUE),
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
  group_by(County.Name) %>% 
  summarise(total_enrollment = sum(total_enrollment, na.rm = TRUE),
            per_pupil_exp = mean(per_pupil_exp, na.rm = TRUE),
            total_expenditures = sum(total_expenditures, na.rm = TRUE),
            agi = mean(agi, na.rm = TRUE),
            total_returns = mean(num_returns, na.rm = TRUE))


# calculating percentage of county expenditure to county agi
prc_agi_ed$agi_prc <- prc_agi_ed$total_expenditures / prc_agi_ed$agi

# calculating total spent on school
total_tn_school_expenditure <- sum(prc_agi_ed$total_expenditures)

# calculating county's expenditure as percentage of state total
prc_agi_ed$county_prc <- prc_agi_ed$total_expenditures / total_tn_school_expenditure

# calculating average agi per return
prc_agi_ed$avg_agi_per_return <- prc_agi_ed$agi / prc_agi_ed$total_returns

agi_county_prc <- ggplot(prc_agi_ed, aes(y=county_prc, x=agi_prc, label=County.Name)) + 
  geom_point(color = "dark green") +
  geom_text(aes(label=ifelse(county_prc>0.03|(agi_prc>0.13&county_prc<0.001), as.character(County.Name), "")), 
            hjust=-0.2, vjust=0, size=3) +
  labs(y="County Ed Exp As % of State Exp", 
       x="County Education Expenditure As % of County AGI")
#  annotate(geom = "text", x = prc_agi_ed$agi_prc, y = prc_agi_ed$county_prc, label=prc_agi_ed$County.Name, size = 2) +

# CHLOROPLETH AGI PER RETURN
colnames(county_df)[colnames(county_df)=="county"] <- "County.Name"

# joining for map chloropleth
chloropleth2 <- join(county_df, prc_agi_ed, by="County.Name")

# rbPal <- colorRampPalette(c('light blue', 'dark blue'))
chloropleth2$Col <- log10(chloropleth2$avg_agi_per_return) / log10(110280.91)

# plotting
avg_agi_by_county <- ggplot(chloropleth2, aes(long, lat, group = group, fill=Col)) +
  geom_polygon(color = "white") +
  coord_fixed(ratio = 1/1)

# HIGH SCHOOL

high_school <- districts %>% 
  group_by(system, County.Name) %>% 
  summarise(AlgI=mean(AlgI, na.rm = TRUE),
            AlgII=mean(AlgII, na.rm = TRUE),
            BioI=mean(BioI, na.rm = TRUE),
            Chemistry=mean(Chemistry, na.rm = TRUE),
            ELA=mean(ELA, na.rm = TRUE),
            EngI=mean(EngI, na.rm = TRUE),
            EngII=mean(EngII, na.rm = TRUE),
            EngIII=mean(EngIII, na.rm = TRUE),
            Math=mean(Math, na.rm = TRUE),
            Science=mean(Science, na.rm = TRUE),
            per_pupil=mean(Per_Pupil_Expenditures, na.rm = TRUE),
            act_comp=mean(ACT_Composite, na.rm = TRUE)) %>%
  na.omit()

# renaming counties in districts
high_school$County.Name <- tolower(gsub("(.*) (County)", "\\1", high_school$County.Name))
high_school$County.Name <- tolower(gsub("dekalb", "de kalb", high_school$County.Name))

# joining districts to irs13_counties
hs_irs <- merge(high_school, irs13_counties, by.x="County.Name", by.y="county")

# grouping further by county alone and averaging/summing
hs_irs %<>% 
  group_by(County.Name) %>% 
  summarise(AlgI=mean(AlgI, na.rm = TRUE),
            AlgII=mean(AlgII, na.rm = TRUE),
            BioI=mean(BioI, na.rm = TRUE),
            Chemistry=mean(Chemistry, na.rm = TRUE),
            ELA=mean(ELA, na.rm = TRUE),
            EngI=mean(EngI, na.rm = TRUE),
            EngII=mean(EngII, na.rm = TRUE),
            EngIII=mean(EngIII, na.rm = TRUE),
            Math=mean(Math, na.rm = TRUE),
            Science=mean(Science, na.rm = TRUE),
            per_pupil=mean(per_pupil, na.rm = TRUE),
            agi = mean(agi, na.rm = TRUE),
            act_comp = mean(act_comp, na.rm = TRUE),
            total_returns = mean(num_returns, na.rm = TRUE))

hs_irs$agi_per_return <- hs_irs$agi /hs_irs$total_returns

hs_irs_no_outliers <- hs_irs[-c(22, 89),]

act_agi_per_return <- ggplot(hs_irs_no_outliers, aes(y=act_comp, x=agi_per_return)) +
  geom_point(color = "dark green") + 
  geom_smooth(method="lm") + 
  geom_text(aes(label = County.Name), 
             check_overlap = T, nudge_y = .15, #color = "orange"
             ) +
  geom_line(aes(x = 40000), color = "red", alpha = .75) +
  geom_line(aes(x = 50000), color = "red", alpha = .75) +
  labs(y="County ACT", x="County AGI Per Return")

hs <- melt(hs_irs, id.vars=c("County.Name", "agi", "act_comp", "total_returns", "agi_per_return", "per_pupil"))

top_5_agi <- head(arrange(hs, desc(agi_per_return)), n = 50)
top_5_agi$county <- factor(top_5_agi$County.Name, 
                           levels=c("williamson", "fayette", "knox", "davidson", "wilson"))
bot_5_agi <- head(arrange(hs, agi_per_return), n= 50)
bot_5_agi$county <- factor(bot_5_agi$County.Name, 
                           levels=c("hancock", "clay", "cocke", "van buren", "lake"))

top_5_agi_facet <- ggplot(top_5_agi, aes(y=value, x=variable)) +
  geom_bar(stat="identity", fill = "orange", alpha = .5) + 
  ylim(0, 100) +
  facet_grid(.~county) +
  #facet_wrap(~county, scales="free_x") +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(x="Subject", y="Proficiency Rate")

bot_5_agi_facet <- ggplot(bot_5_agi, aes(y=value, x=variable)) +
  geom_bar(stat="identity", fill = "orange", alpha = .5) + 
  ylim(0, 100) +
  #facet_wrap(~county, scales="free_x") +
  facet_grid(.~county) +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(x="Subject", y="Proficiency Rate")




# ACT COMP VS AGI PER RETURN


# --------------------------------------------------------------------


final <- districts %>% 
  select("system", "Per_Pupil_Expenditures", "ACT_Composite", "Enrollment", "County.Name")

final$system_exp <- final$Per_Pupil_Expenditures * final$Enrollment
final$act_brute <- final$ACT_Composite * final$Enrollment
final$act_enrollment <- final$Enrollment * final$ACT_Composite/final$ACT_Composite

final %<>%
  group_by(County.Name) %>% 
  summarise(enr=sum(Enrollment, na.rm = TRUE),
            exp=sum(system_exp, na.rm = TRUE),
            act_unweighted = mean(ACT_Composite, na.rm = TRUE), # ERROR: need weighted mean of act
            act = mean(sum(act_brute, na.rm = T)/sum(Enrollment, na.rm = T))) #this should serve as a weighted mean

final$ppe <- final$exp / final$enr

final$County.Name <- tolower(gsub("(.*) (County)", "\\1", final$County.Name))
final$County.Name <- tolower(gsub("dekalb", "de kalb", final$County.Name))

final <- merge(final, irs13_counties, by.x="County.Name", by.y="county")

final$ppe_agi <- final$ppe / final$agi

final$ppe_exp <- final$ppe / final$exp
final %<>%
  select(-act_unweighted)

bar_plots <- arrange(final, desc(act))
bar_plots$County.Name <- factor(bar_plots$County.Name, levels=bar_plots$County.Name)

ppe_over_agi <- ggplot(bar_plots, aes(x=County.Name, y=ppe_agi)) +
  geom_col(fill='light blue') +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  labs(x="County", y="PPE / AGI")

ppe_over_exp <- ggplot(bar_plots, aes(x=County.Name, y=ppe_exp)) +
  geom_col(fill='pink') +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  labs(x="County", y="PPE / County Exp")

ppe <- ggplot(bar_plots, aes(x=County.Name, y=ppe)) +
  geom_col(fill='green') +
  coord_cartesian(ylim=c(7000,12000)) +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  labs(x="County", y="PPE")

act <- ggplot(bar_plots, aes(x=County.Name, y=act)) +
  geom_col(fill='orange') +
  coord_cartesian(ylim=c(16,24)) +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  labs(x='County', y='ACT')

final_corr <- #final %>% dplyr::select()
  ggcorr(final, geom = "circle", digits = 0)


final <- join(final, county_df, by="County.Name")

new_final <- final %>%
  group_by(County.Name) %>% 
  summarise(act=mean(act, na.rm=T),
            lat=mean(lat, na.rm=T),
            long=mean(long, na.rm=T))
#lat=(max(lat, na.rm=T) + min(lat, na.rm=T))/2,
#long=(max(long, na.rm=T) + min(long, na.rm=T))/2)

new_final %<>% mutate(act = round(act, 1))

#Choropleths
combo_chloro3 <- ggplot(chloropleth, aes(long, lat, group = group, 
                                         fill=Cnty_AGI)) +
  geom_polygon(color = "white") +
  labs(title = "Choropleth of AGI by County with ACT Scores", subtitle = "County AGI shade of blue scaled logarithmically; darker indicates higher AGI") +
  geom_text(aes( label=act, x=long, y=lat), data = new_final,
            inherit.aes = F, size = 4.5, color='orange', check_overlap = T) +
  coord_fixed(ratio = 1/1)

act_counties <- ggplot(final, aes(x = long, y = lat, group = group, 
                                  fill=-1*ppe)) +
  geom_polygon(color = "white") +
  labs(title = "Choropleth of PPE by County w/ ACT") + 
  geom_text(aes( label=act, x=long, y=lat), data = new_final, 
            inherit.aes = F, size = 4.5, color='orange', check_overlap = T) +
  geom_text(aes(label = County.Name, x = long, y = lat, size = act, alpha = act), nudge_y = -.08,  data = new_final, 
            inherit.aes = F, color = "yellow", check_overlap = T) +
  #geom_point
  coord_fixed(ratio = 1/1)

#------------------------------------------------------------------------
#Exporting plots
save(agi_by_county, act_agi_irs, act_counties, final_corr, act, ppe, ppe_over_exp, ppe_over_agi, corr_matrix, 
     agi_county_prc, top_5_agi_facet, bot_5_agi_facet, act_agi_per_return, combo_chloro1, combo_chloro2, combo_chloro3,
     file = "final-graphs-p.Rda")
