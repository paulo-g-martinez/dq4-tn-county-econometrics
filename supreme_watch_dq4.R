library("gdata")
library("tidyverse")

# string vector of file names in our local tax_data folder
files <- list.files(path = "./data/tax_data")

#path and variable building loop for irs data
for (f in files) {
  #make new variable name out of df and the first two characters of the target file name
  v = paste("df", substr(f, 1, 2), sep = "")
  print(v) #spot check
  #making a string path to each target file
  d = paste("./data/tax_data", f, sep = "/")
  #reading in each irs xls into the variable created for it
  assign(v, read.xls(d, stringsAsFactors = FALSE, blank.lines.skip = TRUE))
}

# function to untangle the xls columns names mess the irs gave us
column_assigner <- function(df) {
  columns <- c(df[3,]) #column names component from the third forw
  columns_2 <- c(df[4,]) #column names component from the fourth row
  
  for (i in 1:length(columns)) { #do this once for each column
    if (i == 1) {
      c1 <- columns[i] #no need to initialize c1 befor this since the irs xls files all have a string for the first element of the third row
    }
    if (columns[i] != "") {
      c1 <- columns[i] #store the 3rd row's col name component. Notice that if it is blank c1 will retain the preceding name component
    }
    full_name <- paste(c1, columns_2[i])#paste it to the 4th rows's col name component (if any)
    full_name <- gsub("\\[.*\\]|\\n|\\(.*\\)", "", full_name)#cleaning the column name string
    full_name <- str_trim(full_name) #eliminating the white space
    full_name <- gsub("\\s", "_", full_name)#substituting spaces for underscores for dplyrs's benefit
    print(str_to_lower(full_name))# spot check for simplicity
    df[5,i] <- str_to_lower(full_name)# the 5th row is a convenient location for the irs xls files
  }
  colnames(df) <- df[5,]# assign the names from the 5th row to the dataframe's actual column names
  df <- df[-c(1:5),]# now we can dropp the first five rows
  df
}
df11 <- column_assigner(df11)
df12 <- column_assigner(df12)
df13 <- column_assigner(df13)
df14 <- column_assigner(df14)
df15 <- column_assigner(df15)

dfs <- list(df11, df12, df14, df15)

#convert irs columns to their appropriate data types
#this code asserts that "number" is a good discriminant between the dollar and count columns: str_detect(colnames(df15), "amount") & str_detect(colnames(df15), "number")

irs_cleaner <- function(df) {
  df2 <- df[!(df$size_of_adjusted_gross_income == ""),]
  df2 <- df2[!(grepl("Total", df2$size_of_adjusted_gross_income)),]
  df2[,3:ncol(df2)] = sapply(df2[,3:ncol(df2)], function(x) suppressWarnings(as.numeric(as.character(x))))
  for (name in names(df2)) {
    if (grepl("zip", name)) next
    if (grepl("size_of_adjusted_gross_income", name)) next
    if (grepl("number", name)) next
    
    df2[name] <- df2[name] * 1000
  }
  df2
}

df11 <- irs_cleaner(df11)
df12 <- irs_cleaner(df12)
df13 <- irs_cleaner(df13)
df14 <- irs_cleaner(df14)
df15 <- irs_cleaner(df15)

View(df11)
View(df12)
View(df13)
View(df14)
View(df15)

#Time to read in the education data frame
# Let's just start with Alex's df to simplify our life for now
ach_profile <- read_csv("data/achievement_profile_data_with_CORE.csv")
#give better name to Enrollment column
ach_profile <- rename(ach_profile, school_enrollment = Enrollment)
View(ach_profile)

#now let's read in the zip code data frame
     # Commented all this out because reading it in took a while
'
tnzips_df <- read.xls("data/zip_code_database.xlsx", dec = ".", blank.lines.skip = T)
zip_code <- tnzips_df[(tnzips_df["state"] == "TN"),]
save(zip_code, file = "zip_code.Rda")
'
load('zip_code.Rda')

zipcodes <- merge(zip_code, df11, by.x="zip", by.y="zipcode")
#select columns
zipcodes <- zipcodes %>% 
  select("zip", "county")
zipcodes <- zipcodes[!duplicated(zipcodes),] #drop duplicate rows
View(zip_code)

#add county names to irs11
irs11_w_counties <- merge(zipcodes, df11, by.y="zipcode", by.x="zip", all.y = T)
irs12_w_counties <- merge(zipcodes, df12, by.y = "zipcode", by.x = "zip", all.y = T)
irs13_w_counties <- merge(zipcodes, df13, by.y = "zipcode", by.x = "zip", all.y = T)
irs14_w_counties <- merge(zipcodes, df14, by.y = "zipcode", by.x = "zip", all.y = T)
irs15_w_counties <- merge(zipcodes, df15, by.y = "zipcode", by.x = "zip", all.y = T)

#

#read in 2015 school membership dataframe from the education tn.gov website
membership <- read_csv("data/data_2015_membership_school.csv")
hs_membership <- membership %>% 
  #filter by highschool grades
  filter(
    grade %in% c(9, 10, 11, 12),
    race_or_ethnicity == "All Race/Ethnic Groups", gender == "All Genders") %>% 
  #give better name to enrollment column
  rename(grade_enrollment = enrollment)
View(hs_membership)

#Read in crosswalks because crosswalks District_number column is the same as achs system column. 
 # This means I can assign a county column to ach (which currently only has a system name)
crosswalk <- read.xls('data/data_district_to_county_crosswalk.xls', stringsAsFactors = F, blank.lines.skip = T)
View(crosswalk)
# Merge ach and crosswalk to add county name to each school
merged_ed <- merge(ach_profile, crosswalk, by.y = "District.Number", by.x = "system", all.x = T)
View(merged_ed)

#filter merged_ed by county
anderson_ed <- merged_ed %>% filter(County.Name == 'Anderson County')
View(anderson_ed)
# by visual inspection I see the districts that belong to the selected county are the unique values in the system column
  #in this case they are 10, 11, 12

#now I can filter hs_membership by the districts of a county
anderson_mbrs <- hs_membership %>% 
  filter(district_id %in% c(10, 11, 12))
View(anderson_mbrs)
#this allows me to calculate the total highschool membership of each highschool and thus of each county

#now I can merge in the columns with membership/enrollment
anderson_ed <- merge(anderson_ed, anderson_mbrs, by.x = 'system', by.y = 'district_id')
View(anderson_ed)

#calculate expenditure per school and expenditure per grade
anderson_ed <- mutate(anderson_ed, school_expenditure = Per_Pupil_Expenditures*school_enrollment)
anderson_ed <- mutate(anderson_ed, grade_expenditure = Per_Pupil_Expenditures*grade_enrollment)

#calculate income per county in 2013
county_agi_13 <- irs13_w_counties %>% 
  group_by(county) %>% 
  summarise(agi = sum(adjusted_gross_income, na.rm = T))
  
