# XANDER's github

setwd("/Users/ghfmhf/git/data-question-4-data-question-4-supreme-watch")

library("gdata")
library("tidyverse")

options(stringsAsFactors = FALSE)

files <- list.files(path="./data/tax_data")

for (f in files) {
  v = paste("df", substr(f, 1, 2), sep="")
  print(v)
  d = paste("./data/tax_data", f, sep="/")
  assign(v, read.xls(d, dec = ".", blank.lines.skip = T))
}

dfs = list(df11, df12, df13, df14, df15)

column_assigner <- function(df) {
  columns <- c(df[3,])
  columns_2 <- c(df[4,])
  
  for (i in 1:length(columns)) {
    if (i == 1) {
      c1 <- columns[i]
    }
    if (columns[i] != "") {
      c1 <- columns[i]
    }
    full_name <- paste(c1, columns_2[i])
    full_name <- gsub("\\[.*\\]|\\n|\\(.*\\)|'", "", full_name)
    full_name <- str_trim(full_name)
    full_name <- gsub(" +|-", "_", full_name)
    print(str_to_lower(full_name))
    df[5,i] <- str_to_lower(full_name)
  }
  colnames(df) <- df[5,]
  df <- df[-c(1:5),]
  df <- df[, !names(df) %in% c("na_na")]
  df
}

clean_zips <- function(df) {
  df2 <- df[!(df$size_of_adjusted_gross_income == "Total" | df$size_of_adjusted_gross_income == ""),]
  df2[,3:ncol(df2)] = sapply(df2[,3:ncol(df2)], function(x) suppressWarnings(as.numeric(as.character(x))))
  for (name in names(df2)) {
    if (grepl("zip", name)) next
    if (grepl("size_of_adjusted_gross_income", name)) next
    if (grepl("number", name)) next
    
    df2[name] <- df2[name] * 1000
  }
  df2
}

dfs <- lapply(dfs, column_assigner)
dfs <- lapply(dfs, clean_zips)

ach_profile <- read_csv("data/achievement_profile_data_with_CORE.csv")
zip_cleaned <- zip_code()


# The ZIP code script is commented to avoid running every time slowing down the procedure
# zip_code <- read.xls("data/zip_code_database.xlsx", dec=".", blank.lines.skip = TRUE)
# zip_code <- zip_code[(zip_code["state"] == "TN"),]


membership <- read_csv("data/data_2015_membership_school.csv") %>% 
  filter(race_or_ethnicity == "All Race/Ethnic Groups", gender == "All Genders")

crosswalk <- read_xls("data/data_district_to_county_crosswalk.xls")

# Joining two databases (District number in Crosswalk is tha same as system in school data)
merged_ed <- merge(ach_profile, crosswalk, by.y = "District Number", by.x = "system", all.x = T)

df_irs_11 <- dfs[[1]] %>% 
  group_by(zipcode) %>% 
  summarise(num_ret_tot=sum(number_of_returns, na.rm = T), avg_agi=mean(adjusted_gross_income, na.rm = T))

irs_ed_merge <- merge(df_irs_11,zip_code, by.x = "zipcode", by.y = "zip", all.x = T) %>% 
  drop(              
       [2] "num_ret_tot"                  
       [3] "avg_agi"                      
       [4] "type"                         
       [5] "decommissioned"               
       [6] "primary_city"                 
       [7] "acceptable_cities"            
       [8] "unacceptable_cities"          
       [9] "state"                        
       [10] "county"                       
       [11] "timezone"                     
       [12] "area_codes"                   
       [13] "world_region"                 
       [14] "country"                      
       [15] "latitude"                     
       [16] "longitude"                    
       [17] "irs_estimated_population_2014")


