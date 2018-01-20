# startup settings
setwd("/Users/ghfmhf/git/data-question-4-data-question-4-supreme-watch")
library("tidyverse")
library("magrittr")
library("purrr", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("readxl", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("gdata", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")

options(stringsAsFactors=F)
# list of spreadsheets
irs_xls <-  list.files(path=".", pattern = "??zp43tn.xls")

# This FOR loop creates dataframe names for 2011 to 2015 spreadsheets
for (i in irs_xls) {
  df_name <-  paste("df_irs_", substr(i, 1, 2), sep="")
  print(paste(df_name," is:",i))
  # Creating dataframes 
  assign(df_name, read.xls(i,col_names = F))
}

df_list = list(df_irs_11, df_irs_12, df_irs_13, df_irs_14, df_irs_15)

create_header <- function(df_x) {
    # A1. Fill the blank column with the same header left to it
    df_cols <- c(df_x[4,])
    df_cols2 <- c(df_x[5,])
  
    for (i in 1:length(df_cols)) {
      if (i == 1) {
        c1 <- df_cols[i]
      }
      print(df_cols[i])
      if (df_cols[i] != "") {
        c1 <- df_cols[i]
      }
      
      full_name <- paste(c1, df_cols2[i])
      # B1. everything lowercase
      full_name <- str_to_lower(full_name)
      
      # B2. Remove space from beginning and end
      full_name <- str_trim(full_name) 
      
      # B3. Replace space with underscore
      full_name <- gsub(" +|-","_",full_name)
      
      # B4. Remove brackets and parantheses
      full_name <- gsub("\\[.*\\]|\\n|\\(.*\\)|'", "",full_name)
      df_x[5,i] <- full_name
      
      # if (is.na(df_x[3,i]) == T) { 
      #   df_x[i] <- df_x[i + 1]  
      # }
    }
    colnames(df_x) <- df_x[5,]
    df_x <- df_x[-c(1:5),]
    df_x <- df_x[, !names(df_x) %in% c("na_na")]
    df_x
    

    # 
    # # A2. Merge rows 4 & 5 and put it in row 5. 
    # 
    # for (i in 1:length(df_x)) { # This IF block prevents NA values be merged 
    #   df_x[2,i] <- paste(df_x[1,i] , df_x[2,i])
    #   }
    # 
    # # A: Row 5 will have the unique title
    
    # B: Standardize column headers
    # B1. everything lowercase
    # df_x[2,] <- tolower(df_x[1,])
    # 
    # # B2. Remove space from beginning and end
    # df_x[2,] <- str_trim(df_x[1,]) 
    # 
    # # B3. Replace space with underscore
    # df_x[2,] <- gsub(" +|-","_",df_x[1,])
    # 
    # # B4. Remove brackets and parantheses
    # df_x[2,] <- gsub("\\[.*\\]|\\n|\\(.*\\)|'", "",df_x[1,])
}

# Run the function for all dataframes
df_list <- lapply(df_list, create_header)