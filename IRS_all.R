# startup settings
setwd("/Users/ghfmhf/git/data-question-4-data-question-4-supreme-watch")
library("tidyverse")
library("magrittr")
library("purrr", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("readxl", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")

# list of spreadsheets
irs_xls <-  list.files(path=".", pattern = "??zp43tn.xls")

# This FOR loop creates dataframe names for 2011 to 2015 spreadsheets
for (i in irs_xls) {
  df_name <-  paste("df_irs_", substr(i, 1, 2), sep="")
  print(paste(df_name," is:",i))
  # Creating dataframes 
  assign(df_name, read_excel(i, range = cell_rows(4:4725)))
}

df_list = list(df_irs_11, df_irs_12, df_irs_13, df_irs_14, df_irs_15)

create_header <- function(df_x) {
    # A1. Fill the blank column with the same header left to it
    for (i in 1:length(df_x)) {
      top_row_3 <- substr(colnames(df_x)[i], 1, 3)
      if (top_row_3 == "X__") { #blank / merged header
        colnames(df_x)[i] <- colnames(df_x)[i-1]  
      }
    }
    # A2. Merge rows 4 & 5 and put it in row 5. 
    
    for (i in 1:length(df_x)) { # This IF block prevents NA values be merged 
      if (is.na(df_x[1,i]) == TRUE) {
        df_x[1,i] <- colnames(df_x)[i]
      } else {
        df_x[1,i] <- paste(colnames(df_x)[i] , df_x[1,i])
      }
    }
    # A: Row 5 will have the unique title
    
    # B: Standardize column headers
    # B1. everything lowercase
    df_x[1,] <- tolower(df_x[1,])
    
    # B2. Remove space from beginning and end
    df_x[1,] <- str_trim(df_x[1,]) 
    
    # B3. Replace space with underscore
    df_x[1,] <- gsub(" +|-","_",df_x[1,])
    
    # B4. Remove brackets and parantheses
    df_x[1,] <- gsub("\\[.*\\]|\\n|\\(.*\\)|'", "",df_x[1,])
}

# Run the function for all dataframes
df_list <- lapply(df_list, create_header)