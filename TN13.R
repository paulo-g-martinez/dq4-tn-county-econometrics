# startup settings
setwd("/Users/ghfmhf/git/data-question-4-data-question-4-supreme-watch")
library("tidyverse")
library("magrittr")
library("purrr", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("readxl", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")

# Reading data
tn2013xls <- read_excel("13zp43tn.xls", range = cell_rows(4:4725))

# This block will create unique column names in two steps:
# 1. Fill the blank column with the same header left to it
for (i in 1:length(tn2013xls)) {
  top_row_3 <- substr(colnames(tn2013xls)[i], 1, 3)
  if (top_row_3 == "X__") { #blank / merged header
    colnames(tn2013xls)[i] <- colnames(tn2013xls)[i-1]  
  }
}
# 2. Merge rows 4 & 5 and put it in row 5. 

for (i in 1:length(tn2013xls)) { # This IF block prevents NA values be merged 
  if (is.na(tn2013xls[1,i]) == TRUE) {
    tn2013xls[1,i] <- colnames(tn2013xls)[i]
  } else {
    tn2013xls[1,i] <- paste(colnames(tn2013xls)[i] , tn2013xls[1,i])
  }
}
# Row 5 will have the unique title

# Standardize column headers
# everything lowercase
tn2013xls[1,] <- tolower(tn2013xls[1,])

# Remove space from beginning and end
tn2013xls[1,] <- str_trim(tn2013xls[1,]) 

# Replace space with underscore
tn2013xls[1,] <- gsub(" +|-","_",tn2013xls[1,])

# Remove brackets and parantheses
tn2013xls[1,] <- gsub("\\[.*\\]|\\n|\\(.*\\)|'", "",tn2013xls[1,])


