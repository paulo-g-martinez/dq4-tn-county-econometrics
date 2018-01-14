library("gdata")
library("tidyverse")

# string vector of file names in our local tax_data folder
files <- list.files(path = "./data/tax_data")

for (f in files) {
  #make new variable name out of df and the first two characters of the target file name
  v = paste("df", substr(f, 1, 2), sep = "")
  print(v) #spot check
  #making a string path to each target file
  d = paste("./data/tax_data", f, sep = "/")
  #reading in each irs xls into the variable created for it
  assign(v, read.xls(d, stringsAsFactors = FALSE))
}

# function to untangle the xls columns names mess the irs gave us
column_assigner <- function(df) {
  columns <-c(df[3,]) #column names component from the third forw
  columns_2 <-c(df[4,]) #column names component from the fourth row
  
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

View(df11)
View(df12)
View(df13)
View(df14)
View(df15)