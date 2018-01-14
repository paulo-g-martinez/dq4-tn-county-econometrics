library("dplyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("magrittr", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("tidyverse", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")

#read in the irs 2011 csv (becaue the xls was too troublesome)
irs11 <- read.csv("https://www.irs.gov/pub/irs-soi/11zpallagi.csv", 
         header = T, dec = ".", blank.lines.skip = T, stringsAsFactors = F) %>% 
  filter(STATE == "TN")
#spot check 
class(irs11)
View(irs11)

#read in the table of names which I reconstructed from the irs word doc
library("gdata", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
names_table <- read.xls("column names tn irs 11.xlsx", sheet = 5,
                        pattern = "STATEFIPS", blank.lines.skip = T, header = F, stringsAsFactors = F)
str(names_table)
View(names_table)

#make the names lower case
names_table[,2] %>% 
  str_to_lower() %>% 
#trim whitespace
  str_trim() %>% 
#sub all blank space for underscores
  
#substitute the column names for the meaningful names
colnames(irs11) <- names_table[,2]
View(irs11)
