setwd("/Users/xandermorrison/git/data-question-4-data-question-4-supreme-watch")

install.packages("gdata")
library("gdata")

options(stringsAsFactors = FALSE)

df <- read.xls("12zp43tn.xls")

View(df)

columns <- c(df[3,])
columns_2 <- c(df[4,])

for (column in columns) {
  print(column)
}

c_names <- list()

for (i in 1:length(columns)) {
  if (i == 1) {
    c1 <- columns[i]
  }
  if (columns[i] != "") {
    c1 <- columns[i]
  }
  full_name <- paste(c1, columns_2[i])
  full_name <- gsub("[.*]|\\n", "", full_name)
  print(full_name)
}
