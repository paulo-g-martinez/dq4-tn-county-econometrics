df_irs_tn11 <- read.xls("11zp43tn.xls", )
options(stringsAsFactors = FALSE)
View(df_irs_tn11)

columns <- c(df_irs_tn11[3,])
columns_2 <- c(df_irs_tn11[4,])

c_names <- list()

for (i in 1:length(columns)) {
  if (i == 1) {
    c1 <- columns[i]
  }
  if (columns[i] != "") {
    c1 <- columns[i]
  }
  print(paste(c1, columns_2[i]))
}
# this function should fill blank cells in the 3rd row with the preceding cells string
for (i in 1:length(df_irs_tn11[3,])) {
  string <- "dummy string"
  if (df_irs_tn11[3,i] == "") {
    df_irs_tn11[3,i] <- df_irs_tn11[3,i-1]
  }
}
# let's try that again but just on columns
for (i in 1:length(columns)) {
  if (columns[i] == "") {
    columns[i] <- columns[i-1]
  }
}
