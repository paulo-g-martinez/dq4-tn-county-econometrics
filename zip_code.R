zip_code <- read.xls("data/zip_code_database.xlsx", dec=".", blank.lines.skip = TRUE)
zip_code <- zip_code[(zip_code["state"] == "TN"),]

save(zip_code ,file="zip_code.Rda")
