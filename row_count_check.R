# establish connection to OAO oracle cloud db
con <- dbConnect(odbc(), "OracleODBC-21_5",
                 uid = "lenang01",
                 pwd = "A9xXY#fztK")

# let user select BISLR or MSHQ to determine destination table
data_site <-
  select.list(
    choices = c("MSHQ", "BISLR"),
    title = "Select Hospitals in dataset")

# set data directory based on data_site user input
if (data_site == "MSHQ") {
  dir <- paste0("/SharedDrive/deans/Presidents/SixSIgma/Individual Folders/",
                "Current Employees/Engineers/Greg Lenane/MSHQ Raw/")
} else {
  dir <- paste0("/SharedDrive/deans/Presidents/SixSIgma/Individual Folders/",
                "Current Employees/Engineers/Greg Lenane/BISLR Raw/")
}

row_count <- data.frame(
  file = 1:length(list.files(dir)),
  source = "",
  sql = "",
  match = "")
  
library(stringr)
for (i in 1:length(list.files(dir))) {
  file_name <- mixedsort(list.files(dir))[i]
  
  source_table_r <- 
    read.csv(file = paste0(dir, mixedsort(list.files(dir))[i]), header = T, sep = "~",
             stringsAsFactors = F, colClasses = rep("character", 33))
  
  row_count$source[i] <- nrow(source_table_r)
  
  sql_row <- tbl(con, paste0("DATA_", data_site, "_ORACLE")) %>%
    filter(FILE_NAME == file_name) %>%
    count() %>%
    collect()
  
  row_count$sql[i] <- sql_row %>%
    pull()
  
  if(row_count$source[i] == row_count$sql[i]) {
    row_count$match[i] <- TRUE
  } else {
    row_count$match[i] <- FALSE
  }
}
