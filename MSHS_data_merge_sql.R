# load libraries
suppressMessages({
  library(odbc)
  library(DBI)
  library(dbplyr)
  library(glue)
  library(dplyr)
  library(doParallel)
  library(gtools)
})

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
                "Current Employees/Engineers/Greg Lenane/MSHQ Raw")
} else {
  dir <- paste0("/SharedDrive/deans/Presidents/SixSIgma/Individual Folders/",
                "Current Employees/Engineers/Greg Lenane/BISLR Raw")
}

# set destination table based on user input for data site
destination_table_sql <- paste0("DATA_", data_site, "_ORACLE")

# list filenames already in the Oracle DB for user selected data_site
filenames_sql <- tbl(con, paste0("DATA_", data_site, "_ORACLE")) %>% 
  select(FILE_NAME) %>%
  distinct() %>%
  collect()
print(mixedsort(filenames_sql$FILE_NAME))

# user selects file(s) they would like to insert/overwrite within DB
data_files <- select.list(
  choices = mixedsort(list.files(dir)),
  title = "Which files would you like to insert/overwrite within the DB",
  multiple = T
)

# identify which files will be inserted vs. overwritten
inserts <- setdiff(data_files, filenames_sql$FILE_NAME)
overwrite <- intersect(data_files, filenames_sql$FILE_NAME)
cat(paste0("The following files will be INSERTED to DATA_", data_site,
           "_ORACLE: \n", paste(inserts, collapse = "\n"), "\n",
           "The following files will be OVERWRITTEN to DATA_",
           data_site, "_ORACLE: \n", paste(overwrite, collapse = "\n")))

# create syntax to delete all overwrite files
delete_syntax <- paste0("\'",overwrite, "\'", collapse = " OR ")

# create delete query for all overwrite files
sql_delete <- glue("DELETE FROM 
                        \"{destination_table_sql}\"
                     WHERE
                        FILE_NAME = {delete_syntax};")

# try catch for deletion of overwrite files
tryCatch({
  dbBegin(con)
  dbExecute(con, sql_delete)
  dbCommit(con)
  dbDisconnect(con)
},
error = function(err){
  print(err)
  dbRollback(con)
  dbDisconnect(con)
  print("error")
})

# function to convert each record of df to insert statement
get_values <- function(source_table_r = source_table_r, destination_table_sql) {
  
  PARTNER                   <- source_table_r[1]
  HOME_FACILITY             <- source_table_r[2]
  HOME_DEPARTMENT           <- source_table_r[3]
  WORKED_FACILITY           <- source_table_r[4]
  WORKED_DEPARTMENT         <- source_table_r[5]
  START_DATE                <- source_table_r[6]
  END_DATE                  <- source_table_r[7]
  EMPLOYEE_ID               <- source_table_r[8]
  EMPLOYEE_NAME             <- source_table_r[9]
  APPROVED_HOURS            <- source_table_r[10]
  POSITION_CODE             <- source_table_r[11]
  JOBCODE                   <- source_table_r[12]
  PAYCODE                   <- source_table_r[13]
  WD_HOURS                  <- source_table_r[14]
  WD_EXPENSE                <- source_table_r[15]
  HOME_DEPARTMENT_NAME      <- source_table_r[16]
  WORKED_DEPARTMENT_NAME    <- source_table_r[17]
  POSITION_CODE_DESCRIPTION <- source_table_r[18]
  LOCATION_DESCRIPTION      <- source_table_r[19]
  WD_COFT                   <- source_table_r[20]
  WD_ACCOUNT                <- source_table_r[21]
  WD_LOCATION               <- source_table_r[22]
  WD_DEPARTMENT             <- source_table_r[23]
  WD_FUND_NUMBER            <- source_table_r[24]
  HD_COFT                   <- source_table_r[25]
  HD_LOCATION               <- source_table_r[26]
  HD_DEPARTMENT             <- source_table_r[27]
  WD_COA                    <- source_table_r[28]
  HD_COA                    <- source_table_r[29]
  PAYROLL_NAME              <- source_table_r[30]
  REVERSE_MAP_WORKED        <- source_table_r[31]
  REVERSE_MAP_HOME          <- source_table_r[32]
  FILE_NAME                 <- source_table_r[33]
  
  values <- glue("INTO \"{destination_table_sql}\"
                 (PARTNER, HOME_FACILITY, HOME_DEPARTMENT, WORKED_FACILITY, 
                 WORKED_DEPARTMENT, START_DATE, END_DATE, EMPLOYEE_ID, 
                 EMPLOYEE_NAME, APPROVED_HOURS, POSITION_CODE, JOBCODE, PAYCODE,
                 WD_HOURS, WD_EXPENSE, HOME_DEPARTMENT_NAME, 
                 WORKED_DEPARTMENT_NAME, POSITION_CODE_DESCRIPTION, 
                 LOCATION_DESCRIPTION, WD_COFT, WD_ACCOUNT, WD_LOCATION, 
                 WD_DEPARTMENT, WD_FUND_NUMBER, HD_COFT, HD_LOCATION, 
                 HD_DEPARTMENT, WD_COA, HD_COA, PAYROLL_NAME, 
                 REVERSE_MAP_WORKED, REVERSE_MAP_HOME, FILE_NAME)
                 VALUES ('{PARTNER}', '{HOME_FACILITY}', '{HOME_DEPARTMENT}',
                 '{WORKED_FACILITY}', '{WORKED_DEPARTMENT}', 
                 TO_DATE('{START_DATE}', 'mm/dd/yyyy'), 
                 TO_DATE('{END_DATE}', 'mm/dd/yyyy'), 
                 '{EMPLOYEE_ID}', '{EMPLOYEE_NAME}', '{APPROVED_HOURS}', 
                 '{POSITION_CODE}', '{JOBCODE}', '{PAYCODE}', '{WD_HOURS}',
                 '{WD_EXPENSE}', '{HOME_DEPARTMENT_NAME}', 
                 '{WORKED_DEPARTMENT_NAME}', '{POSITION_CODE_DESCRIPTION}',
                 '{LOCATION_DESCRIPTION}', '{WD_COFT}', '{WD_ACCOUNT}',
                 '{WD_LOCATION}', '{WD_DEPARTMENT}', '{WD_FUND_NUMBER}',
                 '{HD_COFT}', '{HD_LOCATION}', '{HD_DEPARTMENT}', '{WD_COA}',
                 '{HD_COA}', '{PAYROLL_NAME}', '{REVERSE_MAP_WORKED}', 
                 '{REVERSE_MAP_HOME}', '{FILE_NAME}')")
  
  return(values)
}
