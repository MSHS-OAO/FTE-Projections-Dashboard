library(dplyr)
library(tidyr)
library(here)
library(gtools)

#file path for all raw files
folderOracle <- paste0("/SharedDrive/deans/Presidents/SixSigma/MSHS Productivity/",
                       "Productivity/Universal Data/Labor/Raw Data/BISLR Oracle/")
#List files from MSHQ Raw folder
Oracle_file_list <- list.files(path=folderOracle, pattern="*.csv")
details <- file.info(list.files(path = folderOracle, pattern="*.csv", full.names = T)) %>%
  mutate(path = basename(rownames(.))) %>%
  slice(mixedorder(path))
Oracle_file_list <- rownames(details)
#Read files in MSQ Raw as csv
ORACLElist = lapply(Oracle_file_list,
                    function(x)read.csv(x, sep = ",", header=T,
                                        stringsAsFactors = F,
                                        colClasses = rep("character",32),
                                        strip.white = TRUE) %>%
                      mutate(Filename = paste0(sub("\\..*", "", basename(x)), ".txt"))) 

# read rds from current raw refresh 
data_BISLR_oracle <- readRDS(paste0("/SharedDrive/deans/Presidents/SixSigma/MSHS Productivity/",
                                      "Productivity/Universal Data//Labor/RDS/data_BISLR_oracle.rds"))

# get the min start date and max end date associated with each file in rds
dates_check <- data_BISLR_oracle %>%
  select(Filename, Start.Date, End.Date) %>%
  distinct() %>%
  mutate(Start.Date = as.Date(Start.Date, format = "%m/%d/%Y"),
         End.Date = as.Date(End.Date, format = "%m/%d/%Y")) %>%
  group_by(Filename) %>%
  summarise(start = min(Start.Date),
            end = max(End.Date))

# get start and end dates in order of files
start_dates <- dates_check %>% select(start) %>% pull()
end_dates <- dates_check %>% select(end) %>% pull()
start_dates <- sort(start_dates)
end_dates <- sort(end_dates)

# filter each file by their respective start and end dates
for (i in 1:nrow(dates_check)) {
  ORACLElist[[i]] <- ORACLElist[[i]] %>%
    filter(as.Date(Start.Date, format = "%m/%d/%Y") >= start_dates[i],
           as.Date(End.Date, format = "%m/%d/%Y") <= end_dates[i])
}

# data types for column names and column data type
data_types <- c(PARTNER = "VARCHAR2(6 CHAR)",
                HOME_FACILITY = "VARCHAR2(6 CHAR)",
                HOME_DEPARTMENT = "VARCHAR2(15 CHAR)",
                WORKED_FACILITY = "VARCHAR2(6 CHAR)",
                WORKED_DEPARTMENT = "VARCHAR2(15 CHAR)",
                START_DATE = "DATE",
                END_DATE = "DATE",
                EMPLOYEE_ID = "VARCHAR2(7 CHAR)",
                EMPLOYEE_NAME = "VARCHAR2(60 CHAR)",
                APPROVED_HOURS = "VARCHAR2(6 CHAR)",
                POSITION_CODE = "VARCHAR2(15 CHAR)",
                JOBCODE = "VARCHAR2(50 CHAR)",
                PAYCODE = "VARCHAR2(60 CHAR)",
                WD_HOURS = "NUMERIC(6,2)",
                WD_EXPENSE = "NUMERIC(10,2)",
                HOME_DEPARTMENT_NAME = "VARCHAR2(60 CHAR)",
                WORKED_DEPARTMENT_NAME = "VARCHAR2(60 CHAR)",
                POSITION_CODE_DESCRIPTION = "VARCHAR2(80 CHAR)",
                LOCATION_DESCRIPTION = "VARCHAR2(80 CHAR)",
                WD_COFT = "VARCHAR2(7 CHAR)",
                WD_ACCOUNT = "VARCHAR2(6 CHAR)",
                WD_LOCATION = "VARCHAR2(3 CHAR)",
                WD_DEPARTMENT = "VARCHAR2(5 CHAR)",
                WD_FUND_NUMBER = "VARCHAR2(11 CHAR)",
                HD_COFT = "VARCHAR2(7 CHAR)",
                HD_LOCATION = "VARCHAR2(80 CHAR)",
                HD_DEPARTMENT = "VARCHAR2(5 CHAR)",
                WD_COA = "VARCHAR2(44 CHAR)",
                HD_COA = "VARCHAR2(44 CHAR)",
                PAYROLL_NAME = "VARCHAR2(15 CHAR)",
                REVERSE_MAP_WORKED = "VARCHAR2(45 CHAR)",
                REVERSE_MAP_HOME = "VARCHAR2(45 CHAR)",
                FILE_NAME = "VARCHAR2(60 CHAR)")

# apply names of each data type to the columns in each file for easy imoirt in DB
for (i in 1:length(ORACLElist)) {
  colnames(ORACLElist[[i]]) <- names(data_types)
}

# convert all dates to Oracle default date format
for (i in 1:length(ORACLElist)) {
  ORACLElist[[i]]$START_DATE <- as.Date(ORACLElist[[i]]$START_DATE, format = "%m/%d/%Y")
  ORACLElist[[i]]$END_DATE <- as.Date(ORACLElist[[i]]$END_DATE, format = "%m/%d/%Y")
}

# replace blank job codes with "MISSING_JOBCODE" because jobcode column in not null in DB
for (i in 1:length(ORACLElist)) {
  ORACLElist[[i]] <- ORACLElist[[i]] %>%
    mutate(
      JOBCODE = case_when(
        JOBCODE == "" ~ "MISSING_JOBCODE",
        TRUE ~ JOBCODE
      )
    )
}

# save each file to folder
for (i in 1:length(ORACLElist)) {
  write.table(ORACLElist[[i]],
              file = paste0("/SharedDrive/deans/Presidents/SixSigma/Individual Folders/Current Employees/Engineers/Greg Lenane/BISLR Raw/",
                            i,"_BISLR_Raw.txt"),
              row.names = F, sep = "~")
}
