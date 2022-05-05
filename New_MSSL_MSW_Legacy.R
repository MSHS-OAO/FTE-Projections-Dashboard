

### MSM- MSW Legacy

rm(list=ls())

# Import libraries
suppressMessages({
  library(readxl)
  library(tidyverse)
  library(dplyr)
  library(tidyr)
  library(here)
  library(rstudioapi)
})



memory.limit(size = 8000000)

# Working directory ----------------------------------------------------------
dir <- "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Universal Data/Labor/"
#dir <- "C:/Users/aghaer01/Downloads/FTE-Projections-Dashboard-Oracle_CC"



#universal directory
universal_dir <- "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Universal Data/"


# Imports data ----------------------------------------------------------
dict_cc_conversion <- read_xlsx(paste0(universal_dir,
                                       "/Mapping/MSHS_Code_Conversion_Mapping.xlsx"))


#Import the latest aggregated file 
repo <- file.info(list.files(path = paste0(dir,"REPOS/MSSLW_Repo/"), full.names = T , pattern = "data_MSSL_MSW"))
repo_file <- rownames(repo)[which.max(repo$ctime)]
repo <- readRDS(repo_file) 


# Import the most recent data
details = file.info(list.files(path = paste0(dir,"Raw Data/MSMW Legacy/MSMW Legacy"), pattern="*.xlsx", full.names = T)) %>% arrange(mtime)
details = details[with(details, order(as.POSIXct(ctime),  decreasing = F)), ]

MSMW_file_list <- rownames(details)[!(rownames(details) %in% repo$Filename) ]
MSMW_file_list <- MSMW_file_list[!(MSMW_file_list %in% 
                                     c( "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Universal Data/Labor/Raw Data/MSMW Legacy/MSMW Legacy/FEMA_MSSLW_JUN_JUL_AUG_SEP_2020_v1.xlsx",
                                       "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Universal Data/Labor/Raw Data/MSMW Legacy/MSMW Legacy/MSSLW_JAN_DEC19 and JAN_APR20 (3).xlsx" ))]

MSMW_data_list <- lapply(MSMW_file_list, function(x){
  data <- read_xlsx(x, sheet= "Export Worksheet", ) %>% mutate(Filename = x,
                                                               `START DATE`= paste0(substr(`START DATE`,1,2), "/",
                                                                                    substr(`START DATE`,3,4), "/",
                                                                                    substr(`START DATE`,5,8)),
                                                               `START DATE` = as.Date(`START DATE`, "%m/%d/%Y"),
                                                               `END DATE` = paste0(substr(`END DATE`,1,2), "/",
                                                                                   substr(`END DATE`,3,4), "/",
                                                                                   substr(`END DATE`,5,8)),
                                                               `END DATE` = as.Date(`END DATE`, "%m/%d/%Y")
                                                               )})

start_dates <- as.Date("01/01/2022", format = "%m/%d/%Y")  
end_dates <- as.Date("01/01/2022", format = "%m/%d/%Y")


#Filtering each file by start/end date specified
MSMW_data_list <- lapply(1:length(MSMW_data_list), function(x)
  MSMW_data_list[[x]] <- MSMW_data_list[[x]] %>%
    filter(`END DATE` <= end_dates[x],
           `END DATE` >= start_dates[x]))




MSMW_raw_data <- do.call("rbind", MSMW_data_list)



# Preprocessing Data ------------------------------------------------------
dict_cc_conversion <- dict_cc_conversion %>%
  filter(PAYROLL %in% c("MSMW")) %>%
  select(COST.CENTER.LEGACY, COST.CENTER.ORACLE) %>%
  distinct()


#Determine PAYROLL based on WRKD.ENTITY
MSMW_raw_data   <- MSMW_raw_data   %>% mutate(PAYROLL = case_when(`Facility Hospital Id_Worked` == "NY2162" ~ "MSW", TRUE ~ "MSM"))


# Cost Center ("Department") ---------------------------------------------
MSMW_raw_data$DPT.WRKD.Legacy <- paste0(MSMW_raw_data$WD_COFT,
                                        MSMW_raw_data$WD_Location,
                                        MSMW_raw_data$WD_Department)
MSMW_raw_data$DPT.HOME.Legacy <- paste0(MSMW_raw_data$HD_COFT,
                                        MSMW_raw_data$HD_Location,
                                        MSMW_raw_data$HD_Department)

# Add Oracle Cost Centers -------------------------------------------------
row_count <- nrow(MSMW_raw_data)
#Looking up oracle conversion for legacy home cost center
MSMW_raw_data <- left_join(MSMW_raw_data, dict_cc_conversion,
                           by = c("DPT.HOME.Legacy" = "COST.CENTER.LEGACY"))

#Looking up oracle conversion for legacy worked cost center
MSMW_raw_data <- left_join(MSMW_raw_data, dict_cc_conversion,
                           by = c("DPT.WRKD.Legacy" = "COST.CENTER.LEGACY"))


#Renaming columns
MSMW_raw_data <- MSMW_raw_data %>%
  rename(DPT.HOME = COST.CENTER.ORACLE.x,
         DPT.WRKD = COST.CENTER.ORACLE.y)

if(nrow(MSMW_raw_data) != row_count){
  stop(paste("Row count failed at", basename(getSourceEditorContext()$path)))}

#If there is no Oracle format use the Legacy cost center
MSMW_raw_data <- MSMW_raw_data %>%
  mutate(DPT.HOME = case_when(
    is.na(DPT.HOME) ~ DPT.HOME.Legacy,
    TRUE ~ DPT.HOME),
    DPT.WRKD = case_when(
      is.na(DPT.WRKD) ~ DPT.WRKD.Legacy,
      TRUE ~ DPT.WRKD))


# Remove Duplicates -------------------------------------------------------
data_MSSL_MSW <- MSMW_raw_data %>% distinct()


# Import References -------------------------------------------------------
dict_jobcodes_all <- read_xlsx(paste0(universal_dir,
                                      "/Mapping/MSHS_Jobcode_Mapping.xlsx"))


dict_jobcodes <- dict_jobcodes_all %>%
  filter(PAYROLL == "MSMW") %>%
  select(J.C, J.C.DESCRIPTION) %>%
  rename(`Position Code Description` = J.C.DESCRIPTION) %>%
  distinct_at("Position Code Description", .keep_all = T)


dict_jobcodes_MSBIB <- dict_jobcodes_all %>%
  filter(PAYROLL == "MSBIB") %>%
  select(J.C, J.C.DESCRIPTION) %>%
  rename(`Position Code Description` = J.C.DESCRIPTION, J.C_MSBIB = J.C) %>%
  distinct_at("Position Code Description", .keep_all = T)




# Lookup Jobcodes ---------------------------------------------------------
data_MSSL_MSW <- data_MSSL_MSW %>%
  mutate(`Position Code Description` = case_when(
    is.na(`Position Code Description`) ~ "OTHER",
    TRUE ~ `Position Code Description`))

#Checking row count before left join
row_count <- nrow(data_MSSL_MSW)
data_MSSL_MSW <- left_join(data_MSSL_MSW, dict_jobcodes)

#If rows added during left join stop executing code
if(nrow(data_MSSL_MSW ) != row_count){
  stop(paste("Row count failed at", basename(getSourceEditorContext()$path)))}

#Checking row count before left join
row_count <- nrow(data_MSSL_MSW)
data_MSSL_MSW <- left_join(data_MSSL_MSW, dict_jobcodes_MSBIB)

#If rows added during left join stop executing code
if(nrow(data_MSSL_MSW ) != row_count){
  stop(paste("Row count failed at", basename(getSourceEditorContext()$path)))}

data_MSSL_MSW <- data_MSSL_MSW %>%
  mutate(J.C = case_when(
    is.na(J.C) ~ J.C_MSBIB,
    TRUE ~ J.C),
    J.C_MSBIB = NULL)

# Lookup Location ---------------------------------------------------------
data_MSSL_MSW <- data_MSSL_MSW %>%
  mutate(WRKD.LOCATION = `Location Description`, HOME.LOCATION = NA)


# Lookup Site -------------------------------------------------------------
#Determine Home.Site and WRKD.SITE 
data_MSSL_MSW <- data_MSSL_MSW %>% mutate(HOME.SITE = case_when(`Home FacilityOR Hospital ID` == "NY2162" ~ "MSW", TRUE ~ "MSSL"),
                                          WRKD.SITE= case_when(`Facility Hospital Id_Worked` == "NY2162" ~ "MSW", TRUE ~ "MSSL") )




# Rename Columns ---------------------------------------------------
data_MSSL_MSW <- data_MSSL_MSW %>%
  rename(HOURS = Hours,
         EXPENSE = Expense,
         WRKD.DESCRIPTION = `Department Name Worked Dept`,
         HOME.DESCRIPTION = `Department Name Home Dept`,
         PAY.CODE = `Pay Code`,
         J.C.DESCRIPTION = `Position Code Description`,
         END.DATE = `END DATE`,
         LIFE = `Employee ID`)

# Format Columns ----------------------------------------------------------
data_MSSL_MSW <- data_MSSL_MSW %>%
  mutate(END.DATE = as.Date(END.DATE, format = "%m/%d/%Y"),
         PAY.CODE = as.character(PAY.CODE))


# Bind NEW data with repository
new_repo <- rbind(repo, data_MSSL_MSW)
new_repo <- new_repo  %>% distinct()


#save RDS
#saveRDS(new_repo , file = paste0("C:\\Users\\aghaer01\\Downloads\\FTE-Projections-Dashboard-Oracle_CC\\New Data\\MSSLW_Repo\\data_MSSL_MSW_", Sys.Date(),".rds"))

saveRDS(data_MSSL_MSW, file = paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Universal Data/Labor/REPOS/MSSLW_Repo/data_MSSL_MSW_ ", Sys.Date(),".rds"))

