
# MSH MSQ Oracle 

rm(list=ls())

# Import libraries
suppressMessages({
  library(tidyverse)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(here)
  library(gsubfn)
  library(rstudioapi)
})


memory.limit(size = 8000000)

# Working directory
#dir <- "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Universal Data/Labor/Raw Data/"
dir <- "C:/Users/aghaer01/Downloads/FTE-Projections-Dashboard-Oracle_CC"

#universal directory
universal_dir <- "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Universal Data/"


#Read COA for department location
coa <- read.csv(paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/FEMA Reimbursement/",
                       "MSHS-FEMA-Reimbursement/Reference Tables/COA.csv"), header = T, stringsAsFactors = F, strip.white = TRUE)


# import pay cycle data and filter required date
Pay_Cycle_data <- read_xlsx(paste0(universal_dir,  "Mapping/MSHS_Pay_Cycle.xlsx"))
Pay_Cycle_data <- Pay_Cycle_data %>% filter(PREMIER.DISTRIBUTION== "TRUE")


# get unique end date
unique_end_date <- unique(Pay_Cycle_data$END.DATE)
unique_end_date <- as.Date(unique_end_date)



#Import the latest aggregated file 
repo <- file.info(list.files(path = paste0(dir,"/New Data/MSHQ_Oracle_Repo"), full.names = T , pattern = "data_MSH_MSQ_oracle"))
repo_file <- rownames(repo)[which.max(repo$ctime)]
repo <- readRDS(repo_file)

# Get the max date in repo
repo_max_date <- as.numeric(format(max(repo$END.DATE), "%m") )


# Import the most recent data
details = file.info(list.files(path = paste0(dir,"/Raw Data/MSHQ Oracle/"), pattern="*.txt", full.names = T)) %>% arrange(mtime)
details = details[with(details, order(as.POSIXct(ctime),  decreasing = F)), ]

# get the month of the most recent data set
new_data_date <- rownames(details)[which.max(details$ctime)]
new_data_date <- as.numeric(substr(gsub('.*-([0-9]+)_','\\', new_data_date ), 1,2))

# Find the difference between the max date in repo and the current data
dif_time <- new_data_date - repo_max_date


Oracle_file_list <- rownames(tail(details, n = dif_time ))

#Read files in MSQ Raw as csv
Oracle <- lapply(Oracle_file_list, function(x){read.csv(x, sep = "~", header=T,
                                        stringsAsFactors = F,
                                        colClasses = rep("character",32),
                                        strip.white = TRUE)})

Oracle <- lapply(Oracle, transform, End.Date =  as.Date(End.Date, format = "%m/%d/%Y"),
                         Start.Date = as.Date(Start.Date, format = "%m/%d/%Y"))


# get the required end_date and start date
#start_dates <- as.Date(c("2022-01-01", "2022-01-29" ))
#end_dates <- as.Date(c("2022-01-29", "2022-02-26"))
end_dates <- tail(unique_end_date, n = dif_time )
start_dates <- lag(unique_end_date, n=1)
start_dates <- tail(start_dates, n = dif_time )




#Filtering each file by start/end date specified
Oracle <- lapply(1:length(Oracle), function(x)
  Oracle[[x]] <- Oracle[[x]] %>%
    filter(End.Date <= end_dates[x],
           Start.Date >= start_dates[x]))



Oracle <- do.call("rbind", Oracle )


#Remove Duplicate rows and add worked entity column
Oracle  <- Oracle  %>%  mutate(WRKD.ENTITY = substr(WD_COFT,1,3),
                             Hours = as.numeric(Hours), Expense = as.numeric(Expense))
                        
Oracle  <- Oracle  %>% group_by_at(c(1:13,16:33)) %>% summarise(Hours = sum(Hours, na.rm = T),
            Expense = sum(Expense,na.rm = T)) %>% ungroup() %>% distinct()


#Determine PAYROLL based on WRKD.ENTITY
Oracle  <- Oracle  %>% mutate(PAYROLL = case_when(WRKD.ENTITY == "102" ~ "MSQ", TRUE ~ "MSH"))



#Check sum of hours by end date to make sure data follows proper pattern
check <- Oracle  %>%
  ungroup() %>%
  group_by(PAYROLL,End.Date) %>%
  summarise(Hours = sum(Hours)) %>%
  mutate(End.Date = as.Date(End.Date, format = "%m/%d/%Y")) %>%
  arrange(End.Date) 
check1 <- pivot_wider(check,id_cols = PAYROLL,values_from = Hours,names_from = End.Date)



#Read in job code descriptions
jc_desc <- read_xlsx(paste0(universal_dir, "Mapping/MSHS_Jobcode_Mapping.xlsx")) %>%
                     filter(PAYROLL == "MSHQ") %>% select(J.C, J.C.DESCRIPTION)


#Replace departments that failed GEAC map
Oracle  <- Oracle  %>% mutate(Reverse.Map.for.Worked = case_when(
    Reverse.Map.for.Worked == "" ~ Department.IdWHERE.Worked, TRUE ~ Reverse.Map.for.Worked),
    Reverse.Map.for.Home = case_when(
    Reverse.Map.for.Home == "" ~ paste0(HD_COFT, HD_Location, HD_Department), TRUE ~ Reverse.Map.for.Home)) %>%
    #Build full COA for Oracle  home department
    mutate(Department.ID.Home.Department = paste0(substr(Full.COA.for.Home, 1, 3),
                                                  substr(Full.COA.for.Home,  41, 44),
                                                  substr(Full.COA.for.Home,  5, 7),
                                                  substr(Full.COA.for.Home,  12, 16)))

#Take first 8 digits of Home and Worked department for reverse map
Oracle  <- Oracle  %>% mutate( Reverse.Map.for.Worked = case_when(nchar(Reverse.Map.for.Worked) == 12 ~
                             substr(Reverse.Map.for.Worked, 1, 8), TRUE ~ Reverse.Map.for.Worked),
                             Reverse.Map.for.Home = case_when(nchar(Reverse.Map.for.Home) == 12 ~
                             substr(Reverse.Map.for.Home, 1, 8), TRUE ~ Reverse.Map.for.Home))                                                      



#Bring in department location
row_count <- nrow(Oracle )
Oracle  <- left_join(Oracle , coa, by = c("Reverse.Map.for.Worked" = "Column2")) %>% select(1:35)
  
if(nrow(Oracle ) != row_count) {
  stop(paste("Row count failed at", basename(getSourceEditorContext()$path)))
}

row_count <- nrow(Oracle )
Oracle  <- left_join(Oracle , coa, by = c("Reverse.Map.for.Home" = "Column2")) %>% select(1:36)

if(nrow(Oracle ) != row_count) {
  stop(paste("Row count failed at", basename(getSourceEditorContext()$path)))
}


#Bring in standardized JC Description
row_count <- nrow(Oracle )
Oracle  <- left_join(Oracle , jc_desc, by = c("Job.Code" = "J.C"))
if(nrow(Oracle ) != row_count) {
  stop(paste("Row count failed at", basename(getSourceEditorContext()$path)))
}

#Format necessary columns
Oracle  <- Oracle  %>% mutate(End.Date = as.Date(End.Date, format = "%m/%d/%Y"),
                              Hours = as.numeric(Hours), Expense = as.numeric(Expense))
#Column names
new_col_names <- c("DPT.WRKD", "DPT.HOME", "START.DATE", "END.DATE", "J.C",
                   "PAY.CODE", "HOME.DESCRIPTION", "WRKD.DESCRIPTION", "HOURS",
                   "EXPENSE", "WRKD.LOCATION", "HOME.LOCATION", "J.C.DESCRIPTION")
                   
colnames(Oracle )[c(3, 5, 6, 7, 12:15, 32, 33, 35:37)] <- new_col_names


# Bind NEW data with repository
new_repo <- rbind(repo, Oracle)
new_repo <- new_repo %>% distinct()





#save RDS
saveRDS(new_repo, file = paste0("C:\\Users\\aghaer01\\Downloads\\FTE-Projections-Dashboard-Oracle_CC\\New Data\\data_MSH_MSQ_oracle-", Sys.Date(),".rds"))
  



