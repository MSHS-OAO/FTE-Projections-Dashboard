
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

# Working directory -------------------------------------------------------------
dir <- "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Universal Data/Labor"
#dir <- "C:/Users/aghaer01/Downloads/FTE-Projections-Dashboard-Oracle_CC"

#universal directory
universal_dir <- "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Universal Data/"


# Import data -------------------------------------------------------------
#Read COA for department location
coa <- read.csv(paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/FEMA Reimbursement/",
                       "MSHS-FEMA-Reimbursement/Reference Tables/COA.csv"), header = T, stringsAsFactors = F, strip.white = TRUE)


# import pay cycle data and filter required date
# Pay_Cycle_data <- read_xlsx(paste0(universal_dir,  "Mapping/MSHS_Pay_Cycle.xlsx"), col_types =c("date" ,"date" , "date" , "numeric") )
# Pay_Cycle_data <- Pay_Cycle_data %>% mutate(DATE =as.Date(DATE),
#                                             START.DATE= as.Date(START.DATE),
#                                             END.DATE= as.Date(END.DATE))


#Read in job code descriptions
jc_desc <- read_xlsx(paste0(universal_dir, "Mapping/MSHS_Jobcode_Mapping.xlsx")) %>%
  filter(PAYROLL == "MSHQ") %>% select(J.C, J.C.DESCRIPTION)



#Import the latest aggregated file 
repo <- file.info(list.files(path = paste0(dir,"/REPOS/MSHQ_Oracle_Repo"), full.names = T , pattern = "data_MSH_MSQ_oracle"))
repo_file <- rownames(repo)[which.max(repo$ctime)]
repo <- readRDS(repo_file)

# get max date in repo
max(repo$END.DATE)


# Run this if you need to update a data in repo
#repo <- repo %>% filter(Filename != "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Universal Data/Labor/Raw Data/MSHQ Oracle/MSHQ Oracle/25_MSH_LD_FTI_MAY-22_05_16_2022_0243.txt"")



# Import the most recent data
details = file.info(list.files(path = paste0(dir,"/Raw Data/MSHQ Oracle/MSHQ Oracle/"), pattern="*.txt", full.names = T)) %>% arrange(mtime)
details = details[with(details, order(as.POSIXct(ctime),  decreasing = F)), ]


# read the file that is not in the repos
Oracle_file_list <- rownames(details)[!(rownames(details) %in% repo$Filename) ]

#Read files in MSQ Raw as csv
ORACLElist <- lapply(Oracle_file_list, function(x){
                  data <- read.csv(x, sep = "~", header=T,
                                      stringsAsFactors = F,
                                      colClasses = rep("character",32),
                                      strip.white = TRUE) %>% mutate( Filename = x)
})



# get the required end_date and start date
#start_dates <- Pay_Cycle_data$START.DATE[Pay_Cycle_data$DATE== Sys.Date()]
start_dates <- as.Date(c("02/26/2022", "03/26/2022" ), format = "%m/%d/%Y")
                         

#end_dates <- Pay_Cycle_data$END.DATE[Pay_Cycle_data$DATE== Sys.Date()]
end_dates <- as.Date(c("03/26/2022","04/23/2022"), format = "%m/%d/%Y")



#Filtering each file by start/end date specified
ORACLElist <- lapply(1:length(ORACLElist), function(x)
  ORACLElist[[x]] <- ORACLElist[[x]] %>%
    filter(as.Date(End.Date, format = "%m/%d/%Y") <= end_dates[x],
           as.Date(Start.Date, format = "%m/%d/%Y") > start_dates[x]))




Oracle <- do.call("rbind", ORACLElist )


#Remove Duplicate rows and add worked entity column
Oracle  <- Oracle  %>%  mutate(WRKD.ENTITY = substr(WD_COFT,1,3),
                       Hours = as.numeric(Hours), Expense = as.numeric(Expense)) 

  
Oracle  <- Oracle  %>% group_by_at(c(1:13,16:34)) %>% summarise(Hours = sum(Hours, na.rm = T),
            Expense = sum(Expense,na.rm = T)) %>% ungroup() %>% distinct()



#Determine PAYROLL based on WRKD.ENTITY
Oracle  <- Oracle  %>% mutate(PAYROLL = case_when(WRKD.ENTITY == "102" ~ "MSQ", TRUE ~ "MSH"))



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



#Bring in department location --------------------------------------------
row_count <- nrow(Oracle )
Oracle  <- left_join(Oracle , coa, by = c("Reverse.Map.for.Worked" = "Column2")) %>% select(1:36)
  
if(nrow(Oracle ) != row_count) {
  stop(paste("Row count failed at", basename(getSourceEditorContext()$path)))
}

row_count <- nrow(Oracle )
Oracle  <- left_join(Oracle , coa, by = c("Reverse.Map.for.Home" = "Column2")) %>% select(1:37)

if(nrow(Oracle ) != row_count) {
  stop(paste("Row count failed at", basename(getSourceEditorContext()$path)))
}


#Bring in standardized JC Description -----------------------------------------------
row_count <- nrow(Oracle )
Oracle  <- left_join(Oracle , jc_desc, by = c("Job.Code" = "J.C"))
if(nrow(Oracle ) != row_count) {
  stop(paste("Row count failed at", basename(getSourceEditorContext()$path)))
}

#Format necessary columns
Oracle  <- Oracle  %>% mutate(End.Date = as.Date(End.Date, format = "%m/%d/%Y"),
                              Hours = as.numeric(Hours), Expense = as.numeric(Expense))

# Move Filename to the end
Oracle  <- Oracle %>% relocate(Filename, .after = last_col()) 

#Column names
colnames(Oracle)[colnames(Oracle) =="Department.ID.Home.Department"] <- "DPT.HOME"
colnames(Oracle)[colnames(Oracle) =="Department.IdWHERE.Worked"] <- "DPT.WRKD"

new_col_names <- c("START.DATE", "END.DATE", "J.C", "PAY.CODE", "HOME.DESCRIPTION", "WRKD.DESCRIPTION",
                   "HOURS", "EXPENSE", "WRKD.LOCATION", "HOME.LOCATION", "J.C.DESCRIPTION")
                   
colnames(Oracle )[c(6, 7, 12:15, 32, 33, 35:37)] <- new_col_names


# Bind NEW data with repository -----------------------------------------
new_repo <- rbind(repo, Oracle)
new_repo <- new_repo %>% distinct()


#Check sum of hours by end date to make sure data follows proper pattern
check <- new_repo  %>%
  ungroup() %>%
  group_by(PAYROLL,END.DATE) %>%
  summarise(HOURS = sum(HOURS)) %>%
 # mutate(End.Date = as.Date(End.Date, format = "%m/%d/%Y")) %>%
  arrange(END.DATE) 
check1 <- pivot_wider(check,id_cols = PAYROLL,values_from = HOURS,names_from = END.DATE)

rm(check, check1)



#save RDS --------------------------------------------
saveRDS(new_repo, file= paste0("J:\\deans\\Presidents\\SixSigma\\MSHS Productivity\\Productivity\\Universal Data\\Labor\\REPOS\\MSHQ_Oracle_Repo\\data_MSH_MSQ_oracle-", Sys.Date(),".rds"))



