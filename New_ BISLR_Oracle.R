
### BISLR_Oracle

rm(list=ls())

# Import libraries
suppressMessages({
  library(readxl)
  library(tidyverse)
  library(dplyr)
  library(tidyr)
  library(here)
  library(gsubfn)
  library(rstudioapi)
})



memory.limit(size = 8000000)

# Working directory -------------------------------------------------------------
repo_dir <- "C:/Users/aghaer01/Downloads/FTE-Projections-Dashboard-Oracle_CC"

# data directory
dir <- "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Universal Data/Labor/Raw Data/"

#universal directory
universal_dir <- "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Universal Data/"

# Import data sets --------------------------------------------------------------

# import pay cycle data and filter required date
Pay_Cycle_data <- read_xlsx(paste0(universal_dir,  "Mapping/MSHS_Pay_Cycle.xlsx"))
Pay_Cycle_data <- Pay_Cycle_data %>% filter(PREMIER.DISTRIBUTION== "TRUE")


# get unique end date
unique_end_date <- unique(Pay_Cycle_data$END.DATE)
unique_end_date <- as.Date(unique_end_date)


#Import the latest aggregated file 
repo <- file.info(list.files(path = paste0(repo_dir,"/New Data/BISLR_Repo"), full.names = T , pattern = "data_BISLR"))
repo_file <- rownames(repo)[which.max(repo$ctime)]
repo <- readRDS(repo_file)

# Get the max date in repo
repo_max_date <- as.numeric(format(max(repo$END.DATE), "%m") )


# Import the most recent data
details = file.info(list.files(path = paste0(dir,"BISLR Oracle/"), pattern="*.csv", full.names = T)) 
details = details[with(details, order(as.POSIXct(ctime),  decreasing = F)), ]


# get the month of the most recent data set
new_data_date <- rownames(details)[which.max(details$ctime)]
new_data_date <- as.numeric(substr(gsub('.*-([0-9]+)_','\\', new_data_date ), 1,2))

# Find the difference between the max date in repo and the current data
dif_time <- new_data_date - repo_max_date



BISLR_file_list <- rownames(tail(details, n = dif_time ))

BISLR_data_raw <- lapply(BISLR_file_list, function(x){read.csv(x, as.is= T, strip.white = T)})


BISLR_data_raw <- lapply(BISLR_data_raw, transform, End.Date =  as.Date(End.Date, format = "%m/%d/%Y"),
                                                     Start.Date = as.Date(Start.Date, format = "%m/%d/%Y"))


# get the required end_date and start date

#Start date is 1 day after the end of the last Premier Distribution
#start_dates <- as.Date(c("2022-01-02", "2022-01-30" ))
start_dates <- lag(unique_end_date, n=1)
start_dates <- tail(start_dates, n = dif_time )+1


#End date is 1 week after the end of the current Premier Distribution
#end_dates <- as.Date(c( "2022-02-05", "2022-03-05"))
end_dates <- tail(unique_end_date, n = dif_time )+ 7


#Filtering each file by start/end date specified
data_BISLR <- lapply(1:length(BISLR_data_raw), function(x)
  BISLR_data_raw[[x]] <- BISLR_data_raw[[x]] %>%
    filter(End.Date <= end_dates[x],
           Start.Date >= start_dates[x]))



#Names of the weekly paycyle names in the payroll name column in data files
weekly_pc <- c("WEST WEEKLY", "BIB WEEKLY")


#Function to delete overlapping weekly pay cycle in BISLR
delete_weekly <- function(df, pay_cycles){
  #list out all weekly pay cycles in file from oldest to newest
  delete_pc <- df %>% filter(Payroll.Name %in% pay_cycles) %>%
    arrange(Start.Date, End.Date) %>%
    mutate(Start_End = paste0(Start.Date, "_", End.Date)) %>%
    select(Start.Date, End.Date, Start_End) %>%
    distinct()
  #filter out the oldest pay cycle
  data_export <- df %>%
    mutate(Start_End = paste0(Start.Date, "_", End.Date)) %>%
    filter(!Start_End %in% delete_pc$Start_End[1]) %>%
    mutate(Start_End = NULL)
  #return the filtered data frame
  return(data_export)
}




#Applying function
data_BISLR <- lapply(data_BISLR, function(x) delete_weekly(x, weekly_pc)) 


data_BISLR <- do.call("rbind", data_BISLR )

  
  
  #Assigning Payroll values and Removing duplicates
data_BISLR <- data_BISLR %>%
  mutate(PAYROLL = case_when(
    Facility.Hospital.Id_Worked == "NY2162" ~ "MSW",
    Facility.Hospital.Id_Worked == "NY2163" ~ "MSM",
    substr(Full.COA.for.Worked, 1, 3) %in% c("402", "410") ~ "MSB",
    TRUE ~ "MSBI")) %>%
  distinct()

data_BISLR <- data_BISLR %>%
  mutate(Position.Code.Description = case_when(
    (Department.IdWHERE.Worked == 407210340412756 & Employee.Name == "CHIANG, JACQUELINE PE") ~ "DUS_REMOVE",
    (Department.IdWHERE.Worked == 414000040312763 & Employee.Name == "BESTREICH, ERIN S") ~ "DUS_REMOVE",
    (Department.IdWHERE.Worked == 414000040312755 & Employee.Name == "CHIANG, JACQUELINE PE") ~ "DUS_REMOVE",
    (Department.IdWHERE.Worked == 414000040312755 & Employee.Name == "CRAIG, BRITTANY PIERCE") ~ "DUS_REMOVE",
    (Department.IdWHERE.Worked == 414000040312756 & Employee.Name == "DELAPENHA, SANDRA ELAINE") ~ "DUS_REMOVE",
    (Department.IdWHERE.Worked == 414000040312755 & Employee.Name == "GANZ, CINDY MARIE") ~ "DUS_REMOVE",
    (Department.IdWHERE.Worked == 407210340412756 & Employee.Name == "MEEHAN, JENNIFER JOYCE") ~ "DUS_REMOVE",
    (Department.IdWHERE.Worked == 414000040312702 & Employee.Name == "MEEHAN, JENNIFER JOYCE") ~ "DUS_REMOVE",
    (Department.IdWHERE.Worked == 407210340412756 & Employee.Name == "OKAY, DEVIN JOSEPH") ~ "DUS_REMOVE",
    (Department.IdWHERE.Worked == 407210340412756 & Employee.Name == "ROBBINS, STEPHANIE") ~ "DUS_REMOVE",
    (Department.IdWHERE.Worked == 414000040312763 & Employee.Name == "WALKER, THERESA L") ~ "DUS_REMOVE",
    (Department.IdWHERE.Worked == 414000040312763 & Employee.Name == "WALKER, THERESA L (Lauren)") ~ "DUS_REMOVE",
    TRUE ~ Position.Code.Description)
  )

data_BISLR <- data_BISLR %>%
  mutate(Job.Code = case_when(
    (Department.IdWHERE.Worked == 407210340412756 & Employee.Name == "CHIANG, JACQUELINE PE") ~ "DUS_RMV",
    (Department.IdWHERE.Worked == 414000040312763 & Employee.Name == "BESTREICH, ERIN S") ~ "DUS_RMV",
    (Department.IdWHERE.Worked == 414000040312755 & Employee.Name == "CHIANG, JACQUELINE PE") ~ "DUS_RMV",
    (Department.IdWHERE.Worked == 414000040312755 & Employee.Name == "CRAIG, BRITTANY PIERCE") ~ "DUS_RMV",
    (Department.IdWHERE.Worked == 414000040312756 & Employee.Name == "DELAPENHA, SANDRA ELAINE") ~ "DUS_RMV",
    (Department.IdWHERE.Worked == 414000040312755 & Employee.Name == "GANZ, CINDY MARIE") ~ "DUS_RMV",
    (Department.IdWHERE.Worked == 407210340412756 & Employee.Name == "MEEHAN, JENNIFER JOYCE") ~ "DUS_RMV",
    (Department.IdWHERE.Worked == 414000040312702 & Employee.Name == "MEEHAN, JENNIFER JOYCE") ~ "DUS_RMV",
    (Department.IdWHERE.Worked == 407210340412756 & Employee.Name == "OKAY, DEVIN JOSEPH") ~ "DUS_RMV",
    (Department.IdWHERE.Worked == 407210340412756 & Employee.Name == "ROBBINS, STEPHANIE") ~ "DUS_RMV",
    (Department.IdWHERE.Worked == 414000040312763 & Employee.Name == "WALKER, THERESA L") ~ "DUS_RMV",
    (Department.IdWHERE.Worked == 414000040312763 & Employee.Name == "WALKER, THERESA L (Lauren)") ~ "DUS_RMV",
    TRUE ~ Job.Code)
  )


#Add in worked and home cost center based on Full COA - Oracle Format
data_BISLR <- data_BISLR %>%
  mutate(DPT.WRKD = paste0(substr(Full.COA.for.Worked,1,3),
                           substr(Full.COA.for.Worked,41,44),
                           substr(Full.COA.for.Worked,5,7),
                           substr(Full.COA.for.Worked,12,16)),
         DPT.HOME = paste0(substr(Full.COA.for.Home,1,3),
                           substr(Full.COA.for.Home,41,44),
                           substr(Full.COA.for.Home,5,7),
                           substr(Full.COA.for.Home,12,16)))


#Add in worked and home cost center based on Full COA - Oracle Format
data_BISLR <- data_BISLR %>%
  mutate(DPT.WRKD = paste0(substr(Full.COA.for.Worked,1,3),
                           substr(Full.COA.for.Worked,41,44),
                           substr(Full.COA.for.Worked,5,7),
                           substr(Full.COA.for.Worked,12,16)),
         DPT.HOME = paste0(substr(Full.COA.for.Home,1,3),
                           substr(Full.COA.for.Home,41,44),
                           substr(Full.COA.for.Home,5,7),
                           substr(Full.COA.for.Home,12,16)))

#Add in reverse mapping for Legacy cost centers worked and home
# data_BISLR_oracle <- data_BISLR_oracle %>%
#   mutate(DPT.WRKD = paste0(substr(Reverse.Map.for.Worked, 1, 4),
#                            substr(Reverse.Map.for.Worked, 13, 14),
#                            substr(Reverse.Map.for.Worked, 16, 19)),
#          DPT.HOME = paste0(substr(Reverse.Map.for.Home, 1, 4),
#                            substr(Reverse.Map.for.Home, 13, 14),
#                            substr(Reverse.Map.for.Home, 16, 19)))

#Formatting column data types
data_BISLR <- data_BISLR %>%
  mutate(Hours = as.numeric(Hours),
         Expense = as.numeric(Expense),
         Pay.Code = as.character(Pay.Code))

#Rename columns to FTE trend column names
data_BISLR <- data_BISLR %>%
  rename(J.C.DESCRIPTION = Position.Code.Description,
         J.C = Job.Code,
         PAY.CODE = Pay.Code,
         Start.Date = Start.Date,
         End.Date = End.Date,
         HOME.LOCATION = Home.FacilityOR.Hospital.ID,
         WRKD.LOCATION = Facility.Hospital.Id_Worked,
         HOME.DESCRIPTION = Department.Name.Home.Dept,
         WRKD.DESCRIPTION = Department.Name.Worked.Dept,
         HOURS = Hours,
         EXPENSE = Expense)



data_BISLR <- data_BISLR %>% rename( START.DATE= Start.Date, 
                                     END.DATE= End.Date )

# Bind NEW data with repository
new_repo <- rbind(repo, data_BISLR)
new_repo <- new_repo  %>% distinct()


#save RDS
saveRDS(new_repo , file = paste0("C:\\Users\\aghaer01\\Downloads\\FTE-Projections-Dashboard-Oracle_CC\\New Data\\data_BISLR-", Sys.Date(),".rds"))



