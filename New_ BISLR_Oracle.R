
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
  library(mondate)
})



memory.limit(size = 8000000)

# Working directory -------------------------------------------------------------

dir <- "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Universal Data/Labor/"

#universal directory
#universal_dir <- "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Universal Data/"

# Import data sets --------------------------------------------------------------
# Pay_Cycle_data <- read_xlsx(paste0(universal_dir,  "Mapping/MSHS_Pay_Cycle.xlsx"), col_types =c("date" ,"date" , "date" , "numeric"))
# 
# Pay_Cycle_data <- Pay_Cycle_data %>% mutate(DATE =as.Date(DATE),
#                                             START.DATE= as.Date(START.DATE),
#                                             END.DATE= as.Date(END.DATE))


#Import the latest aggregated file 
repo <- file.info(list.files(path = paste0(dir,"REPOS/"), full.names = T , pattern = "data_BISLR"))
repo_file <- rownames(repo)[which.max(repo$ctime)]
repo <- readRDS(repo_file)

# get max date in repo
max(repo$End.Date)


# Run this if you need to update a data in repo
#repo <- repo %>% filter(Filename != "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Universal Data/Labor/Raw Data/BISLR Oracle/14_MSBISLW_FEMA_MAY-22_05_16_2022_0157.csv")


# Import the most recent data
details = file.info(list.files(path = paste0(dir,"Raw Data/BISLR Oracle/"), pattern="*.csv", full.names = T)) 
details = details[with(details, order(as.POSIXct(ctime),  decreasing = F)), ]


BISLR_file_list <- rownames(details)[!(rownames(details) %in% repo$Filename) ]


# add a column including the name of the data set 
BISLR_data_raw <- lapply(BISLR_file_list, function(x){data <- read.csv(x, as.is= T, strip.white = T) %>%
  mutate( Filename = x,
         #Filename = str_extract(x, '\\d+\\_MSBISLW_FEMA_[A-Z]{3}'),
         End.Date =  as.Date(End.Date, format = "%m/%d/%Y"),
         Start.Date = as.Date(Start.Date, format = "%m/%d/%Y"))
})




# get the required end date and start date
#Start date is 1 day after the end of the last Premier Distribution
#start_dates <- Pay_Cycle_data$START.DATE[Pay_Cycle_data$DATE== Sys.Date()]+1
start_dates <- as.Date("2022-03-26")+1

#End date is 1 week after the end of the current Premier Distribution
#end_dates <- Pay_Cycle_data$END.DATE[Pay_Cycle_data$DATE== Sys.Date()]+7
end_dates <- as.Date("2022-04-23")+7

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



# Bind NEW data with repository
new_repo <- rbind(repo, data_BISLR)
new_repo <- new_repo  %>% distinct()


#save RDS
saveRDS(new_repo , file= paste0(dir, "REPOS/data_BISLR_oracle.rds"))

