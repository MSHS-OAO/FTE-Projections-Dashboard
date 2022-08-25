
# BISLR_Oracle

rm(list = ls())

# Import libraries -------------------------------------------------------
suppressMessages({
  library(readxl)
  library(tidyverse)
  library(dplyr)
  library(tidyr)
  library(gsubfn)
  library(rstudioapi)
  library(mondate)
})



memory.limit(size = 8000000)

# Working directory --------------------------------------------------------
dir <- paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/",
                                                       "Universal Data/")


# Import data sets ----------------------------------------------------------
## Import Pay cycle data
# Pay_Cycle_data <- read_xlsx(paste0(dir, "Mapping/MSHS_Pay_Cycle.xlsx"),
#        col_types =c("date", "date", "date", "numeric"))


# Pay_Cycle_data <- Pay_Cycle_data %>% mutate(DATE =as.Date(DATE),
#                                             START.DATE= as.Date(START.DATE),
#                                             END.DATE= as.Date(END.DATE))

## Import the latest aggregated file -----------------------------------------
repo <- file.info(list.files(path = paste0(dir, "Labor/REPOS/"), full.names = T,
                                                    pattern = "data_BISLR"))
repo_file <- rownames(repo)
repo <- readRDS(repo_file)

## get max date in repo --------------------------------------------------------
max(as.Date(repo$End.Date, format = "%m/%d/%Y"))


## Run this if you need to update a data in repo ------------------------------
# repo <- repo %>% filter(Filename != paste0("J:/deans/Presidents/SixSigma/",
#           "MSHS Productivity/Productivity/Universal Data/Labor/Raw Data/",
#           "BISLR Oracle/15_MSBISLW_FEMA_JUN-22_06_16_2022_0242.csv"))


## Import the most recent datasets --------------------------------------------
details <- file.info(list.files(path =
                                  paste0(dir, "Labor/Raw Data/BISLR Oracle/"),
                                          pattern = "*.csv", full.names = T))

details <- details[with(details, order(as.POSIXct(ctime),  decreasing = F)), ]


bislr_file_list <- rownames(details)[!(rownames(details) %in% repo$Filename)]


# add a column including the name of the data set
bislr_data_raw <- lapply(bislr_file_list, function(x) {
             data <- read.csv(x, as.is = T, strip.white = T,
                              colClasses = rep("character", 32)) %>%
             mutate(Filename = x,
             #Filename = str_extract(x, '\\d+\\_MSBISLW_FEMA_[A-Z]{3}'),
             End.Date =  as.Date(End.Date, format = "%m/%d/%Y"),
             Start.Date = as.Date(Start.Date, format = "%m/%d/%Y"))
})



# get the required end and start date -----------------------------------------
#Start date is 1 day after the end of the last Premier Distribution
#start_dates <- Pay_Cycle_data$START.DATE[Pay_Cycle_data$DATE== Sys.Date()]+1
start_dates <- as.Date(c("2022-05-21", "2022-07-02")) + 1

#End date is 1 week after the end of the current Premier Distribution
#end_dates <- Pay_Cycle_data$END.DATE[Pay_Cycle_data$DATE== Sys.Date()]+7
end_dates <- as.Date("2022-07-02", "2022-07-30") + 7

# Filtering each file by start/end date specified------------------------------
data_bislr <- lapply(1 : seq_len(bislr_data_raw), function(x)
  bislr_data_raw[[x]] <- bislr_data_raw[[x]] %>%
    filter(End.Date <= end_dates[x],
           Start.Date >= start_dates[x]))


# Filter overlapping weekly pay cycle in BISLR --------------------------------
#Names of the weekly paycyle names in the payroll name column in data files
weekly_pc <- c("WEST WEEKLY", "BIB WEEKLY")


#Function to delete overlapping weekly pay cycle in BISLR
delete_weekly <- function(df, pay_cycles) {
  #list out all weekly pay cycles in file from oldest to newest
  delete_pc <- df %>%
    filter(Payroll.Name %in% pay_cycles) %>%
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
data_bislr <- lapply(data_bislr, function(x) delete_weekly(x, weekly_pc))

data_bislr <- do.call("rbind", data_bislr)


  
# Assigning Payroll values and Removing duplicates ----------------------------
data_bislr <- data_bislr %>%
  mutate(PAYROLL = case_when(
    Facility.Hospital.Id_Worked == "NY2162" ~ "MSW",
    Facility.Hospital.Id_Worked == "NY2163" ~ "MSM",
    substr(Full.COA.for.Worked, 1, 3) %in% c("402", "410") ~ "MSB",
    TRUE ~ "MSBI")) %>%
  distinct()


# Clean Position.Code.Description -----------------------------------------
data_bislr <- data_bislr %>%
  mutate(Position.Code.Description = case_when(
    (Department.IdWHERE.Worked == 407210340412756 &
       Employee.Name == "CHIANG, JACQUELINE PE") ~ "DUS_REMOVE",
    (Department.IdWHERE.Worked == 414000040312763 &
       Employee.Name == "BESTREICH, ERIN S") ~ "DUS_REMOVE",
    (Department.IdWHERE.Worked == 414000040312755 &
       Employee.Name == "CHIANG, JACQUELINE PE") ~ "DUS_REMOVE",
    (Department.IdWHERE.Worked == 414000040312755 &
       Employee.Name == "CRAIG, BRITTANY PIERCE") ~ "DUS_REMOVE",
    (Department.IdWHERE.Worked == 414000040312756 &
       Employee.Name == "DELAPENHA, SANDRA ELAINE") ~ "DUS_REMOVE",
    (Department.IdWHERE.Worked == 414000040312755 &
       Employee.Name == "GANZ, CINDY MARIE") ~ "DUS_REMOVE",
    (Department.IdWHERE.Worked == 407210340412756 &
       Employee.Name == "MEEHAN, JENNIFER JOYCE") ~ "DUS_REMOVE",
    (Department.IdWHERE.Worked == 414000040312702 &
       Employee.Name == "MEEHAN, JENNIFER JOYCE") ~ "DUS_REMOVE",
    (Department.IdWHERE.Worked == 407210340412756 &
       Employee.Name == "OKAY, DEVIN JOSEPH") ~ "DUS_REMOVE",
    (Department.IdWHERE.Worked == 407210340412756 &
       Employee.Name == "ROBBINS, STEPHANIE") ~ "DUS_REMOVE",
    (Department.IdWHERE.Worked == 414000040312763 &
       Employee.Name == "WALKER, THERESA L") ~ "DUS_REMOVE",
    (Department.IdWHERE.Worked == 414000040312763 &
       Employee.Name == "WALKER, THERESA L (Lauren)") ~ "DUS_REMOVE",
    TRUE ~ Position.Code.Description)
  )

# Clean Job Code -----------------------------------------
data_bislr <- data_bislr %>%
  mutate(Job.Code = case_when(
    (Department.IdWHERE.Worked == 407210340412756 &
       Employee.Name == "CHIANG, JACQUELINE PE") ~ "DUS_RMV",
    (Department.IdWHERE.Worked == 414000040312763 &
       Employee.Name == "BESTREICH, ERIN S") ~ "DUS_RMV",
    (Department.IdWHERE.Worked == 414000040312755 &
       Employee.Name == "CHIANG, JACQUELINE PE") ~ "DUS_RMV",
    (Department.IdWHERE.Worked == 414000040312755 &
       Employee.Name == "CRAIG, BRITTANY PIERCE") ~ "DUS_RMV",
    (Department.IdWHERE.Worked == 414000040312756 &
       Employee.Name == "DELAPENHA, SANDRA ELAINE") ~ "DUS_RMV",
    (Department.IdWHERE.Worked == 414000040312755 &
       Employee.Name == "GANZ, CINDY MARIE") ~ "DUS_RMV",
    (Department.IdWHERE.Worked == 407210340412756 &
       Employee.Name == "MEEHAN, JENNIFER JOYCE") ~ "DUS_RMV",
    (Department.IdWHERE.Worked == 414000040312702 &
       Employee.Name == "MEEHAN, JENNIFER JOYCE") ~ "DUS_RMV",
    (Department.IdWHERE.Worked == 407210340412756 &
       Employee.Name == "OKAY, DEVIN JOSEPH") ~ "DUS_RMV",
    (Department.IdWHERE.Worked == 407210340412756 &
       Employee.Name == "ROBBINS, STEPHANIE") ~ "DUS_RMV",
    (Department.IdWHERE.Worked == 414000040312763 &
       Employee.Name == "WALKER, THERESA L") ~ "DUS_RMV",
    (Department.IdWHERE.Worked == 414000040312763 &
       Employee.Name == "WALKER, THERESA L (Lauren)") ~ "DUS_RMV",
    TRUE ~ Job.Code)
  )



# Bind NEW data with repository ---------------------------------------------
new_repo <- rbind(repo, data_bislr)
new_repo <- new_repo  %>% distinct()


# save RDS -----------------------------------------------------------------
saveRDS(new_repo, file = paste0(dir, "REPOS/data_BISLR_oracle.rds"))