
# BISLR_Oracle

rm(list = ls())

# Import libraries -------------------------------------------------------
suppressMessages({
  library(readxl)
  library(tidyverse)
  library(dplyr)
})



memory.limit(size = 8000000)

# Working directory --------------------------------------------------------
dir <- paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/",
                                                       "Universal Data/")


# Import data sets ----------------------------------------------------------
# Import the latest aggregated file
repo <- readRDS(paste0(dir, "Labor/RDS/data_BISLR_oracle.rds"))

# import pay cylcle mapping file
dates <- read_xlsx(paste0(dir, "Mapping/MSHS_Pay_Cycle.xlsx"))

# visual check for max date in repo
max(as.Date(repo$End.Date, format = "%m/%d/%Y"))


# Get file names in raw data folder --------------------------------------------
details <- file.info(list.files(path =
                                  paste0(dir, "Labor/Raw Data/BISLR Oracle/"),
                                          pattern = "*.csv", full.names = T))

details <- details[with(details, order(as.POSIXct(ctime),  decreasing = F)), ]


# check if user expects a new data set is available
answer <- select.list(choices = c("Yes", "No"),
                      preselect = "Yes",
                      multiple = F,
                      title = "Is there a new data?",
                      graphics = T)


if (answer == "Yes" &
    length(rownames(details)[!(rownames(details) %in% repo$Filename)]) == 0) {
  # let user know they need to update raw data folder
  stop("Please update the raw data folder first.")
} else if (answer == "Yes" &
      length(rownames(details)[!(rownames(details) %in% repo$Filename)]) > 0) {
  # get path(s) of file(s) to be appended to REPO file
  bislr_file_list <- rownames(details)[!(rownames(details) %in% repo$Filename)]
  print(bislr_file_list)
} else {
  # user selects files they would like to update within the REPO file
  update_file_list <-  select.list(choices = rownames(details),
                                   multiple = T,
                                   title = "Select the data you want to update",
                                   graphics = T)
  # remove the update files from current REPO
  repo <- repo %>% filter(!(Filename %in% update_file_list))
  # get path(s) of file(s) to be updated within REPO
  bislr_file_list <- rownames(details)[rownames(details) %in% update_file_list]
  print(bislr_file_list)
}

#Read files in BISLR Raw as csv
bislr_data_raw <- lapply(bislr_file_list, function(x) {
             data <- read.csv(x, as.is = T, strip.white = T,
                              colClasses = rep("character", 32)) %>%
             mutate(Filename = x,
             End.Date =  as.Date(End.Date, format = "%m/%d/%Y"),
             Start.Date = as.Date(Start.Date, format = "%m/%d/%Y"))
})


# get the required end_date and start date-------------------------------------
##Table of distribution dates
dist_dates <- dates %>%
  select(END.DATE, PREMIER.DISTRIBUTION) %>%
  distinct() %>%
  drop_na() %>%
  arrange(END.DATE) %>%
  #filter only on distribution end dates
  filter(PREMIER.DISTRIBUTION %in% c(TRUE, 1),
#filter 3 weeks from run date (21 days) for data collection lag before run date
         END.DATE < as.POSIXct(Sys.Date() - 14))


#Selecting current and previous distribution dates

#End date is 1 week after the end of the current Premier Distribution
end_dates <- as.Date(format(tail(dist_dates$END.DATE,
                                 n = length(bislr_file_list)), "%m/%d/%Y"),
                     format = "%m/%d/%Y") + 7

#Start date is 1 day after the end of the last Premier Distribution
start_dates <- as.Date(format(tail(dist_dates$END.DATE,
                            n = length(bislr_file_list) + 1), "%m/%d/%Y") %>%
                         head(previous_distribution, n = -1),
                       format = "%m/%d/%Y") + 1

#Confirming distribution dates
cat("File end dates are", format(end_dates, "%m/%d/%Y"),
    "\nFile start dates are", format(start_dates, "%m/%d/%Y"))
answer <- select.list(choices = c("Yes", "No"),
                      preselect = "Yes",
                      multiple = F,
                      title = "Correct dates?",
                      graphics = T)
if (answer == "No") {
  end_dates <- select.list(choices =
          format(sort.POSIXlt(dist_dates$END.DATE, decreasing = T), "%m/%d/%Y"),
                           multiple = T,
                           title = "Select end dates",
                           graphics = T)
  end_dates <- sort(as.Date(end_dates, format = "%m/%d/%Y"))+7
  start_dates_index <- sapply(end_dates-7 , function(x)
                         grep(x, dist_dates$END.DATE )-1)
  start_dates <- as.Date(dist_dates$END.DATE[start_dates_index],
                         format= "%m/%d/%Y")+1
}


# Filtering each file by start/end date specified------------------------------
data_bislr <- lapply(1 : length(bislr_data_raw), function(x)
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
saveRDS(new_repo, file = paste0(dir, "Labor/REPOS/data_BISLR_oracle.rds"))
