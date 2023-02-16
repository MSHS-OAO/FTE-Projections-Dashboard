
# MSH MSQ Oracle

rm(list = ls())

# Import Libraries --------------------------------------------------------
suppressMessages({
  library(tidyverse)
  library(dplyr)
  library(readxl)
})


memory.limit(size = 8000000)

# Working directory --------------------------------------------------------
dir <- paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/",
              "Universal Data/")

# Import data --------------------------------------------------------
# Import the latest aggregated file
repo <- readRDS(paste0(dir, "Labor/RDS/data_MSH_MSQ_oracle.rds"))

# import pay cylcle mapping file
dates <- read_xlsx(paste0(dir, "Mapping/MSHS_Pay_Cycle.xlsx"))

# visual check for max date in repo
max(as.Date(repo$End.Date, format = "%m/%d/%Y"))

# Get file names in raw data folder
details <- file.info(list.files(path = paste0(dir,
                                   "Labor/Raw Data/MSHQ Oracle/"),
                                pattern = "*.txt", full.names = T)) %>%
                                  arrange(mtime)

details <- details[with(details, order(as.POSIXct(mtime), decreasing = F)), ]

# check if user expects a new data set is available
answer <- select.list(choices = c("Yes", "No"),
                      preselect = "Yes",
                      multiple = F,
                      title = "Is there a new data?",
                      graphics = T)


if (answer == "Yes" &
    length(basename(rownames(details))[!(basename(rownames(details)) 
                                         %in% repo$Filename)]) == 0) {
  # let user know they need to update raw data folder
  stop("Please update the raw data folder first.")
} else if (answer == "Yes" &
           length(basename(rownames(details))[!(basename(rownames(details)) 
                                                %in% repo$Filename)]) > 0) {
  # get path(s) of file(s) to be appended to REPO file
  oracle_file_list <- basename(rownames(details))[!(basename(rownames(details)) 
                                                    %in% repo$Filename)]
  print(oracle_file_list)
} else {
  # user selects files they would like to update within the REPO file
  update_file_list <-  select.list(choices = rownames(details),
                                   multiple = T,
                                   title = "Select the data you want to update",
                                   graphics = T)
  # remove the update files from current REPO
  repo <- repo %>% filter(!(Filename %in% basename(update_file_list)))
  # get path(s) of file(s) to be updated within REPO
  oracle_file_list <- basename(rownames(details))[basename(rownames(details))
                                                  %in% basename(update_file_list)]
  print(oracle_file_list)
}

#Read files in MSHQ Raw as csv
oracle_list <- lapply(paste0(dir, "Labor/Raw Data/MSHQ Oracle/", 
                             oracle_file_list), function(x) {
  data <- read.csv(x, sep = "~", header = T,
                   stringsAsFactors = F,
                   colClasses = rep("character", 32),
                   strip.white = TRUE) %>%
    mutate(Filename = basename(x))
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
end_dates <- as.Date(format(tail(dist_dates$END.DATE,
                                 n = length(oracle_file_list)), "%m/%d/%Y"),
                                   format = "%m/%d/%Y")
start_dates <- as.Date(format(tail(dist_dates$END.DATE,
                            n = length(oracle_file_list) + 1), "%m/%d/%Y") %>%
                              head(start_dates, n = -1),
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
  end_dates <- sort(as.Date(end_dates, format = "%m/%d/%Y"))
  start_dates_index <- sapply(end_dates,  function(x) grep(x,
                                                dist_dates$END.DATE) - 1)
  start_dates <- as.Date(dist_dates$END.DATE[start_dates_index],
                         format = "%m/%d/%Y") + 1
}



#Filtering each file by start/end date specified
oracle_list <- lapply(1:length(oracle_list), function(x)
  oracle_list[[x]] <- oracle_list[[x]] %>%
    filter(as.Date(End.Date, format = "%m/%d/%Y") <= end_dates[x],
           as.Date(Start.Date, format = "%m/%d/%Y") >= start_dates[x]))

# bind all new/updated data
oracle <- do.call("rbind", oracle_list)

# Determine PAYROLL based on WRKD.ENTITY  -------------------------------------
oracle  <- oracle  %>%
  mutate(PAYROLL = case_when(substr(WD_COFT, 1, 3) == "102" ~ "MSQ",
                             TRUE ~ "MSH"))

# Bind NEW data with repository -----------------------------------------------
new_repo <- rbind(repo, oracle)

# Check sum of hours by end date to make sure data follows proper pattern-------
check <- new_repo %>%
  ungroup() %>%
  group_by(PAYROLL, End.Date) %>%
  summarise(Hours = sum(as.numeric(Hours))) %>%
  mutate(End.Date = as.Date(End.Date, format = "%m/%d/%Y")) %>%
  arrange(End.Date)

check1 <- pivot_wider(check, id_cols = PAYROLL, values_from = Hours,
                      names_from = End.Date)

#save RDS ----------------------------------------------------------------------
saveRDS(new_repo, file = paste0(dir, "Labor/RDS/data_MSH_MSQ_oracle.rds"))
