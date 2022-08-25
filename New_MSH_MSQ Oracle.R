
# MSH MSQ Oracle

rm(list = ls())

# Import Libraries --------------------------------------------------------
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

# Working directory --------------------------------------------------------
dir <- paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/",
                                     "Universal Data/")



# Import data --------------------------------------------------------
## import pay cycle data and filter required date
# Pay_Cycle_data <- read_xlsx(paste0(dir,
#                            "Mapping/MSHS_Pay_Cycle.xlsx"),
#                             col_types =c("date", "date", "date", "numeric"))



## Import the latest aggregated file
repo <- file.info(list.files(path = paste0(dir, "Labor/REPOS/"), full.names = T,
                             pattern = "data_MSH_MSQ_oracle"))
repo_file <- rownames(repo)
repo <- readRDS(repo_file)

# get max date in repo
max(as.Date(repo$End.Date, format = "%m/%d/%Y"))


# Run this if you need to update a data in repo
# repo <- repo %>% filter(Filename != paste0("J:/deans/Presidents/SixSigma/",
#   "MSHS Productivity/Productivity/Universal Data/Labor/Raw Data/MSHQ Oracle/",
#                      "MSHQ Oracle/25_MSH_LD_FTI_MAY-22_05_16_2022_0243.txt"))


# Import the most recent data
details <- file.info(list.files(path = paste0(dir,
                                    "Labor/Raw Data/MSHQ Oracle/MSHQ Oracle/"),
                                    pattern = "*.txt", full.names = T)) %>%
                                      arrange(mtime)
details <- details[with(details, order(as.POSIXct(ctime), decreasing = F)), ]


# read the file that is not in the repos
oracle_file_list <- rownames(details)[!(rownames(details) %in% repo$Filename)]

#Read files in MSQ Raw as csv
oracle_list <- lapply(oracle_file_list, function(x) {
                  data <- read.csv(x, sep = "~", header = T,
                                  stringsAsFactors = F,
                                  colClasses = rep("character", 32),
                                  strip.white = TRUE) %>%
                                  mutate(Filename = x)
})



# get the required end_date and start date-------------------------------------
#start_dates <- Pay_Cycle_data$START.DATE[Pay_Cycle_data$DATE== Sys.Date()]
start_dates <- as.Date(c("04/23/2022"), format = "%m/%d/%Y")
                         

#end_dates <- Pay_Cycle_data$END.DATE[Pay_Cycle_data$DATE== Sys.Date()]
end_dates <- as.Date(c("05/21/2022"), format = "%m/%d/%Y")



#Filtering each file by start/end date specified
oracle_list <- lapply(1:seq_len(oracle_list), function(x)
  oracle_list[[x]] <- oracle_list[[x]] %>%
    filter(as.Date(End.Date, format = "%m/%d/%Y") <= end_dates[x],
           as.Date(Start.Date, format = "%m/%d/%Y") > start_dates[x]))



oracle <- do.call("rbind", oracle_list)


# Remove Duplicate rows and add worked entity column --------------------------
oracle  <- oracle  %>% mutate(WRKD.ENTITY = substr(WD_COFT, 1, 3),
                      Hours = as.numeric(Hours), Expense = as.numeric(Expense))

  
oracle  <- oracle  %>%
  group_by_at(c(1:13, 16:34)) %>%
    summarise(Hours = sum(Hours, na.rm = T),
              Expense = sum(Expense, na.rm = T)) %>%
               ungroup() %>%
                distinct()



# Determine PAYROLL based on WRKD.ENTITY  -------------------------------------
oracle  <- oracle  %>%
  mutate(PAYROLL = case_when(WRKD.ENTITY == "102" ~ "MSQ",
                                             TRUE ~ "MSH"))


# Bind NEW data with repository -----------------------------------------------
new_repo <- rbind(repo, oracle)
new_repo <- new_repo %>%
  distinct()

# Check sum of hours by end date to make sure data follows proper pattern-------
check <- new_repo %>%
  ungroup() %>%
  group_by(PAYROLL, End.Date) %>%
  summarise(Hours = sum(Hours)) %>%
  mutate(End.Date = as.Date(End.Date, format = "%m/%d/%Y")) %>%
  arrange(End.Date)

check1 <- pivot_wider(check, id_cols = PAYROLL, values_from = Hours,
                      names_from = End.Date)



#save RDS ----------------------------------------------------------------------
saveRDS(new_repo, file = paste0(dir, "/REPOS/data_MSH_MSQ_oracle.rds"))