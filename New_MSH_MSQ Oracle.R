
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
#Read in pay cycle file
dates <- read_xlsx(paste0(dir, "Mapping/MSHS_Pay_Cycle.xlsx"))



## Import the latest aggregated file
repo <- readRDS(paste0(dir, "Labor/RDS/data_MSH_MSQ_oracle.rds"))

### get max date in repo
max(as.Date(repo$End.Date, format = "%m/%d/%Y"))


# Import the most recent data
details <- file.info(list.files(path = paste0(dir,
                                    "Labor/Raw Data/MSHQ Oracle/MSHQ Oracle/"),
                                    pattern = "*.txt", full.names = T)) %>%
                                      arrange(mtime)
details <- details[with(details, order(as.POSIXct(ctime), decreasing = F)), ]


# read the file that is not in the repos
oracle_file_list <- rownames(details)[!(rownames(details) %in% repo$Filename)]


# check if a new data set is available
if (length(oracle_file_list) == 0) {
  stop(paste("The repo is already updated."))
  
}else{
  paste("Continue from line 85.")
  }

answer <- select.list(choices = c("Yes", "No"),
                      preselect = "Yes",
                      multiple = F,
                      title = "Is there a new data?",
                      graphics = T)

if (answer == "No") {
  file_list <-  select.list(choices = rownames(details),
                              multiple = T,
                              title = "Select the data you want to update",
                              graphics = T)
  repo <- repo %>% filter(!(Filename %in% file_list))
  oracle_file_list <- rownames(details)[!(rownames(details) %in% repo$Filename)]
} else{
  paste("Please update the folder first.")
  }


if (answer == "Yes") {
  details <- file.info(list.files(path = paste0(dir,
                                    "Labor/Raw Data/MSHQ Oracle/MSHQ Oracle/"),
                                  pattern = "*.txt", full.names = T)) %>%
                                          arrange(mtime)
  details <- details[with(details, order(as.POSIXct(ctime), decreasing = F)), ]
  # read the file that is not in the repos
  oracle_file_list <- rownames(details)[!(rownames(details) %in% repo$Filename)]
}

writeLines(paste0("MSHQ file lists includes: \n", oracle_file_list))
answer <- select.list(choices = c("Yes", "No"),
                      preselect = "Yes",
                      multiple = F,
                      title = "Correct files?",
                      graphics = T)
if (answer == "No") {
  paste("Please Start from line 31.")
}


#Read files in MSQ Raw as csv
oracle_list <- lapply(oracle_file_list, function(x) {
                  data <- read.csv(x, sep = "~", header = T,
                                  stringsAsFactors = F,
                                  colClasses = rep("character", 32),
                                  strip.white = TRUE) %>%
                                  mutate(Filename = x)
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
         END.DATE < as.POSIXct(Sys.Date() - 21))




#Selecting current and previous distribution dates
distribution <- format(tail(dist_dates$END.DATE, 
                            n = length(oracle_file_list)),"%m/%d/%Y")
previous_distribution <- format(tail(dist_dates$END.DATE, 
                                n= length(oracle_file_list)+1 ),"%m/%d/%Y")%>%
                       head(previous_distribution, n=-1)

#Confirming distribution dates
cat("Current distribution is", distribution,
    "\nPrevious distribution is", previous_distribution)
answer <- select.list(choices = c("Yes", "No"),
                      preselect = "Yes",
                      multiple = F,
                      title = "Correct distribution?",
                      graphics = T)
if (answer == "No") {
  distribution <- select.list(choices =
                      format(sort.POSIXlt(dist_dates$END.DATE, decreasing = T),
                                       "%m/%d/%Y"),
                              multiple = T,
                              title = "Select current distribution",
                              graphics = T)
  which(distribution == format(dist_dates$END.DATE, "%m/%d/%Y"))
  previous_distribution <- format(dist_dates$END.DATE[which(distribution == format(dist_dates$END.DATE, "%m/%d/%Y"))-1],"%m/%d/%Y")
  
}



# Filtering each file by start/end date specified
oracle_list <- lapply(1:length(oracle_list), function(x)
  oracle_list[[x]] <- oracle_list[[x]] %>%
    filter(as.Date(End.Date, format = "%m/%d/%Y") <= end_dates[x],
           as.Date(Start.Date, format = "%m/%d/%Y") > start_dates[x]))



oracle <- do.call("rbind", oracle_list)


# Remove Duplicate rows and add worked entity column --------------------------
oracle  <- oracle  %>% mutate(WRKD.ENTITY = substr(WD_COFT, 1, 3),
                      Hours = as.numeric(Hours), Expense = as.numeric(Expense))

  
oracle  <- oracle  %>%
  #group_by_at(c(1:13, 16:34)) %>%
  group_by_at(vars(-Hours, -Expense)) %>%
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
saveRDS(new_repo, file = paste0(dir, "Labor/RDS/data_MSH_MSQ_oracle.rds"))