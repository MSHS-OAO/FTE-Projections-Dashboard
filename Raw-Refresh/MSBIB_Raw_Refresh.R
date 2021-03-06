# Intro -------------------------------------------------------------------

# Libraries Setup and Memory Clear-----------------------------------------

library(dplyr)
library(readxl) # needed for import

# home location for working directory folder
dir <- "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/FEMA Reimbursement/MSHS-FEMA-Reimbursement/"

dir_raw <- paste0(dir, "MSBIB Raw/")

# Inputs/Imports ----------------------------------------------------------


MSBIB_file_list <- list.files(path = dir_raw, pattern = "*.xlsx")
MSBIB_file_list <- MSBIB_file_list[-grep("~", MSBIB_file_list, value = FALSE)]

MSBIB_data_list <- lapply(
  MSBIB_file_list,
  function(x) read_excel(paste0(dir_raw, x), col_names = T)
)
names(MSBIB_data_list) <- MSBIB_file_list
MSBIB_raw_data <- do.call("rbind", MSBIB_data_list)
rownames(MSBIB_raw_data) <- c()


# duplicates removed by this line should only be coming from the duplication
# of data pulled at different time periods.

# Because the Position Code Description was updated during different data pulls,
# the Position Code Description is excluded from the distinct() check
# And because the position title shouldn't change such that an employee becomes
# a provider, the sort order of rows isn't critical.

MSBIB_raw_data <- MSBIB_raw_data %>% distinct_at(vars(-c(`Position Code Description`, `POSITION CODE`)), .keep_all = TRUE)


MSBIB_raw_data$PAYROLL <- "MSBI"
MSBIB_raw_data$PAYROLL[MSBIB_raw_data$WD_COFT == "4709" & MSBIB_raw_data$WD_Location == "07"] <- "MSB"
MSBIB_raw_data$PAYROLL[MSBIB_raw_data$WD_COFT == "4709" & MSBIB_raw_data$WD_Location == "00"] <- "MSB"
MSBIB_raw_data$PAYROLL[MSBIB_raw_data$WD_COFT == "4709" & MSBIB_raw_data$WD_Location == "20"] <- "MSB"
MSBIB_raw_data$PAYROLL[MSBIB_raw_data$WD_COFT == "5709" & MSBIB_raw_data$WD_Location == "07"] <- "MSB"
MSBIB_raw_data$PAYROLL[MSBIB_raw_data$WD_COFT == "5309" & MSBIB_raw_data$WD_Location == "07"] <- "MSB"

MSBIB_raw_data$`Position Code Description`[MSBIB_raw_data$WD_COFT == "4408" & MSBIB_raw_data$WD_Location == "00" & MSBIB_raw_data$WD_Department == "4269" & MSBIB_raw_data$`Employee Name` == "CHIANG, JACQUELINE PE"] <- "DUS_REMOVE"
MSBIB_raw_data$`Position Code Description`[MSBIB_raw_data$WD_COFT == "4409" & MSBIB_raw_data$WD_Location == "03" & MSBIB_raw_data$WD_Department == "4268" & MSBIB_raw_data$`Employee Name` == "BESTREICH, ERIN S"] <- "DUS_REMOVE"
MSBIB_raw_data$`Position Code Description`[MSBIB_raw_data$WD_COFT == "4409" & MSBIB_raw_data$WD_Location == "03" & MSBIB_raw_data$WD_Department == "4269" & MSBIB_raw_data$`Employee Name` == "CHIANG, JACQUELINE PE"] <- "DUS_REMOVE"
MSBIB_raw_data$`Position Code Description`[MSBIB_raw_data$WD_COFT == "4409" & MSBIB_raw_data$WD_Location == "03" & MSBIB_raw_data$WD_Department == "4269" & MSBIB_raw_data$`Employee Name` == "CRAIG, BRITTANY PIERCE"] <- "DUS_REMOVE"
MSBIB_raw_data$`Position Code Description`[MSBIB_raw_data$WD_COFT == "4409" & MSBIB_raw_data$WD_Location == "03" & MSBIB_raw_data$WD_Department == "4262" & MSBIB_raw_data$`Employee Name` == "DELAPENHA, SANDRA ELAINE"] <- "DUS_REMOVE"
MSBIB_raw_data$`Position Code Description`[MSBIB_raw_data$WD_COFT == "4409" & MSBIB_raw_data$WD_Location == "03" & MSBIB_raw_data$WD_Department == "4269" & MSBIB_raw_data$`Employee Name` == "GANZ, CINDY MARIE"] <- "DUS_REMOVE"
MSBIB_raw_data$`Position Code Description`[MSBIB_raw_data$WD_COFT == "4408" & MSBIB_raw_data$WD_Location == "00" & MSBIB_raw_data$WD_Department == "4269" & MSBIB_raw_data$`Employee Name` == "MEEHAN, JENNIFER JOYCE"] <- "DUS_REMOVE"
MSBIB_raw_data$`Position Code Description`[MSBIB_raw_data$WD_COFT == "4409" & MSBIB_raw_data$WD_Location == "03" & MSBIB_raw_data$WD_Department == "4820" & MSBIB_raw_data$`Employee Name` == "MEEHAN, JENNIFER JOYCE"] <- "DUS_REMOVE"
MSBIB_raw_data$`Position Code Description`[MSBIB_raw_data$WD_COFT == "4408" & MSBIB_raw_data$WD_Location == "00" & MSBIB_raw_data$WD_Department == "4269" & MSBIB_raw_data$`Employee Name` == "OKAY, DEVIN JOSEPH"] <- "DUS_REMOVE"
MSBIB_raw_data$`Position Code Description`[MSBIB_raw_data$WD_COFT == "4408" & MSBIB_raw_data$WD_Location == "00" & MSBIB_raw_data$WD_Department == "4269" & MSBIB_raw_data$`Employee Name` == "ROBBINS, STEPHANIE"] <- "DUS_REMOVE"
MSBIB_raw_data$`Position Code Description`[MSBIB_raw_data$WD_COFT == "4409" & MSBIB_raw_data$WD_Location == "03" & MSBIB_raw_data$WD_Department == "4268" & MSBIB_raw_data$`Employee Name` == "WALKER, THERESA L"] <- "DUS_REMOVE"
MSBIB_raw_data$`Position Code Description`[MSBIB_raw_data$WD_COFT == "4409" & MSBIB_raw_data$WD_Location == "03" & MSBIB_raw_data$WD_Department == "4268" & MSBIB_raw_data$`Employee Name` == "WALKER, THERESA L (Lauren)"] <- "DUS_REMOVE"

RDS_path <- paste0(dir, "Reference Tables/")
saveRDS(MSBIB_raw_data, file = paste0(RDS_path, "data_MSBI_MSB.rds"))

rm(dir_raw, MSBIB_file_list, MSBIB_data_list, MSBIB_raw_data, RDS_path)
