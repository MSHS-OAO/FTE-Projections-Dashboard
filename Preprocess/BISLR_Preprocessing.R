# Load Libraries ----------------------------------------------------------
library(tidyverse)
#library(xlsx)

# Constants ---------------------------------------------------------------
dir_universal <- paste0("J:/deans/Presidents/SixSigma/MSHS Productivity",
                        "/Productivity/Universal Data")

# Load Data ---------------------------------------------------------------
#Import raw BISLR oracle data
#data_BISLR_oracle <- readRDS(paste0(dir_universal, "/Labor/RDS/data_BISLR_oracle.rds"))

data_BISLR_oracle <- readRDS(paste0(dir_universal ,
                            "/Labor/RDS/data_BISLR_oracle.rds"))



# Preprocessing -----------------------------------------------------------

#Add in worked and home cost center based on Full COA - Oracle Format
data_BISLR_oracle <- data_BISLR_oracle %>%
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
data_BISLR_oracle <- data_BISLR_oracle %>%
  mutate(Hours = as.numeric(Hours),
         Expense = as.numeric(Expense),
         Pay.Code = as.character(Pay.Code))

#Rename columns to FTE trend column names
data_BISLR_oracle <- data_BISLR_oracle %>%
  rename(J.C.DESCRIPTION = Position.Code.Description,
         J.C = Job.Code,
         PAY.CODE = Pay.Code,
         START.DATE = Start.Date,
         END.DATE = End.Date,
         HOME.LOCATION = Home.FacilityOR.Hospital.ID,
         WRKD.LOCATION = Facility.Hospital.Id_Worked,
         HOME.DESCRIPTION = Department.Name.Home.Dept,
         WRKD.DESCRIPTION = Department.Name.Worked.Dept,
         HOURS = Hours,
         EXPENSE = Expense)

#remove all varibales in envirnoment expect data BISLR
rm(dir_universal)
