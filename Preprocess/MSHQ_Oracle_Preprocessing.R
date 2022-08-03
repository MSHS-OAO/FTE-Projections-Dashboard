library(dplyr)
library(readxl)
library(rstudioapi)

memory.limit(size = 8000000)

#universal directory
universal_dir <- paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                        "Productivity/Universal Data/")

dir <- "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Universal Data/Labor"

#Read in raw MSHQ Oracle raw refresh
# data_msh_msq_oracle <- readRDS(paste0(universal_dir,
#                                        "Labor/RDS/data_msh_msq_oracle.rds"))



data_msh_msq_oracle <- readRDS(paste0(universal_dir,
                                      "Labor/REPOS/data_MSH_MSQ_oracle.rds"))

#Read COA for department location
coa <- read.csv(paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                       "Productivity/Analysis/FEMA Reimbursement/",
                       "MSHS-FEMA-Reimbursement/Reference Tables/COA.csv"),
                header = T, stringsAsFactors = F, strip.white = TRUE)

#Read in job code descriptions
jc_desc <- read_xlsx(paste0(universal_dir,
                           "Mapping/MSHS_Jobcode_Mapping.xlsx")) %>%
  filter(PAYROLL == "MSHQ") %>%
  select(J.C, J.C.DESCRIPTION)

#Replace departments that failed GEAC map
oracle <- data_msh_msq_oracle %>%
  mutate(Reverse.Map.for.Worked = case_when(
    Reverse.Map.for.Worked == "" ~ Department.IdWHERE.Worked,
    TRUE ~ Reverse.Map.for.Worked),
    Reverse.Map.for.Home = case_when(
      Reverse.Map.for.Home == "" ~ paste0(HD_COFT, HD_Location, HD_Department),
      TRUE ~ Reverse.Map.for.Home)) %>%
  #Build full COA for oracle home department
  mutate(Department.ID.Home.Department = paste0(substr(Full.COA.for.Home,
                                                1, 3),
                                         substr(Full.COA.for.Home,
                                                41, 44),
                                         substr(Full.COA.for.Home,
                                                5, 7),
                                         substr(Full.COA.for.Home,
                                                12, 16)))

#Take first 8 digits of Home and Worked department for reverse map
oracle <- oracle %>%
  mutate(
    Reverse.Map.for.Worked =
      case_when(
        nchar(Reverse.Map.for.Worked) == 12 ~
          substr(Reverse.Map.for.Worked, 1, 8),
        TRUE ~ Reverse.Map.for.Worked),
    Reverse.Map.for.Home =
      case_when(
        nchar(Reverse.Map.for.Home) == 12 ~
          substr(Reverse.Map.for.Home, 1, 8),
        TRUE ~ Reverse.Map.for.Home))

#Bring in department location
row_count <- nrow(oracle)
oracle <- left_join(oracle, coa %>% select(Column1, Column2) ,
                    by = c("Reverse.Map.for.Worked" = "Column2"))

# oracle <- left_join(oracle, coa ,
#                     by = c("Reverse.Map.for.Worked" = "Column2")) %>%
#   select(1:36)
if(nrow(oracle) != row_count) {
  stop(paste("Row count failed at", basename(getSourceEditorContext()$path)))
  }


row_count <- nrow(oracle)
oracle <- left_join(oracle, coa  %>% select(Column1, Column2) ,
                    by = c("Reverse.Map.for.Home" = "Column2"))


# oracle <- left_join(oracle, coa ,
#                     by = c("Reverse.Map.for.Home" = "Column2")) %>%
#   select(1:37)
if(nrow(oracle) != row_count) {
  stop(paste("Row count failed at", basename(getSourceEditorContext()$path)))
  }

#Bring in standardized JC Description
row_count <- nrow(oracle)
oracle <- left_join(oracle, jc_desc, by = c("Job.Code" = "J.C"))
if(nrow(oracle) != row_count) {
  stop(paste("Row count failed at", basename(getSourceEditorContext()$path)))
}

#Format necessary columns
oracle <- oracle %>%
  mutate(End.Date = as.Date(End.Date, format = "%m/%d/%Y"),
         Hours = as.numeric(Hours),
         Expense = as.numeric(Expense))


#Column names
oracle <- oracle %>% rename(DPT.HOME= Department.ID.Home.Department, 
                        DPT.WRKD= Department.IdWHERE.Worked,
                        START.DATE= Start.Date, END.DATE= End.Date,
                        J.C= Job.Code,  PAY.CODE= Pay.Code,
                        HOME.DESCRIPTION= Department.Name.Home.Dept,
                        WRKD.DESCRIPTION= Department.Name.Worked.Dept,
                        HOURS= Hours,  EXPENSE= Expense, 
                        WRKD.LOCATION= Column1.x, HOME.LOCATION= Column1.y, 
                        J.C.DESCRIPTION= J.C.DESCRIPTION )


# #Column names
# new_col_names <- c("DPT.HOME", "DPT.WRKD", "START.DATE", "END.DATE", "J.C",
#                    "PAY.CODE", "HOME.DESCRIPTION", "WRKD.DESCRIPTION", "HOURS",
#                    "EXPENSE", "WRKD.LOCATION", "HOME.LOCATION",
#                    "J.C.DESCRIPTION")
# colnames(oracle)[c(3, 5, 6, 7, 12:15, 32, 33, 35:37)] <- new_col_names

data_msh_msq_oracle <<- oracle

rm(coa, jc_desc, oracle, universal_dir)
