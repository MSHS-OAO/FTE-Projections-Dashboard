library(dplyr)
library(tidyr)
library(here)

memory.limit(size = 8000000)

##MSHQ##
#PP end dates for filtering each raw file
PPend_list <- list("04/25/2020","05/23/2020","06/20/2020","08/01/2020",
                   "08/29/2020","09/26/2020","10/24/2020","11/21/2020",
                   "01/02/2021","01/23/2021","02/27/2021",
                   "03/27/2021","04/24/2021","05/22/2021","06/19/2021",
                   "07/31/2021","08/28/2021","09/25/2021","10/23/2021",
                   "11/20/2021","01/01/2022","01/29/2022","02/26/2022",
                   "03/26/2022","04/23/2022")
#file path for all raw files
folderOracle <- paste0(here(),"/Raw Data/MSHQ Oracle/")
#List files from MSHQ Raw folder
Oracle_file_list <- list.files(path=folderOracle, pattern="*.txt")
details = file.info(list.files(path = folderOracle, pattern="*.txt", full.names = T)) %>% arrange(mtime)
Oracle_file_list <- rownames(details)
#Read files in MSQ Raw as csv
ORACLElist = lapply(Oracle_file_list,
                    function(x)read.csv(x, sep = "~", header=T,
                                        stringsAsFactors = F,
                                        colClasses = rep("character",32),
                                        strip.white = TRUE))
#Remove overlapping dates from raw txt files
for(i in 1:length(ORACLElist)){
  if(i == 1){
    ORACLElist[[i]] <- ORACLElist[[i]] %>%
      filter(as.Date(End.Date, format = "%m/%d/%Y") <= as.Date(PPend_list[[i]], format = "%m/%d/%Y"))
  } else if(i == 2){
    ORACLElist[[i]] <- ORACLElist[[i]] %>%
      filter(as.Date(End.Date, format = "%m/%d/%Y") <= as.Date(PPend_list[[i]], format = "%m/%d/%Y"),
             as.Date(End.Date, format = "%m/%d/%Y") >= max(as.Date(ORACLElist[[i-1]]$End.Date,format = "%m/%d/%Y")))
  } else if(i == 11){
    ORACLElist[[i]] <- ORACLElist[[i]] %>%
      filter(as.Date(End.Date, format = "%m/%d/%Y") <= as.Date(PPend_list[[i]], format = "%m/%d/%Y"),
             as.Date(End.Date, format = "%m/%d/%Y") > max(as.Date(ORACLElist[[i-1]]$End.Date,format = "%m/%d/%Y")))
  } else {
    ORACLElist[[i]] <- ORACLElist[[i]] %>%
      filter(as.Date(End.Date, format = "%m/%d/%Y") <= as.Date(PPend_list[[i]], format = "%m/%d/%Y"),
             as.Date(Start.Date, format = "%m/%d/%Y") > max(as.Date(ORACLElist[[i-1]]$End.Date,format = "%m/%d/%Y")))
  }
}



#Bind all MSHQ files
Oracle = do.call("rbind", ORACLElist)
#Remove Duplicate rows and add worked entity column
Oracle <- Oracle %>%
  mutate(WRKD.ENTITY = substr(Oracle$WD_COFT,1,3),
         Hours = as.numeric(Hours),
         Expense = as.numeric(Expense))
Oracle <- Oracle%>%
  group_by_at(c(1:13,16:33)) %>%
  summarise(Hours = sum(Hours, na.rm = T),
            Expense = sum(Expense,na.rm = T)) %>%
  ungroup() %>%
  distinct()
#Determine PAYROLL based on WRKD.ENTITY
Oracle <- Oracle %>%
  mutate(PAYROLL = case_when(
    WRKD.ENTITY == "102" ~ "MSQ",
    TRUE ~ "MSH"))


#Check sum of hours by end date to make sure data follows proper pattern
check <- Oracle %>%
  ungroup() %>%
  group_by(PAYROLL,End.Date) %>%
  summarise(Hours = sum(Hours)) %>%
  mutate(End.Date = as.Date(End.Date, format = "%m/%d/%Y")) %>%
  arrange(End.Date) 
check1 <- pivot_wider(check,id_cols = PAYROLL,values_from = Hours,names_from = End.Date)
#Save .rds
saveRDS(Oracle,file = "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Universal Data/Labor/RDS/data_MSH_MSQ_oracle.rds")
