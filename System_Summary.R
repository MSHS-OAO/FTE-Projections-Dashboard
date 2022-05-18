
library(here)
memory.limit(size = 8000000)

##Refresh Master#########################################################################
#create list of preprocessed files
preprocess <- list.files(path=paste0(here(),"/Preprocess"))
#bring in functions to run preprocess scripts and summarize them
source("Source_Summary_FTE Trend.R")
#Run source function
System_Source <- lapply(preprocess,function(x)Source_Func(x))
System_Preprocess <- list(BISLR_Oracle = data_BISLR_oracle,MSH_MSQ = data_MSH_MSQ,MSBIB = data_MSBI_MSB,MSSLW = data_MSSL_MSW,MSH_MSQ_Oracle = data_msh_msq_oracle)
rm(data_BISLR_oracle, data_MSBI_MSB, data_MSH_MSQ, data_msh_msq_oracle, data_MSSL_MSW)
#summarize all preprocessed dataframes
System_Preprocess <- lapply(System_Preprocess, function(x)Source_Summary(x))
#bind all summary tables into system summary
System_Summary = do.call("rbind",System_Preprocess)
#Create table of all rows that failed a mapping
#Payroll, Jobcode, Jobcode description, Pay Code, Provider
Error_Report <- filter(System_Summary,is.na(PAYROLL)|is.na(J.C)|is.na(J.C.DESCRIPTION)|is.na(PAY.CODE.MAPPING)|is.na(PROVIDER))
#Save error report
library(xlsx)
write.xlsx(Error_Report,paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/FTE Projections Dashboard/Error Reports/Error_Report_",Sys.Date(),".xlsx"))
#########################################################################################

