Source_Func <- function(x){
  source(paste0(here(),"/Preprocess/",x))
  setwd(here())
}

Source_Summary <- function(data){
  library(readxl)
  library(rstudioapi)
  #Read paycode mapping file and Pay cycle file
  System_Paycode <- read_xlsx(paste0('J:/deans/Presidents/SixSigma',
                                     '/MSHS Productivity/Productivity',
                                     '/Universal Data/Mapping',
                                     '/MSHS_Paycode_Mapping.xlsx'))%>%
    select(RAW.PAY.CODE, PAY.CODE.NAME, PAY.CODE.CATEGORY, INCLUDE.HOURS,
           INCLUDE.EXPENSES, WORKED.PAY.CODE) %>%
    rename(PAY.CODE = RAW.PAY.CODE,
           PAY.CODE.MAPPING = PAY.CODE.CATEGORY)

  #Read in paycycle
  PayCycle <- read_excel("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Universal Data/Mapping/MSHS_Pay_Cycle.xlsx")
  PayCycle <- PayCycle %>%
    mutate(DATE = as.Date(DATE),
           START.DATE = as.Date(START.DATE),
           END.DATE = as.Date(END.DATE)) 
  
  #Begin summarization
  Department <- data %>%
    group_by(PAYROLL,WRKD.LOCATION,HOME.LOCATION,DPT.WRKD,DPT.HOME,WRKD.DESCRIPTION,HOME.DESCRIPTION,J.C,J.C.DESCRIPTION,PAY.CODE,END.DATE) %>%
    summarise(HOURS = sum(HOURS, na.rm = T), EXPENSE = sum(EXPENSE, na.rm = T))
  
  
  #assign pp end dates and summarize
  row_count <- nrow(Department)
  department_paycylce <- left_join(Department,PayCycle,by = c("END.DATE" = "DATE")) 
  if(nrow(department_paycylce) != row_count){
    stop(paste("Row count failed at", basename(getSourceEditorContext()$path)))}
  Summary <- department_paycylce %>%
    select(PAYROLL,WRKD.LOCATION,HOME.LOCATION,DPT.WRKD,DPT.HOME,WRKD.DESCRIPTION,HOME.DESCRIPTION,J.C,J.C.DESCRIPTION,PAY.CODE,END.DATE.y,HOURS,EXPENSE) %>%
    group_by(PAYROLL,WRKD.LOCATION,HOME.LOCATION,DPT.WRKD,DPT.HOME,WRKD.DESCRIPTION,HOME.DESCRIPTION,J.C,J.C.DESCRIPTION,PAY.CODE,END.DATE.y) %>%
    summarize(HOURS = sum(HOURS, na.rm = T),EXPENSE = sum(EXPENSE, na.rm = T))
  
  colnames(Summary)[11] <- "PP.END.DATE"
  
  #Bring in paycode mapping and hours included columns
  row_count <- nrow(Summary)
  Site_Summary <- left_join(Summary,System_Paycode) %>%
    select(-PAY.CODE.NAME)
  if(nrow(Site_Summary) != row_count){
    stop(paste("Row count failed at", basename(getSourceEditorContext()$path)))}
  
  #Bring in cost center mappings
  System_Department <- read_xlsx("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Universal Data/Mapping/MSHS_Reporting_Definition_Mapping.xlsx")
  System_Department <- System_Department %>%
    filter(FTE.TREND == 1,
           is.na(CLOSED)) %>%
    select(ORACLE.COST.CENTER, DEFINITION.CODE, DEFINITION.NAME, CORPORATE.SERVICE.LINE, SITE)
  row_count <- nrow(Site_Summary)
  Site_Summary <- left_join(Site_Summary,System_Department, by = c("DPT.WRKD" = "ORACLE.COST.CENTER")) %>%
    ungroup() %>%
    mutate(SITE = case_when(
      is.na(SITE) ~ PAYROLL,
      TRUE ~ SITE),
      PAYROLL = SITE,
      SITE = NULL)
  if(nrow(Site_Summary) != row_count){
    stop(paste("Row count failed at", basename(getSourceEditorContext()$path)))}
  
  #Bring in Provider Column
  System_Jobcode <- read_xlsx("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Universal Data/Mapping/MSHS_Jobcode_Mapping.xlsx")
  System_Jobcode <- System_Jobcode %>%
    select(PAYROLL,J.C, PROVIDER) %>%
    distinct()
  
  #System_Jobcode <- select(System_Jobcode, J.C, PROVIDER)
  #Site_Summary <- left_join(Site_Summary,System_Jobcode, by = c("J.C"="J.C")) 
  
  #filter on BISLR JC for MSM,MSW,MSBIB,MSB,BISLR
  BISLR_jc <- System_Jobcode %>% filter(PAYROLL == "BISLR") %>% select(J.C,PROVIDER)
  #fiter on MSHQ for MSH,MSQ
  MSHQ_jc <- System_Jobcode %>% filter(PAYROLL == "MSHQ") %>% select(J.C,PROVIDER)
  #left join for BISLR provider mapping
  row_count <- nrow(Site_Summary)
  Site_Summary <- left_join(Site_Summary,BISLR_jc, by = c("J.C"="J.C")) 
  if(nrow(Site_Summary) != row_count){
    stop(paste("Row count failed at", basename(getSourceEditorContext()$path)))}
  #left join for MSHQ provider mapping
  row_count <- nrow(Site_Summary)
  Site_Summary <- left_join(Site_Summary,MSHQ_jc, by = c("J.C"="J.C")) 
  if(nrow(Site_Summary) != row_count){
    stop(paste("Row count failed at", basename(getSourceEditorContext()$path)))}
  #select correct provider column based on PAYROLL
  Site_Summary <- Site_Summary %>% 
    mutate(PROVIDER = case_when(
       PAYROLL %in% c("MSM","MSW","MSBI","MSB") ~ PROVIDER.x,
       PAYROLL %in% c("MSH","MSQ","Corporate") ~ PROVIDER.y)) %>%
     select(-PROVIDER.x,-PROVIDER.y)
  
  Site_Summary <- Site_Summary %>% distinct()
  
  rm(row_count)
  return(Site_Summary)
}
