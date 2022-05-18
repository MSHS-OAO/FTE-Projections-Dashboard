#FTE Dashboard Render
library(rmarkdown)
library(here)

#Source system summary to update the system_summary.rds
###Check error report for any mapping issues###########
source("System_Summary.R")

#Enter max PP end date for dashboard
end <- "01/29/2022"

#Save System Summary table
saveRDS(System_Summary,file=paste0("J:/deans/Presidents/SixSigma/",
                                   "MSHS Productivity/Productivity/",
                                   "Universal Data/Labor/RDS/",
                                   "System_Summary.rds"))
#Save System SUmmary table pre filtered for dashboard
saveRDS(System_Summary %>% 
          filter(PP.END.DATE >= as.Date("12/22/2019",format="%m/%d/%Y"),
                 PP.END.DATE <= as.Date(end,format="%m/%d/%Y")) %>%
          replace_na(list(CORPORATE.SERVICE.LINE = "Other")),
        file=paste0("J:/deans/Presidents/SixSigma/",
                                   "MSHS Productivity/Productivity/",
                                   "Universal Data/Labor/RDS/",
                                   "System_Summary_Dashboard.rds"))

#render FTE Trend Dashboard
render("FTE_Trend_Dashboard_apply.rmd")

#Archive the FTE Trend Dashboard with date of max pay period
file.copy(paste0(here(),"/FTE_Trend_Dashboard_apply.html"),"J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/FTE Projections Dashboard/Dashboards")
file.rename("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/FTE Projections Dashboard/Dashboards/FTE_Trend_Dashboard_apply.html",
            paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/FTE Projections Dashboard/Dashboards/FTE_Trend_Dashboard_",as.Date(end,format="%m/%d/%Y"),".html"))
