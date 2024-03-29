library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(knitr)
library(kableExtra)

##Reload Master##########################################################################
System_Summary <- readRDS("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Universal Data/Labor/RDS/System_Summary_Dashboard.rds")
#########################################################################################

#Worked hour pay code mappings
worked_paycodes <- c('REGULAR', 'OVERTIME', 'OTHER_WORKED', 'EDUCATION',
                     'ORIENTATION', 'AGENCY')
#pre covid pay period end dates
pre_covid_PP <- as.Date(c('2020-01-04', '2020-01-18', '2020-02-01',
                          '2020-02-15','2020-02-29'))
####get unique service lines and sites####
service_lines <- c('MSHS', 'Hospital', 'All Nursing', 'IT', 'HR', 'CMO',
                   'Nursing - Administration', 'Nursing - Adolescent Psych', 
                   'Nursing - Adult Psych', 'Nursing - Antepartum / Postpartum', 
                   'Nursing - Cardiology', 'Nursing - Critical Care', 
                   'Nursing - Critical Care / Intermediate Care Blend',
                   'Nursing - Critical Care Cardiac', 'Nursing - Dialysis',
                   'Nursing - Education', 'Nursing - Emergency Department',
                   'Nursing - Emergency Medicine',
                   'Nursing - Intermediate Care', 'Nursing - Labor & Delivery',
                   'Nursing - Med Surg',
                   'Nursing - Med Surg Intermediate Care Blend',
                   'Nursing - Neonatal ICU', 'Nursing - Observation',
                   'Nursing - Palliative', 'Nursing - Postpartum',
                   'Nursing - Psych ED', 'Nursing - Rehab',
                   'Nursing - Telemetry Cardiac',
                   'Nursing - Telemetry Med Surg',
                   'Nursing - Telemetry Observation Blend', 'Nursing',
                   'Radiology - CT', 'Radiology - CT/Diagnostic',
                   'Radiology - Diagnostic', 'Radiology - Interventional',
                   'Radiology - Interventional/CT', 'Radiology - Mammography',
                   'Radiology - Mammography/Diagnostic',
                   'Radiology - Mammography/Interventional', 'Radiology - MRI',
                   'Radiology - Nuclear Medicine',
                   'Radiology - Nuclear Medicine/PET', 'Radiology - PET/CT',
                   'Radiology - Support', 'Radiology - Ultrasound',
                   'Support Services - Biomed / Clinical Engineering',
                   'Support Services - Blood Bank',
                   'Support Services - Clinical Nutrition',
                   'Support Services - Engineering',
                   'Support Services - Environmental Services',
                   'Support Services - Food Services',
                   'Support Services - Linen',
                   'Support Services - Patient Transport',
                   'Support Services - Safety', 'Support Services - Security',
                   'Perioperative Services', 'Admitting', 'Ambulatory - MSBI',
                   'Ambulatory - MSDUS', 'Cardiology', 'Emergency Medicine',
                   'Employee Health Services', 'Lab', 'Materials Management',
                   'Medical Records', 'Pharmacy', 'Rehab','Supply Chain',
                   'System CMO', 'System CMO - Case Management', 'Other')
####end####

site_list <- c("MSH", "MSQ", "MSBI", "MSB", "MSM", "MSW", "Corporate")

report_period_length <- 3
biweekly_fte <- 75
digits_round <- 2

#Pre filter data 
data <- System_Summary %>%
  filter(PP.END.DATE < as.Date("3/1/2020",format="%m/%d/%Y") | #date must be earlier than 3/1/2020
           PP.END.DATE >as.Date("5/9/2020",format="%m/%d/%Y"),   #or greater than 5/9/2020
         PROVIDER == 0, #remove providers
         INCLUDE.HOURS == 1, #only use included hour paycodes
         PAY.CODE.MAPPING %in% worked_paycodes) %>% #remove unproductive paycodes 
  group_by(PAYROLL,DEFINITION.CODE,DEFINITION.NAME,CORPORATE.SERVICE.LINE,PP.END.DATE) %>%
  summarise(FTE = sum(HOURS, na.rm = T)/biweekly_fte) %>% #calculate FTE
  pivot_wider(id_cols = c(PAYROLL,DEFINITION.CODE,DEFINITION.NAME,CORPORATE.SERVICE.LINE),values_from = FTE,names_from = PP.END.DATE)
data[,5:ncol(data)][is.na(data[,5:ncol(data)])] <- 0
data <- data %>%
  pivot_longer(cols = 5:ncol(data),names_to = "PP.END.DATE", values_to = "FTE")
data <- data %>%
  mutate(DEPARTMENT = case_when(
    CORPORATE.SERVICE.LINE %in% c("IT", "HR", "CMO") ~ CORPORATE.SERVICE.LINE,
    is.na(DEFINITION.CODE) ~ "Non-Premier",
    TRUE ~ paste0(DEFINITION.CODE," - ",toupper(DEFINITION.NAME))), #capitalize department
    DATES = as.character(PP.END.DATE),
    PP.END.DATE = as.Date(PP.END.DATE,format="%Y-%m-%d")) %>% #add character form of data
  mutate(CORPORATE.SERVICE.LINE = factor(
    x = CORPORATE.SERVICE.LINE,
    levels = service_lines)) %>%
  mutate(PAYROLL = factor(
    x = PAYROLL,
    levels = site_list)) %>%
  arrange(CORPORATE.SERVICE.LINE) #arrange by pay period end date

#Get Reporting Period data range
rep <- data %>% 
  ungroup() %>% 
  arrange(PP.END.DATE) %>% 
  select(PP.END.DATE) %>% 
  as.vector() %>% 
  distinct() 
rep_per <- as.vector(rep[c(nrow(rep)-2,nrow(rep)-1,nrow(rep)),1])
rep_per[1,1] <- rep_per[1,1]-13
rep_per$PP.END.DATE <- as.character(rep_per$PP.END.DATE)
rep <- paste0(substr(rep_per[1,1],6,7),"/",
              substr(rep_per[1,1],9,10),"/",
              substr(rep_per[1,1],1,4)," to ",
              substr(rep_per[3,1],6,7),"/",
              substr(rep_per[3,1],9,10),"/",
              substr(rep_per[3,1],1,4))

# Mount Sinai corporate colors "USE THIS TO ADD COLORS"
MountSinai_colors <- c(
  `light pink`   = "#fcc9e9",
  `med pink`     = "#fa93d4",
  `dark pink`    = "#d80b8c",
  `light purple` = "#c7c6ef",
  `med purple`   = "#8f8ce0",
  `light blue`   = "#5cd3ff",
  `med blue`     = "#06ABEB",
  `dark blue`    = "#212070",
  `light grey`   = "#b2b3b2",
  `dark grey`    = "#686868",
  `yellow`       = "#E69F00"
)

# Function to extract Mount Sinai colors as hex codes
# Use Character names of MountSinai_colors

MountSinai_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (MountSinai_colors)
  
  MountSinai_colors[cols]
}

# Create palettes
MountSinai_palettes <- list(
  `all`   = MountSinai_cols("med blue","dark pink","dark blue","light grey", "light blue",
                            "light pink", "light purple","med pink","med purple","yellow" ),
  
  `main`  = MountSinai_cols("med blue","dark pink","dark blue","dark grey","light pink","light blue","light grey"),
  
  `pink`  = MountSinai_cols("light pink", "dark pink"),
  
  `blue`  = MountSinai_cols("light blue", "dark blue"),
  
  `grey`  = MountSinai_cols("light grey", "med blue")
  
)
MountSinai_palettes

MountSinai_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- MountSinai_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

#Dashboard Outputs
#Graph styling
graph_style <- function(graph , hosp = NULL, service = NULL, level = "DEFINITION.CODE"){
  title <- trimws(paste(hosp,service))
  graph <- graph+
    geom_line(size=1.5)+
    geom_point(size=2.75)+
    ggtitle(paste(title,"Worked FTE's By Pay Period"))+
    xlab("Pay Period")+
    ylab("FTE (Full Time Equivalent)")+
    scale_color_manual(values=MountSinai_pal("main")(length(unique(data_service[,level]))))+
    theme(plot.title=element_text(hjust=.5,size=20),
          axis.title = element_text(face="bold"),
          legend.text=element_text(size = 6)) #create and style service line graph
  graphly <- ggplotly(graph,tooltip=c("group","x","y")) %>%
    config(displaylogo = F,
           modeBarButtonsToRemove = c("lasso2d","autoScale2d","select2d","toggleSpikelines")) %>%
    layout(title = list(xanchor = "center")) #turn graph into plotly interactive
  return(graphly)
}

#Function for all types of graphs used in html
graph_data <- function(hosp = NULL, serv.line){
  if(!is.null(hosp)){ #graph for within each site section for a specific service line
    data_service <- data %>% #take pre-filtered data
      filter(PAYROLL == hosp, CORPORATE.SERVICE.LINE == serv.line) %>% 
      pivot_wider(id_cols = c("DEFINITION.CODE","DEFINITION.NAME","DEPARTMENT"),
                  names_from = "PP.END.DATE",
                  values_from = FTE) #pivot dataframe to bring in NAs for missing PP
    data_service <- data_service[,c(1:3,(ncol(data_service)-9):ncol(data_service))]
    data_service <- data_service  %>% 
      pivot_longer(cols = 4:ncol(data_service),
                   names_to = "PP.END.DATE") %>% 
      mutate(FTE = case_when(
        is.na(value) ~ 0, #if FTE is NA -> 0
        !is.na(value) ~ value), #else leave the value
        DATES = as.factor(PP.END.DATE),
        FTE = round(value,digits_round)) #turn dates into factor
    data_service <<- as.data.frame(data_service) 
    service_line_graph <- ggplot(data = data_service, aes(x=DATES,y=FTE,group=DEPARTMENT,color=DEPARTMENT))
    graph_style(graph = service_line_graph ,hosp = hosp,service = serv.line)
  } else if(serv.line == 'MSHS'){ #highest level graph for ALL system FTEs
    data_service <- data %>% 
      ungroup() %>%
      select(FTE,PP.END.DATE,DATES) %>%
      group_by(PP.END.DATE,DATES) %>%
      summarise(FTE = round(sum(FTE, na.rm = T),digits_round)) %>%
      mutate(MSHS = "MSHS") %>%
      ungroup()
    data_service <- data_service[(nrow(data_service)-9):nrow(data_service),]
    data_service <<- as.data.frame(data_service) 
    system_total_graph <- ggplot(data = data_service, aes(x=DATES,y=FTE,group=MSHS,color=MSHS))
    graph_style(graph = system_total_graph, hosp = "MSHS",level = "MSHS")
  } else if(serv.line == 'Hospital'){ #graph for FTEs rolled up by site (ignores service line)
    data_service <- data %>% 
      ungroup() %>%
      select(PAYROLL,FTE,PP.END.DATE,DATES) %>%
      group_by(PAYROLL,PP.END.DATE,DATES) %>%
      summarise(FTE = round(sum(FTE, na.rm = T),digits_round)) %>%
      ungroup()
    data_service <- data_service %>% 
      pivot_wider(id_cols=PAYROLL,names_from = DATES,values_from = FTE)
    data_service <- data_service[,c(1,(ncol(data_service)-9):ncol(data_service))]
    data_service <- data_service %>% 
      pivot_longer(cols = 2:ncol(data_service),
                   names_to = "DATES") %>%
      rename(Site = PAYROLL)
    data_service <- data_service %>% 
      mutate(FTE = case_when(
        is.na(value) ~ 0, #if FTE is NA -> 0
        !is.na(value) ~ value), #else leave the value
        FTE = round(value,digits_round))
    data_service <<- as.data.frame(data_service) 
    site_total_graph <- ggplot(data = data_service, aes(x=DATES,y=FTE,group=Site,color=Site))
    graph_style(graph = site_total_graph, hosp = "Hospital", level = "Site")
  } else if(serv.line == "All Nursing"){ #graph for all nursing units rolled up by site
    nursing <- grep("Nursing", service_lines, value = T)
    data_service <- data %>% 
      ungroup() %>%
      select(PAYROLL,CORPORATE.SERVICE.LINE,FTE,PP.END.DATE,DATES) %>%
      filter(CORPORATE.SERVICE.LINE %in% nursing) %>%
      group_by(PAYROLL,PP.END.DATE,DATES) %>%
      summarise(FTE = round(sum(FTE, na.rm = T),digits_round)) %>%
      ungroup()
    data_service <- data_service %>% 
      pivot_wider(id_cols=PAYROLL,names_from = DATES,values_from = FTE)
    data_service <- data_service[,c(1,(ncol(data_service)-9):ncol(data_service))]
    data_service <- data_service %>% 
      pivot_longer(cols = 2:ncol(data_service),
                   names_to = "DATES") %>%
      rename(Site = PAYROLL)
    data_service <- data_service %>% 
      mutate(FTE = case_when(
        is.na(value) ~ 0, #if FTE is NA -> 0
        !is.na(value) ~ value), #else leave the value
        FTE = round(value,digits_round))
    data_service <<- as.data.frame(data_service) 
    nursing_total_graph <- ggplot(data = data_service, aes(x=DATES,y=FTE,group=Site,color=Site))
    graph_style(graph = nursing_total_graph, hosp = "All Nursing", level = "Site")
  } else { #graph for all sites by service line
    data_service <- data %>% 
      ungroup() %>%
      select(PAYROLL,CORPORATE.SERVICE.LINE,FTE,PP.END.DATE,DATES) %>%
      filter(CORPORATE.SERVICE.LINE %in% serv.line) %>%
      group_by(PAYROLL,CORPORATE.SERVICE.LINE,PP.END.DATE,DATES) %>%
      summarise(FTE = round(sum(FTE, na.rm = T),digits_round)) %>%
      ungroup()
    data_service <- data_service %>% 
      pivot_wider(id_cols=c(PAYROLL,CORPORATE.SERVICE.LINE),names_from = DATES,values_from = FTE)
    data_service <- data_service[,c(1,2,(ncol(data_service)-9):ncol(data_service))]
    data_service <- data_service %>% 
      pivot_longer(cols = 3:ncol(data_service),
                   names_to = "DATES") %>%
      rename(Site = PAYROLL)
    data_service <- data_service %>% 
      mutate(FTE = case_when(
        is.na(value) ~ 0, #if FTE is NA -> 0
        !is.na(value) ~ value), #else leave the value
        FTE = round(value,digits_round))
    data_service <<- as.data.frame(data_service) 
    site_service_graph <- ggplot(data = data_service, aes(x=DATES,y=FTE,group=Site,color=Site))
    graph_style(graph = site_service_graph, service = serv.line, level = "Site")
  }
}

#Kable function
k <- function(hosp = NULL, service = NULL){
  if(service == "MSHS"){ #system rollup kable
    kdata <- data %>%
      group_by(DATES) %>%
      summarise(FTE = sum(FTE, na.rm = T)) %>%
      mutate(Site = "MSHS") %>%
      pivot_wider(id_cols = Site,
                  names_from = DATES,
                  values_from = FTE)
  } else if(service == "Hospital"){#FTE by site rollup kable
    kdata <- data %>%
      group_by(PAYROLL, DATES) %>%
      summarise(FTE = sum(FTE, na.rm = T)) %>%
      rename(Site = PAYROLL) %>%
      pivot_wider(id_cols = Site,
                  names_from = DATES,
                  values_from = FTE)
  } else if(service == "All Nursing"){#Nursing FTE by site rollup
    nursing <- grep("Nursing", service_lines, value = T)
    kdata <- data %>%
      filter(CORPORATE.SERVICE.LINE %in% nursing) %>%
      group_by(PAYROLL, DATES) %>%
      summarise(FTE = sum(FTE, na.rm = T)) %>%
      rename(Site = PAYROLL) %>%
      pivot_wider(id_cols = Site,
                  names_from = DATES,
                  values_from = FTE)
  } else if(is.null(hosp)){#servce line FTE by site rollup
    kdata <- data %>%
      filter(CORPORATE.SERVICE.LINE == service) %>%
      group_by(PAYROLL, DATES) %>%
      summarise(FTE = sum(FTE, na.rm = T)) %>%
      rename(Site = PAYROLL) %>%
      pivot_wider(id_cols = Site,
                  names_from = DATES,
                  values_from = FTE)
  } else {
    kdata <- data %>% 
      filter(PAYROLL == hosp, 
             CORPORATE.SERVICE.LINE == service) %>% 
      pivot_wider(id_cols = DEPARTMENT,
                  names_from = DATES,
                  values_from = FTE)
  }
  kdata[is.na(kdata)] <- 0
  kdata <- kdata %>% 
    ungroup() %>% 
    arrange(desc(colnames(kdata)[ncol(kdata)])) 
  kdata$`Reporting Period Avg.` <- apply(kdata[,(ncol(kdata)-2):ncol(kdata)],1,mean)
  kdata <- kdata[,c(1,(ncol(kdata)-10):ncol(kdata))]
  kdata[,(ncol(kdata)-10):ncol(kdata)] <- round(kdata[,(ncol(kdata)-10):ncol(kdata)],digits_round)
  Ktable <- kdata 
  kable(Ktable) %>%
    kable_styling(bootstrap_options = c("striped", "hover"), fixed_thead = T) %>%
    row_spec(0, background = "#212070", color = "white") %>%
    row_spec(1:nrow(Ktable), color = "black") %>%
    row_spec(0:nrow(Ktable), align = "c", font_size = 11) %>%
    column_spec(1,bold = T) %>%
    collapse_rows(1)
}