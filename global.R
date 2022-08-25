
# Shiny App for FTE Trend 

rm(list=ls())

# Import Libraries -------------------------------------------------------------
suppressMessages({
  library(readxl)
  library(tidyverse)
  library(dplyr)
  library(reshape2)
  library(data.table)
  library(plotly)
  library(shiny)
  library(shinythemes)
  library(shinydashboard)
  library(shinycssloaders)
  library(shinyWidgets)
  library(patchwork)
  library(gridExtra)
  library(grid)
  library(scales)
  library(DT)  
  library(knitr)
  library(lubridate)
  library(kableExtra)
})



# ### (0) Maximize R Memory Size 
memory.limit(size = 8000000)

### (1) Set color theme --------------------------------------------------------

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


### (2) Import Data ------------------------------------------------------------
System_Summary <- readRDS("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Universal Data/Labor/RDS/System_Summary_Dashboard.rds")


#Worked hour pay code mappings
worked_paycodes <- c('REGULAR', 'OVERTIME', 'OTHER_WORKED', 'EDUCATION',
                     'ORIENTATION', 'AGENCY')

#pre covid pay period end dates
pre_covid_PP <- as.Date(c('2020-01-04', '2020-01-18', '2020-02-01',
                          '2020-02-15','2020-02-29'))


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
    PP.END.DATE = as.Date(PP.END.DATE,format="%Y-%m-%d")) 

#Get Reporting Period data range
report_start_date <- format(as.Date(data$PP.END.DATE[nrow(data)-2]-13, "%B %d %Y"), "%m/%d/%Y")
report_end_date <- format(as.Date(data$PP.END.DATE[nrow(data)], "%B %d %Y"), "%m/%d/%Y")


# sites_services_df <- data %>% 
#   ungroup() %>% 
#   select(PAYROLL, CORPORATE.SERVICE.LINE) %>% 
#   filter(!is.na(PAYROLL)) %>% 
#   distinct()
# 
# system_service_lines <- 
#   service_lines[which(!service_lines %in% c("IT", "HR", "CMO"))]



