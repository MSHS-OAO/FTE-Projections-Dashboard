
#start_date <- max(data$PP.END.DATE) %m-% months(5) 
start_date <- max(data$PP.END.DATE) - 130 

payroll_choices <- sort( unique(as.character(data$PAYROLL)))

group_choices <- sort(unique(as.character(data$service_group[data$PAYROLL %in% "MSH"])))

service_choices <- sort(unique(as.character(data$CORPORATE.SERVICE.LINE[data$PAYROLL %in% "MSH" & 
                                                                          data$service_group %in% "Nursing"])))
  

ui <- dashboardPage(
      dashboardHeader(title= "MSHS Worked FTE Dashboard", titleWidth = 250),
      
      dashboardSidebar(width = 250,
                       sidebarMenu(menuItem("Home", tabName = "home", icon = icon("home")),
                                   menuItem("Site", tabName = "mshs", icon = icon("hospital")),
                                   menuItem("Department", tabName = "mshs", icon = icon("hospital"))
                                   )),
                                  
      
      dashboardBody(
        tabItems(
          tabItem(tabName = "home",
                  column(12, 
                         tags$div("MSHS Worked FTE Dashboard", style = "color:	#221f72; font-weight:bold; font-size:34px; margin-left: 20px" ,
                                  h3("Health System Operations"),
                                  h4(paste0("Publish Date: ", Sys.Date() )),
                                  h4(paste0("Reporting Period: ",report_start_date, " to ", report_end_date )))),
                  column(12, 
                         tags$div( id = "Objective", style= "color:	#221f72; margin-left: 20px",
                                   h3("Description:"),
                                   p("This dashboard summarizes MSHS Worked FTE by pay period. The data is stratified by hospital and service line.",
                                     style= "font-size:16px"))),
                  
                
                  column(12,
                         tags$div(id= "data description", style= "color:	#221f72; font-size:14px; margin-left: 20px",
                                  h3("Data Notes"),
                                  h5("* Department does not currently have a Premier productivity report"),
                                  h5("1 FTE = 75 Worked Hours/Pay Period")))
                  
                  ),
          
          
          tabItem(tabName = "mshs",
                  div("Worked FTE Dashboard", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:25px; margin-left: 20px"),
                  textOutput("siteName_DateShow"),
                  tags$head(tags$style("#siteName_DateShow{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 18px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
                  
                  
            fluidRow(
              
              column(11,
                     box(
                       title = NULL, width = 12, status = "primary", 
                       solidHeader = TRUE, collapsible = TRUE, closable = TRUE, br(),
                       fluidRow(
                         box(width = 3, height = "100px", title = "Select Hospital:", solidHeader = F,
                             pickerInput("selectedPayroll",label= NULL, multiple= T, options = pickerOptions(actionsBox = TRUE),
                                         choices = payroll_choices,  selected = "MSH")),
                         box(width = 3, height = "100px", title = "Select Service Line Category:", solidHeader = F,
                             pickerInput("selectedGroup",label= NULL, multiple= T, options = pickerOptions(actionsBox = TRUE),
                                         choices = group_choices ,  selected = "Nursing")),
                         
                         box(width = 3, height = "100px", title = "Select Service Line:", solidHeader = F,
                             pickerInput("selectedService",label= NULL, multiple= T, options = pickerOptions(actionsBox = TRUE),
                                         choices = service_choices,  selected = "Nursing")),
                         
                         box(width = 3, height = "100px",
                             title = "Select Date Range:",  solidHeader = FALSE, 
                             dateRangeInput("DateRange", label = NULL, width = "75%",
                                            start = start_date, end = max(data$PP.END.DATE),
                                            min = min(data$PP.END.DATE), max = max(data$PP.END.DATE))),
                         
                         
                         
                         column(5,
                                actionButton("FiltersUpdate", "CLICK TO UPDATE", width = "75%"),
                                br(),
                                br()
                         )
                       )
                     )),
              
              
              column(11,
                     box(title = NULL, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         plotlyOutput("mshs_plot"),  width= 12 )),
              
              column(11, 
                     box(
                       title = NULL, width = 12, status = "primary", 
                       solidHeader = TRUE, collapsible = TRUE, closable = TRUE, 
                       DTOutput("mshs_table") %>% 
                         withSpinner(type = 5, color = "#d80b8c")))
              
          
                     
                     ) # Close fluidrow
            ) # close tabitem mshs
         
          
        )# close tabItems
        
        
      ) # close dashboardBody
) # close dashboardPage
