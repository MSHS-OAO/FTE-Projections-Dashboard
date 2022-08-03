
#start_date <- max(data$PP.END.DATE) %m-% months(5) 
start_date <- max(data$PP.END.DATE) - 130 


ui <- dashboardPage(
      dashboardHeader(title= "MSHS Worked FTE Dashboard", titleWidth = 250),
      
      dashboardSidebar(width = 250,
                       sidebarMenu(menuItem("Home", tabName = "home", icon = icon("home")),
                                   menuItem("MSHS", tabName = "mshs", icon = icon("hospital")) )),
                                  
      
      dashboardBody(
        tabItems(
          tabItem(tabName = "home" ),
          tabItem(tabName = "mshs",
                  div("MSHS", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:25px; margin-left: 20px"),
                  textOutput("siteName_DateShow"),
                  tags$head(tags$style("#siteName_DateShow{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 18px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
                  
                  
            fluidRow(
              
              column(11,
                     box(
                       title = "Customize Dashboard", width = 12, status = "primary", 
                       solidHeader = TRUE, collapsible = TRUE, closable = TRUE, br(),
                       fluidRow(
                         box(width = 4, height = "100px", title = "Select Campus:", solidHeader = F,
                             pickerInput("selectedPayroll",label= NULL, multiple= F,
                                         choices = sort( unique(as.character(data$PAYROLL))),  selected = "MSH")),
                         box(width = 4, height = "100px", title = "Select Services:", solidHeader = F,
                             pickerInput("selectedService",label= NULL, multiple= F,
                                         choices = sort( unique(as.character( data$CORPORATE.SERVICE.LINE))),  selected = "Pharmacy")),
                         
                         box(width = 4, height = "100px",
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
                     box(
                       title = "Pay Period", width = 12, status = "primary", 
                       solidHeader = TRUE, collapsible = TRUE, closable = TRUE, 
                       DTOutput("mshs_table") %>% 
                         withSpinner(type = 5, color = "#d80b8c"))),
              
              column(11,
                     box(plotOutput("mshs_plot"),  width= 12,
                         title = "MSHS Pay Period", status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE ))
                    
                     
                     ) # Close fluidrow
            ) # close tabitem mshs
         
          
        )# close tabItems
        
        
      ) # close dashboardBody
) # close dashboardPage
