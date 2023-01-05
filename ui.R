
#start_date <- max(data$PP.END.DATE) %m-% months(5) 
start_date <- max(data$PP.END.DATE) - 130 

payroll_choices <- sort( unique(as.character(data$PAYROLL)))

group_choices <- sort(unique(as.character(data$service_group[data$PAYROLL %in% "MSH"])))

service_choices <- sort(unique(as.character(data$CORPORATE.SERVICE.LINE[data$PAYROLL %in% "MSH" & 
                                                                          data$service_group %in% "Nursing"])))

date_options <- sort(unique(data$PP.END.DATE), decreasing = T)
date_options <- format(as.Date(date_options, "%B %d %Y"), "%m/%d/%y")




header <- dashboardHeader(title= "MSHS Worked FTE Dashboard", titleWidth = 450)
header$children[[2]]$children[[2]] <- header$children[[2]]$children[[1]]
header$children[[2]]$children[[1]] <-  tags$a(href='https://peak.mountsinai.org/',
                                              tags$img(src='Sinai_logo_white.png',height='100%',width='30%'))
  

ui <- dashboardPage(
      #dashboardHeader(title= "MSHS Worked FTE Dashboard", titleWidth = 250),
      
      header,
      
      
      dashboardSidebar(width = 300,
                       
      # change the navigation bar color                 
      tags$head(tags$style(HTML('
                              .navbar {
                              background-color: #221F72 !important;
                              }'
                         ))),
      
      #change the logo bar color 
      tags$head(tags$style(HTML('
                              .logo {
                              background-color: #221F72 !important;
                              }'
                         ))),
      
    
                       
      sidebarMenu(menuItem("Home", tabName = "home", icon = icon("home")),
                  menuItem("MSHS", tabName = "mshs", icon = icon("hospital")),
              menuItem("Hospital", tabName = "site", icon = icon("hospital")),
        menuItem("Department", tabName = "department", icon = icon("hospital"))
                                   )),
                                  
      
      dashboardBody(
        tabItems(
          ## tab HOME ----------------------------------------
          tabItem(tabName = "home",
                  
                  column(12, 
                       tags$img(src = "Sinai_logo_color.png", height = "200px", 
                           width = "300px", deleteFiles = FALSE)),
                  
                  column(12, 
                         tags$div("MSHS Worked FTE Dashboard", style = "color: #221f72; font-weight:bold; font-size:34px; margin-left: 20px" ,
                                  h3("Health System Operations"),
                                  h4(paste0("Publish Date: ", format(as.Date(Sys.Date(), "%B %d %Y"), "%m/%d/%y"))),
                                  #h4(paste0("Reporting Period: ",report_start_date, " to ", report_end_date ))
                                  )),
                  column(12, 
                         tags$div( id = "Objective", style= "color:	#221f72; margin-left: 20px",
                                   h3("Description:"),
                                   p("This dashboard summarizes MSHS Worked FTE by pay period. The data is stratified by hospital and service line.",
                                     style= "font-size:16px"))),
                  
                
                  column(12,
                         tags$div(id= "data description", style= "color: #221f72; font-size:14px; margin-left: 20px",
                                  h3("Data Notes"),
                                  h5("* Department does not currently have a Premier productivity report"),
                                  h5("1 FTE = 75 Worked Hours/Pay Period")))
                  
                   ),
          
          ## tab MSHS --------------------------------------------
          tabItem(tabName = "mshs",
                  div("MSHS Worked FTE Dashboard", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:25px; margin-left: 20px"),
                  textOutput("mshs_DateShow"),
                  textOutput("mshs_reportingDate"),
                  tags$head(tags$style("#mshs_DateShow{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 18px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
                  tags$head(tags$style("#mshs_reportingDate{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 16px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
                  
                  tags$head(tags$style(HTML("#mshs_FiltersUpdate {background-color: #d80b8c;color: #FFFFFF; font-size: 18px}"))),
                                            
                  
                  fluidRow(
                    
                  tags$style(HTML(".box.box-solid.box-primary>.box-header {background:#221f72; color:#fff}")),
    
                    column(11,
                           box(
                             title = NULL, width = 12, status = "primary", 
                             solidHeader = TRUE, collapsible = TRUE, closable = TRUE, br(),
                             fluidRow(
                               
                               box(width = 4, height = "100px", title = "Select Service Line Category:", solidHeader = F,
                                   pickerInput("mshs_selectedGroup",label= NULL, multiple= F, options = pickerOptions(actionsBox = TRUE),
                                               choices = group_choices ,  selected = "Nursing")),
                               
                               box(width = 4, height = "100px", title = "Select Service Line:", solidHeader = F,
                                   pickerInput("mshs_selectedService",label= NULL, multiple= T, 
                                               options = pickerOptions(
                                                 liveSearch = TRUE,
                                                 actionsBox = TRUE,
                                                 dropupAuto = FALSE),
                                               choices = service_choices,  selected = "Nursing")),
                               box(width = 4, height = "100px",
                                   title = "Select Pay Period End Date:",  solidHeader = FALSE,
                                   pickerInput("mshs_DateRange",label= NULL, multiple= F, 
                                                options = pickerOptions(
                                                 actionsBox = TRUE,
                                                 dropupAuto = FALSE),
                                               choices = date_options ,  selected = date_options[1])),
                               
                               
                               
                               column(5,
                                      actionButton("mshs_FiltersUpdate", "CLICK TO UPDATE", width = "75%"),
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
          ), # close tabitem mshs
          
          
    
          ## Site Tab -------------------------
          
          tabItem(tabName = "site",
                  div("Hospitals Worked FTE Dashboard", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:25px; margin-left: 20px"),
                  textOutput("siteName_DateShow"),
                  textOutput("site_reportingDate"),
                  tags$head(tags$style("#siteName_DateShow{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 18px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
                  tags$head(tags$style("#site_reportingDate{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 16px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
                  
                  tags$head(tags$style(HTML("#FiltersUpdate {background-color: #d80b8c;color: #FFFFFF; font-size: 18px}"))),
                  
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
                                 pickerInput("selectedGroup",label= NULL, multiple= F, options = pickerOptions(actionsBox = TRUE),
                                             choices = group_choices ,  selected = "Nursing")),
                             
                             box(width = 3, height = "100px", title = "Select Service Line:", solidHeader = F,
                                 pickerInput("selectedService",label= NULL, multiple= T, 
                                             options = pickerOptions(
                                               liveSearch = TRUE,
                                               actionsBox = TRUE),
                                             choices = service_choices,  selected = "Nursing - Administration")),
                             box(width = 3, height = "100px",
                                 title = "Select Pay Period End Date:",  solidHeader = FALSE,
                                 pickerInput("DateRange",label= NULL, multiple= F, 
                                             options = pickerOptions(
                                               actionsBox = TRUE,
                                               dropupAuto = FALSE),
                                               choices = date_options ,  selected = date_options[1])),
                             
                             
                             column(5,
                                    actionButton("FiltersUpdate", "CLICK TO UPDATE", width = "57%"),
                                    br(),
                                    br()
                             )
                           )
                         )),
                  
            
              column(11,
                     box(title = NULL, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         plotlyOutput("site_plot"),  width= 12 )),
              
              column(11, 
                     box(
                       title = NULL, width = 12, status = "primary", 
                       solidHeader = TRUE, collapsible = TRUE, closable = TRUE, 
                       DTOutput("site_table") %>% 
                         withSpinner(type = 5, color = "#d80b8c")))
              
          
                     
                     ) # Close fluidrow
            ), # close tabitem site
          
          ## Department Tab -------------------------
          
          tabItem(tabName = "department",
                  div("Department Worked FTE Dashboard", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:25px; margin-left: 20px"),
                  textOutput("Department_DateShow"),
                  textOutput("Department_reportingDate"),
                  tags$head(tags$style("#Department_DateShow{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 18px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
                  tags$head(tags$style("#Department_reportingDate{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 16px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
                  
                  tags$head(tags$style(HTML("#dep_FiltersUpdate {background-color: #d80b8c;color: #FFFFFF; font-size: 18px}"))),
                  
                  fluidRow(
                  column(11,
                         box(
                           title = NULL, width = 12, status = "primary", 
                           solidHeader = TRUE, collapsible = TRUE, closable = TRUE, br(),
                           fluidRow(
                             box(width = 3, height = "100px", title = "Select Hospital:", solidHeader = F,
                                 pickerInput("dep_selectedPayroll",label= NULL, multiple= F, options = pickerOptions(actionsBox = TRUE),
                                             choices = payroll_choices,  selected = "MSH")),
                             box(width = 3, height = "100px", title = "Select Service Line Category:", solidHeader = F,
                                 pickerInput("dep_selectedGroup",label= NULL, multiple= F, options = pickerOptions(actionsBox = TRUE),
                                             choices = group_choices ,  selected = "Nursing")),
                             
                             box(width = 3, height = "100px", title = "Select Service Line:", solidHeader = F,
                                 pickerInput("dep_selectedService",label= NULL, multiple= T, 
                                             options = pickerOptions(
                                               liveSearch = TRUE,
                                               actionsBox = TRUE,
                                               dropupAuto = FALSE),
                                             choices = service_choices,  selected = "Nursing - Administration")),
                             box(width = 3, height = "100px",
                                 title = "Select Pay Period End Date:",  solidHeader = FALSE,
                                 pickerInput("dep_DateRange",label= NULL, multiple= F, 
                                             options = pickerOptions(
                                               actionsBox = TRUE,
                                               dropupAuto = FALSE),
                                               choices = date_options ,  selected = date_options[1])),
                             
                             
                             
                             column(5,
                                    actionButton("dep_FiltersUpdate", "CLICK TO UPDATE", width = "57%"),
                                    br(),
                                    br()
                             )
                           )
                         )),
                  
                  
                    column(11,
                           box(title = NULL, status = "primary",
                               solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                               plotlyOutput("department_plot"),  width= 12 )),
                    
                    column(11, 
                           box(
                             title = NULL, width = 12, status = "primary", 
                             solidHeader = TRUE, collapsible = TRUE, closable = TRUE, 
                             DTOutput("department_table") %>% 
                               withSpinner(type = 5, color = "#d80b8c")))
                    
                    
                    
                  ) # Close fluidrow
          ) # close tabitem department
          
          
          
        )# close tabItems
        
     
      
      ) # close dashboardBody
) # close dashboardPage
