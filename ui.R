# ui function

# define title for ui
header <- dashboardHeader(title = "MSHS Worked FTE Dashboard", titleWidth = 450)

# define logos for ui
header$children[[2]]$children[[2]] <- header$children[[2]]$children[[1]]
header$children[[2]]$children[[1]] <-
                                  tags$a(href = "https://peak.mountsinai.org/",
                                        tags$img(src = "Sinai_logo_white.png",
                                              height = "100%", width = "30%"))

ui <- dashboardPage(
      header,
      dashboardSidebar(width = 300,
      # change the navigation bar color
      tags$head(tags$style(HTML("
                              .navbar {
                              background-color: #221F72 !important;
                              }"
                         ))),
      #change the logo bar color
      tags$head(tags$style(HTML("
                              .logo {
                              background-color: #221F72 !important;
                              }"
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
                         tags$div("MSHS Worked FTE Dashboard",
                                  style = "color: #221f72;
                                           font-weight:bold;
                                           font-size:34px;
                                           margin-left: 20px",
                                  h3("Health System Operations"),
                                  h4(paste0("Publish Date: ",
                                    format(as.Date(Sys.Date(),
                                    "%B %d %Y"), "%m/%d/%y"))),
                                  #h4(paste0("Reporting Period: ",
                                #report_start_date, " to ", report_end_date ))
                                  )),
                  column(12,
                         tags$div(id = "Objective",
                                  style = "color:	#221f72;
                                  margin-left: 20px",
                                   h3("Description:"),
                                   p(paste("This dashboard summarizes MSHS Worked FTE by pay period. The data is",
                                            "stratified by hospital and service line."),
                                     style = "font-size:16px"))),
                  column(12,
                         tags$div(id = "data description", style = "color: #221f72; font-size:14px; margin-left: 20px",
                                  h3("Data Notes"),
                                  h5("* Department does not currently have a Premier productivity report"),
                                  h5("1 FTE = 75 Worked Hours/Pay Period")))),
          
          ## tab MSHS --------------------------------------------
          tabItem(tabName = "mshs",
                  div("MSHS Worked FTE Dashboard", style = paste("color:	#221f72; font-family:Calibri; font-weight:bold;",
                                                                 "font-size:25px; margin-left: 20px")),
                  textOutput("mshs_date_show"),
                  textOutput("mshs_reporting_date"),
                  tags$head(tags$style(paste("#mshs_date_show{color:#7f7f7f; font-family:Calibri; font-style: italic;",
                                             "font-size: 18px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left:",
                                             "20px}"))), hr(),
                  tags$head(tags$style(paste("#mshs_reporting_date{color:#7f7f7f; font-family:Calibri; font-style: italic;",
                                             "font-size: 16px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left:",
                                             "20px}"))), hr(),
                  tags$head(tags$style(HTML(paste("#mshs_filters_update {background-color: #d80b8c;color: #FFFFFF;",
                                                  "font-size: 18px}")))),
                  fluidRow(
                  tags$style(HTML(".box.box-solid.box-primary>.box-header {background:#221f72; color:#fff}")),
                    column(11,
                           box(
                             title = NULL, width = 12, status = "primary",
                             solidHeader = TRUE, collapsible = TRUE, closable = TRUE, br(),
                             fluidRow(
                               box(width = 4, height = "100px", title = "Select Service Line Category:", solidHeader = FALSE,
                                   pickerInput("mshs_selected_group", label = NULL, multiple = FALSE,
                                               options = pickerOptions(actionsBox = TRUE), choices = group_choices,
                                               selected = "Nursing")),
                               box(width = 4, height = "100px", title = "Select Service Line:", solidHeader = FALSE,
                                   pickerInput("mshs_selected_service", label = NULL, multiple = FALSE,
                                               options = pickerOptions(
                                                 liveSearch = TRUE,
                                                 actionsBox = TRUE,
                                                 dropupAuto = FALSE),
                                               choices = service_choices, selected = service_choices[1])),
                               box(width = 4, height = "100px",
                                   title = "Select Pay Period End Date:",  solidHeader = FALSE,
                                   pickerInput("mshs_date_range", label = NULL, multiple = FALSE,
                                               options = pickerOptions(
                                                 actionsBox = TRUE,
                                                 dropupAuto = FALSE),
                                               choices = date_options, selected = date_options[1])),
                               column(5,
                                      actionButton("mshs_filters_update", "CLICK TO UPDATE", width = "75%"),
                                      br(),
                                      br())))),
                    column(11,
                           box(title = NULL, status = "primary",
                               solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                               plotlyOutput("mshs_plot"),  width = 12)),
                    column(11,
                           box(
                             title = NULL, width = 12, status = "primary",
                             solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                             DTOutput("mshs_table") %>%
                               withSpinner(type = 5, color = "#d80b8c"))))),
    
          ## Site Tab ------------------------
          tabItem(tabName = "site",
                  div("Hospitals Worked FTE Dashboard", style = paste("color:	#221f72; font-family:Calibri;",
                                                                      "font-weight:bold; font-size:25px; margin-left: 20px")),
                  textOutput("site_name_date_show"),
                  textOutput("site_reporting_date"),
                  tags$head(tags$style(paste("#site_name_date_show{color:#7f7f7f; font-family:Calibri; font-style: italic;",
                                             "font-size: 18px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left:",
                                             "20px}"))), hr(),
                  tags$head(tags$style(paste("#site_reporting_date{color:#7f7f7f; font-family:Calibri; font-style: italic;",
                                             "font-size: 16px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left:",
                                             "20px}"))), hr(),
                  tags$head(tags$style(HTML("#filters_update {background-color: #d80b8c;color: #FFFFFF; font-size: 18px}"))),
                  fluidRow(
                  column(11,
                         box(
                           title = NULL, width = 12, status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, closable = TRUE, br(),
                           fluidRow(
                             box(width = 3, height = "100px", title = "Select Hospital:", solidHeader = FALSE,
                                 pickerInput("selected_payroll", label = NULL, multiple = TRUE,
                                             options = pickerOptions(actionsBox = TRUE), choices = payroll_choices,
                                             selected = "MSH")),
                             box(width = 3, height = "100px", title = "Select Service Line Category:", solidHeader = FALSE,
                                 pickerInput("selected_group", label = NULL, multiple = FALSE,
                                             options = pickerOptions(actionsBox = TRUE), choices = group_choices,
                                             selected = "Nursing")),
                             box(width = 3, height = "100px", title = "Select Service Line:", solidHeader = FALSE,
                                 pickerInput("selected_service", label = NULL, multiple = TRUE,
                                             options = pickerOptions(
                                               liveSearch = TRUE,
                                               actionsBox = TRUE),
                                             choices = service_choices,  selected = service_choices[1])),
                             box(width = 3, height = "100px",
                                 title = "Select Pay Period End Date:",  solidHeader = FALSE,
                                 pickerInput("date_range", label = NULL, multiple = FALSE,
                                             options = pickerOptions(
                                               actionsBox = TRUE,
                                               dropupAuto = FALSE),
                                               choices = date_options,  selected = date_options[1])),
                             column(5,
                                    actionButton("filters_update", "CLICK TO UPDATE", width = "57%"),
                                    br(),
                                    br())))),
              column(11,
                     box(title = NULL, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         plotlyOutput("site_plot"),  width = 12)),
              column(11,
                     box(
                       title = NULL, width = 12, status = "primary",
                       solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                       DTOutput("site_table") %>%
                         withSpinner(type = 5, color = "#d80b8c"))))),
          ## Department Tab -------------------------
          tabItem(tabName = "department",
                  div("Department Worked FTE Dashboard", style = paste("color:	#221f72; font-family:Calibri;",
                                                                       "font-weight:bold; font-size:25px; margin-left:",
                                                                       "20px")),
                  textOutput("department_date_show"),
                  textOutput("department_reporting_date"),
                  tags$head(tags$style(paste("#department_date_show{color:#7f7f7f; font-family:Calibri; font-style: italic;",
                                             "font-size: 18px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left:",
                                             "20px}"))), hr(),
                  tags$head(tags$style(paste("#department_reporting_date{color:#7f7f7f; font-family:Calibri; font-style:",
                                             "italic; font-size: 16px; margin-top: -0.2em; margin-bottom: 0.5em;",
                                             "margin-left: 20px}"))), hr(),
                  tags$head(tags$style(HTML(paste("#dep_filters_update {background-color: #d80b8c;color: #FFFFFF; font-size:",
                                                  "18px}")))),
                  fluidRow(
                  column(11,
                         box(
                           title = NULL, width = 12, status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, closable = TRUE, br(),
                           fluidRow(
                             box(width = 3, height = "100px", title = "Select Hospital:", solidHeader = FALSE,
                                 pickerInput("dep_selected_payroll", label = NULL, multiple = FALSE,
                                             options = pickerOptions(actionsBox = TRUE), choices = payroll_choices,
                                             selected = "MSH")),
                             box(width = 3, height = "100px", title = "Select Service Line Category:", solidHeader = FALSE,
                                 pickerInput("dep_selected_group", label = NULL, multiple = FALSE,
                                             options = pickerOptions(actionsBox = TRUE), choices = group_choices,
                                             selected = "Nursing")),
                             box(width = 3, height = "100px", title = "Select Service Line:", solidHeader = FALSE,
                                 pickerInput("dep_selected_service", label = NULL, multiple = TRUE,
                                             options = pickerOptions(
                                               liveSearch = TRUE,
                                               actionsBox = TRUE,
                                               dropupAuto = FALSE),
                                             choices = service_choices,  selected = service_choices[1])),
                             box(width = 3, height = "100px",
                                 title = "Select Pay Period End Date:",  solidHeader = FALSE,
                                 pickerInput("dep_date_range", label = NULL, multiple = FALSE,
                                             options = pickerOptions(
                                               actionsBox = TRUE,
                                               dropupAuto = FALSE),
                                               choices = date_options, selected = date_options[1])),
                             column(5,
                                    actionButton("dep_filters_update", "CLICK TO UPDATE", width = "57%"),
                                    br(),
                                    br())))),
                    column(11,
                           box(title = NULL, status = "primary",
                               solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                               plotlyOutput("department_plot"), width = 12)),
                    column(11,
                           box(
                             title = NULL, width = 12, status = "primary",
                             solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                             DTOutput("department_table") %>%
                               withSpinner(type = 5, color = "#d80b8c"))))))))
