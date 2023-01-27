

server <- function(input, output, session) {
  
  ## Text output for MSHS -------------------------------
  mshs_text <- eventReactive(input$mshs_filters_update, {
    end_date <- isolate(input$mshs_date_range)
    dates_index <- sapply(end_date,  function(x) grep(x, date_options) + 10)
    start_date_input <- as.Date(date_options[dates_index], format = "%m/%d/%y") + 1
    start_date_input <- format(as.Date(start_date_input, "%B %d %Y"), "%m/%d/%y")
    paste0("Based on data from ", start_date_input, " to ", end_date, " for MSHS")
    }, ignoreNULL = FALSE)
  
  output$mshs_date_show  <- renderText({
    mshs_text()
    })
  
  mshs_reporting <- eventReactive(input$mshs_filters_update, {
    end_date <- isolate(input$mshs_date_range)
    reporting_index <- sapply(end_date,  function(x) grep(x, date_options) + 3)
    reporting_start_date <- as.Date(date_options[reporting_index], format = "%m/%d/%y") + 1
    reporting_start_date <- format(as.Date(reporting_start_date, "%B %d %Y"), "%m/%d/%y")
    paste0("Reporting period: ", reporting_start_date, " to ", end_date)
    }, ignoreNULL = FALSE)
  
  output$mshs_reporting_date <- renderText({
    mshs_reporting()
    })
  
  ## text output for Site ---------------------------------------------
  site_text <- eventReactive(input$filters_update, {
    end_date <- isolate(input$date_range)
    dates_index <- sapply(end_date,  function(x) grep(x, date_options) + 10)
    start_date_input <- as.Date(date_options[dates_index], format = "%m/%d/%y") + 1
    start_date_input <- format(as.Date(start_date_input, "%B %d %Y"), "%m/%d/%y")
    paste0("Based on data from ", min(start_date_input), " to ", max(end_date),
           " for ", paste(sort(input$selected_payroll), collapse = ", "))
    }, ignoreNULL = FALSE)
  
  output$site_name_date_show <- renderText({
    site_text()
    })
  
  site_reporting <- eventReactive(input$filters_update, {
    end_date <- isolate(input$date_range)
    reporting_index <- sapply(end_date, function(x) grep(x, date_options) + 3)
    reporting_start_date <- as.Date(date_options[reporting_index], format = "%m/%d/%y") + 1
    reporting_start_date <- format(as.Date(reporting_start_date, "%B %d %Y"), "%m/%d/%y")
    paste0("Reporting period: ", min(reporting_start_date), " to ", max(end_date))
    }, ignoreNULL = FALSE)
  
  output$site_reporting_date <- renderText({
    site_reporting()
    })
  
  ## Text output for department -------------------------------------
  dep_text <- eventReactive(input$dep_filters_update, {
    end_date <- isolate(input$dep_date_range)
    dates_index <- sapply(end_date,  function(x) grep(x, date_options) + 10)
    start_date_input <- as.Date(date_options[dates_index], format = "%m/%d/%y") + 1
    start_date_input <- format(as.Date(start_date_input, "%B %d %Y"), "%m/%d/%y")
    paste0("Based on data from ", min(start_date_input), " to ", max(end_date),
           " for ", paste(sort(input$dep_selected_payroll), collapse = ", "))
    }, ignoreNULL = FALSE)
  
  output$department_date_show <- renderText({
    dep_text()
    })
  
  dep_reporting <- eventReactive(input$dep_filters_update, {
    end_date <- isolate(input$dep_date_range)
    reporting_index <- sapply(end_date,  function(x) grep(x, date_options) + 3)
    reporting_start_date <- as.Date(date_options[reporting_index], format = "%m/%d/%y") + 1
    reporting_start_date <- format(as.Date(reporting_start_date, "%B %d %Y"), "%m/%d/%y")
    paste0("Reporting period: ", min(reporting_start_date), " to ", max(end_date))
    }, ignoreNULL = FALSE)
  
  output$department_reporting_date <- renderText({
    dep_reporting()
    })
  
  ## eventReactive for MSHS ------------------------------
  data_mshs  <- eventReactive(input$mshs_filters_update, {
    validate(need(input$mshs_selected_group != "", "Please Select a Group"),
            need(input$mshs_date_range != "", "Please Select a Date"))
    
 dates_index <- sapply(input$mshs_date_range, function(x) grep(x, date_options) + 9)
    start_date_input <- as.Date(date_options[dates_index], format = "%m/%d/%y")
    start_date_input <- format(as.Date(start_date_input, "%B %d %Y"), "%m/%d/%y")

    data %>%
      filter(service_group %in% input$mshs_selected_group,
             CORPORATE.SERVICE.LINE %in% input$mshs_selected_service,
             PP.END.DATE >= as.Date(min(start_date_input), format = "%m/%d/%y") &
               PP.END.DATE <= as.Date(max(input$mshs_date_range), format = "%m/%d/%y"))
    }, ignoreNULL = FALSE)
  
  ## eventReactive for sites ------------------------------
  data_service  <- eventReactive(input$filters_update, {
    validate(need(input$selected_payroll != "", "Please Select a Campus"),
             need(input$selected_group != "", "Please Select a Group"),
             need(input$date_range != "", "Please Select a Date"))
    
  dates_index <- sapply(input$date_range, function(x) grep(x, date_options) + 9)
  start_date_input <- as.Date(date_options[dates_index], format = "%m/%d/%y")
  start_date_input <- format(as.Date(start_date_input, "%B %d %Y"), "%m/%d/%y")
  
    data %>%
      filter(PAYROLL %in% input$selected_payroll,
             service_group %in% input$selected_group,
             CORPORATE.SERVICE.LINE %in% input$selected_service,
             PP.END.DATE >= as.Date(min(start_date_input), format = "%m/%d/%y") &
               PP.END.DATE <= as.Date(max(input$date_range), format = "%m/%d/%y"))
    }, ignoreNULL = FALSE)
  
  ## eventReactive for department ------------------------------
  department_date  <- eventReactive(input$dep_filters_update, {
    validate(need(input$dep_selected_payroll != "", "Please Select a Campus"),
             need(input$dep_selected_group != "", "Please Select a Group"),
             need(input$dep_date_range != "", "Please Select a Date"))
    
    dates_index <- sapply(input$dep_date_range, function(x) grep(x, date_options) + 9)
    start_date_input <- as.Date(date_options[dates_index], format = "%m/%d/%y")
    start_date_input <- format(as.Date(start_date_input, "%B %d %Y"), "%m/%d/%y")
   
    data %>%
      filter(PAYROLL %in% input$dep_selected_payroll,
             service_group %in% input$dep_selected_group,
             CORPORATE.SERVICE.LINE %in% input$dep_selected_service,
             PP.END.DATE >= as.Date(min(start_date_input), format = "%m/%d/%y") &
               PP.END.DATE <= as.Date(max(input$dep_date_range), format = "%m/%d/%y"))
    }, ignoreNULL = FALSE)
  
  # Observeevent for MSHS ----------------------------------------------
  ## Observeevent for services
  observeEvent(input$mshs_selected_group, {
    service_choices <- sort(unique(data$CORPORATE.SERVICE.LINE[
      data$service_group %in% input$mshs_selected_group]))
    updatePickerInput(session,
                      inputId = "mshs_selected_service",
                      choices = service_choices,
                      selected = service_choices[1])
    },
    ignoreInit = TRUE,
    ignoreNULL = FALSE)

  # Observeevent for Site ----------------------------------------------
  ## Observeevent for services category
  observeEvent(input$selected_payroll, {
      group_choices <- sort(unique(data$selected_group[
                            data$PAYROLL %in% input$selected_payroll]))
      updatePickerInput(session,
                        inputId = "selected_group",
                        choices = group_choices,
                        selected = group_choices[1])
      },
      ignoreInit = TRUE,
      ignoreNULL = FALSE)

  ## Observeevent for services
  observeEvent(input$selected_group, {
    service_choices <- sort(unique(data$CORPORATE.SERVICE.LINE[
      data$PAYROLL %in% input$selected_payroll &
                              data$service_group %in% input$selected_group]))
    updatePickerInput(session,
                      inputId = "selected_service",
                      choices = service_choices,
                      selected = service_choices[1])
    },
    ignoreInit = TRUE,
    ignoreNULL = FALSE)
  
  # Observeevent for Department ----------------------------------------------
  ## Observeevent for services category
  observeEvent(input$dep_selected_payroll, {
    group_choices <- sort(unique(data$dep_selected_group[
      data$PAYROLL %in% input$dep_selected_payroll]))
    updatePickerInput(session,
                      inputId = "dep_selected_group",
                      choices = group_choices,
                      selected = group_choices[1])
    },
    ignoreInit = TRUE,
    ignoreNULL = FALSE)
  
  ## Observeevent for services
  observeEvent(input$dep_selected_group, {
    service_choices <- sort(unique(data$CORPORATE.SERVICE.LINE[
      data$PAYROLL %in% input$dep_selected_payroll &
        data$service_group %in% input$dep_selected_group]))
    updatePickerInput(session,
                      inputId = "dep_selected_service",
                      choices = service_choices,
                      selected = service_choices[1])
    },
    ignoreInit = TRUE,
    ignoreNULL = FALSE)
  
  ## MSHS Tab ---------------------------------------------------
  output$mshs_plot <- renderPlotly({
    kdata <- data_mshs()
    kdata <- kdata %>%
      group_by(dates) %>%
      summarise(FTE = sum(FTE, na.rm = T)) %>%
      mutate(PAYROLL = "MSHS") %>%
      select(PAYROLL, dates, FTE)
    kdata[is.na(kdata)] <- 0
    kdata <- kdata %>%
      ungroup() %>%
      arrange(desc(colnames(kdata)[ncol(kdata)])) %>%
      rename(Site = PAYROLL)
    kdata[3:length(kdata)] <- round(kdata[3:length(kdata)], digits_round)
    
    ggplotly(
      ggplot(data = kdata,
             aes(x = dates, y = FTE, group = Site, color = Site,
                 text = paste0("MSHS", "\n", "DATE: ", dates, "\n",
                               "FTEs: ", FTE))) +
        geom_line(size = 1.25) +
        geom_point(size = 2.6) +
        ggtitle(label = "placeholder") +
        xlab("Pay Period") +
        ylab("FTE (Full Time Equivalent)") +
        scale_color_manual(values = mount_sinai_pal("main")(length(kdata$Site))) +
        scale_y_continuous(limits = c(0, max(kdata$FTE) * 1.2)) +
        theme(plot.title = element_text(hjust = 0.5, size = 20),
              axis.title = element_text(face = "bold"),
              legend.text = element_text(size = 6)),
      tooltip = "text") %>%
      layout(title = list(text =
                            isolate(
                              paste0(
                                paste0(c("MSHS",
                                         if (sum(nchar(input$mshs_selected_service)) > 40) {
                                           paste0("Multiple ",
                                                  input$mshs_selected_group,
                                                  " Departments")
                                           } else {input$mshs_selected_service}),
                                       collapse = ", "),
                                "<br>",
                                "<sup>",
                                "Worked FTE's By Pay Period",
                                "</sup>"))),
             margin = list(l = 75, t = 75))
  })
  
  output$mshs_table <- renderDT({
    kdata <- data_mshs()
    # Calculate FYTD Avg
    avg <- kdata %>%
      filter(year(PP.END.DATE) == max(year(data$PP.END.DATE))) %>%
      group_by(dates) %>%
      summarise(FTE = sum(FTE, na.rm = T)) %>%
      summarise(FTE = as.numeric(format(round(mean(FTE, na.rm = T),
                                              digits = digits_round),
                                        nsmall = 2))) %>%
      rename(`FYTD Avg.` = FTE) %>%
      mutate(Site = "MSHS")
    kdata <- kdata %>%
      group_by(dates) %>%
      summarise(FTE = sum(FTE, na.rm = T)) %>%
      mutate(Site = "MSHS") %>%
      pivot_wider(id_cols = Site,
                  names_from = dates,
                  values_from = FTE)
    kdata[is.na(kdata)] <- 0
    kdata <- kdata %>%
      ungroup() %>%
      arrange(desc(colnames(kdata)[ncol(kdata)]))
    kdata$`Reporting Period Avg.` <- apply(kdata[, (ncol(kdata) - 2):ncol(kdata)], 1, mean)
    kdata <- left_join(kdata, avg, by = "Site")
    kdata[2:length(kdata)] <- round(kdata[2:length(kdata)], digits_round)
    kdata <-  datatable(kdata,
                        class = "cell-border stripe",
                        rownames = FALSE,
                        options = list(
                          columnDefs = list(list(className = "dt-center", targets = "_all")))) %>%
      formatStyle(columns = c("Site", "Reporting Period Avg.", "FYTD Avg."), fontWeight = "bold")
  })
  
  ## Site tab -----------------------------------------------
  output$site_plot <- renderPlotly({
    kdata <- data_service()
    test <<- kdata
    kdata <- kdata %>%
      group_by(PAYROLL, dates) %>%
      summarise(FTE = sum(FTE, na.rm = T))
    kdata[is.na(kdata)] <- 0
    kdata <- kdata %>%
      ungroup() %>%
      arrange(desc(colnames(kdata)[ncol(kdata)]))
    kdata[3:length(kdata)] <- round(kdata[3:length(kdata)], digits_round)
    kdata <- kdata %>%
      rename(Site = PAYROLL)
  
    ggplotly(
      ggplot(data = kdata,
             aes(x = dates, y = FTE, group = Site, color = Site,
                 text = paste0("HOSPITAL: ", Site, "\n", "DATE: ", dates,
                               "\n", "FTEs: ", FTE))) +
        geom_line(size = 1.25) +
        geom_point(size = 2.6) +
        ggtitle(label = "placeholder") +
        xlab("Pay Period") +
        ylab("FTE (Full Time Equivalent)") +
        scale_color_manual(values = mount_sinai_pal("main")(length(kdata$Site))) +
        scale_y_continuous(limits = c(0, max(kdata$FTE) * 1.2)) +
        theme(plot.title = element_text(hjust = 0.5, size = 20),
              axis.title = element_text(face = "bold"),
              legend.text = element_text(size = 6)),
      tooltip = "text") %>%
      layout(title = list(text =
                            isolate(
                              paste0(
                                paste0(c(input$selected_payroll,
                                         if (sum(nchar(input$selected_service)) > 40) {
                                           paste0("Multiple ",
                                                  input$selected_group,
                                                  " Departments")
                                           } else {input$selected_service}),
                                       collapse = ", "),
                                "<br>",
                                "<sup>",
                                "Worked FTE's By Pay Period",
                                "</sup>"))),
             margin = list(l = 75, t = 75))
  })
  
  output$site_table <- renderDT({
    kdata <- data_service()
    avg <- kdata %>%
      filter(year(PP.END.DATE) == max(year(data$PP.END.DATE))) %>%
      group_by(PAYROLL, dates) %>%
      summarise(FTE = sum(FTE, na.rm = T)) %>%
      group_by(PAYROLL) %>%
      summarise(FTE = as.numeric(format(round(mean(FTE, na.rm = T),
                                              digits = digits_round),
                                        nsmall = 2))) %>%
      rename(`FYTD Avg.` = FTE)
    kdata <- kdata %>%
      group_by(PAYROLL, dates) %>%
      summarise(FTE = sum(FTE, na.rm = T)) %>%
      pivot_wider(id_cols = PAYROLL,
                  names_from = dates,
                  values_from = FTE)
    kdata[is.na(kdata)] <- 0
    kdata <- kdata %>%
      ungroup() %>%
      arrange(desc(colnames(kdata)[ncol(kdata)]))
    kdata$`Reporting Period Avg.` <- apply(kdata[, (ncol(kdata) - 2):ncol(kdata)], 1, mean)
    kdata <- left_join(kdata, avg, by = "PAYROLL")
    kdata[2:length(kdata)] <- round(kdata[2:length(kdata)], digits_round)
    kdata <- kdata %>%
      rename(Site = PAYROLL)
    kdata <-  datatable(kdata,
                        class = "cell-border stripe",
                        rownames = FALSE,
                        options = list(
                          columnDefs = list(list(className = "dt-center", targets = "_all")))) %>%
      formatStyle(columns = c("Site", "Reporting Period Avg.", "FYTD Avg."), fontWeight = "bold")
  })
  
  ## department tab -----------------------------------------------
  output$department_plot <- renderPlotly({
    data_service <- department_date()
    data_service <-  data_service %>%
      pivot_wider(id_cols = c("DEFINITION.CODE", "DEFINITION.NAME", "DEPARTMENT"),
                  names_from = "dates",
                  values_from = FTE) #pivot dataframe to bring in NAs for missing PP
    data_service <- data_service  %>%
      pivot_longer(cols = 4:ncol(data_service), names_to = "dates") %>%
      mutate(FTE = case_when(
        is.na(value) ~ 0, #if FTE is NA -> 0
        !is.na(value) ~ value), #else leave the value
        FTE = round(value, digits_round)) #turn dates into factor
    data_service$DEPARTMENT <- sapply(data_service$DEPARTMENT,
                                      function(x)
                                        string_separate_to_lines(x, max_length = 20))
    ggplotly(
      ggplot(data = data_service,
             aes(x = dates, y = FTE, group = DEPARTMENT, color = DEPARTMENT,
                 text = paste0("DEPARTMENT: ", DEPARTMENT, "\n", "DATE: ",
                               dates, "\n", "FTEs: ", FTE))) +
        geom_line(size = 1.25) +
        geom_point(size = 2.6) +
        ggtitle("placeholder") +
        xlab("Pay Period") +
        ylab("FTE (Full Time Equivalent)") +
        scale_color_manual(values = mount_sinai_pal("main")(length(data_service$DEPARTMENT))) +
        scale_y_continuous(limits = c(0, max(data_service$FTE) * 1.2)) +
        theme(plot.title = element_text(hjust = 0.5, size = 20),
              axis.title = element_text(face = "bold"),
              legend.text = element_text(size = 6)),
      tooltip = "text") %>%
      layout(title = list(text =
                            isolate(
                              paste0(
                                paste0(c(input$dep_selected_payroll,
                                         if (sum(nchar(input$dep_selected_service)) > 40) {
                                           paste0("Multiple ",
                                                  input$dep_selected_group,
                                                  " Departments")
                                           } else {input$dep_selected_service}),
                                       collapse = ", "),
                                "<br>",
                                "<sup>",
                                "Worked FTE's By Pay Period",
                                "</sup>"))),
             margin = list(l = 75, t = 75))
  })
  
  output$department_table <- renderDT({
    kdata <- department_date()
    avg <- kdata %>%
      filter(year(PP.END.DATE) == max(year(data$PP.END.DATE))) %>%
      group_by(DEPARTMENT, dates) %>%
      summarise(FTE = sum(FTE, na.rm = T)) %>%
      group_by(DEPARTMENT) %>%
      summarise(FTE = as.numeric(format(round(mean(FTE, na.rm = T),
                                              digits = digits_round),
                                        nsmall = 2))) %>%
      rename(`FYTD Avg.` = FTE)
    kdata <- kdata %>%
      pivot_wider(id_cols = DEPARTMENT,
                  names_from = dates,
                  values_from = FTE)
    kdata[is.na(kdata)] <- 0
    kdata <- kdata %>%
      ungroup() %>%
      arrange(desc(colnames(kdata)[ncol(kdata)]))
    kdata$`Reporting Period Avg.` <- apply(kdata[, (ncol(kdata) - 2):ncol(kdata)], 1, mean)
    kdata <- left_join(kdata, avg, by = "DEPARTMENT")
    kdata[2:length(kdata)] <- round(kdata[2:length(kdata)], digits_round)
    kdata <-  datatable(kdata,
                        class = "cell-border stripe",
                        rownames = FALSE,
                        options = list(
                          columnDefs = list(list(className = "dt-center", targets = "_all")))) %>%
      formatStyle(columns = c("DEPARTMENT", "Reporting Period Avg.", "FYTD Avg."), fontWeight = "bold")
  })
}