

server <- function(input, output, session) {
  
  output$mshs_DateShow <- renderText({
    paste0("Based on data from ", input$mshs_DateRange[length(input$mshs_DateRange)], " to ", input$mshs_DateRange[1], 
           " for MSHS")
  })
  
  output$siteName_DateShow <- renderText({
    paste0("Based on data from ", input$DateRange[1]," to ", input$DateRange[2],
           " for ", paste(sort(input$selectedPayroll), collapse = ', ')) 
           
  })
  
  output$Department_DateShow <- renderText({
    paste0("Based on data from ", input$dep_DateRange[1]," to ", input$dep_DateRange[2],
           " for ", paste(sort(input$dep_selectedPayroll), collapse = ', ')) 
    
  })
  
  ## eventReactive for MSHS ------------------------------
  
  Data_MSHS  <- eventReactive(input$mshs_FiltersUpdate, {
    validate(need(input$mshs_selectedGroup != "", "Please Select a Group"),
             need(input$mshs_DateRange[1] > input$mshs_DateRange[2], "Error: Start date should be earlier than end date."))
    data %>% 
      filter( service_group %in% input$mshs_selectedGroup,
              CORPORATE.SERVICE.LINE %in% input$mshs_selectedService,
              dates %in% input$mshs_DateRange)
  }, ignoreNULL = FALSE)
  
  ## eventReactive for sites ------------------------------
  
  Data_Service  <- eventReactive(input$FiltersUpdate, {
    validate(need(input$selectedPayroll != "" , "Please Select a Campus"), 
             need(input$selectedGroup != "", "Please Select a Group"),
             need(input$DateRange[1] < input$DateRange[2], "Error: Start date should be earlier than end date."))
    data %>% 
      filter( PAYROLL %in% input$selectedPayroll,
              service_group %in% input$selectedGroup,
              CORPORATE.SERVICE.LINE %in% input$selectedService,
              PP.END.DATE >= as.Date(input$DateRange[1]) &PP.END.DATE  <= as.Date(input$DateRange[2] ))
  }, ignoreNULL = FALSE)
  
  
  ## eventReactive for sites ------------------------------
  
  Department_Data  <- eventReactive(input$dep_FiltersUpdate, {
    validate(need(input$dep_selectedPayroll != "" , "Please Select a Campus"), 
             need(input$dep_selectedGroup != "", "Please Select a Group"),
             need(input$dep_DateRange[1] < input$dep_DateRange[2], "Error: Start date should be earlier than end date."))
    data %>% 
      filter( PAYROLL %in% input$dep_selectedPayroll,
              service_group %in% input$dep_selectedGroup,
              CORPORATE.SERVICE.LINE %in% input$dep_selectedService,
              PP.END.DATE >= as.Date(input$dep_DateRange[1]) &PP.END.DATE  <= as.Date(input$dep_DateRange[2] ))
  }, ignoreNULL = FALSE)
  
  
  # Observeevent for MSHS ----------------------------------------------
  
  # Observeevent for services
  observeEvent(input$mshs_selectedGroup,{
    service_choices <- sort(unique(data$CORPORATE.SERVICE.LINE[ 
        data$service_group %in% input$mshs_selectedGroup]))
    
    updatePickerInput(session,
                      inputId = "mshs_selectedService",
                      choices = service_choices,
                      selected = service_choices[1])
  },
  ignoreInit = TRUE,
  ignoreNULL = FALSE)

  # Observeevent for Site ----------------------------------------------
  
  # Observeevent for services category
  observeEvent(input$selectedPayroll,{
      group_choices <- sort(unique(data$service_group[
                            data$PAYROLL %in% input$selectedPayroll]))
      updatePickerInput(session,
                        inputId = "selectedGroup",
                        choices = group_choices,
                        selected = group_choices[1])
  },
  ignoreInit = TRUE,
  ignoreNULL = FALSE)

  # Observeevent for services
  observeEvent(input$selectedGroup,{
    service_choices <- sort(unique(data$CORPORATE.SERVICE.LINE[ 
                            data$PAYROLL %in% input$selectedPayroll &
                              data$service_group %in% input$selectedGroup]))
    
    updatePickerInput(session,
                      inputId = "selectedService",
                      choices = service_choices,
                      selected = service_choices[1])
  },
  ignoreInit = TRUE,
  ignoreNULL = FALSE)
  
  # Observeevent for Department ----------------------------------------------
  
  # Observeevent for services category
  observeEvent(input$dep_selectedPayroll,{
    group_choices <- sort(unique(data$dep_service_group[
      data$PAYROLL %in% input$dep_selectedPayroll]))
    updatePickerInput(session,
                      inputId = "dep_selectedGroup",
                      choices = group_choices,
                      selected = group_choices[1])
  },
  ignoreInit = TRUE,
  ignoreNULL = FALSE)
  
  
  # Observeevent for services
  observeEvent(input$dep_selectedGroup,{
    service_choices <- sort(unique(data$CORPORATE.SERVICE.LINE[ 
      data$PAYROLL %in% input$dep_selectedPayroll &
        data$service_group %in% input$dep_selectedGroup]))
    
    updatePickerInput(session,
                      inputId = "dep_selectedService",
                      choices = service_choices,
                      selected = service_choices[1])
  },
  ignoreInit = TRUE,
  ignoreNULL = FALSE)
  
  
  ## MSHS Tab ---------------------------------------------------
  
  output$mshs_plot <- renderPlotly({
    
    kdata <- Data_MSHS() 

    kdata <- kdata %>% 
      group_by(dates) %>%
      summarise(FTE = sum(FTE, na.rm = T)) %>%
      mutate(PAYROLL = "MSHS") %>% 
      select(PAYROLL, dates, FTE)
    
    kdata[is.na(kdata)] <- 0
    kdata <- kdata %>% 
      ungroup() %>% 
      arrange(desc(colnames(kdata)[ncol(kdata)])) %>%
      rename(Site= PAYROLL)
    
    
    kdata[3:length(kdata)] <- round(kdata[3:length(kdata)] , digits_round) 
    
    ggplotly(
      ggplot(data = kdata, aes(x = dates, y = FTE, group = Site, color = Site))+
        geom_line(size=1.5)+
        geom_point(size=2.75)+
        ggtitle(label = 'placeholder')+
        xlab("Pay Period")+
        ylab("FTE (Full Time Equivalent)")+
        scale_color_manual(values = MountSinai_pal("main")(length(kdata$Site)))+
        scale_y_continuous(limits = c(0, max(kdata$FTE)*1.2))+
        theme(plot.title = element_text(hjust = 0.5, size = 20),
              axis.title = element_text(face ="bold"),
              axis.text.x = element_text(angle = 45),
              legend.text = element_text(size = 6))) %>%
      layout(title = list(text = paste0('MSHS ', isolate(input$mshs_selectedService),
                                        '<br>',
                                        '<sup>',
                                        "Worked FTE's By Pay Period",
                                        '</sup>')),
             margin = list(l = 75, t = 75))
    
  }) 
  
  
  output$mshs_table <- renderDT({
    
    kdata <- Data_MSHS() 
    
    # Estimate Reporting Year Avg
    avg <- kdata %>%
      filter(year(PP.END.DATE) == max(year(data$PP.END.DATE)))%>%
      group_by(dates) %>%
      summarise(FTE = sum(FTE, na.rm = T)) %>%
      summarise(FTE = mean(FTE, na.rm = T))%>%
      rename(`Reporting Year Avg.`= FTE)%>%
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
    
    
    kdata$`Reporting Period Avg.` <- apply(kdata[,(ncol(kdata)-2):ncol(kdata)], 1,mean)
    kdata <- left_join(kdata , avg, by= "Site")

    kdata[2:length(kdata)] <- round(kdata[2:length(kdata)] , digits_round)
    
    
    kdata <-  datatable(kdata, 
                        class = 'cell-border stripe',
                        rownames = FALSE,
                        options = list(
                          columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>%
      formatStyle(columns = c("Site", "Reporting Period Avg.", "Reporting Year Avg." ), fontWeight = 'bold')
  
  })
  
  
  ## Site tab -----------------------------------------------
 
  output$site_plot <- renderPlotly({
  
    kdata <- Data_Service() 
    kdata <- kdata %>% 
      group_by(PAYROLL, dates) %>%
      summarise(FTE = sum(FTE, na.rm = T)) 

    kdata[is.na(kdata)] <- 0
    kdata <- kdata %>% 
      ungroup() %>% 
      arrange(desc(colnames(kdata)[ncol(kdata)])) 
    
    kdata[3:length(kdata)] <- round(kdata[3:length(kdata)] , digits_round) 
    kdata <- kdata %>%
      rename(Site = PAYROLL)
    
    ggplotly(
      ggplot(data = kdata, aes(x = dates, y = FTE, group = Site, color = Site))+
        geom_line(size = 1.5)+
        geom_point(size = 2.75)+
        ggtitle(label = 'placeholder')+
        xlab("Pay Period")+
        ylab("FTE (Full Time Equivalent)")+
        scale_color_manual(values = MountSinai_pal("main")(length(kdata$Site)))+
        scale_y_continuous(limits = c(0, max(kdata$FTE)*1.2))+
        theme(plot.title = element_text(hjust = 0.5, size = 20),
              axis.title = element_text(face = "bold"),
              axis.text.x = element_text(angle = 45),
              legend.text = element_text(size = 6))) %>%
      layout(title = list(text = 
                            paste0(paste0(c(isolate(input$selectedPayroll),
                                            if(sum(nchar(input$selectedService)) > 40){
                                              paste0('Multiple ',
                                                     isolate(input$selectedGroup),
                                                     ' Departments')
                                                 }else{
                                                   isolate(input$selectedService)}),
                                               collapse = ", "),
                                   '<br>',
                                   '<sup>',
                                   "Worked FTE's By Pay Period",
                                   '</sup>')),
             margin = list(l = 75, t = 75))
  
  }) 
  
  
  output$site_table <- renderDT({
    
    kdata <- Data_Service() 

    avg <- kdata %>%
      filter(year(PP.END.DATE) == max(year(data$PP.END.DATE)))%>%
      group_by(PAYROLL, dates) %>%
      summarise(FTE = sum(FTE, na.rm = T)) %>%
      group_by(PAYROLL)%>%
      summarise(FTE = mean(FTE, na.rm = T))%>%
      rename(`Reporting Year Avg.`= FTE)
  
    
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
    
    
    kdata$`Reporting Period Avg.` <- apply(kdata[,(ncol(kdata)-2):ncol(kdata)], 1,mean)
    kdata <- left_join(kdata, avg, by = "PAYROLL")
    kdata[2:length(kdata)] <- round(kdata[2:length(kdata)] , digits_round) 
    
    kdata <- kdata %>%
      rename(Site= PAYROLL)
    
    
    kdata <-  datatable(kdata, 
                        class = 'cell-border stripe',
                        rownames = FALSE,
                        options = list(
                          columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>%
      formatStyle(columns = c("Site", "Reporting Period Avg.", "Reporting Year Avg."), fontWeight = 'bold')
 
  })
  
  ## department tab -----------------------------------------------
  
  output$department_plot <- renderPlotly({
    
    data_service <- Department_Data() 

    data_service <-  data_service %>% 
      pivot_wider(id_cols = c("DEFINITION.CODE","DEFINITION.NAME","DEPARTMENT"),
                  names_from = "dates",
                  values_from = FTE) #pivot dataframe to bring in NAs for missing PP

    data_service <- data_service  %>% 
      pivot_longer(cols = 4:ncol(data_service),
                   names_to = "dates") %>% 
      mutate(FTE = case_when(
        is.na(value) ~ 0, #if FTE is NA -> 0
        !is.na(value) ~ value), #else leave the value
        FTE = round(value,digits_round)) #turn dates into factor
  
    ggplotly(
      ggplot(data = data_service,
             aes(x = dates , y = FTE, group = DEPARTMENT, color = DEPARTMENT))+
        geom_line(size = 1.5)+
        geom_point(size = 2.75)+
        ggtitle('placeholder')+
        xlab("Pay Period")+
        ylab("FTE (Full Time Equivalent)")+
        scale_color_manual(values = MountSinai_pal("main")(length(data_service$DEPARTMENT)))+
        scale_y_continuous(limits = c(0, max(data_service$FTE)*1.2))+
        theme(plot.title= element_text(hjust = 0.5, size = 20),
              axis.title = element_text(face ="bold"),
              axis.text.x = element_text(angle = 45),
              legend.text = element_text(size = 6))) %>%
      layout(title = list(text = 
                            paste0(paste0(c(isolate(input$dep_selectedPayroll),
                                            if(sum(nchar(input$dep_selectedService)) > 38){
                                              paste0('Multiple ',
                                                     isolate(input$dep_selectedGroup),
                                                     ' Departments')
                                            }else{
                                              isolate(input$dep_selectedService)}),
                                          collapse = ", "),
                                   '<br>',
                                   '<sup>',
                                   "Worked FTE's By Pay Period",
                                   '</sup>')),
             margin = list(l = 75, t = 75))
    
  }) 
  
  
  output$department_table <- renderDT({
    
    kdata <- Department_Data() 

    avg <- kdata %>%
      filter(year(PP.END.DATE) == max(year(data$PP.END.DATE)))%>%
      group_by(DEPARTMENT, dates) %>%
      summarise(FTE = sum(FTE, na.rm = T)) %>%
      group_by(DEPARTMENT)%>%
      summarise(FTE = mean(FTE, na.rm = T))%>%
      rename(`Reporting Year Avg.`= FTE)
    
    
    kdata <- kdata %>% 
      pivot_wider(id_cols = DEPARTMENT,
                  names_from = dates,
                  values_from = FTE)

    kdata[is.na(kdata)] <- 0
    kdata <- kdata %>% 
      ungroup() %>% 
      arrange(desc(colnames(kdata)[ncol(kdata)])) 
 
    kdata$`Reporting Period Avg.` <- apply(kdata[,(ncol(kdata)-2):ncol(kdata)], 1,mean)
    kdata <- left_join(kdata, avg , by = "DEPARTMENT")
    kdata[2:length(kdata)] <- round(kdata[2:length(kdata)] , digits_round) 
    
    kdata <-  datatable(kdata, 
                        class = 'cell-border stripe',
                        rownames = FALSE,
                        options = list(
                          columnDefs = list(list(className = 'dt-center',
                                                 targets = "_all")))) %>%
      formatStyle(columns = c("DEPARTMENT",
                              "Reporting Period Avg.",
                              "Reporting Year Avg."),
                  fontWeight = 'bold')
    
  })
    
  
}

shinyApp(ui,server) 
 