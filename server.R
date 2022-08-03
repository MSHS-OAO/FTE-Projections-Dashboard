

server <- function(input, output, session) {
  
  output$siteName_DateShow <- renderText({
    paste0("Based on data from ", input$DateRange[1]," to ", input$DateRange[2]) 
           
  })
  
  
  Data_Service  <- eventReactive(input$FiltersUpdate, {
    validate(need(input$selectedPayroll != "" , "Please Select a Campus"), 
             need(input$DateRange[1] < input$DateRange[2], "Error: Start date should be earlier than end date."))
    data %>% 
      filter( PAYROLL %in% input$selectedPayroll, CORPORATE.SERVICE.LINE %in% input$selectedService,
              PP.END.DATE >= as.Date(input$DateRange[1]) &PP.END.DATE  <= as.Date(input$DateRange[2] ))
  }, ignoreNULL = FALSE)
  
  
  
  # Observeevent for services
  observeEvent(input$selectedPayroll,{
      service_choices <- sort(unique(data$CORPORATE.SERVICE.LINE[data$PAYROLL %in% input$selectedPayroll]))
      updatePickerInput(session,
                        inputId = "selectedService",
                        choices = service_choices,
                        selected = service_choices)
  },
  ignoreInit = TRUE,
  ignoreNULL = FALSE)
  

  
  
  
  
  
  output$mshs_table <- renderDT({
    
    kdata <- Data_Service() 
    
    #kdata <- data %>% filter(PAYROLL== "MSH" & CORPORATE.SERVICE.LINE== "Nursing")
    
    
    kdata <- kdata %>% 
      group_by(DATES) %>%
      summarise(FTE = sum(FTE, na.rm = T)) %>%
      mutate(DEPARTMENT= "MSH") %>%
      #mutate(DEPARTMENT= input$selectedPayroll) %>%
      pivot_wider(id_cols = DEPARTMENT,
                  names_from = DATES,
                  values_from = FTE)
    
    
    kdata[is.na(kdata)] <- 0
    kdata <- kdata %>% 
      ungroup() %>% 
      arrange(desc(colnames(kdata)[ncol(kdata)])) 
    
    kdata$`Reporting Period Avg.` <- apply(kdata[,(ncol(kdata)-2):ncol(kdata)],1,mean)
    kdata[2:length(kdata)] <- round(kdata[2:length(kdata)] , digits_round) 
  
  
     kdata <-  datatable(kdata, 
                      class = 'cell-border stripe',
                      rownames = FALSE,
                      
                      options = list(
                        columnDefs = list(list(className = 'dt-center', targets = "_all")))
                     
                      ) %>%
                      formatStyle(columns = c("DEPARTMENT", "Reporting Period Avg."), fontWeight = 'bold')
  
                      
                      
  })
  
  
 
  output$mshs_plot <- renderPlot({
  
    kdata <- Data_Service() 
    
    #kdata <- data %>% filter(PAYROLL== "MSH" & CORPORATE.SERVICE.LINE== "Nursing")
    
    
    kdata <- kdata %>% 
      group_by(DATES) %>%
      summarise(FTE = sum(FTE, na.rm = T)) 
    
  
    
    kdata[is.na(kdata)] <- 0
    kdata <- kdata %>% 
      ungroup() %>% 
      arrange(desc(colnames(kdata)[ncol(kdata)])) 
    
    #kdata$`Reporting Period Avg.` <- apply(kdata[,(ncol(kdata)-2):ncol(kdata)],1,mean)
    kdata[2:length(kdata)] <- round(kdata[2:length(kdata)] , digits_round) 
    
  

      ggplot(data = kdata, aes(x=DATES,y=FTE))+
        geom_line(size=1.5)+
        geom_point(size=2.75)+
        ggtitle(paste(input$selectedPayroll, input$selectedService, "Worked FTE's By Pay Period"))+
        xlab("Pay Period")+
        ylab("FTE (Full Time Equivalent)")+
        scale_color_manual(values=MountSinai_pal("main")(length(unique(kdata$FTE))))+
        theme(plot.title=element_text(hjust = 0.5, size = 20),
              axis.title = element_text(face="bold"),
              legend.text=element_text(size = 6)) #create and style service line graph

    
    
  
  
  })  
    
  
}

shinyApp(ui,server) 
 