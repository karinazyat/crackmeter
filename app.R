#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# install.packages('shinyWidgets', dependencies = T)
#don't forget to comment out before publish!

.libPaths("C:/Users/kzyatitsky/R/win-library/4.1") #RUN FIRST!!
library(shiny)
library(tidyverse)
library(lubridate)
library(DT)
library(shinythemes)
library(shinyjs)
library(plotly)
library(zoo)
library(stringr)
library(daterangepicker)
library(shinyWidgets)
library(tidytable)
library(clock)
# library(runner)
# library(data.table)




#changes max upload size to 600 mb
options(shiny.maxRequestSize=600*1024^2)


# Define UI for application that draws a histogram
ui <- bootstrapPage(theme = shinytheme("sandstone"),
                    useShinyjs(), 
                    # App title ----                 
                    h1(id = "big-heading", "Crack the Crackmeter!"),
                    tags$style(HTML("#big-heading{color: sandstone;}")),
                    h5(em(id = "small-heading", "Enter crackmeter logger data to see plots and other cool visuals!")),
                    
                    navbarPage("Navbar!", id = "nav",
                               
                               
                               tabPanel("Uploading Files",
                                        
                                        # Sidebar layout with input and output definitions ----
                                        sidebarLayout(
                                          
                                          # Sidebar panel for inputs ----
                                          sidebarPanel(
                                            
                                            # Input: Select a file ----
                                            fileInput("file2", "Choose Calibration File",
                                                      accept = c("text/csv",
                                                                 "text/comma-separated-values,text/plain",
                                                                 ".csv", ".dat")),
                                            fileInput("file1", "Choose Data File",
                                                      multiple = TRUE,
                                                      accept = c("text/csv",
                                                                 "text/comma-separated-values,text/plain",
                                                                 ".csv", ".dat", '.xslx')),
                                            # Horizontal line ----
                                            tags$hr(),
                                            
                                            # conditional panel calibration -------------------------------------------
                                            
                                            
                                            # conditionalPanel(condition = "output.fileUploaded2 == true",
                                            #                  pickerInput("cMeter", label = "Select Column with Instrument Names"
                                            #                              )),
                                            
                                            # conditional panel -------------------------------------------------------
                                            
                                            
                                            conditionalPanel(condition = "output.fileUploaded == true",
                                                             daterangepicker("dRange", label = "Select Date Range",
                                                                             options = daterangepickerOptions(
                                                                               showDropdowns = T,
                                                                               showCustomRangeLabel = T,
                                                                               timePicker = T,
                                                                               timePickerIncrement = c(0,10,15,20,30,40,45, 50, 60),
                                                                               timePicker24Hour = T
                                                                             ))),
                                            actionButton("action", "Reset!")
                                            
                                          ), # sidebar panel
                                          
                                          
                                          # Main panel for displaying outputs ----
                                          mainPanel(
                                            
                                            # Output: Data file ----
                                            DT:: DTOutput("contents")
                                            
                                          ) #main panel
                                        ) #sidebar layout
                               ), # tab panel
                               
                               tabPanel("Visuals",
                                        
                                        # Main panel for displaying outputs ----
                                        mainPanel(
                                          
                                          # Output: Plot ----
                                          plotlyOutput("plot")
                                          
                                        ) #main panel
                                        
                               ) # tab panel
                    ) # end navbar
) #end UI

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  # calibration file input --------------------------------------------------
  
  getData2 <- reactive({
    # validate(
    #   need(input$file2 != "", "Please upload a calibration values file")
    # ) #not sure if i need this
    inFile2 <- input$file2
    if (is.null(inFile2)){
      return(NULL)
    }else {
      read.table(inFile2$datapath, header = T, sep = ',', stringsAsFactors = F)#if doesn't work change to input$file2
    }    
  })
  
  observe(print(str(getData2())))
  
  output$fileUploaded2 <- reactive({ 
    return(!is.null(getData2()))
  })
  
  outputOptions(output, 'fileUploaded2', suspendWhenHidden = F)
  
  
  # get Data input and merge files ------------------------------------------
  
  
  getData <- reactive({
    # validate(
    #   need(input$file1 != "", "Please upload a calibration file, then data file(s)")
    # )
    inFile <- input$file1
    if (is.null(inFile)){
      return(NULL)
    }else {
      # browser()
      numfiles = nrow(inFile) 
      merged_data = list()
      
      
      for (i in 1:numfiles)
      {
        
        data.dat <- read.table(input$file1[[i, 'datapath']], skip = 1,
                               header = T, sep = ',', na.strings = 'NAN', stringsAsFactors = F)
        
        
        merged_data[[i]] <- data.dat[-c(1:2),] 
        
        
      }
      
      data.bound <- bind_rows(merged_data)
      data.bound <- unique(data.bound)
      
      
    }
    
    observe(print(str(data.bound)))
    data.bound$TIMESTAMP <- as.POSIXct(data.bound$TIMESTAMP)
    
    
    # equations displacement --------------------------------------------------
    
    
    data.bound[is.na(data.bound)] <- 0
    
    #this is temperature 
    lower <- (getData2()$input$cols2 -getData2()$input$cols1) * 
      getData2()$input$cols3 + (22.76 - 22.8) * 
      (((getData2()$input$cols1 * 0.000384) + (-0.3482)) * 0.02828)
    # input$dLower-2631) * 0.02828 + (22.76 - 22.8) * ##TT_Lower = 22.76
    # (((input$dLower * 0.000384) + (-0.3482)) * 0.02828)
    upper <- (input$dUpper -2584) * 0.02828 + (28.37 - 22.8) * #TT_Upper = 28.37
      (((input$dUpper * 0.000384) + (-0.3482)) * 0.02828)
    control <- (input$dControl -2520) * 0.02869 + (28.37 - 22.8) * #TT_Upper = 28.37
      (((input$dControl * 0.000384) + (-0.3482)) * 0.02869)
    
    
    data.bound %>%
      arrange(TIMESTAMP) %>%
      mutate(across(where(is.character), as.numeric)) %>%
      mutate(raT.24 = rollapply(T109_C, 96, mean, na.rm = T, align = 'right', partial=TRUE, fill = NA)) %>%
      mutate(disp.lower = (Digits_Lower -2631) * 0.02828 + (TT_Lower - 22.8) *
               (((Digits_Lower * 0.000384) + (-0.3482)) * 0.02828)) %>%
      mutate(norm.dispL = disp.lower- lower) %>%
      mutate(raL.24 = rollapply(norm.dispL, 96, mean, na.rm = T,align = 'right', partial=TRUE, fill = NA)) %>%
      mutate(disp.upper = (Digits_Upper -2584) * 0.02828 + (TT_Upper - 22.8) *
               (((Digits_Upper * 0.000384) + (-0.3482)) * 0.02828)) %>%
      mutate(norm.dispU = disp.upper- upper) %>%
      mutate(raU.24 = rollapply(norm.dispU, 96, mean, na.rm = T, align = 'right', partial=TRUE, fill = NA)) %>%
      mutate(disp.control = (Digits_CTRL -2520) * 0.02869 + (TT_Upper - 22.8) *
               (((Digits_CTRL * 0.000384) + (-0.3482)) * 0.02869)) %>%
      mutate(norm.dispC = disp.control- control) %>%
      mutate(raC.24 = rollapply(norm.dispC, 96, mean, na.rm = T, align = 'right', partial=TRUE, fill = NA)) %>% 
      mutate(poly.noTL = (0.000000077403*(Digits_Lower^2))+(0.02746*Digits_Lower)+poly.lower) %>% 
      mutate(raL.noT = rollapply(poly.noTL, 96, mean, na.rm = T,align = 'right', partial=TRUE, fill = NA)) %>%
      mutate(poly.noTU = (0.000000088762*(Digits_Upper^2))+(0.02727*Digits_Upper)+poly.upper) %>% 
      mutate(raU.noT = rollapply(poly.noTU, 96, mean, na.rm = T,align = 'right', partial=TRUE, fill = NA)) %>%
      mutate(poly.noTC = (0.00000010095*(Digits_CTRL^2))+(0.02765*Digits_CTRL)+poly.control) %>% 
      mutate(raC.noT = rollapply(poly.noTC, 96, mean, na.rm = T,align = 'right', partial=TRUE, fill = NA)) 
    
  })
  
  observe(print(str(getData())))
  
  output$fileUploaded <- reactive({
    return(!is.null(getData()))
  })
  
  outputOptions(output, 'fileUploaded', suspendWhenHidden = F)
  
  # dates -------------------------------------------------------------------
  
  
  observeEvent(input$action, {
    getData <- getData()
    dates <-  getData$TIMESTAMP
    minval <- as.character(min(dates))
    maxval <- as.character(max(dates))
    
    updateDaterangepicker(
      
      session, "dRange",
      start = minval, end = maxval,
      min = minval,  max = maxval
      
    )
  })
  
  date.character <- reactive ({
    #use validate and need to suppress warning messages
    #when no input file
    validate(
      need(input$file1 != "", "Please upload a calibration file, then data file(s)")
    )
    getData() %>%
      mutate_if(is.POSIXct, as.character) %>% 
      filter(TIMESTAMP >= format(input$dRange[1]) &
               TIMESTAMP <= format(input$dRange[2]) )
  })
  
  output$contents <- DT::renderDT({
    
    DT:: datatable(date.character(),
                   # DT:: datatable( getData(),
                   filter = 'top'
                   
    )
  })
  
  
  
  output$plot <- renderPlotly({
    
    
    getData <- getData()
    date.character <- date.character()
    
    date.POSIX <- date.character %>% 
      mutate_if(is.character, as.POSIXct)
    
    
    plot_ly(date.POSIX) %>% 
      add_trace(x = ~TIMESTAMP, y = ~raT.24, type = "scatter", 
                mode = "markers", marker = list(color = "#00AFBB"), yaxis = "y2", name = "Temperature") %>%
      add_trace(x = ~TIMESTAMP, y = ~raL.24,
                type = "scatter", mode = "markers", marker = list(color = '#00FF00'),
                name = "Lower Displacement", textposition = "top center") %>%
      add_trace(x = ~TIMESTAMP, y = ~raU.24, type = "scatter", 
                mode = "markers", marker = list(color = "#E7B800"), name = "Upper Displacement") %>%
      add_trace(x = ~TIMESTAMP, y = ~raC.24, type = "scatter", 
                mode = "markers", marker = list(color = '#FF0033'), name = "Control") %>%
      add_trace(x = ~TIMESTAMP, y = ~raL.noT,
                type = "scatter", mode = "markers", marker = list(color = '#00FF00'),
                name = "Lower Displacement No Temp", textposition = "top center") %>%
      add_trace(x = ~TIMESTAMP, y = ~raU.noT, type = "scatter", 
                mode = "markers", marker = list(color = "#E7B800"), name = "Upper Displacement No Temp") %>%
      add_trace(x = ~TIMESTAMP, y = ~raC.noT, type = "scatter", 
                mode = "markers", marker = list(color = '#FF0033'), name = "Control No Temp") %>%
      layout(
        legend = list(orientation = 'v'),
        yaxis2 = list(side = "right", title = "Temperature (deg C)"),
        title = "Pine Creek Housing Crackmeter Displacement (24 hour rolling average data)",
        xaxis = list(title="Time"),
        yaxis = list(overlaying = "y2", title="Displacement (mm)")
      )
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

