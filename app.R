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

# time_var <- c("Station Time" = "TIMESTAMP", 
#               "Time for Visuals" = "datetime_visuals") #better if this would actually be UTC or specify time zone 
#could also just get rid of time zone in xaxis

# time zones --------------------------------------------------------------


time_zones <- c("US/Alaska", "US/Aleutian", "US/Central", "US/Eastern", "US/Mountain", 
                "US/Hawaii", "US/Pacific", "US/Samoa", "UTC", "Pacific/Guam", "America/Puerto_Rico")

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
                                                             pickerInput(inputId = "tz",
                                                                         label = "Station Time", 
                                                                         choices = time_zones, 
                                                                         selected = c("US/Mountain")),
                                                             pickerInput(inputId = "time",
                                                                         label = "Choose Time for Visuals", 
                                                                         choices = time_zones,
                                                                         selected = c("US/Mountain")),
                                                             pickerInput(inputId = "temp",
                                                                         label = "Select Temperature Column", 
                                                                         choices = "" ),
                                                               textInput(inputId = "title", label = "Name Your Plot", 
                                                                         value = "Pine Creek Housing Crackmeter Displacement (24 hour rolling average data)"),
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
                                        
                               ),  # tab panel
                               
                               tabPanel("Plots",

                                        plotlyOutput("plot1"),
                                        # plotlyOutput("plot3"),

                                        absolutePanel(id = "controls", class = "panel panel-default",
                                                      fixed = TRUE,
                                                      draggable = TRUE, #need to fix
                                                      #so not everything drags
                                                      top = 120, left = "auto", right = 30,
                                                      bottom = "auto", width = 300, height = "auto",
                                                      style="z-index: 410;",
                                                      conditionalPanel(condition = "output.fileUploaded == true",
                                                                       pickerInput(inputId = "temp",
                                                                                   label = "Select Temperature Column", 
                                                                                   choices = ""),
                                                                       pickerInput(inputId = "control",
                                                                                   label = "Select Control Instrument Column", 
                                                                                   choices = ""))
                                                 
                                        )   #close absolute panel
                               ) #close tabPanel
                    
                               
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
        # calibration <- getData2() %>% 
    #   #delete first three rows calibration file which are Site_name, Serial_Number, Model 
    #   select(-c(1:3)) %>% 
    #   mutate(base.value = (Install_Zero - Cal_Zero) * 
    #            L_gauge_factor + (First_Temp - Temp) * 
    #            (((Install_Zero * TempCor_M) + 
    #                (TempCor_B)) * L_gauge_factor)) %>% 
    #   mutate(poly.value = -((Poly_GF_A*(Install_Zero^2))+(Poly_GF_B*Install_Zero)))

    
  })
  
  #not sure if this should be in reactive or not
  
  
  
  output$fileUploaded2 <- reactive({
    return(!is.null(getData2()))
  })
  # might need to comment this back in 
  
  
  
  observe(print(str(getData2())))
  
  outputOptions(output, 'fileUploaded2', suspendWhenHidden = F) #might need this code
  
  
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
    
    df.date <- data.bound %>% 
      mutate(
        TIMESTAMP = parse_date_time(TIMESTAMP, orders = c("mdy HM", "ymd HMS"),
                                    #double check this is right date format 
                                    tz = input$tz)) # try 'US/Mountain' if this doesn't work
      #   datetime_visuals = date_set_zone(TIMESTAMP, zone = input$time), 
      # ) %>% 
      # select(datetime_visuals, everything())
    
    
    # equations displacement --------------------------------------------------
    
    
    df.date[is.na(df.date)] <- 0
    
    df.24 <- df.date %>% 
      arrange(TIMESTAMP) %>% 
      mutate(across(where(is.character), as.numeric)) 
    
    #merge datasets by extending rows of df.dat by rows of calibration 
    calibration <- getData2() %>% 
      #delete first three rows calibration file which are Site_name, Serial_Number, Model 
      select(-c(1:3)) %>% 
      mutate(base.value = (Install_Zero - Cal_Zero) * 
               L_gauge_factor + (First_Temp - Temp) * 
               (((Install_Zero * TempCor_M) + 
                   (TempCor_B)) * L_gauge_factor)) %>% 
      mutate(poly.value = -((Poly_GF_A*(Install_Zero^2))+(Poly_GF_B*Install_Zero)))
    
 
    results2 <- df.24 %>% 
      crossing.(calibration) %>% 
      mutate_rowwise.(disp = (get(`Digits`)- Cal_Zero) * L_gauge_factor 
                      + (get(`TT`) - Temp) * (((get(`Digits`) * TempCor_M) + 
                                                 (TempCor_B)) * L_gauge_factor)) %>% 
      mutate_rowwise.(poly = (Poly_GF_A * (get(`Digits`)^2)) +
                        (Poly_GF_B * get(`Digits`)) + poly.value) %>%
      mutate.(norm = disp - base.value) %>% 
      mutate.(ra = rollapply(norm, 1:n() - findInterval(TIMESTAMP - 24 * 3600, 
                                                        TIMESTAMP), mean, na.rm = T, align = 'right', partial=TRUE, fill = 0),
              .by = "Disp") %>% 
      mutate.(ra_poly = rollapply(poly, 1:n() - findInterval(TIMESTAMP - 24 * 3600,
                                                             TIMESTAMP), mean, na.rm = T, align = 'right', partial=TRUE, fill = 0),
              .by = "Disp") %>%
      pivot_wider.(names_from = c(Digits),
                   values_from = c(disp, norm, poly, ra, ra_poly))
    
    calibration2 <- calibration[ , !(names(calibration) %in% "Digits")]
    
    if(is.null(input$temp)){return()}
    
    #clean data
    results3 <- results2 %>% 
      fill.(starts_with("disp") | starts_with("norm") | starts_with("poly") | starts_with("ra") , .direction = c("downup"), .by = "TIMESTAMP") %>% # | starts_with("ra.RUN")
      select.(!(names(calibration2))) %>% 
      distinct.() %>% 
      rename_at(vars(matches("^ra") | matches("^norm") | matches("^poly")), ~ str_remove(., "Digits_"))
  # if(is.null(input$temp)){return()}, else(
    #if(is.null(input$temp)){return()}, else {}
  })
  
  observe(print(str(getData())))
  
  output$fileUploaded <- reactive({
    return(!is.null(getData()))
  })
  
  outputOptions(output, 'fileUploaded', suspendWhenHidden = F)
  

# update picker input temp ------------------------------------------------

  observeEvent(input$file1, {
    getData <- getData()
    ntemp <- getData %>%
      select(ends_with("_C") | starts_with("TT_"))

    updatePickerInput(session = session, inputId = "temp",
                      choices = names(ntemp))
    }, ignoreInit = TRUE)

  
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
  

# plot displacement and temp vs. time -------------------------------------

  
  output$plot <- renderPlotly({
    
    
    getData <- getData()
    date.character <- date.character()
    
    date.POSIX <- date.character %>%  #check if i actually need to do this since date.character might just be changed and not getData()?
      mutate_if(is.character, as.POSIXct) %>% 
      mutate.(T_ra = rollapply(get(input$temp), 1:n() - findInterval(date_set_zone(TIMESTAMP, zone = input$time)
                                                                     - 24 * 3600,
                                                                     date_set_zone(TIMESTAMP, zone = input$time)),
                               mean, na.rm = T, align = 'right', partial=TRUE, fill = 0)) %>%
      pivot_longer.(starts_with("ra") | starts_with("norm") | starts_with("poly"), names_to = 'variables', values_to = "values") %>% 
      select.(c("TIMESTAMP", "variables", "values", "T_ra")) %>% #"datetime_visuals", 
      #need to add in NAs so don't have 6 traces
      mutate.(T_ra = ifelse(duplicated(T_ra), NA, T_ra))
    
    
    plot_so <- plot_ly(data = date.POSIX, width = 1000, height = 550,
                        x = ~date_set_zone(TIMESTAMP, zone = input$time), y = ~values,
                        color = ~variables,
                        type = "scatter", mode = "lines + markers") %>%
      add_trace(y = ~T_ra,
                name = "Temperature",
                line = list(color = "#0066FF"),
                mode = "lines",
                showlegend = TRUE,
                yaxis = "y2") %>%
      layout(title = list(text = input$title, y = 0.98),
             xaxis = list(title = input$time),#names(time_var[which(time_var == input$time)])),
             yaxis = list(title = 'Displacement (mm)'),
             legend = list(orientation = 'v', x = 1.05),
             yaxis2 = list(overlaying = "y",
                           side = "right", title = "Temperature (deg C)",
                           range = range(na.omit(date.POSIX$T_ra)))
             )
  })
    

    observeEvent(input$file1, {
      getData <- getData()
      norm_dat <- getData %>%
        select(starts_with("poly"))

      updatePickerInput(session = session, inputId = "control",
                        choices = names(norm_dat))
    }, ignoreInit = TRUE)

# plot poly CTRL norm vs. temp  -------------------------------------------
    output$plot1 <- renderPlotly({


      plot_so1 <- plot_ly(data = getData(), width = 1000, height = 550,
                          x = ~get(input$temp), y = ~get(input$control),
                          type = "scatter", mode = "lines + markers") %>%
        layout(title = list(text = "Control vs. Temperature", y = 0.98),
               xaxis = list(title = input$temp),#names(time_var[which(time_var == input$time)])),
               yaxis = list(title = 'Displacement (mm)'), #figure out units-- is it still displacement (mm) ?
               legend = list(orientation = 'v', x = 1.05)
        )



    })



  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
