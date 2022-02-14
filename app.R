#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages('shinythemes', dependencies = T)
#don't forget to comment out before publish!
.libPaths("C:/Users/kzyatitsky/R/win-library/4.1")
library(shiny)
library(tidyverse)
library(lubridate)
library(DT)
library(shinythemes)
library(shinyjs)
library(plotly)
library(zoo)
# library(scales)
# library(zoo)



#changes max upload size to 600 mb
options(shiny.maxRequestSize=600*1024^2)

#brings in data cleaning function
#source("clean_data_function.R")

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
      fileInput("file1", "Choose File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv", ".dat")),
      # Horizontal line ----
      tags$hr()
      
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
server <- function(input, output) {
  
  getData <- reactive({
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
                               header = T, sep = ',', na.strings = 'NAN')
        #, row.names = 2)
        
        #data.dat$TIMESTAMP <- as.POSIXct(data.dat$TIMESTAMP)

        #remove rows until RECORD == 0
        
       # merged_data[[i]] <- ifelse(data.dat$RECORD[1] == 'RN',
       #                            subset(data.dat, RECORD!="" & RECORD!= "RN"),
       #                            data.dat)
     
          #data.dat[which.max(data.dat$RECORD == "0") : nrow(data.dat), ]

        #this code removes first 2 rows (indexes)
        # comment out for now while playing
      #  merged_data[[i]] <- data.dat[-c(1:2),]
        
        #if(data.dat$TIMESTAMP[1] == 'TS') {
          
       
        merged_data[[i]] <- data.dat[-c(1:2),]
        # }
        # else {
        #   merged_data[[i]] <- data.dat
        # }
       }
      # browser()
      #data.bound <- do.call(bind_rows, merged_data) #might not need do.call
      data.bound <- bind_rows(merged_data)
      data.bound <- unique(data.bound)
    
  
      # 
      # data.bound$TIMESTAMP <- as.POSIXct(data.bound$TIMESTAMP)
      # 
      # data.bound %>% 
      #   mutate(across(where(is.character), as.numeric)) %>% 
      #   mutate(raT.24 = rollapply(T109_C, 100, mean, align = 'right', fill = 0)) %>% 
      #   mutate(disp.lower = (Digits_Lower -2631) * 0.02828 + (TT_Lower - 22.8) *
      #            (((Digits_Lower * 0.000384) + (-0.3482)) * 0.02828)) %>% 
      #   mutate(norm.dispL = disp.lower- disp.lower[1]) %>% 
      #   mutate(raL.24 = rollapply(norm.dispL, 100, mean, align = 'right', fill = 0)) %>% 
      #   mutate(disp.upper = (Digits_Upper -2584) * 0.02828 + (TT_Upper - 22.8) *
      #            (((Digits_Upper * 0.000384) + (-0.3482)) * 0.02828)) %>% 
      #   mutate(norm.dispU = disp.upper- disp.upper[1]) %>% 
      #   mutate(raU.24 = rollapply(norm.dispU, 100, mean, align = 'right', fill = 0)) %>%
      #   mutate(disp.control = (Digits_CTRL -2520) * 0.02869 + (TT_CTRL - 22.8) *
      #            (((Digits_CTRL * 0.000384) + (-0.3482)) * 0.02869)) %>% 
      #   mutate(norm.dispC = disp.control- disp.control[1]) %>% 
      #   mutate(raC.24 = rollapply(norm.dispC, 144, mean, align = 'right', fill = 0)) # %>%
    
        #select(c(TIMESTAMP, raC.24, raT.24, raL.24, raU.24)) %>% 
    
    }
    data.bound$TIMESTAMP <- as.POSIXct(data.bound$TIMESTAMP)
    data.bound[is.na(data.bound)] <- 0
    
    data.bound %>%
      #group_by(TIMESTAMP) %>% 
      arrange(desc(TIMESTAMP)) %>% 
      mutate(across(where(is.character), as.numeric)) %>%
      
     # mutate_all(~replace(., is.na(.), 0)) %>% 
      #mutate(across(where(is.na), 0)) %>%
      mutate(raT.24 = rollapply(T109_C, 96, mean, na.rm = T, align = 'right', partial=TRUE, fill = 0)) %>%
      mutate(disp.lower = (Digits_Lower -2631) * 0.02828 + (TT_Lower - 22.8) *
               (((Digits_Lower * 0.000384) + (-0.3482)) * 0.02828)) %>%
      mutate(norm.dispL = disp.lower- disp.lower[1]) %>%
      mutate(raL.24 = rollapply(norm.dispL, 96, mean, na.rm = T,align = 'right', partial=TRUE, fill = 0)) %>%
      mutate(disp.upper = (Digits_Upper -2584) * 0.02828 + (TT_Upper - 22.8) *
               (((Digits_Upper * 0.000384) + (-0.3482)) * 0.02828)) %>%
      mutate(norm.dispU = disp.upper- disp.upper[1]) %>%
      mutate(raU.24 = rollapply(norm.dispU, 96, mean, na.rm = T, align = 'right', partial=TRUE, fill = 0)) %>%
      mutate(disp.control = (Digits_CTRL -2520) * 0.02869 + (TT_CTRL - 22.8) *
               (((Digits_CTRL * 0.000384) + (-0.3482)) * 0.02869)) %>%
      mutate(norm.dispC = disp.control- disp.control[1]) %>%
      mutate(raC.24 = rollapply(norm.dispC, 96, mean, na.rm = T, align = 'right', partial=TRUE, fill = 0)) 
    
    
    #write function to remove NaN values 
    # is.nan.data.frame <- function(x)
    #   do.call(cbind, lapply(x, is.nan))
    # 
    # 
    # #remove NaN values -- not sure any are actually here
    # data.bound[is.nan(data.bound)] <- 0
    
  })

  observe(print(str(getData())))
  
  output$contents <- DT::renderDT(
    
       getData(),
    filter = 'top'
    
  )
  
  
    
    output$plot <- renderPlotly({
      
     #browser()
      
     getData <- getData()
      
     # getData$TIMESTAMP <- as.POSIXct(getData$TIMESTAMP)
      
     # getData %>%
     #   # mutate(across(where(is.character), as.numeric)) %>%
     #    mutate(raT.24 = rollapply(T109_C, 96, mean, align = 'right', fill = 0)) %>%
     #    mutate(disp.lower = (Digits_Lower -2631) * 0.02828 + (TT_Lower - 22.8) *
     #             (((Digits_Lower * 0.000384) + (-0.3482)) * 0.02828)) %>%
     #    mutate(norm.dispL = disp.lower- disp.lower[1]) %>%
     #    mutate(raL.24 = rollapply(norm.dispL, 96, mean, align = 'right', fill = 0)) %>%
     #    mutate(disp.upper = (Digits_Upper -2584) * 0.02828 + (TT_Upper - 22.8) *
     #             (((Digits_Upper * 0.000384) + (-0.3482)) * 0.02828)) %>%
     #    mutate(norm.dispU = disp.upper- disp.upper[1]) %>%
     #    mutate(raU.24 = rollapply(norm.dispU, 96, mean, align = 'right', fill = 0)) %>%
     #    mutate(disp.control = (Digits_CTRL -2520) * 0.02869 + (TT_CTRL - 22.8) *
     #             (((Digits_CTRL * 0.000384) + (-0.3482)) * 0.02869)) %>%
     #    mutate(norm.dispC = disp.control- disp.control[1]) %>%
     #    mutate(raC.24 = rollapply(norm.dispC, 96, mean, align = 'right', fill = 0)) %>%
      
  plot_ly(getData) %>% 
  add_trace(x = ~TIMESTAMP, y = ~raT.24, type = "scatter", 
            mode = "markers", marker = list(color = "#00AFBB"), yaxis = "y2", name = "Temperature") %>%
  add_trace(x = ~TIMESTAMP, y = ~raL.24,
            type = "scatter", mode = "markers", marker = list(color = '#00FF00'),
            name = "Lower Displacement", textposition = "top center") %>%
  add_trace(x = ~TIMESTAMP, y = ~raU.24, type = "scatter", 
            mode = "markers", marker = list(color = "#E7B800"), name = "Upper Displacement") %>%
  add_trace(x = ~TIMESTAMP, y = ~raC.24, type = "scatter", 
            mode = "markers", marker = list(color = '#FF0033'), name = "Control") %>%
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
