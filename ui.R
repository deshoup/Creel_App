#Packages to be installed and loaded############################
library(shiny)
library(DT)
library(shinyjs)#enables some java script functions in shinylibrary(shiny)
library(tidyverse)
library(fst)
library(data.table)
library(lubridate)
library(hms)

#start main ui.r code#########
fluidPage(
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Application title with ODWC logos#####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  titlePanel(
    wellPanel(
      fluidRow(
        column(3,align="center", img(src="ODWClogo.gif", height="auto", width="100px")),
        column(6, align="center", h2("Oklahoma Angler Creel Analysis Application"),
               hr(), 
               h5("Created by Daniel E. Shoup, Drew Wallace, Brooke Beverly, Douglas L. Zentner, Alexis Whiles, and Jory Bartnicki ")),
        #below line vertically centers OSU logo...sets height to 110 px
        tags$style(HTML('
                      .verticalcenter {
                      display: table-cell;                      
                      height: 110px;
                      vertical-align: middle;
                      }')),
        column(3, align="center", img(src="osulogo.png", height="auto", width="auto",class="verticalcenter"))
      )
    ),
    windowTitle = "OK Creel Analysis Application" #this text is what appears as browser title
  ),
  

  # Menu structure with main functions of app. Each "tabPanel" line makes another tab with code for each page underneath
  tabsetPanel(id="tabs", type = c("tabs"),
                
                
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    ##Creel Planning Tab##########
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #this has start and end date inputs 
    tabPanel("Creel Planning",
             hr(),
             fluidRow(
               column(width = 3,
                      dateInput("start_date", "Select Start Date:", value = Sys.Date(), format = "mm-dd-yyyy"),
                      dateInput("end_date", "Select End Date:", value = Sys.Date() + years(1), format = "mm-dd-yyyy"),
                      actionButton("toggleTimeFormat", "Toggle Time Format"),
                      downloadButton("downloadData", "Download Table")),
               column(width = 3,
                      sliderInput("sample_percentage", "Percent Effort:", 
                                  min = 10, max = 100, value = 10, step = 5),
                      selectizeInput("lakeSelector", "Select a Lake", choices = NULL, multiple = FALSE,
                                     options = list(placeholder = 'Select a lake')),
                      sliderInput("num_sections", "Number of Lake Sections:",  min = 1, max = 50, value = 1, step = 1) # Allow any input by setting a low minimum value
               )),
             hr(),
             DT::dataTableOutput("creel_table")  
    ),
       
    
     #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     ##Data Validation Tab##########
     #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     tabPanel("Data Validation",
        hr(),  
        fluidRow(
          #delete this comment and start putting your tab's code here
          
          
          
          
        )
     ),
                
        
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      ##Analysis Tab##########
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      tabPanel("Creel Analysis",
         hr(),  
         fluidRow(
           #delete this comment and start putting your tab's code here
           
           
           
           
         )
      ),
    
    
     #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      ##User's guide tab Tab##########
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      tabPanel("User's Guide",
         hr(),  
         fluidRow(
           #delete this comment and start putting your tab's code here
           
           
           
           
         )
    )
                 
  )
)