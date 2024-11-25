#Packages to be installed and loaded############################
library(shiny)
library(DT)
library(shinyjs)#enables some java script functions in shinylibrary(shiny)
library(tidyverse)
library(fst)
library(data.table)
library(lubridate)
library(hms)

#variables used to set starting date to first day of a quarter
season_starts <- c(3, 6, 9, 12,13) 
cur_month <- as.numeric(format(Sys.Date(), "%m"))

#start main ui.r code#########
fluidPage(
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Application title with ODWC logos#####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  titlePanel(
    wellPanel(
      fluidRow(
        column(3,align="center", img(src="ODWClogo.gif", height="auto", width="100px")),
        column(6, align="center", tags$b(h2(HTML(paste0(
          "Oklahoma Angler Creel Analysis Application<sup style='font-size: 0.3em; vertical-align: super;'>© ",
          format(Sys.Date(), "%Y"),"</sup>")))),
        # column(6, align="center", h2("Oklahoma Angler Creel Analysis Application"),
               hr(), 
               h5("Created by Daniel E. Shoup and Brooke Wetteland")), #will add authors as other contributions are made
              # h5("Created by Daniel E. Shoup, Drew Wallace, Brooke Beverly, Douglas L. Zentner, Alexis Whiles, and Jory Bartnicki ")),
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
    #Creel Planning Tab##########
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #this has start and end date inputs 
    tabPanel("Creel Planning",
             hr(),
             fluidRow(
               #used for Will Sims app with weighted randomization for creel section...retaining in case we later want to add this to shiny app
               # column(width = 3,
               #        wellPanel(
               #          actionButton("addRow", "Add Row"),
               #          DTOutput("ProbDataTable"),
               #          textOutput("totalProb")
               #        )
               #        ),
               column(width = 3,
                      dateInput("start_date", "Select Start Date:", value = 
                                    tryCatch({ #below finds the start of the next quarter for start date unless <=2d into current quarter
                                      if(cur_month %in% season_starts & as.numeric(format(Sys.Date(), "%d")) <= 2){
                                        Sys.Date()
                                      }else{
                                        next_season_index <- (min(which(season_starts >= cur_month+1)))
                                        if(next_season_index == 5) {
                                          as.Date(paste0(format(Sys.Date(), "%Y")+1, "-03-01"))
                                        }else{
                                          as.Date(paste0(format(Sys.Date(), "%Y"), "-", sprintf("%02d", season_starts[next_season_index]), "-01"))
                                        }
                                      }
                                    }, error = function(e) Sys.Date()),
                                format = "mm-dd-yyyy"),
                      dateInput("end_date", "Select End Date:", value = Sys.Date() + years(1), format = "mm-dd-yyyy"),
                      #below for calculating days from target hr/wk rather than % of days
                      # sliderInput("sample_hPerWk", "Target hr/wk:", 
                      #             min = 10, max = 40, value = 30, step = 1),
                      selectizeInput("lakeSelector", "Select Lake Name", choices = NULL, multiple = FALSE,
                                     options = list(placeholder = 'Select a lake')),
                      
                      actionButton("toggleTimeFormat", "Toggle sm/pm or military"),
                      downloadButton("downloadData", "Download Table")),
               
               column(width = 3,
                      sliderInput("sample_percentage", "Percent Effort:",
                                  min = 10, max = 100, value = 10, step = 5),
                      sliderInput("num_sections", "Number of Lake Sections:",  min = 1, max = 50, value = 1, step = 1) # Allow any input by setting a low minimum value
               )),
             hr(), 
             DT::dataTableOutput("creel_table")
    ),
       
    
     #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     #Data Validation Tab##########
     #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     tabPanel("Data Validation",
        hr(),  
        fluidRow(
          #delete this comment and start putting your tab's code here
          h1("This tab has not yet been coded...Data validation functionality will be comming soon")
          
          
          
        )
     ),
                
        
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      #Analysis Tab##########
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      tabPanel("Creel Analysis",
         hr(),  
         fluidRow(
           #delete this comment and start putting your tab's code here
           h1("This tab has not yet been coded...creel data analysis functionality will be comming soon")
           
           
           
           
         )
      ),
    
    
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      ##User's guide tab Tab##########
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      tabPanel("User's Guide",
         hr(),  
         fluidRow(
           #delete this comment and start putting your tab's code here
           h1("This tab has not yet been coded...a user's guide will be comming soon")
           
           
           
           
         )
    )
                 
  )
)