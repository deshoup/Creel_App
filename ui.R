#Packages to be installed and loaded############################
library(shiny)
library(DT)
library(shinyjs)

#start main ui.r code#########
fluidPage(
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Application title with ODWC logos#####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  titlePanel(
    wellPanel(
      fluidRow(
        column(3,align="center", img(src="ODWClogo.gif", height="auto", width="150px")),
        column(6, align="center", h2("Oklahoma Angler Creel Analysis Application"),
               hr(), 
               h5("Created by Douglas L. Zentner, Alexis Whiles, Jory Bartnicki and Daniel E. Shoup")),
        column(3, align="center",img(src="osulogo.png", height="auto", width="180px"))
      )
    ),
    windowTitle = "OK Creel Analysis Application" #this text is what appears as browser title
  ),
  

  # Menu structure with main functions of app. Each "tabPanel" line makes another tab with code for each page underneath
  tabsetPanel(id="tabs", type = c("tabs"),
                
                
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    ##Creel Planning Tab##########
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    tabPanel("Creel Planning",
       hr(),  
       fluidRow(
         
             )
    ),
       
    
     #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     ##Data Validation Tab##########
     #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     tabPanel("Data Validation",
        hr(),  
        fluidRow(
          
        )
     ),
                
        
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      ##Analysis Tab##########
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      tabPanel("Creel Analysis",
         hr(),  
         fluidRow(
           
         )
      ),
    
    
     #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      ##User's guide tab Tab##########
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      tabPanel("User's Guide",
         hr(),  
         fluidRow(
           
         )
    )
                 
  )
)