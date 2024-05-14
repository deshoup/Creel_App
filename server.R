#Packages to be installed and loaded############################
library(shiny)
library(tidyverse)
library(fst)
library(data.table)
library(lubridate)
library(hms)
library(DT)
library(shinyjs)#enables some java script functions in shinylibrary(shiny)

par(bg = "white") #fixes problem where figures that are downloaded have grey background. This makes them white

#Load needed files
lakeinfo <- read.fst("lakeinfo.fst", as.data.table = TRUE)
speciesinfo <- read.fst("speciesinfo.fst", as.data.table = TRUE)
# creelData <- read.fst(creeldata.fst, as.data.table=T)#this will eventually read in the main creel database


function(input, output, session) {
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##Creel Planning Tab code##########
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  # Observe changes in the selected input and update choices
  observe({
    updateSelectizeInput(session, "lakeSelector", choices = unique(lakeinfo$Lake.Name))
  })
  
  # Function to toggle time format
  timeFormat <- reactiveVal("military")
  
  ########Building a calendar and function to randomly sample dates from a quarter###########
  
  #Building a calendar to randomly sample from with start and end date inputs
  # Generate the full calendar dataset
  calendarFull <- reactive({
    calendar <- tibble(date = seq(as.Date(input$start_date), as.Date(input$end_date), by = "1 day"))
    calendar %>%
      mutate(
        year = year(date),
        month = month(date),
        dayWeek = wday(date),
        strata = ifelse(dayWeek %in% c(1, 7), "weekend day", "weekday"), #1 = Sunday, 7 = Saturday
        group = 1
      ) %>%
      expand_grid(shift = c("morning", "afternoon")) %>%
      mutate(
        strata = ifelse(dayWeek == 6 & shift == "afternoon", "weekend day", strata), #6 = Saturday
        quarter = case_when(
          month %in% c(12, 1, 2) ~ "Winter",
          month %in% c(3, 4, 5) ~ "Spring",
          month %in% c(6, 7, 8) ~ "Summer",
          month %in% c(9, 10, 11) ~ "Fall"
        ),
        strata = case_when(
          month == 7 & day(date) == 4 ~ "weekend day", #accounts for 4th of July
          month == 7 & day(date) == 3 & (dayWeek %in% c(2, 3)) ~ "weekend day", #4th of July weird Mon and Tues
          month == 7 & day(date) == 5 & (dayWeek %in% c(5, 6)) ~ "weekend day", #4th of July weird Thur and Fri
          month == 5 & day(date) == max(day(date[month == 5 & dayWeek == 2])) ~ "weekend day",
          #Memorial day or the last Monday in May
          month == 9 & day(date) == min(day(date[month == 9 & dayWeek == 2])) ~ "weekend day",
          #Labor day or first Monday in Sept
          TRUE ~ strata
        ),
        #adding the start and end times from creel calendar
        #I combined the times for Mar and Nov given for daylight savings to match calendar full
        startTime = case_when(
          month == 1 & shift == "morning" ~ 0800, #add & timetype = military and then copy line and add timetype = standard
          month == 2 & shift == "morning" ~ 0730,
          month == 3 & shift == "morning" ~ 0730,
          month == 4 & shift == "morning" ~ 0700,
          month == 5 & shift == "morning" ~ 0630,
          month == 6 & shift == "morning" ~ 0630,
          month == 7 & shift == "morning" ~ 0630,
          month == 8 & shift == "morning" ~ 0700,
          month == 9 & shift == "morning" ~ 0730,
          month == 10 & shift == "morning" ~ 0730,
          month == 11 & shift == "morning" ~ 0730,
          month == 12 & shift == "morning" ~ 0730,
          month == 1 & shift == "afternoon" ~ 1300,
          month == 2 & shift == "afternoon" ~ 1300,
          month == 3 & shift == "afternoon" ~ 1330,
          month == 4 & shift == "afternoon" ~ 1330,
          month == 5 & shift == "afternoon" ~ 1330,
          month == 6 & shift == "afternoon" ~ 1400,
          month == 7 & shift == "afternoon" ~ 1400,
          month == 8 & shift == "afternoon" ~ 1400,
          month == 9 & shift == "afternoon" ~ 1400,
          month == 10 & shift == "afternoon" ~ 1330,
          month == 11 & shift == "afternoon" ~ 1300,
          month == 12 & shift == "afternoon" ~ 1230
        ),
        endTime = case_when(
          month == 1 & shift == "morning" ~ 1300,
          month == 2 & shift == "morning" ~ 1300,
          month == 3 & shift == "morning" ~ 1330,
          month == 4 & shift == "morning" ~ 1330,
          month == 5 & shift == "morning" ~ 1330,
          month == 6 & shift == "morning" ~ 1400,
          month == 7 & shift == "morning" ~ 1400,
          month == 8 & shift == "morning" ~ 1400,
          month == 9 & shift == "morning" ~ 1400,
          month == 10 & shift == "morning" ~ 1330,
          month == 11 & shift == "morning" ~ 1300,
          month == 12 & shift == "morning" ~ 1230,
          month == 1 & shift == "afternoon" ~ 1800,
          month == 2 & shift == "afternoon" ~ 1830,
          month == 3 & shift == "afternoon" ~ 1930,
          month == 4 & shift == "afternoon" ~ 2000,
          month == 5 & shift == "afternoon" ~ 2030,
          month == 6 & shift == "afternoon" ~ 2100,
          month == 7 & shift == "afternoon" ~ 2100,
          month == 8 & shift == "afternoon" ~ 2030,
          month == 9 & shift == "afternoon" ~ 2000,
          month == 10 & shift == "afternoon" ~ 1900,
          month == 11 & shift == "afternoon" ~ 1800,
          month == 12 & shift == "afternoon" ~ 1730
        )
      )
  })
  
  # sample shifts and extra shifts to get the final output
  # building a function to randomly sample dates from calendarFull
  
  creelSchedule <- reactive({
    sample_percentage <- input$sample_percentage / 100  # Convert percentage to fraction
    
    target_days <- calendarFull() %>% group_by(year, quarter) %>%
      summarise(
        start_date2 = min(date),
        end_date2 = max(date),
        totDays = as.numeric(difftime(end_date2, start_date2, units = "days")),
        target_weekdays = round(totDays * 4 / (4 + 5) * sample_percentage),
        target_weekends = round(totDays * 5 / (4 + 5) * sample_percentage)
      )
    
    quarterly_dates <- list() # Create a blank list that will fill in loop below
    
    for (i in 1:nrow(target_days)) {
      quarterly_dates <- bind_rows(
        quarterly_dates,
        calendarFull() %>%
          filter(
            shift %in% c("morning", "afternoon"),
            strata == "weekday",
            year == target_days$year[i],
            quarter == target_days$quarter[i]
          ) %>%
          slice_sample(n = target_days$target_weekdays[i] + 2) %>% # Add 2 extra days
          ungroup(),
        
        calendarFull() %>%
          filter(
            shift %in% c("morning", "afternoon"),
            strata == "weekend day",
            year == target_days$year[i],
            quarter == target_days$quarter[i]
          ) %>%
          slice_sample(n = target_days$target_weekends[i] + 2) %>%
          ungroup()
      )
    }
    
    quarterly_dates %>%
      select(dayWeek, shift, strata, date, quarter, startTime, endTime) %>%
      arrange(date) %>%
      mutate(
        direction = sample(c("clockwise", "counterclockwise"), n(), replace = TRUE, prob = c(0.5, 0.5)),
        #generating a pressure count 3 hours before end time (300) and larger than start time
        pressure_count_1 = case_when(
          !is.na(endTime) & (endTime - startTime) >= 300 ~ {
            random_start_time <- ceiling(runif(n()) * (endTime - startTime - 300)) + startTime
            ifelse(random_start_time < endTime - 300, random_start_time, NA_real_)
          },
          TRUE ~ NA_real_
        ),
        #generating a pressure count at least 1 hour before end time (100) and hour after p. count 1
        pressure_count_2 = case_when(
          !is.na(pressure_count_1) &
            (as.numeric(endTime) - as.numeric(pressure_count_1)) >= 200 ~
            ceiling((as.numeric(pressure_count_1) + 100) + runif(n()) * (as.numeric(endTime) - (as.numeric(pressure_count_1) + 200))),
          TRUE ~ NA_real_
        ),
        #converting to portions of 60 minutes rather than 100
        pressure_count_1_hour = floor(pressure_count_1 / 100),
        pressure_count_1_minute = (pressure_count_1 %% 100) * 0.6, #%% is used to find the percent out of 100, the 0.6 converts it to minutes out of 60 available
        pressure_count_2_hour = floor(pressure_count_2 / 100),
        pressure_count_2_minute = (pressure_count_2 %% 100) * 0.6,
        #here I am converting all of this to military time separated by a colon
        
        pressureCount1 = case_when(
          timeFormat() == "military" ~
            ifelse(!is.na(pressure_count_1),
                   paste0(ifelse(pressure_count_1_hour > 12, pressure_count_1_hour - 12, pressure_count_1_hour), ":", ifelse(pressure_count_1_minute < 10,
                                                                                                                             paste0("0", floor(pressure_count_1_minute)), floor(pressure_count_1_minute))), NA_character_),
          TRUE ~  ifelse(!is.na(pressure_count_1),
                         paste0(ifelse(pressure_count_1_hour > 12, pressure_count_1_hour - 0, pressure_count_1_hour), ifelse(pressure_count_1_minute < 10,
                                                                                                                              paste0("0", floor(pressure_count_1_minute)), floor(pressure_count_1_minute))), NA_character_)
        ),
        pressureCount2 = case_when(
          timeFormat() == "military" ~
            ifelse(!is.na(pressure_count_2),
                   paste0(ifelse(pressure_count_2_hour > 12, pressure_count_2_hour - 12, pressure_count_2_hour), ":", ifelse(pressure_count_2_minute < 10,
                                                                                                                             paste0("0", floor(pressure_count_2_minute)), floor(pressure_count_2_minute))), NA_character_),
          TRUE ~ ifelse(!is.na(pressure_count_2),
                        paste0(ifelse(pressure_count_2_hour > 12, pressure_count_2_hour - 0, pressure_count_2_hour), ifelse(pressure_count_2_minute < 10,
                                                                                                                             paste0("0", floor(pressure_count_2_minute)), floor(pressure_count_2_minute))), NA_character_)
        )
      ) %>%
      
      # Add dayName column to switch from numeric to alpha. - shiny doesn't like the lubridate function
      mutate(
        dayName = case_when(
          dayWeek == 1 ~ "Sunday",
          dayWeek == 2 ~ "Monday",
          dayWeek == 3 ~ "Tuesday",
          dayWeek == 4 ~ "Wednesday",
          dayWeek == 5 ~ "Thursday",
          dayWeek == 6 ~ "Friday",
          dayWeek == 7 ~ "Saturday",
          TRUE ~ as.character(dayWeek)  # Handle other values if any
        ),
        # Remove intermediate columns
        pressure_count_1_hour = NULL,
        pressure_count_1_minute = NULL,
        pressure_count_2_hour = NULL,
        pressure_count_2_minute = NULL,
        pressure_count_1 = NULL,
        pressure_count_2 = NULL,
        dayWeek = NULL
      ) %>%
      mutate(
        # Convert startTime and endTime to character type
        startTime = as.character(startTime),
        endTime = as.character(endTime),
        # Add leading zeros to hour if necessary
        startTime = ifelse(nchar(startTime) == 3, paste0("0", startTime), startTime),
        endTime = ifelse(nchar(endTime) == 3, paste0("0", endTime), endTime),
        # Extract hour and minute components from startTime and endTime
        startHour = substr(startTime, 1, 2),
        startMinute = substr(startTime, 3, 4),
        endHour = substr(endTime, 1, 2),
        endMinute = substr(endTime, 3, 4),
        # Subtract 12 from startHour and endHour if they are greater than 12
        startHour = ifelse(as.numeric(startHour) > 12, as.character(as.numeric(startHour) - 12), startHour),
        endHour = ifelse(as.numeric(endHour) > 12, as.character(as.numeric(endHour) - 12), endHour),
        # Combine hour and minute components with a colon
        # ###Dan modified below
        startTimeFormatted1 = paste(startHour, startMinute, sep = ":"),
        startTimeFormatted = case_when(shift=="afternoon" ~ paste(startTimeFormatted1, " pm"),
                                       shift=="morning" & startHour>="12" ~ paste(startTimeFormatted1, " pm"),
                                       TRUE ~ paste(startTimeFormatted1, " am")),
        ###end Dan modified code
        endTimeFormatted = paste(endHour, endMinute, sep = ":")
      ) %>%
      # Drop intermediate columns
      select(-startHour, -startMinute, -endHour, -endMinute) %>%
      mutate(
        lakeName = input$lakeSelector  # Use input$lakeSelector to get the selected lake
      ) 
    
  })
  
  # Toggle time format when the button is clicked
  observeEvent(input$toggleTimeFormat, {
    if (timeFormat() == "military") {
      timeFormat("standard")
    } else {
      timeFormat("military")
    }
  })
  
  # Render the table
  output$creel_table <- renderDT({
    df <- creelSchedule()
    
    datatable(
      df,
      options = list(
        lengthMenu = list(c(50), c("50 rows")),
        columnDefs = list(
          list(visible = if (timeFormat() == "military") FALSE else TRUE, targets = which(colnames(df) == "startTime")),
          list(visible = if (timeFormat() == "military") FALSE else TRUE, targets = which(colnames(df) == "endTime")),
          list(visible = if (timeFormat() == "military") TRUE else FALSE, targets = which(colnames(df) == "startTimeFormatted")),
          list(visible = if (timeFormat() == "military") TRUE else FALSE, targets = which(colnames(df) == "endTimeFormatted"))
        )
      )
    )
  })
  
  
  #add a download option for the creel schedule
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("creel_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Export the data used in the table to a CSV file
      write.csv(creelSchedule(), file, row.names = FALSE)
    }
  ) 
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##Data Validation Tab code##########
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  
  
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##Analysis Tab code##########
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  
  
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##User's guide Tab code##########
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  
  
  
  
  
}