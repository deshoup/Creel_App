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
    updateSelectizeInput(session, "lakeSelector", 
                         choices = c(unique(lakeinfo$Lake.Name)),  
                         options = list(onInitialize = I('function() { this.setValue(""); }'))) #prevents selecting first item in list by default
  })
  
  # Function to toggle time format
  timeFormat <- reactiveVal("military") #This will get reversed with ToggleTimeFormat button initiates, so really is setting default to "standard"
  lakeSelected <- reactiveVal("noLake") #create reactive value to track if a lake has been selected yet
  
  observeEvent(input$lakeSelector, {
    ifelse(is.na(input$lakeSelector) | input$lakeSelector == "",
           lakeSelected("No lake selected"), 
           lakeSelected(input$lakeSelector)
    )
  })
  
  
  # Add function to pick end date as 1 year after start date when user enters a start date in the date selection box.
  observeEvent(input$start_date, {
    updateDateInput(session, "end_date", value = as.Date(input$start_date) + years(1))
  })
  
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
        strata = ifelse(dayWeek %in% c(1, 7), "weekend", "weekday"), #1 = Sunday, 7 = Saturday
        group = 1
      ) %>%
      expand_grid(shift = c("morning", "afternoon")) %>%
      mutate(
        strata = ifelse(dayWeek == 6 & shift == "afternoon", "weekend", strata), #6 = Saturday
        quarter = case_when(
          month %in% c(12, 1, 2) ~ "Winter",
          month %in% c(3, 4, 5) ~ "Spring",
          month %in% c(6, 7, 8) ~ "Summer",
          month %in% c(9, 10, 11) ~ "Fall"
        ),
        Qyear = case_when( #used to prevent having 2 extra sample dates for the 2 caledar years of winter
          quarter == "Winter" & month %in% c(1, 2) ~ year - 1,
          TRUE ~ year #all other cases besides winter in month 1 or 2
        ),
        strata = case_when(
          month == 7 & day(date) == 4 ~ "weekend", #accounts for 4th of July
          month == 7 & day(date) == 3 & (dayWeek %in% c(2, 3)) ~ "weekend", #4th of July weird Mon and Tues
          month == 7 & day(date) == 5 & (dayWeek %in% c(5, 6)) ~ "weekend", #4th of July weird Thur and Fri
          month == 5 & day(date) == max(day(date[month == 5 & dayWeek == 2])) ~ "weekend",
          #Memorial day or the last Monday in May
          month == 9 & day(date) == min(day(date[month == 9 & dayWeek == 2])) ~ "weekend",
          #Labor day or first Monday in Sept
          TRUE ~ strata
        ),
        #adding the start and end times from creel calendar
        #I combined the times for Mar and Nov given for daylight savings to match calendar full
        startTime = case_when(
          month == 1 & shift == "morning" ~ 0800,
          month == 2 & shift == "morning" ~ 0730,
          month == 3 & shift == "morning" ~ if_else(date >= floor_date(as.Date(paste(year, "03", "10", sep = "-")), "week", week_start = 1) + days(6), 0700, 0800),
          month == 4 & shift == "morning" ~ 0700,
          month == 5 & shift == "morning" ~ 0630,
          month == 6 & shift == "morning" ~ 0630,
          month == 7 & shift == "morning" ~ 0630,
          month == 8 & shift == "morning" ~ 0700,
          month == 9 & shift == "morning" ~ 0730,
          month == 10 & shift == "morning" ~ 0730,
          month == 11 & shift == "morning" ~ if_else(date < floor_date(as.Date(paste(year, "11", "01", sep = "-")), "week", week_start = 1) + days(7 - wday(floor_date(as.Date(paste(year, "11", "01", sep = "-")), "week", week_start = 1) + 1)), 0800, 0700),
          month == 12 & shift == "morning" ~ 0730,
          month == 1 & shift == "afternoon" ~ 1300,
          month == 2 & shift == "afternoon" ~ 1300,
          month == 3 & shift == "afternoon" ~ if_else(date >= floor_date(as.Date(paste(year, "03", "10", sep = "-")), "week", week_start = 1) + days(6), 1300, 1400),
          month == 4 & shift == "afternoon" ~ 1330,
          month == 5 & shift == "afternoon" ~ 1330,
          month == 6 & shift == "afternoon" ~ 1400,
          month == 7 & shift == "afternoon" ~ 1400,
          month == 8 & shift == "afternoon" ~ 1400,
          month == 9 & shift == "afternoon" ~ 1400,
          month == 10 & shift == "afternoon" ~ 1330,
          month == 11 & shift == "afternoon" ~ if_else(date < floor_date(as.Date(paste(year, "11", "01", sep = "-")), "week", week_start = 1) + days(7 - wday(floor_date(as.Date(paste(year, "11", "01", sep = "-")), "week", week_start = 1) + 1)), 1330, 1230),
          month == 12 & shift == "afternoon" ~ 1230
        ),
        endTime = case_when(
          month == 1 & shift == "morning" ~ 1300,
          month == 2 & shift == "morning" ~ 1300,
          month == 3 & shift == "morning" ~ if_else(date >= floor_date(as.Date(paste(year, "03", "10", sep = "-")), "week", week_start = 1) + days(6), 1300, 1400),
          month == 4 & shift == "morning" ~ 1330,
          month == 5 & shift == "morning" ~ 1330,
          month == 6 & shift == "morning" ~ 1400,
          month == 7 & shift == "morning" ~ 1400,
          month == 8 & shift == "morning" ~ 1400,
          month == 9 & shift == "morning" ~ 1400,
          month == 10 & shift == "morning" ~ 1330,
          month == 11 & shift == "morning" ~ if_else(date < floor_date(as.Date(paste(year, "11", "01", sep = "-")), "week", week_start = 1) + days(7 - wday(floor_date(as.Date(paste(year, "11", "01", sep = "-")), "week", week_start = 1) + 1)), 1330, 1230),
          month == 12 & shift == "morning" ~ 1230,
          month == 1 & shift == "afternoon" ~ 1800,
          month == 2 & shift == "afternoon" ~ 1830,
          month == 3 & shift == "afternoon" ~ if_else(date >= floor_date(as.Date(paste(year, "03", "10", sep = "-")), "week", week_start = 1) + days(6), 1900, 2000),
          month == 4 & shift == "afternoon" ~ 2000,
          month == 5 & shift == "afternoon" ~ 2030,
          month == 6 & shift == "afternoon" ~ 2100,
          month == 7 & shift == "afternoon" ~ 2100,
          month == 8 & shift == "afternoon" ~ 2030,
          month == 9 & shift == "afternoon" ~ 2000,
          month == 10 & shift == "afternoon" ~ 1900,
          month == 11 & shift == "afternoon" ~ if_else(date < floor_date(as.Date(paste(year, "11", "01", sep = "-")), "week", week_start = 1) + days(7 - wday(floor_date(as.Date(paste(year, "11", "01", sep = "-")), "week", week_start = 1) + 1)), 1830, 1730),
          month == 12 & shift == "afternoon" ~ 1730
        )
      )
  })
  
  # sample shifts and extra shifts to get the final output
  # building a function to randomly sample dates from calendarFull
  
  creelSchedule <- reactive({
    sample_percentage <- input$sample_percentage / 100  # Convert percentage to fraction
    
    target_days <- calendarFull() %>% group_by(Qyear, quarter) %>%
      summarise(
        start_date2 = min(date),
        end_date2 = max(date),
        totDays = as.numeric(difftime(end_date2, start_date2, units = "days")),
        target_weekdays = round(totDays * 4 / (4 + 5) * sample_percentage),
        target_weekends = round(totDays * 5 / (4 + 5) * sample_percentage),
        .groups = "drop_last" #silences warning message...this is the default behavior but adding this makes less output in Console
      )
    
    quarterly_dates <- list() # Create a blank list that will fill in loop below
    
    for (i in 1:nrow(target_days)) {
      quarterly_dates <- bind_rows(
        quarterly_dates,
        calendarFull() %>%
          filter(
            shift %in% c("morning", "afternoon"),
            strata == "weekday",
            Qyear == target_days$Qyear[i],
            quarter == target_days$quarter[i]
          ) %>%
          slice_sample(n = target_days$target_weekdays[i] + 2) %>% # Add 2 extra days
          ungroup(),
        
        calendarFull() %>%
          filter(
            shift %in% c("morning", "afternoon"),
            strata == "weekend",
            year == target_days$Qyear[i],
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
        pressure_count_1 = random_start_time <- ceiling(runif(n()) * (endTime - startTime - 300)) + startTime,
        #disabling below lines because with some regularity it creates missing pressure count times when 
        #runif(n()) was over 0.9949, resulting in a pressure_count time that was exactly (endTime - 300)
        #Note...there is still time to get both counts in if pressure count occurs at exactly (endTime - 300),
        #Also should never be time with blank endTime or endTime - startTime < 300
        #   pressure_count_1 =case_when(
        #   !is.na(endTime) & (endTime - startTime) >= 300 ~ {
        #     random_start_time <- ceiling(runif(n()) * (endTime - startTime - 300)) + startTime
        #     ifelse(random_start_time < endTime - 300, random_start_time, NA_real_)
        #   },
        #   TRUE ~ NA_real_
        # ),
        #generating a pressure count at least 1 hour (100) after p. count 1 and 2h (200) before endTime
        pressure_count_2 = ceiling((as.numeric(pressure_count_1) + 100) + runif(n()) * (as.numeric(endTime) - 
                                                                                          (as.numeric(pressure_count_1) + 200))),
        # pressure_count_2 = case_when(
        #   !is.na(pressure_count_1) &
        #     (as.numeric(endTime) - as.numeric(pressure_count_1)) >= 200 ~
        #     ceiling((as.numeric(pressure_count_1) + 100) + runif(n()) * (as.numeric(endTime) - 
        #                    (as.numeric(pressure_count_1) + 200))),
        #   TRUE ~ NA_real_
        # ),
        #converting to portions of 60 minutes rather than 100
        pressure_count_1_hour = floor(pressure_count_1 / 100),
        pressure_count_1_minute = (pressure_count_1 %% 100) * 0.6, #%% is used to find the percent out of 100, the 0.6 converts it to minutes out of 60 available
        pressure_count_2_hour = floor(pressure_count_2 / 100),
        pressure_count_2_minute = (pressure_count_2 %% 100) * 0.6,
        #Moving below code to external reactive so can toggle between standard/military time without recalculating days/times
        # #here I am converting all of this to military time separated by a colon
        # 
        # pressureCount1 = case_when(
        #   timeFormat() == "military" ~
        #     ifelse(!is.na(pressure_count_1),
        #            paste0(ifelse(pressure_count_1_hour > 12, pressure_count_1_hour - 12, pressure_count_1_hour), ":", ifelse(pressure_count_1_minute < 10,
        #                                                                                                                      paste0("0", floor(pressure_count_1_minute)), floor(pressure_count_1_minute))), NA_character_),
        #   TRUE ~  ifelse(!is.na(pressure_count_1),
        #                  paste0(ifelse(pressure_count_1_hour > 12, pressure_count_1_hour - 0, pressure_count_1_hour), ifelse(pressure_count_1_minute < 10,
        #                                                                                                                       paste0("0", floor(pressure_count_1_minute)), floor(pressure_count_1_minute))), NA_character_)
        # ),
        # pressureCount2 = case_when(
        #   timeFormat() == "military" ~
        #     ifelse(!is.na(pressure_count_2),
        #            paste0(ifelse(pressure_count_2_hour > 12, pressure_count_2_hour - 12, pressure_count_2_hour), ":", ifelse(pressure_count_2_minute < 10,
        #                                                                                                                      paste0("0", floor(pressure_count_2_minute)), floor(pressure_count_2_minute))), NA_character_),
        #   TRUE ~ ifelse(!is.na(pressure_count_2),
        #                 paste0(ifelse(pressure_count_2_hour > 12, pressure_count_2_hour - 0, pressure_count_2_hour), ifelse(pressure_count_2_minute < 10,
        #                                                                                                                      paste0("0", floor(pressure_count_2_minute)), floor(pressure_count_2_minute))), NA_character_)
        # )
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
        #Moving below code to external reactive so can toggle between standard/military time without recalculating days/times
        # pressure_count_1_hour = NULL,
        # pressure_count_1_minute = NULL,
        # pressure_count_2_hour = NULL,
        # pressure_count_2_minute = NULL,
        # pressure_count_1 = NULL,
        # pressure_count_2 = NULL,
        dayWeek = NULL
      )
    # ) %>%
    
    #Moving below code to external reactive so can toggle between standard/military time without recalculating days/times
    # mutate(
    #   # Convert startTime and endTime to character type
    #   startTime = as.character(startTime),
    #   endTime = as.character(endTime),
    #   # Add leading zeros to hour if necessary
    #   startTime = ifelse(nchar(startTime) == 3, paste0("0", startTime), startTime),
    #   endTime = ifelse(nchar(endTime) == 3, paste0("0", endTime), endTime),
    #   # Extract hour and minute components from startTime and endTime
    #   startHour = substr(startTime, 1, 2),
    #   startMinute = substr(startTime, 3, 4),
    #   endHour = substr(endTime, 1, 2),
    #   endMinute = substr(endTime, 3, 4),
    #   # Subtract 12 from startHour and endHour if they are greater than 12
    #   startHour = ifelse(as.numeric(startHour) > 12, as.character(as.numeric(startHour) - 12), startHour),
    #   endHour = ifelse(as.numeric(endHour) > 12, as.character(as.numeric(endHour) - 12), endHour),
    #   # Combine hour and minute components with a colon
    #   # ###Dan modified below
    #   #startTimeFormatted1 = paste(startHour, startMinute, sep = ":"),
    #   startTimeFormatted = case_when(shift=="afternoon" ~ paste(paste(startHour, startMinute, sep = ":"), " pm"),
    #                                  shift=="morning" & startHour>="12" ~ paste(paste(startHour, startMinute, sep = ":"), " pm"),
    #                                  TRUE ~ paste(paste(startHour, startMinute, sep = ":"), " am")),
    #   ###end Dan modified code
    #   endTimeFormatted = paste(endHour, endMinute, sep = ":")
    # ) %>%
    # # Drop intermediate columns
    # select(-startHour, -startMinute, -endHour, -endMinute) %>%
    # mutate(lakeName = case_when(is.na(input$lakeSelector), "No lake selected", input$lakeSelector))  # Use input$lakeSelector to get the selected lake
    
    
  })
  
  # Toggle time format when the button is clicked
  observeEvent(input$toggleTimeFormat, {
    if (timeFormat() == "military") {
      timeFormat("standard")
    } else {
      timeFormat("military")
    }
    
  })
  
  #convert times between standard/military time as toggleTimeFormat is changed
  creelScheduleTimeFormated <- reactive({
    creelSchedule() %>% mutate(
      pressureCount1 = case_when(timeFormat() == "standard" ~
                                   ifelse(!is.na(pressure_count_1),
                                          paste0(ifelse(pressure_count_1_hour > 12, pressure_count_1_hour - 12, pressure_count_1_hour), ":",
                                                 ifelse(pressure_count_1_minute < 10, paste0(0, floor(pressure_count_1_minute)), floor(pressure_count_1_minute)),
                                                 ifelse(pressure_count_1_hour > 12, " pm", " am")), NA_character_),
                                 TRUE ~  ifelse(!is.na(pressure_count_1),
                                                paste0(ifelse(pressure_count_1_hour < 10, paste0("0", pressure_count_1_hour), pressure_count_1_hour), 
                                                       ifelse(pressure_count_1_minute < 10, paste0("0", floor(pressure_count_1_minute)), 
                                                              floor(pressure_count_1_minute))), NA_character_)
      ),
      
      pressureCount2 = case_when(timeFormat() == "standard" ~
                                   ifelse(!is.na(pressure_count_2),
                                          paste0(ifelse(pressure_count_2_hour > 12, pressure_count_2_hour - 12, pressure_count_2_hour), ":",
                                                 ifelse(pressure_count_2_minute < 10, paste0(0, floor(pressure_count_2_minute)), floor(pressure_count_2_minute)),
                                                 ifelse(pressure_count_2_hour > 12, " pm", " am")), NA_character_),
                                 TRUE ~  ifelse(!is.na(pressure_count_2),
                                                paste0(ifelse(pressure_count_2_hour < 10, paste0("0", pressure_count_2_hour), pressure_count_2_hour), 
                                                       ifelse(pressure_count_2_minute < 10, paste0("0", floor(pressure_count_2_minute)), 
                                                              floor(pressure_count_2_minute))), NA_character_)
      ),
      
      startHour = as.numeric(substr(as.character(startTime), 1, nchar(as.character(startTime))-2)),
      startMinute = as.numeric(substr(as.character(startTime), nchar(as.character(startTime))-1, nchar(as.character(startTime)))),
      start_time = case_when(timeFormat() == "standard" ~
                               ifelse(!is.na(startTime),
                                      paste0(ifelse(startHour > 12, startHour - 12, startHour), ":",
                                             ifelse(startMinute < 10, paste0(0, floor(startMinute)), floor(startMinute)),
                                             ifelse(startHour > 12, " pm", " am")), NA_character_),
                             TRUE ~  ifelse(!is.na(startTime),
                                            paste0(ifelse(startHour < 10, paste0("0", startHour), startHour), 
                                                   ifelse(startMinute < 10, paste0("0", floor(startMinute)), 
                                                          floor(startMinute))), NA_character_)
      ),
      
      endHour = as.numeric(substr(as.character(endTime), 1, nchar(as.character(endTime))-2)),
      endMinute = as.numeric(substr(as.character(endTime), nchar(as.character(endTime))-1, nchar(as.character(endTime)))),
      end_time = case_when(timeFormat() == "standard" ~
                             ifelse(!is.na(endTime),
                                    paste0(ifelse(endHour > 12, endHour - 12, endHour), ":",
                                           ifelse(endMinute < 10, paste0(0, floor(endMinute)), floor(endMinute)),
                                           ifelse(endHour > 12, " pm", " am")), NA_character_),
                           TRUE ~  ifelse(!is.na(endTime),
                                          paste0(ifelse(endHour < 10, paste0("0", endHour), endHour), 
                                                 ifelse(endMinute < 10, paste0("0", floor(endMinute)), 
                                                        floor(endMinute))), NA_character_),
      ),
      lakeName = lakeSelected()
    ) %>% 
      select(lakeName, quarter, date, dayName, strata, shift, start_time, end_time, 
             pressureCount1, pressureCount2, direction) %>% 
      #rename columns with prettier names that have spaces for display
      rename('Lake'='lakeName', 'Quarter'='quarter', 'Date'='date','Day of week'='dayName', 'Day type'='strata', 'Shift'='shift',
             'Start creel'='start_time', 'End creel'='end_time', 'Time of pressure count 1'='pressureCount1',
             'Time of pressure count 2'= 'pressureCount2', 'Pressure count direction'='direction')
  })
  
  # Render the table
  output$creel_table <- renderDT({
    df <- creelScheduleTimeFormated()
    
    datatable(
      df,
      options = list(
        lengthMenu = list(50, 100, 200, 300, 400),
        pageLength = 100,
        columnDefs = list(list(width = '8%', targets = c(7,8,9,10)),list(width = '11%', targets = c(3,11)))#set preferred column widths to get long titles to line wrap without getting too wide
      )
    )
  })
  
  
  #add a download option for the creel schedule
  # output$downloadData <- downloadHandler(
  #   filename = function() {
  #     paste("creel_data_", Sys.Date(), ".csv", sep = "")
  #   },
  #   content = function(file) {
  #     # Export the data used in the table to a CSV file
  #     write.csv(creelSchedule(), file, row.names = FALSE)
  #   }
  # ) 
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Creel_schedule_", unique(creelScheduleTimeFormated()$Lake), "_", input$start_date, 
            "_to_", input$end_date, "_generated_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Export the data used in the table to a CSV file
      write.csv(creelScheduleTimeFormated(), file, row.names = FALSE)
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