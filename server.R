#Packages to be loaded and other files/settings used in entire app############################
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
#speciesinfo <- read.fst("speciesinfo.fst", as.data.table = TRUE)
# creelData <- read.fst(creeldata.fst, as.data.table=T)#this will eventually read in the main creel database


function(input, output, session) {
  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Creel Planning Tab code####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  ####initial setup stuff#################
  # Observe changes in the selected input and update choices
  observe({
    updateSelectizeInput(session, "lakeSelector", 
                         choices = c(unique(lakeinfo$Lake.Name)),  
                         options = list(onInitialize = I('function() { this.setValue(""); }'))) #prevents selecting first item in list by default
  })

  # # Code to handle site probability for weighting by site for Will Sims...I'm saving in case we later want to implement this####
  #     siteProbData <- reactiveVal(data.frame(Site.Name = c("Dam","Lower Dam","Simp-Watts WMU","Gore Landing","River Rd"),
  #                                           Probability = c(0.29, 0.22, 0.28, 0.16, 0.05)))
  #     output$ProbDataTable <- renderDT({
  #       datatable(siteProbData(), colnames = c("Creel Section", "Probability"), editable = TRUE, options = list(dom = 't'))
  #     })
  # 
  #     observeEvent(input$addRow, {
  #       newData <- siteProbData()
  #       newRow <- data.frame(Site.Name = "", Probability = NA)
  #       newData <- rbind(newData, newRow)
  #       siteProbData(newData)
  #     })
  # 
  #     observeEvent(input$ProbDataTable_cell_edit, {
  #       info <- input$ProbDataTable_cell_edit
  #       newData <- siteProbData()
  #       newValue <- info$value
  # 
  #       # Check if the column being edited is 'Probability' and convert to numeric
  #       if (info$col == 2) {
  #         newValue <- as.numeric(newValue)
  #         if (is.na(newValue)) {
  #           showNotification("Please enter a valid numeric value for Probability.", type = "error")
  #           return()}
  #       }
  #       newData[info$row, info$col] <- newValue
  #       siteProbData(newData)
  #     })
  # 
  #     output$totalProb <- renderText({
  #       df <- siteProbData()
  #       totalProb <- sum(as.numeric(df$Probability), na.rm = TRUE)
  #       paste("Total Prob (should=1):", totalProb)
  #     })####
  
  # Function to toggle time format
  timeFormat <- reactiveVal("military") #This will get reversed with ToggleTimeFormat button initiates, so really is setting default to "standard"
  lakeSelected <- reactiveVal("noLake") #create reactive value to track if a lake has been selected yet
  numSections <- reactiveVal(NULL)  #create a reactive value for random sections
  
  observeEvent(input$lakeSelector, {
    ifelse(is.na(input$lakeSelector) | input$lakeSelector == "",
           lakeSelected("No lake selected"), 
           lakeSelected(input$lakeSelector)
    )
  })
  
  # Observe Event for Random Lake Section Column
  ## below observer should be disabled if use weighted table to pick random section
  observeEvent(input$num_sections, {
    # Check if input$num_sections is a valid number before updating
    if (!is.null(input$num_sections) && input$num_sections > 0) {
      numSections(input$num_sections)
    } else {
      numSections(NULL) # Clear numSections if input is invalid
    }
  })
  
  # Add function to pick end date as 1 year after start date when user enters a start date in the date selection box.
  observeEvent(input$start_date, {
    updateDateInput(session, "end_date", value = as.Date(input$start_date) + years(1) - days(1))
  })
  
  ########Building a calendar of available dates###########
  calendarFull <- reactive({
    calendar <- tibble(date = seq(as.Date(input$start_date), as.Date(input$end_date), by = "1 day"))

    # Calculate DST start and end dates for each year in the date range
    calendar <- calendar %>%
      mutate(
        year = year(date),
        month = month(date),
        dayWeek = wday(date),
        strata = ifelse(dayWeek %in% c(1, 7), "weekend", "weekday"), #1 = Sunday, 7 = Saturday
        group = 1,
        dst_start = make_date(year, 3, 1) + (weeks(1) + days(7 - wday(make_date(year, 3, 1)))) + days(1), # Second Sunday in March
        dst_end = make_date(year, 11, 1) + (days(7 - wday(make_date(year, 11, 1)))), # First Sunday in November
        is_dst = ifelse(date >= dst_start & date < dst_end, TRUE, FALSE) # TRUE if within DST period
      )
    
    calendar %>%
      expand_grid(shift = c("morning", "afternoon")) %>%
      mutate(
        strata = ifelse(dayWeek == 6 & shift == "afternoon", "weekend", strata), #6 = Saturday
        quarter = case_when(
          month %in% c(12, 1, 2) ~ "Winter",
          month %in% c(3, 4, 5) ~ "Spring",
          month %in% c(6, 7, 8) ~ "Summer",
          month %in% c(9, 10, 11) ~ "Fall"
        ),
        Qyear = case_when(
          quarter == "Winter" & month %in% c(1, 2) ~ year - 1,
          TRUE ~ year
        ),
        strata = case_when(
          month == 7 & day(date) == 4 ~ "weekend", 
          month == 7 & day(date) == 3 & (dayWeek %in% c(2, 3)) ~ "weekend", 
          month == 7 & day(date) == 5 & (dayWeek %in% c(5, 6)) ~ "weekend",
          month == 5 & day(date) == max(day(date[month == 5 & dayWeek == 2])) ~ "weekend", 
          month == 9 & day(date) == min(day(date[month == 9 & dayWeek == 2])) ~ "weekend",
          TRUE ~ strata
        ),
        startTime = case_when(
          month == 1 & shift == "morning" ~ 0800,
          month == 2 & shift == "morning" ~ 0730,
          month == 3 & shift == "morning" ~ if_else(is_dst, 0700, 0800),
          month == 4 & shift == "morning" ~ 0700,
          month == 5 & shift == "morning" ~ 0630,
          month == 6 & shift == "morning" ~ 0630,
          month == 7 & shift == "morning" ~ 0630,
          month == 8 & shift == "morning" ~ 0700,
          month == 9 & shift == "morning" ~ 0730,
          month == 10 & shift == "morning" ~ 0730,
          month == 11 & shift == "morning" ~ if_else(is_dst, 0700, 0800),
          month == 12 & shift == "morning" ~ 0730,
          month == 1 & shift == "afternoon" ~ 1300,
          month == 2 & shift == "afternoon" ~ 1300,
          month == 3 & shift == "afternoon" ~ if_else(is_dst, 1300, 1400),
          month == 4 & shift == "afternoon" ~ 1330,
          month == 5 & shift == "afternoon" ~ 1330,
          month == 6 & shift == "afternoon" ~ 1400,
          month == 7 & shift == "afternoon" ~ 1400,
          month == 8 & shift == "afternoon" ~ 1400,
          month == 9 & shift == "afternoon" ~ 1400,
          month == 10 & shift == "afternoon" ~ 1330,
          month == 11 & shift == "afternoon" ~ if_else(is_dst, 1230, 1330),
          month == 12 & shift == "afternoon" ~ 1230
        ),
        endTime = case_when(
          month == 1 & shift == "morning" ~ 1300,
          month == 2 & shift == "morning" ~ 1300,
          month == 3 & shift == "morning" ~ if_else(is_dst, 1300, 1400),
          month == 4 & shift == "morning" ~ 1330,
          month == 5 & shift == "morning" ~ 1330,
          month == 6 & shift == "morning" ~ 1400,
          month == 7 & shift == "morning" ~ 1400,
          month == 8 & shift == "morning" ~ 1400,
          month == 9 & shift == "morning" ~ 1400,
          month == 10 & shift == "morning" ~ 1330,
          month == 11 & shift == "morning" ~ if_else(is_dst, 1230, 1330),
          month == 12 & shift == "morning" ~ 1230,
          month == 1 & shift == "afternoon" ~ 1800,
          month == 2 & shift == "afternoon" ~ 1830,
          month == 3 & shift == "afternoon" ~ if_else(is_dst, 1900, 2000),
          month == 4 & shift == "afternoon" ~ 2000,
          month == 5 & shift == "afternoon" ~ 2030,
          month == 6 & shift == "afternoon" ~ 2100,
          month == 7 & shift == "afternoon" ~ 2100,
          month == 8 & shift == "afternoon" ~ 2030,
          month == 9 & shift == "afternoon" ~ 2000,
          month == 10 & shift == "afternoon" ~ 1900,
          month == 11 & shift == "afternoon" ~ if_else(is_dst, 1230, 1330),
          month == 12 & shift == "afternoon" ~ 1230
        )
      ) %>%
      select(-dst_start, -dst_end) # Remove intermediate DST columns after usage
  })
  
  ########Selecting dates from calendarFull that will be used for sampling or alternates and create pressure counts###########
  creelSchedule <- reactive({
    sample_percentage <- input$sample_percentage / 100  # Convert percentage to fraction
    
    ##Dan added for Will Sims to calc based on target hr/wk rather than % of days...saving this in case we want to implement it later in shiny app
    # target_days <- calendarFull() %>%       
    #   group_by(Qyear, quarter, month) %>%
    #   summarise(mShift_len=mean(Shift_len), 
    #             nDay=n()/2,
    #             Targ_days=(nDay/7 * input$sample_hPerWk) / mShift_len,
    #             target_weekdays = round(Targ_days * 4 / (4 + 5)),
    #             target_weekends = round(Targ_days * 5 / (4 + 5)),
    #             .groups = "drop_last")
    
    target_days <- calendarFull() %>%
      group_by(Qyear, quarter) %>%
      summarise(
        start_date2 = min(date),
        end_date2 = max(date),
        totDays = as.numeric(difftime(end_date2, start_date2, units = "days")),
        target_weekdays = round(totDays * 4 / (4 + 5) * sample_percentage),
        target_weekends = round(totDays * 5 / (4 + 5) * sample_percentage),
        .groups = "drop_last" # silences warning message
      )
    
    quarterly_dates <- list() # Create a blank list that will fill in loop below
    
    # Sample the required number of weekday and weekend days
    for (i in 1:nrow(target_days)) {
      #Weekends
        # pick proper number of dates, but don't select shift...will prevent ever picking 2 shifts on same date below
          weekends_selected_dates <- calendarFull() %>%
            filter(strata == "weekend",
                   Qyear == target_days$Qyear[i],
                   quarter == target_days$quarter[i]) %>%
            distinct(date) %>% 
            slice_sample(n = target_days$target_weekends[i])
        #now assign shift and other needed information to above picked proper number of dates
          sampled_weekends <- calendarFull() %>%
            inner_join(weekends_selected_dates, by="date")%>%
            group_by(date) %>%
            slice_sample(n=1) %>% 
            mutate(alternate_status = "Standard") # Mark these as standard
      
      #Weekdays
        #pick proper number of dates, but don't select shift...will prevent ever picking 2 shifts on same date below
          weekdays_selected_dates <- calendarFull() %>%
            filter(!(date %in% unique(sampled_weekends$date))) %>% #Removes any Friday dates already picked for PM shife (which is a weekend) so we don't also pick an am shift that day (as weekday)
            filter(strata == "weekday",
                    Qyear == target_days$Qyear[i],
                    quarter == target_days$quarter[i]) %>%
            distinct(date) %>% 
            slice_sample(n = target_days$target_weekdays[i])
        #now assign shift and other needed information to above picked proper number of dates
          sampled_weekdays <- calendarFull() %>%
            inner_join(weekdays_selected_dates, by="date")%>%
            group_by(date) %>%
            slice_sample(n=1) %>% 
            mutate(alternate_status = "Standard") # Mark these as standard

       
      # Add two extra weekend and weekday days per 10% effort for each stratum and mark them as alternates
        #Alternates-Weekends
          extra_weekend_dates <- calendarFull() %>%
            filter(!(date %in% unique(sampled_weekends$date))) %>% 
            filter(!(date %in% unique(sampled_weekdays$date))) %>% #needed because Fri pm is a "weekend" and Fri am is "weekday" and we don't want 2 shifts to ever be picked on a Friday
            filter(strata == "weekend",
                   Qyear == target_days$Qyear[i],
                   quarter == target_days$quarter[i]) %>%
            distinct(date) %>%
            slice_sample(n = max(1, round(target_days$target_weekends[i]*0.445,0)))
          
          extra_weekends <- calendarFull() %>%
            inner_join(extra_weekend_dates, by="date")%>%
            group_by(date) %>%
            slice_sample(n=1) %>%
            mutate(alternate = TRUE, alternate_status = "Alternate") 
         
        #Alternates-Weekdays
          extra_weekday_dates <- calendarFull() %>%
            filter(!(date %in% unique(sampled_weekdays$date))) %>% #prevents duplicate days from being picked
            filter(!(date %in% unique(sampled_weekends$date))) %>% #needed because Fri pm is a "weekend" and Fri am is "weekday" and we don't want 2 shifts to ever be picked on a Friday
            filter(!(date %in% unique(extra_weekends$date))) %>% #also prevent Fri am/pm issue with alternates already picked
            filter(strata == "weekday",
              Qyear == target_days$Qyear[i],
              quarter == target_days$quarter[i]) %>%
            distinct(date) %>%
            slice_sample(n = max(1, round(target_days$target_weekdays[i]*0.445,0)))#add 45% of target days rather n=2...will now pick more alternates at higher sampling efforts than 10%. Max with 1 or this ensures it always picks at least one alternate

          extra_weekdays <- calendarFull() %>%
            inner_join(extra_weekday_dates, by="date")%>%
            group_by(date) %>%
            slice_sample(n=1) %>%
            mutate(alternate = TRUE, alternate_status = "Alternate") 

      # Combine the sampled days with the extra days
      quarterly_dates[[i]] <- bind_rows(
        sampled_weekdays,
        extra_weekdays,
        sampled_weekends,
        extra_weekends
      )
    }
    
    # Bind list from above loop into full data frame
        quarterly_dates <- bind_rows(quarterly_dates) 
    
        
    #Create pressure count information
    quarterly_dates %>%
          #Dan changed next line to use monthly information for Will Sims version
          # select(dayWeek, shift, strata, month, date, quarter, startTime, endTime, Shift_len, alternate_status) %>%
      select(dayWeek, shift, strata, date, Qyear,quarter, startTime, endTime, alternate_status) %>%
      arrange(date) %>%
      mutate(
        direction = sample(c("clockwise", "counterclockwise"), n(), replace = TRUE, prob = c(0.5, 0.5)),
        pressure_count_1 = random_start_time <- ceiling(runif(n()) * (endTime - startTime - 300)) + startTime,
        pressure_count_2 = ceiling((as.numeric(pressure_count_1) + 100) + runif(n()) * (as.numeric(endTime) - 
                                                                                          (as.numeric(pressure_count_1) + 200))),
        pressure_count_1_hour = floor(pressure_count_1 / 100),
        pressure_count_1_minute = (pressure_count_1 %% 100) * 0.6, # Convert to minutes
        pressure_count_2_hour = floor(pressure_count_2 / 100),
        pressure_count_2_minute = (pressure_count_2 %% 100) * 0.6,
      ) %>%
      # Add dayName column to switch from numeric to alpha
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
        dayWeek = NULL
      )
  })
  
  ########Toggle time format when the button is clicked###############
  #use button input to set desired time format
  observeEvent(input$toggleTimeFormat, {
    if (timeFormat() == "military") {
      timeFormat("standard")
    } else {
      timeFormat("military")
    }
  })
  
  # Convert times between standard/military time as toggleTimeFormat is changed
  creelScheduleTimeFormated <- reactive({
    # Get the base data
    creel_data_timeformatted <- creelSchedule() %>%
      mutate(
        pressureCount1 = case_when(
          timeFormat() == "standard" ~
            ifelse(!is.na(pressure_count_1),
                   paste0(
                     ifelse(pressure_count_1_hour > 12, pressure_count_1_hour - 12, pressure_count_1_hour), ":",
                     ifelse(pressure_count_1_minute < 10, paste0(0, floor(pressure_count_1_minute)), floor(pressure_count_1_minute)),
                     ifelse(pressure_count_1_hour >= 12, " pm", " am")
                   ), NA_character_
            ),
          TRUE ~
            ifelse(!is.na(pressure_count_1),
                   paste0(
                     ifelse(pressure_count_1_hour < 10, paste0("0", pressure_count_1_hour), pressure_count_1_hour), 
                     ifelse(pressure_count_1_minute < 10, paste0("0", floor(pressure_count_1_minute)), floor(pressure_count_1_minute))
                   ), NA_character_
            )
        ),
        pressureCount2 = case_when(
          timeFormat() == "standard" ~
            ifelse(!is.na(pressure_count_2),
                   paste0(
                     ifelse(pressure_count_2_hour > 12, pressure_count_2_hour - 12, pressure_count_2_hour), ":",
                     ifelse(pressure_count_2_minute < 10, paste0(0, floor(pressure_count_2_minute)), floor(pressure_count_2_minute)),
                     ifelse(pressure_count_2_hour >= 12, " pm", " am")
                   ), NA_character_
            ),
          TRUE ~
            ifelse(!is.na(pressure_count_2),
                   paste0(
                     ifelse(pressure_count_2_hour < 10, paste0("0", pressure_count_2_hour), pressure_count_2_hour), 
                     ifelse(pressure_count_2_minute < 10, paste0("0", floor(pressure_count_2_minute)), floor(pressure_count_2_minute))
                   ), NA_character_
            )
        ),
        startHour = as.numeric(substr(as.character(startTime), 1, nchar(as.character(startTime)) - 2)),
        startMinute = as.numeric(substr(as.character(startTime), nchar(as.character(startTime)) - 1, nchar(as.character(startTime)))),
        start_time = case_when(
          timeFormat() == "standard" ~
            ifelse(!is.na(startTime),
                   paste0(
                     ifelse(startHour > 12, startHour - 12, startHour), ":",
                     ifelse(startMinute < 10, paste0(0, floor(startMinute)), floor(startMinute)),
                     ifelse(startHour >= 12, " pm", " am")
                   ), NA_character_
            ),
          TRUE ~
            ifelse(!is.na(startTime),
                   paste0(
                     ifelse(startHour < 10, paste0("0", startHour), startHour), 
                     ifelse(startMinute < 10, paste0("0", floor(startMinute)), floor(startMinute))
                   ), NA_character_
            )
        ),
        endHour = as.numeric(substr(as.character(endTime), 1, nchar(as.character(endTime)) - 2)),
        endMinute = as.numeric(substr(as.character(endTime), nchar(as.character(endTime)) - 1, nchar(as.character(endTime)))),
        end_time = case_when(
          timeFormat() == "standard" ~
            ifelse(!is.na(endTime),
                   paste0(
                     ifelse(endHour > 12, endHour - 12, endHour), ":",
                     ifelse(endMinute < 10, paste0(0, floor(endMinute)), floor(endMinute)),
                     ifelse(endHour >= 12, " pm", " am")
                   ), NA_character_
            ),
          TRUE ~
            ifelse(!is.na(endTime),
                   paste0(
                     ifelse(endHour < 10, paste0("0", endHour), endHour), 
                     ifelse(endMinute < 10, paste0("0", floor(endMinute)), floor(endMinute))
                   ), NA_character_
            )
        )
      )
      return(creel_data_timeformatted)
    })
    
  ########Adding random lake section selection and formatting for final table###########
  creelScheduleFinalFormated <- reactive({
    creel_data <- creelScheduleTimeFormated() %>% 
      mutate(lakeName = lakeSelected(),
            #below line used only if creel site selection is done with weighted randomization
            # siteProbData2 <- siteProbData() %>% filter(!is.na(Site.Name) & !is.na(Probability))
            # 
            #below line could be used to produce weighted random sites selection
            # random_section = sample(siteProbData2$Site.Name, size = n(), replace = TRUE, prob = 
            #     as.numeric(siteProbData2$Probability)),
            # if (!is.null(numSections()) && numSections() > 0) {
            #   creel_data <- creel_data %>%
            #     mutate(random_section = sample(1:numSections(), n(), replace = TRUE))
            # }
          random_section = case_when(!is.null(numSections()) && numSections() > 0 ~
                                   sample(1:numSections(), n(), replace = TRUE),
                                   TRUE ~ NA),
          date=format(date,"%m/%d/%Y"),
          quarter_sort = case_when(
                                   quarter=="Spring" ~ 1,
                                   quarter=="Summer" ~ 2,
                                   quarter=="Fall" ~ 3,
                                   quarter=="Winter" ~ 4)
        ) %>% 
          #Dan modified below to include month...retaining in case we get to adding function to target sampling hrs/wk
          # arrange(month, desc(alternate_status), date) %>% 
          # select(lakeName, Qyear, quarter, date, dayName, strata, random_section, shift, start_time, end_time, Shift_len,
          #        pressureCount1, pressureCount2, direction, alternate_status) %>%
          arrange(Qyear, quarter_sort, desc(alternate_status), as.Date(date, "%m-%d-%Y")) %>%
          select(lakeName, quarter, date, dayName, strata, random_section, shift, start_time, end_time,
                 pressureCount1, pressureCount2, direction, alternate_status) %>%
          # Rename columns with prettier names that have spaces for display
              #below version should be used if doing monthly stratification and you want shift duration in final table
              # rename(
              #   'Lake'='lakeName', 'Quarter'='quarter', 'Date'='date', 'Day of week'='dayName',
              #   'Day type'='strata', 'Creel section'='random_section','Shift'='shift',
              #   'Start creel'='start_time', 'End creel'='end_time', 'Shift duration'='Shift_len',
              #   'Time of pressure count 1'='pressureCount1',
              #   'Time of pressure count 2'= 'pressureCount2', 'Pressure count direction'='direction', 'Alternate status'= 'alternate_status'
              # )
            rename(
              'Lake'='lakeName', 'Quarter'='quarter', 'Date'='date', 'Day of week'='dayName',
              'Day type'='strata', 'Creel section'='random_section','Shift'='shift',
              'Start creel'='start_time', 'End creel'='end_time', 'Time of pressure count 1'='pressureCount1',
              'Time of pressure count 2'= 'pressureCount2', 'Pressure count direction'='direction', 'Alternate status'= 'alternate_status'
            )
        
        creel_data
    })
  
  
  ########Render the table and download button###########
  output$creel_table <- renderDT({
    df <- creelScheduleFinalFormated()
    
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
#Data Validation Tab code##########
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  
  
  
  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Analysis Tab code##########
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  
  
  
  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#User's guide Tab code##########
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  
  
  
  
#end function...no code below this line  
}