#
# This is the server logic of a Shiny web application. 
#
# You can run the application by clicking 'Run App' above.
#
#to install a package, follow the following format:
#install.packages('plyr')
#or open the packages.R file

library(shiny)
library(plyr)
library(anytime)
library(ggplot2)
library(scales)
library(data.table)
library(tidyr)

# SWITCH THIS TO "testing = TRUE" WHEN YOU WOULD LIKE TO RUN THE APP WITH testing.csv 
testing = FALSE

# CSV names
csv_database = "firebase_data_shining_heat_4904_nonredundant.csv"
csv_database_test = "tests/testing.csv"

# Don't read column labels of CSV - data for sequence of events charts
if(testing == FALSE) {
  data = read.csv(csv_database, na.strings=c(""," ","NA"))
  nonredundant_data = read.table(csv_database, sep=",",skip = 1)
}  else {
  data = read.csv(csv_database_test, na.strings=c(""," ","NA"))
  nonredundant_data = read.table(csv_database_test, sep=",",skip = 1)
}
session_ids <- as.character(unique(nonredundant_data[,"V2"], incomparables = FALSE, MARGIN = 1, fromLast = FALSE))
num_sessions <- NROW(session_ids)
op <- options(digits.secs = 6)
# Set the timezone
Sys.setenv(TZ='EST')

colour_reference <- list(
  "New Keyframe" = "darkgreen",
  "Copy Main" = "darkturquoise",
  "Paste" = "darkslategray",
  "Delete Keyframe" = "red",
  "Copy Example" = "hotpink2",
  "Example Selected: v4" = "lightcoral",
  "Example Selected: v3" = "plum1",
  "Example Selected: sineExample" = "gold2",
  "Example Selected: randomExample" = "pink",
  "Example Selected: v10_09_1_8" = "aquamarine2",
  "Example Selected: v10_augmented" = "purple",
  "Example Selected: textures" = "blue",
  "Frame Selected: example" = "royalblue1",
  "Frame Selected: main" = "skyblue1", 
  "Playback Unmuted" = 'cadetblue2',
  "Playback On" = "limegreen",
  "Dragging Main Playhead" = "orange",
  "Dragging Example Playhead" = "mediumpurple3",
  "Dragging Keyframe" = "sienna2",
  "UNDO" = "grey",
  "REDO" = "yellowgreen",
  "Start Task" = "lightslategray",
  "Other" = "magenta2")


session_id_with_behaviour_counts = as.data.frame(table(data$session))
session_id_with_behaviour_counts = session_id_with_behaviour_counts[order(-session_id_with_behaviour_counts$Freq),]
session_id_with_behaviour_counts = unite(session_id_with_behaviour_counts,"Session Ids with Behaviour Counts",c(Var1,Freq),sep = " Count:", remove=TRUE)


# Define server logic required to draw charts



shinyServer(function(input, output, session) {
  
  observeEvent(input$reset,{
    updateDateRangeInput(session,"dateRange1",
                         start = min(as.Date(data$datetime)),
                         end = max(as.Date(data$datetime)),
                         min = min(as.Date(data$datetime)),
                         max = max(as.Date(data$datetime)))
  }
  )
  
  observe({
    #Date variables passed to and from the filters
    updateDateRangeInput(session,"dateRange1",
                         start = max(input$dateRange1[1],min(as.Date(data$datetime))),
                         end = min(input$dateRange1[2],max(as.Date(data$datetime))),
                         min = max(input$dateRange1[1],min(as.Date(data$datetime))),
                         max = min(input$dateRange1[2],max(as.Date(data$datetime))))


     updateSelectInput(session,"select_id",
                         choices = session_id_with_behaviour_counts)

  })
  
  output$dates <- function(){
    min_date = min(as.Date(data$datetime))
    max_date = max(as.Date(data$datetime))
    
    return(c(min_date,max_date))
  }


 

  output$actionPerSession <- renderPlot({
    #**** number of actions per session histograph****#
    start_date = input$dateRange1[1]
    end_date = input$dateRange1[2]
    y = data[as.Date(data$datetime)>=start_date & as.Date(data$datetime)<=end_date,]
    unique_session= unique(data[as.Date(data$datetime)>=start_date & as.Date(data$datetime)<=end_date,]$sessions)

    #check for data existing within the range 
    number_of_rows = NROW(y)
    if (number_of_rows == 0) {
      par(mar = c(0,0,0,0))
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n')
      
      text(x = 0.34, y = 0.9, paste("No data available within selected date range"), 
           cex = 1.5, col = "black", adj=0.5)
    }
    else {
      #count the number of rows that have the same session name
       y = count(data[as.Date(data$datetime)>=start_date & as.Date(data$datetime)<=end_date,], 'session')
       hist(y[,2], 
          main= "Histogram for Actions per session", 
          xlab = "Number of Actions per Session", 
          ylab = "Unique Session Count", 
          border = "black", 
         col = "pink", 
          breaks = 30, 
         xaxt='n'
            )
    
       #used to create a tick on the x axis for every other bucket
       mround <- function(x,base){ 
         base*round(x/base) + base
       } 
       round_max  = mround(max(y[,2]),15) / 15 
       xtick<-seq(0, round_max*15, by = round_max)
       axis(side=1, at=xtick, labels = FALSE)
       text(x=xtick,
            par("usr")[3], 
            labels = xtick,
            offset = 1.2, 
            srt = 45,
            pos = 1,
            xpd = TRUE)
   
    }
  }
  )
  
  
  output$actionfrequency  <- renderPlot({
    
    #pulling start and end dates
    start_date = input$dateRange1[1]
    end_date = input$dateRange1[2]
    values=data[as.Date(data$datetime)>=start_date & as.Date(data$datetime)<=end_date,]$value   
    
    newvalue=as.character(values)
    counts = count(newvalue)
    num_row=NROW(counts)
    
    
    if (num_row <= 10) {
      par(mar = c(0,0,0,0))
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n')
      
      text(x = 0.34, y = 0.9, paste("Not enough data available within selected date range"), 
           cex = 1.5, col = "black", adj=0.5)
    }
    else{
      #creating vectors for graphs
      alpha=dim(counts)[1]-9
      beta = dim(counts)[1]
      a=counts$freq[order(counts$freq)[alpha:beta]]
      b=as.character(counts$x[order(counts$freq)[alpha:beta]])
      
      
      #plot top 10 actions
      par(mar=c(5,16,4,2))
      par(las=2)
      barplot(a, main="Action Frequency",horiz=TRUE, xlab="number of actions" ,
              names.arg=b)
      
    }
    
    
    
    
  })

  
  output$sessionQuartileLine<-renderPlot({
    #filtering the data on user input dateRange 
    start_date = input$dateRange1[1]
    end_date = input$dateRange1[2]
    
    #set up data 
    y = data[as.Date(data$datetime)>=start_date & as.Date(data$datetime)<=end_date,]
    number_of_rows = NROW(y)
    if (number_of_rows == 0) {
      par(mar = c(0,0,0,0))
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n')
      
      text(x = 0.34, y = 0.9, paste("No data available within selected date range"), 
           cex = 1.5, col = "black", adj=0.5)
    }

    else {
        #quartile reutrn a matrix of one session for the defined percentage 
        quartile_sample <- function(quartile, b  ){
          index  = which.min(abs(b$freq-quartile)) 
          session_id_of_quartile =  b$session[index]
          count = b$freq[index]
          count_vec =  seq(count, 1, -1)
          quartile_action_mat <- y[y$session == session_id_of_quartile,]
          quartile_action_mat$actions_remaining <- count_vec
          return (quartile_action_mat) 
        }
       
        
        #calculate the time difference
        y$to_second <- (y$start_time)/1000
        y$start_seconds<- anytime(y$to_second)
        
        y$diff_time <- difftime(y$datetime,y$start_seconds,units="secs")
        y$diff_time_ceiling <- ceiling(y$diff_time)
        
        #find actions per session
        b = count(y, "session")
        st = sort.list(b$freq)
        #get quantile 
        q = quantile(b$freq)
        #assign quantile count
        q25 = as.numeric(as.character(q[1]))
        q50 = as.numeric(as.character(q[2]))
        q75 = as.numeric(as.character(q[3]))
        q100 = as.numeric(as.character(q[4]))
        #set up sorted datatable and sorted frequency list
        dt = data.table(st, val = st)
        setattr(dt, "sorted", "st")
        #get 25 quartile information 
        q25_action = quartile_sample(q25, b)
        
        ##get 50 quartile information
        q50_action = quartile_sample(q50, b)
        
        ##get 75 quartile information
        q75_action = quartile_sample(q75,b)
        ##get 100 quartile information
        q100_action = quartile_sample(q100,b)
        
        #### setting up plots to print ####
        colors <- rainbow(4) 
        plot(q50_action$diff_time_ceiling,q50_action$actions_remaining,
             type = "o", 
             ylim = c(0,max(q100_action$actions_remaining)),
             xlim = c(0,max(q100_action$diff_time_ceiling)), 
             xlab = "Seconds", 
             ylab = "Actions Remaining in Session (Burndown)", 
             col = colors[2], 
             main= "Quartile User Behavior" )
        
        linetype <- c(1:4) 
        lines(q75_action$diff_time_ceiling, q75_action$actions_remaining, 
              type="o",
              col=colors[3]) 
        lines(q100_action$diff_time_ceiling, q100_action$actions_remaining, 
              type="o",
              col=colors[4]) 
        lines(q25_action$diff_time_ceiling, q25_action$actions_remaining, 
              type="o",
              col=colors[1]) 
        
        
        legend("topright", 
               legend=c("25% User", "50% User", "75% User", "100% User"),
               col=colors,
               lty=1, 
               cex=0.8)
  
      }
    }
  )

  output$sessionOverTime <- renderPlot({
    #***** number of sessions over time*****#
    #Time units will be variable and dependent on user input  ***#
    
    #filtering the data on user input dateRange 
    start_date = input$dateRange1[1]
    end_date = input$dateRange1[2]
   x = data[!duplicated(data$session) & as.Date(data$datetime)>=start_date & as.Date(data$datetime)<=end_date,]
   number_of_rows = NROW(x)
   if (number_of_rows == 0) {
     par(mar = c(0,0,0,0))
     plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n')
     
     text(x = 0.34, y = 0.9, paste("No data available within selected date range"), 
          cex = 1.5, col = "black", adj=0.5)
   }
   else {
     #creating new columns called date, time, week, month
     x$Date <- as.Date(x$datetime)
     x$Week <- cut(x$Date,
                           breaks = "week",
                           start.on.monday = TRUE)
     #counting sessions per week 
      m = count(x, 'Week')
      len_m = length(m[,2])
    
      #extending boundaries below
       plot(m[,2],
          main= "Sessions Per Week", 
          type = "o" ,
          col = "coral2",
          xlab = "", 
          ylab = "Number of sessions", 
          xaxt='n'
     )
       
     axis(1, at=1:len_m, labels=m[,1],  las = 2)
     title(xlab="Weeks" ,mgp=c(7,1,0))
   
     }
   }
   )
  
  output$actionPerSecond  <- renderPlot({
   #filter the data 
   start_date = input$dateRange1[1]
   end_date = input$dateRange1[2]
   y = data[as.Date(data$datetime)>=start_date & as.Date(data$datetime)<=end_date,]
   number_of_rows = NROW(y)
   if (number_of_rows == 0) {
     par(mar = c(0,0,0,0))
     plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n')
     
     text(x = 0.34, y = 0.9, paste("No data available within selected date range"), 
          cex = 1.5, col = "black", adj=0.5)
   }
   else {
     #calculate the time difference
      y$to_second <- (y$start_time)/1000
      y$start_seconds<- anytime(y$to_second)
     
     y$diff_time <- difftime(y$datetime,y$start_seconds,units="secs")
     y$diff_time_ceiling <- ceiling(y$diff_time)
     #count actions per second
     n = count(y, 'diff_time_ceiling')
     #plot as a line graph
     plot(n[,2],
          main= "Average Actions Frequency Per Session (Max 150 seconds)", 
          type = "l",
          col = "deeppink4",
          xlab = "Seconds", 
          ylab = "Number of actions per second",
          xlim = c(0,150) 
     )
   }

 })
  sessionDurationInfo <- function(){
    
    #filter the data 
    start_date = input$dateRange1[1]
    end_date = input$dateRange1[2]
    data = data[as.Date(data$datetime)>=start_date & as.Date(data$datetime)<=end_date,]
    summary_data = data[as.Date(data$datetime)>=start_date & as.Date(data$datetime)<=end_date,]
    number_of_rows = NROW(summary_data)
    if (number_of_rows == 0) {
      return(NULL)
    }
    else {
    counter = 0 
    
    for(i in 2:nrow(data)){
      if(data[i,2]!=data[i-1,2]){
        counter = counter+1
      }
    }
    summary_data = data.frame(matrix('',nrow=counter,ncol=4),stringsAsFactors=FALSE)
    colnames(summary_data) = c("session","start_time","end_time","session_duration")
    counter=0
    
    for(i in 2:nrow(data)){
      if(data[i,2]!=data[(i-1),2]){
        counter = counter+1
        summary_data[counter,1]=toString((data[i,2]))
        summary_data[counter,2]=((data[i,6]))
        summary_data[counter,3]=((data[i,7]))
        summary_data[counter,4]=((as.numeric(data[(i),7]))-as.numeric((data[(i),6])))/1000
      }
      if(i == nrow(data)){
        counter = counter+1
        summary_data[counter,1]=toString((data[i,2]))
        summary_data[counter,2]=((data[i,6]))
        summary_data[counter,3]=((data[i,7]))
        summary_data[counter,4]=((as.numeric(data[(i),7]))-as.numeric((data[(i),6])))/1000
      }
        
    }
    summary_data$num_duration <- as.numeric(as.character(summary_data$session_duration))
    summary_data$rounded_second <- ceiling(summary_data$num_duration)
    n = count(summary_data,"rounded_second")
    
    return(n)
    
    }
  }
  
  output$sessionDuration  <- renderPlot({
    
    n = sessionDurationInfo()
    
    if (empty(n)){
      par(mar = c(0,0,0,0))
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n')
      
      text(x = 0.34, y = 0.9, paste("No data available within selected date range"), 
           cex = 1.5, col = "black", adj=0.5)
    } else{
    barplot(n[,2], 
            main= "Session Duration - Barplot", 
            xlab = "Frequency", 
            ylab = "Session Length in seconds",
            names.arg = n[,1],
            border = "black", 
            col = "deepskyblue3",
            horiz = TRUE, 
            ylim = c(0,50)
            
    )
    }
  })
  
  output$sessionDurationTypeH  <- renderPlot({
    
    n = sessionDurationInfo()
    if( empty(n)){
      par(mar = c(0,0,0,0))
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n')
      
      text(x = 0.34, y = 0.9, paste("No data available within selected date range"), 
           cex = 1.5, col = "black", adj=0.5)
    } else{
    plot(n[,2], 
         type = "h",
         main= "Session Duration - Plot", 
         xlab = "Session Length in seconds", 
         ylab = "Frequency",
         xlim = c(0,50)
         
    )
    }
  })
  
  #Generate the sequence chart
  output$SequencePlot  <- renderPlot(height = 700, width = 1200, units="px", {
    
     # Plot sequence chart 
    session_id = input$select_id
    plot_sequence_chart(gsub(" Count\\:.*","",session_id))
    
  })
  
   
       ######################## Function to produce a sequence plot for a given session id  ###################################
       
       plot_sequence_chart <- function(id) {
         
         # Get all the rows for this session id
         session_rows = as.data.frame(nonredundant_data[nonredundant_data[,2]==id,])
         session_rows = na.omit(session_rows)
         num_rows = NROW(session_rows)
         
         # Can only plot if we have at least one event
         if(num_rows != 0) {
           
           # We use the first row to identify animation because the value is identical across all rows of a session id
           animation = session_rows[1,4]    
           
           session_start_string = session_rows[1,9]
           session_start = as.POSIXct(session_start_string)
           
           #If there's only 1 event in the session, assign an arbitrary session length of 0.5 seconds
           if(num_rows == 1) {
             session_end_string = strftime(session_start + 0.5)
           }
           else {
             session_end_string = session_rows[num_rows, 9]
           }
           session_end = as.POSIXct(session_end_string)
           session_start_date = as.Date(session_start_string)
           time_span = difftime(session_end, session_start, units = "secs")
           if(time_span >= 0) {
             # For events that happen at a single point in time we will use this as a time span
             # We also use this for the last event regardless of its nature because we have no other way of calculating end time
             time_for_event = time_span/140
             
             # We need to adjust the timespan and session end time to account for the time we'll be adding 
             session_end_string = strftime(session_end + time_for_event)
             time_span = time_span + time_for_event
             
             # Calculate spacing for our x axis ticks (we use 10 ticks)
             x_axis_tick_breaks = paste(as.character(time_span/10), "sec", sep=" ")
             
             ################################## Deal with main/exmample frame view events ###########################################
             
             selected_frame_rows = as.data.frame(subset(session_rows, grepl("VTICON_SELECT_", session_rows$V8), drop = FALSE))
             
             if(NROW(selected_frame_rows) != 0) {
               
               timestamps_frame = as.matrix(selected_frame_rows[,9])
               timestamps_frame = rbind(timestamps_frame, as.matrix(session_end_string))
               num_timestamps_frame = NROW(timestamps_frame)
               
               frame_select_events = as.matrix(selected_frame_rows[,8])
               frame_select_events = sapply(frame_select_events, gsub, pattern = "VTICON_SELECT_", replacement = "Frame Selected: ", fixed = TRUE)
               
               event_types_frame = matrix('Frame Selected', nrow = NROW(frame_select_events), ncol = 1 )
               
               xmin_frame = timestamps_frame[1:num_timestamps_frame-1]
               xmax_frame = timestamps_frame[2:num_timestamps_frame]
               ymax_frame = matrix(0.33, nrow = num_timestamps_frame-1, ncol = 1)
             }
             # If neither frames were selected during the session we don't want to add any events to our sequence chart
             else {
               frame_select_events = matrix(nrow = 0, ncol = 1)
               event_types_frame = matrix(nrow = 0, ncol = 1)
               xmin_frame = matrix(nrow = 0, ncol = 1)
               xmax_frame = matrix(nrow = 0, ncol = 1)
               ymax_frame = matrix(nrow = 0, ncol = 1)
             }
             
             ################################## Deal with playback start and end events ###########################################
             
             play_rows = as.data.frame(subset(session_rows, grepl("PLAYBACK_SETPLAY_true|PLAYBACK_PLAYEND", session_rows$V8), drop = FALSE))
             play_start_rows = as.data.frame(subset(session_rows, grepl("PLAYBACK_SETPLAY_true", session_rows$V8), drop = FALSE))
             play_end_rows = as.data.frame(subset(session_rows, grepl("PLAYBACK_SETPLAY_false|PLAYBACK_PLAYEND", session_rows$V8), drop = FALSE))
             
             # If the number of start play events equals the number of stop play events (expected), no need to manipulate data
             if(NROW(play_start_rows) == NROW(play_end_rows) && NROW(play_start_rows) != 0) {
               play_events = matrix('Playback On', nrow = NROW(play_start_rows), ncol = 1 )
               event_types_play = matrix('Playback On', nrow = NROW(play_start_rows), ncol = 1 )
               xmin_play = as.matrix(play_start_rows[,9])
               xmax_play = as.matrix(play_end_rows[,9])
               ymax_play = matrix(0.33, nrow = NROW(play_start_rows), ncol = 1)
             } 
             # If the number of start play events is one greater than the number of stop play events (unusual but has happened),
             # we should add a fake play end event at the session end time
             else if(NROW(play_start_rows) == NROW(play_end_rows) + 1  && NROW(play_start_rows) != 0) {
               play_events = matrix('Playback On', nrow = NROW(play_start_rows), ncol = 1 )
               event_types_play = matrix('Playback On', nrow = NROW(play_start_rows), ncol = 1 )
               xmin_play = as.matrix(play_start_rows[,9])
               xmax_play = as.matrix(play_end_rows[,9])
               xmax_play = rbind(xmax_play, as.matrix(session_end_string))
               ymax_play = matrix(0.33, nrow = NROW(play_start_rows), ncol = 1)
             }
             # If neither frames were selected during the session we don't want to add any events to our sequence chart
             # Also catch any uncrompehsible data here
             else {
               play_events = matrix(nrow = 0, ncol = 1)
               event_types_play = matrix(nrow = 0, ncol = 1)
               xmin_play = matrix(nrow = 0, ncol = 1)
               xmax_play = matrix(nrow = 0, ncol = 1)
               ymax_play = matrix(nrow = 0, ncol = 1)
             }
             
             ################################ Deal with adjsuting main playhead start and end events ###########################################
             
             drag_start_rows = as.data.frame(subset(session_rows, grepl("STARTDRAG", session_rows$V8), drop = FALSE))
             drag_stop_rows = as.data.frame(subset(session_rows, grepl("STOPDRAG", session_rows$V8), drop = FALSE))
             
             # 3 scenarios: 1. No start events 2. Equal number of start and stop events 3. One more start event
             
             xmin_playhead_main = as.character(subset(drag_start_rows$V9, grepl("STARTDRAG_PLAYHEAD_main", drag_start_rows$V8), drop = FALSE))
             xmax_playhead_main = as.character(subset(drag_stop_rows$V9, grepl("STARTDRAG_PLAYHEAD_main", drag_start_rows$V8), drop = FALSE))
             
             # Check to make sure we don't have Scenario 1 and check to make sure we have either Scenario 2 or 3
             if(NROW(xmin_playhead_main) != 0 && sum(is.na(xmax_playhead_main)) <=1) { 
               # Add a fake stopdrag event timestamp at the session end time if the last startdrag doesn't have an accompanying end drag
               if(is.na(xmax_playhead_main[NROW(xmax_playhead_main)])) {
                 xmax_playhead_main[NROW(xmax_playhead_main)] <- as.character(session_end_string)
               }
               
               ymax_playhead_main = matrix(0.33, nrow = NROW(xmin_playhead_main), ncol = 1)
               playhead_main_events = matrix('Dragging Main Playhead', nrow = NROW(xmin_playhead_main), ncol = 1 )
               event_types_playhead_main = matrix('Playhead Drag', nrow = NROW(xmin_playhead_main), ncol = 1 )
               
             }
             # No adjusting playhead rows. Also catch uninterprettable data
             else {
               playhead_main_events = matrix(nrow = 0, ncol = 1)
               event_types_playhead_main = matrix(nrow = 0, ncol = 1)
               xmin_playhead_main = matrix(nrow = 0, ncol = 1)
               xmax_playhead_main = matrix(nrow = 0, ncol = 1)
               ymax_playhead_main = matrix(nrow = 0, ncol = 1)
             }
             
             ################################ Deal with playhead example dragging events ###########################################
             
             drag_start_rows = as.data.frame(subset(session_rows, grepl("STARTDRAG", session_rows$V8), drop = FALSE))
             drag_stop_rows = as.data.frame(subset(session_rows, grepl("STOPDRAG", session_rows$V8), drop = FALSE))
             
             # 3 scenarios: 1. No start events 2. Equal number of start and stop events 3. One more start event
             
             xmin_playhead_example = as.character(subset(drag_start_rows$V9, grepl("STARTDRAG_PLAYHEAD_example", drag_start_rows$V8), drop = FALSE))
             xmax_playhead_example = as.character(subset(drag_stop_rows$V9, grepl("STARTDRAG_PLAYHEAD_example", drag_start_rows$V8), drop = FALSE))
             
             if(NROW(xmin_playhead_example) != 0 && sum(is.na(xmax_playhead_example)) <=1) { 
               # Add a fake stopdrag event timestamp at the session end time if the last startdrag doesn't have an accompanying end drag
               if(is.na(xmax_playhead_example[NROW(xmax_playhead_example)])) {
                 xmax_playhead_example[NROW(xmax_playhead_example)] <- as.character(session_end_string)
               }
               ymax_playhead_example = matrix(0.33, nrow = NROW(xmin_playhead_example), ncol = 1)
               playhead_example_events = matrix('Dragging Example Playhead', nrow = NROW(xmin_playhead_example), ncol = 1 )
               event_types_playhead_example = matrix('Playhead Drag', nrow = NROW(xmin_playhead_example), ncol = 1 )
             }
             # No adjusting playhead rows. Also catch uninterprettable data
             else {
               playhead_example_events = matrix(nrow = 0, ncol = 1)
               event_types_playhead_example = matrix(nrow = 0, ncol = 1)
               xmin_playhead_example = matrix(nrow = 0, ncol = 1)
               xmax_playhead_example = matrix(nrow = 0, ncol = 1)
               ymax_playhead_example = matrix(nrow = 0, ncol = 1)
             }
             
             ################################ Deal with keyframe dragging events ###########################################
             
             drag_start_rows = as.data.frame(subset(session_rows, grepl("STARTDRAG", session_rows$V8), drop = FALSE), stringsAsFactors=FALSE)
             drag_stop_rows = as.data.frame(subset(session_rows, grepl("STOPDRAG", session_rows$V8), drop = FALSE), stringsAsFactors=FALSE)
             
             # 3 scenarios: 1. No start events 2. Equal number of start and stop events 3. One more start event (at the end)
             
             xmin_keyframe = as.character(subset(drag_start_rows$V9, grepl("STARTDRAG_KEYFRAME", drag_start_rows$V8), drop = FALSE), stringsAsFactors=FALSE)
             xmax_keyframe = as.character(subset(drag_stop_rows$V9, grepl("STARTDRAG_KEYFRAME", drag_start_rows$V8), drop = FALSE), stringsAsFactors=FALSE)
             
             if(NROW(xmin_keyframe) != 0 && sum(is.na(xmax_keyframe) <=1)) { 
               # Add a fake stopdrag event timestamp at the session end time if the last startdrag doesn't have an accompanying end drag
               if(is.na(xmax_keyframe[NROW(xmax_keyframe)])) {
                 xmax_keyframe[NROW(xmax_keyframe)] <- as.character(session_end_string)
               }
               ymax_keyframe = matrix(0.33, nrow = NROW(xmin_keyframe), ncol = 1)
               keyframe_events = matrix('Dragging Keyframe', nrow = NROW(xmin_keyframe), ncol = 1 )
               event_types_keyframe = matrix('Keyframe Drag', nrow = NROW(xmin_keyframe), ncol = 1 )
             }
             # No keyframe dragging rows. Also catch uninterprettable data
             else {
               keyframe_events = matrix(nrow = 0, ncol = 1)
               event_types_keyframe = matrix(nrow = 0, ncol = 1)
               xmin_keyframe = matrix(nrow = 0, ncol = 1)
               xmax_keyframe = matrix(nrow = 0, ncol = 1)
               ymax_keyframe = matrix(nrow = 0, ncol = 1)
             }
             
             ################################## Deal with selected_example events ###########################################
             
             selected_example_rows = as.data.frame(subset(session_rows, grepl("EXAMPLE_SELECT_", session_rows$V8), drop = FALSE))
             # If no examples were selected we want to show the default example as selected for the entire session
             if(NROW(selected_example_rows) == 0) {
               example_select_events = as.matrix("Example Selected: v4")
               event_types_examples = matrix('Example Selected', nrow = 1, ncol = 1 )
               xmin_examples = session_start_string
               xmax_examples = session_end_string
               ymax_examples = 0.33
             } 
             else {
               # If the first event is an example selection (i.e. the session starts at the moment an example is selected),
               # we do not need to add an event to convey that an example is selected by default
               if(selected_example_rows[1,9] == session_start_string) {
                 timestamps_examples = as.matrix(selected_example_rows[,9]) 
                 example_select_events = as.matrix(selected_example_rows[,8])
               }
               # If the first event is not an example selection we must add a fake example selection event (and an 
               # associated timestamp) to express the default example that is selected
               else {
                 # If the first example selection event is the default example ("v4"), we don't want to 
                 # show two events for the same example selection so instead, we just change the
                 # start time of the first event to the session start time
                 if(selected_example_rows[1,8] == "EXAMPLE_SELECT_v4") {
                   timestamps_examples = as.matrix(selected_example_rows[,9])
                   timestamps_examples[1,1] = as.matrix(session_start_string)
                   example_select_events = as.matrix(selected_example_rows[,8])
                 } 
                 # If the first example selection event is not selecting the default example (v4), we add in 
                 # a fake example selection event at the session start time to show that v4 is selected by default
                 # until a different example is selected
                 else {
                   timestamps_examples =  as.matrix(session_start_string)
                   timestamps_examples = rbind(timestamps_examples, as.matrix(selected_example_rows[,9]))    
                   example_select_events = as.matrix("EXAMPLE_SELECT_v4")
                   example_select_events = rbind(example_select_events, as.matrix(selected_example_rows[,8]))
                 }
               }
               
               timestamps_examples = rbind(timestamps_examples, as.matrix(session_end_string))
               
               num_timestamps = NROW(timestamps_examples)
               
               xmin_examples = timestamps_examples[1:num_timestamps-1]
               xmax_examples = timestamps_examples[2:num_timestamps]
               ymax_examples = matrix(0.33, nrow = num_timestamps-1, ncol = 1)
               
               example_select_events = sapply(example_select_events, gsub, pattern = "EXAMPLE_SELECT_", replacement = "Example Selected: ", fixed = TRUE)
               event_types_examples = matrix('Example Selected', nrow = NROW(example_select_events), ncol = 1 )
             }
             
             ################################## Deal with mute set to true/false ###########################################
             
             mute_false_rows = as.data.frame(subset(session_rows, grepl("PLAYBACK_MUTE_false", session_rows$V8), drop = FALSE))
             mute_true_rows = as.data.frame(subset(session_rows, grepl("PLAYBACK_MUTE_true", session_rows$V8), drop = FALSE))
             
             # Since PLAYBACK_MUTE always starts as true and the value is logged when the checkbox is selected/unselected,
             # there are only three possible scenarios: 1. Muted is true the whole session (no logged event) 2. There are equal
             # numbers of muted = true and muted = false events 3. There is exactly one more muted = false event than muted = true events
             
             # If the number of PLAYBACK_MUTE_false events equals the number of PLAYBACK_MUTE_true events we have the
             # same number of start and end times so there is no need to manipulate the data
             if(NROW(mute_false_rows) == NROW(mute_true_rows) && NROW(mute_false_rows) != 0 ) {
               mute_events = matrix('Playback Unmuted', nrow = NROW(mute_false_rows), ncol = 1 )
               event_types_mute = matrix('Playback Unmuted', nrow = NROW(mute_false_rows), ncol = 1 )
               xmin_mute = as.matrix(mute_false_rows[,9])
               xmax_mute = as.matrix(mute_true_rows[,9])
               ymax_mute = matrix(0.33, nrow = NROW(mute_false_rows), ncol = 1)
             } 
             # If there is one more PLAYBACK_MUTE_false event than PLAYBACK_MUTE_true events, we will add a fake
             # PLAYBACK_MUTE_true event timestamp at the session end time so we have equal numbers of start and end times 
             else if(NROW(mute_false_rows) == NROW(mute_true_rows) + 1 && NROW(mute_false_rows) != 0 ) {
               mute_events = matrix('Playback Unmuted', nrow = NROW(mute_false_rows), ncol = 1 )
               event_types_mute = matrix('Playback Unmuted', nrow = NROW(mute_false_rows), ncol = 1 )
               xmin_mute = as.matrix(mute_false_rows[,9])
               xmax_mute = as.matrix(mute_true_rows[,9])
               xmax_mute = rbind(xmax_mute, as.matrix(session_end_string))
               ymax_mute = matrix(0.33, nrow = NROW(mute_false_rows), ncol = 1)
             }
             # If the mute checkbox is never changed (i.e. is muted the entire session), don't produce a strip in the chart
             # Also catch any uncrompehsible data here
             else {
               mute_events = matrix(nrow = 0, ncol = 1)
               event_types_mute = matrix(nrow = 0, ncol = 1)
               xmin_mute = matrix(nrow = 0, ncol = 1)
               xmax_mute = matrix(nrow = 0, ncol = 1)
               ymax_mute = matrix(nrow = 0, ncol = 1)
             } 
             
             ################################## Deal with other events ###########################################
             
             rows_other = as.data.frame(
               subset(session_rows,
                      !grepl("VTICON_SELECT_", session_rows$V8) & 
                        !grepl("EXAMPLE_SELECT_", session_rows$V8) &
                        !grepl("PLAYBACK_MUTE_", session_rows$V8) &
                        !grepl("PLAYBACK_PLAYEND", session_rows$V8) &
                        !grepl("PLAYBACK_SETPLAY_", session_rows$V8) &
                        !grepl("STARTDRAG", session_rows$V8) &
                        !grepl("STOPDRAG", session_rows$V8),
                      drop = FALSE), stringsAsFactors=FALSE)
             
             
             if(NROW(rows_other) != 0) {
               events_other = matrix(rows_other$V8)
               events_other[grep("COPY_main", events_other)] <- "Copy Main"
               events_other[grep("COPY_example", events_other)] <- "Copy Example"
               events_other[grep("PASTE", events_other)] <- "Paste"
               events_other[grep("VTICON_DELETEKEYFRAMES", events_other)] <- "Delete Keyframe"
               events_other[grep("VTICON_NEWKEYFRAME", events_other)] <- "New Keyframe"
               events_other[grep("START_TASK", events_other)] <- "Start Task"
               
               
               event_types_other = matrix('User Actions', nrow = NROW(events_other), ncol = 1 )
               xmin_other =  as.POSIXct(rows_other[,9])
               xmax_other = xmin_other
               
               for(i in 1:NROW(xmax_other)) {
                 # Make the end time time_for_event seconds after the start time or if the next event start
                 # is less than time_for_event seconds away make the end time the next event start time - 
                 # for the last event just add time_for_event seconds
                 if(i == NROW(xmax_other)) {
                   xmax_other[i] = xmin_other[i] + time_for_event
                 } 
                 else {
                   current_event_start = xmin_other[i]
                   next_event_start = xmin_other[i + 1]
                   time_diff = difftime(next_event_start, current_event_start, units = "secs")
                   if(time_diff > time_for_event) {
                     xmax_other[i] = xmin_other[i] + time_for_event
                   } 
                   else {
                     xmax_other[i] = xmin_other[i] + time_diff
                   }
                 }
               }
               xmin_other = strftime(xmin_other)
               xmax_other = strftime(xmax_other)
               ymax_other = matrix(0.33, nrow = NROW(events_other), ncol = 1)
             } 
             else {
               events_other = matrix(nrow = 0, ncol = 1)
               event_types_other = matrix(nrow = 0, ncol = 1)
               xmin_other = matrix(nrow = 0, ncol = 1)
               xmax_other = matrix(nrow = 0, ncol = 1)
               ymax_other = matrix(nrow = 0, ncol = 1)
             }
             
             ####################################### Create data frame to pass to ggplot ###################################
             
             event_types = rbind(
               as.matrix(event_types_other), 
               as.matrix(event_types_examples), 
               as.matrix(event_types_frame),
               as.matrix(event_types_mute),
               as.matrix(event_types_play),
               as.matrix(event_types_playhead_main),
               as.matrix(event_types_playhead_example),
               as.matrix(event_types_keyframe))
             
             events = rbind(
               as.matrix(events_other), 
               as.matrix(example_select_events), 
               as.matrix(frame_select_events),
               as.matrix(mute_events),
               as.matrix(play_events),
               as.matrix(playhead_main_events),
               as.matrix(playhead_example_events),
               as.matrix(keyframe_events))
             
             xmin = as.POSIXct(rbind(
               as.matrix(xmin_other), 
               as.matrix(xmin_examples), 
               as.matrix(xmin_frame),
               as.matrix(xmin_mute),
               as.matrix(xmin_play),
               as.matrix(xmin_playhead_main),
               as.matrix(xmin_playhead_example),
               as.matrix(xmin_keyframe)))
             
             xmax = as.POSIXct(rbind(
               as.matrix(xmax_other), 
               as.matrix(xmax_examples), 
               as.matrix(xmax_frame),
               as.matrix(xmax_mute),
               as.matrix(xmax_play),
               as.matrix(xmax_playhead_main),
               as.matrix(xmax_playhead_example),
               as.matrix(xmax_keyframe)))
             
             ymax = rbind(
               as.matrix(ymax_other), 
               as.matrix(ymax_examples), 
               as.matrix(ymax_frame),
               as.matrix(ymax_mute),
               as.matrix(ymax_play),
               as.matrix(ymax_playhead_main),
               as.matrix(ymax_playhead_example),
               as.matrix(ymax_keyframe))
             
             data = data.frame(
               
               subject = event_types,
               Events = events,
               xmin=xmin,
               xmax=xmax,
               ymin = 0,
               ymax = ymax)
             
             ######################################## Construct colour vector for events ######################################
             
             event_types_unique = sort(as.character(unique(events)))
             fill_colours = vector(mode = "character", length=NROW(event_types_unique))
             for(i in 1: NROW(event_types_unique)) {
               event_type = event_types_unique[i]
               if(!is.null(colour_reference[[event_type]])) {
                 fill_colours[i] = as.character(colour_reference[[event_type]])
               } else {
                 fill_colours[i] = as.character(colour_reference[["Other"]])
               }
             }
             
             ################################################### Make the plot ############################################
             
             user_sequence_plot <-  
               ggplot(data, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Events)) +
               scale_fill_manual(values = alpha(fill_colours, 0.92)) +
               theme_bw() + 
               labs(title=paste("Session ID:",id, sep=" "),
                    subtitle=paste("Session Timespan:",round(time_span, digits = 2), "seconds    Start Date:", session_start_date, "    Animation:", animation , sep=" ")) +
               geom_rect(colour = "black", size = 0.3) + 
               facet_grid(subject ~ ., space = "free", scale = "free") + 
               theme(axis.ticks.y=element_blank(), 
                     axis.text.y=element_blank(), 
                     text = element_text(size=12), 
                     axis.text.y.right = element_text(size = 4),
                     legend.text = element_text(size = 14),
                     plot.subtitle = element_text(size =16),
                     plot.title = element_text(color="#666666", face="bold", size=24, hjust=0)) +
               scale_x_datetime(labels = date_format("%H:%M:%S"), 
                                breaks = date_breaks(x_axis_tick_breaks)) 
             
             print(user_sequence_plot)  
           }
           else {
             plot(1,1,col="white",xaxt='n',yaxt='n',ann=FALSE)
             text(1,1,"The timespan for this session's logged events is invalid.  Please select another session.", cex=1.5)   
           }
         }
         else {
           plot(1,1,col="white",xaxt='n',yaxt='n',ann=FALSE)
           text(1,1,"This session ID has no valid logged data. Please select another session.", cex=1.5)   
         }
       }
})

