# FWR-VIS
# TODO change computation of mean to normalize to hours
# TODO include analysis of new format files (and general csv files with time - distance columns)

library(shiny)
library(pryr)
library(dplyr)
library(chron)
library(lubridate)
library(xlsx)
library(stringr)
library(readr)
library(reshape2)
library(ggplot2)
library(grid)
library(gridExtra)


# ************************************************
# constants
# ************************************************

animal_id_string <- paste0("Animal ID", ":")
sampling_interval_string <- paste0("Sampling Interval", ":")
start_date_string <- "Start Date/Time"
end_date_string <- "End Date/Time"
experiment_description_string <- paste0("Experiment Description", ":")
turns_date_string <- "Turns_Date"
turns_time_string <- "Turns_Time"
turns_data_string <- "Turns_Data"
phase_name_vector <- c("Light", "Dark")
to_distance_factor = round(pi * 22 / 100, digits = 3)
night_start_default = "18:00:00"
night_duration = "12:00:00"
sum_func <- c("sum", "mean", "max", "min")
graph_height = 650
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
palette <- list(scale_linetype_manual(values = c(rep("solid", 8), rep("dashed", 8), rep("twodash", 8))),
                scale_color_manual(values = c(rep(cbPalette, 3))))
graph_options <- list(xlab(""), ylab("distance [m]"), theme(axis.text = element_text(size=8)))


# ************************************************
# function declaration
# ************************************************

# adds integer id column to data frame
add_id_time <- function(df) {
  df$id <- 1:dim(df)[1]
  return(df)
}

# functions adds item to the first empty space of character vector
# should be faster than list implementation
add_to_char_vector <- function(vec, ...) {
  arguments <- c(...)
  for(i in seq_along(arguments)) {
    index <- which(vec == "")[1]
    stopifnot(index <= length(vec))
    vec[index] <- arguments[i]
  }
  return(vec)
}

# create intervals for different time aggregations
agg_time <- function(x, init_crop, intervals, records_count) {
  c(rep(1, each = (init_crop[x] - 1)), rep(2:(records_count/intervals[x]+2), each = intervals[x]))[1:records_count]
}

# function that produces Dark/Light shading intervals in the back of the graph
# input: initial amount of phases, number of intervals and amount of dark intervals in one phase
# return: data frame with start/end of interval per row
dark_plot_rect <- function(initial_phase_no, len, dark_intervals, start_phase) {
  st <- seq(from = initial_phase_no, to = len, by = 2*dark_intervals) - 0.5
  en <- seq(from = initial_phase_no + dark_intervals, to = len, by = 2*dark_intervals) - 0.5
  
  if(start_phase == "Dark") {
    swap <- en
    en <- st
    st <- c(0, swap)
    #en <- en - st[1]
    #st <- st - st[1]
  }
  min <- min(length(st), length(en))
  st <- st[1:min]
  en <- en[1:min]
  
  if(en[length(en)] < len - dark_intervals) {
    st <- c(st, st[length(st)] + 2*dark_intervals)
    en <- c(en, len)
  }
  
  return(data.frame(start = st, end = en))
}

# creates aesthetics for ggplot from dark_plot_rect
dark_rect_aes <- function(rects) {
  return(annotate("rect", xmin = rects$start, xmax = rects$end, ymin = -Inf, ymax = Inf, alpha=0.04, fill='blue'))
}

# vectorized function that takes list of chron time objects, and return a list of Light/Dark strings
# based in night start and night duration parameters
date_to_phase_arr <- function(x, night_start = night_start, night_duration = night_duration) {
  #x <- chron::hours(x)
  x <- 24*60*60*as.numeric(times(x))
  #night_end <- (night_start + night_duration) %% 24
  
  night_end = (as.numeric(as.difftime(c(night_start, "00:00:00"), unit = "secs")[1]) +
                     as.numeric(as.difftime(c(night_duration, "00:00:00"), unit = "secs")[1])) %% (24*60*60)
  night_start = 24*60*60*as.numeric(times(night_start))
  
  if(night_start <= (12*60*60)) {
    return(ifelse(x >= night_start & x < night_end, "Dark", "Light"))
  } else {
    return(ifelse(x >= night_start | x < night_end, "Dark", "Light"))
  }
} 

# function takes a list and returns number of occurences of first element until it changes
first_n_ocurrence <- function(list) {
  return(grep(list[1], list, invert = TRUE)[1] - 1)
}

# subset data frame based on column names
group_data <- function(plot_data, columns) {
  df <- do.call(cbind, lapply(1:length(columns), function(x) {
    data.frame(rowMeans(select(plot_data, matches(columns[x])
    )))
  }))
  return(df)
}

# creates group names with predefined pattern "Group #"
name_groups <- function(len) {
  return(vapply(1:len, function (x) {paste("Group ", x)}, character(1)))
}

# removes substring pattern from info string
# used to parse experiment information
parse_info <- function(string, info) {
  return(gsub(string, "", info[grep(string, info)]) %>% str_trim())
}

# sanitizing function that removes spaces, dashes and dots from string
sanitize_header_string <- function(string) {
  vapply(string, gsub, pattern = " ", character(1), replace = "_") %>% 
  vapply(gsub, pattern = "-", character(1), replace = "_") %>%
  vapply(gsub, pattern = "\\.", character(1), replace = "") %>%
  return()
}

# creates regular expression pattern to exactly match items in supplied list of strings
# used to select colums/subjects from data for analysis of means
str_group_sel <- function(lst) {
  lst <- lst[lst != ""]
  temp <- rapply(lapply(lst, strsplit, ","), str_trim, how = "list") %>%
    lapply(unlist)
  temp <- Map(function(x) {paste("^(", paste(x , collapse = "|"), ")$", sep = "")}, temp) %>%
    unlist()
  return(temp)
}

# functions summarizes data frame based on grouping variables and summarizing functions
# input: data frame, list of grouping variables, list of grouping functions
# output: data frame with summarized data variables x functions
summarize_df <- function(df, group_vars, functions) {
  df %>% 
    melt() %>%
    group_by_(.dots = group_vars) %>% 
    summarize_each(functions, value) %>%
    ungroup
}

# ************************************************
# shiny server UI
# ************************************************

ui <- shinyUI(fluidPage(
  # Application title
  titlePanel("FWR-VIS"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      
      fileInput(
        "file1",
        "Choose file to upload (xls, asc)",
        accept = c(".xls", ".xlsx", ".asc", ".csv")
      ),
      
      textInput("night_start", label = "Night Start [hh:mm:ss]", value = "18:00:00"),
      
      # conditional panel to supply groups for mean calculations
      conditionalPanel(
        condition = "input.tabs1 == 'Group Summary' | input.tabs1 == 'Group Time Course'",
        tags$hr(),
        numericInput(
          "no_groups",
          "Choose the number of groups",
          value = 2,
          min = 1,
          width = '100%'
        ),
        uiOutput("groupChar"),
        actionButton("update", "Update Groups")
      ),
      
      # conditional panel - time aggregation selector
      conditionalPanel(
        condition = "input.tabs1 == 'Time Course' | input.tabs1 == 'Group Time Course'",
        tags$hr(),
        selectInput(
          "timeAggregation",
          "Select time aggregation [hrs]",
          c(1, 2, 3, 4, 6, 12),
          selected = 1
        )
      ),
      
      conditionalPanel(
        condition = "input.tabs1 == 'Time Course' | input.tabs1 == 'Group Time Course'",
        checkboxInput("cumulative", "Show cumulative data")
      ),
      
      tags$hr(),
      
      
      downloadButton("download", "Download data"),
      
      width = 2
      
    ),
    
    mainPanel(
      tags$head(tags$script(src = "functions.js")),
      div(

        tabsetPanel(
          id = "tabs1",
          tabPanel("Time Course", plotOutput("timePlot")),
          tabPanel("Group Time Course", plotOutput("groupTimePlot")),
          tabPanel("Summary", plotOutput("summaryPlot")),
          tabPanel("Group Summary", plotOutput("groupSummaryPlot")),
          tabPanel("Hour", plotOutput("hourPlot")),
          tabPanel("Experiment Details", htmlOutput("experimentDetails"))
        )
      , class = "delParentClass")
    )
  )
))


# ************************************************
# shiny server logic
# ************************************************


server <- shinyServer(function(input, output) {
  
  globalValues <- reactiveValues()
  
  # dynamically render textInputs for selections of groups for mean calculations
  output$groupChar <- renderUI({
    lapply(1:as.integer(input$no_groups), function(i) {
      isolate(textInput(
        paste0("n_input_", i),
        label = paste0("Group ", i, ":"),
        value = input[[paste0("n_input_", as.character(i))]]
      ))
    })
  })
  
  
  output$download <- downloadHandler(
    
    # create name for file output
    filename = function() { 
      paste0(format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), "_fwr", ".xlsx") # file name
    },
    
    content = function(file){
      result <- resultInput()
      
      fwr_wb <- createWorkbook()
      
      # put data into individual sheets with one parameter per sheet and time aggregations side by side
      for (i in seq_along(result)) {
        sheet <- createSheet(fwr_wb, sheetName = paste0(globalValues$com_fact[[i]], "_hour_agg"))
        header_row <- data.frame(names(result[[i]]))
        names(header_row) <- "interval"
        addDataFrame(t(header_row), sheet, col.names = FALSE, startRow = 1)
        addDataFrame(result[[i]], sheet, col.names = FALSE, startRow = 2)
      }
      saveWorkbook(fwr_wb, file)
    }
    
  )
  
  
  resultInput <- reactive({
    
    # reactive expression if the file has been loaded
    filename <- input$file1
    # extract extension from filename
    ext <- tools::file_ext(filename)[1]
    
    if (grepl("^([0-1][0-9]|2[0-3]):([0-5][0-9]):([0-5][0-9])$", input$night_start)) {
      night_start = input$night_start
    } else {
      night_start = night_start_default
    }
    
    print(night_start)
    
    # if the file was not specified, load dummy data
    if(is.null(filename)) {
      filename <- "week4.asc"
    } else {
      # filenames doesn't persist with upload, extension needs to be supllied again
      file.rename(filename$datapath,
                  paste(filename$datapath, ext, sep="."))
      filename <- paste(filename$datapath, ext, sep=".")
    }
  
    # count column numbers to determine where the data starts
    col_numbers <- count.fields(filename, sep=",")
    # index of header row of data
    header_row_index <- match(max(col_numbers), col_numbers)
    
    first_data_row_index <- header_row_index + 1
    last_data_row_index <- 1 + length(col_numbers) - match(max(col_numbers), rev(col_numbers))
    
    # read header for data and sanitize it
    header <- 
      scan(filename, what=character(), skip = header_row_index + 1, nlines = 1, sep = ",") %>% 
      sanitize_header_string() %>%
      unname()
    
    # list of lines with experiment description
    # appears before data table in the .asc file
    info <- read_lines(filename, n_max = header_row_index - 1)
    
    # read experiment data into the table, skip rows with experiment information
    file <- read.table(filename, header = FALSE, sep = ",", col.names = header, fill = TRUE, stringsAsFactors = FALSE, skip = header_row_index + 2)
    
    # partial application of parse_info function
    parse_partial <- partial(parse_info, info = info)
    
    # parse experiment information into individual variables
    # use partial application of parse_info with info variable containing experiment details
    # TODO simplify
    animal_ids <- parse_partial(animal_id_string) %>% sanitize_header_string()
    sampling_intervals <- parse_partial(sampling_interval_string)
    experiment_description <- parse_partial(experiment_description_string)
    start_date <- parse_date_time(parse_partial(start_date_string), order="mdy HMS")
    end_date <- parse_date_time(parse_partial(end_date_string), order="mdy HMS")
    
    
    # fill experiment details
    expDetails <- add_to_char_vector(vector("character", 50), 
                                     paste(experiment_description_string, experiment_description),
                                     paste(start_date_string, start_date),
                                     paste(end_date_string, end_date),
                                     paste("Number of subjects: ", length(animal_ids)),
                                     paste(animal_id_string, paste(animal_ids, collapse = ", ")),
                                     paste(sampling_interval_string, sampling_intervals[1]),
                                     paste("Number of measurments: ", dim(file)[1]))
    
    # create date and time chron objects using columns in experiment data table
    dts <- chron(dates. = dates(select(file, matches(turns_date_string))[[1]]))
    tms <- chron(times. = times(select(file, matches(turns_time_string))[[1]]))
    
    # total number of records
    globalValues$records_count <- dim(file)[1]
    
    # aggregation time in hours
    globalValues$com_fact <- c(1, 2, 3, 4, 6, 12)
    
    # experiment duration parsed as difference between parsed end and start time from experiment detail
    experiment_duration <- as.integer(difftime(end_date, start_date, units = "secs"))
    
    # time inverval between successive measurements parsed from experiment details
    time_interval_min = parse_time(sampling_intervals[1]) %>% as.integer() / 60
    
    # list of number of intervals that comprise individual time aggregations
    intervals <- globalValues$com_fact*60 / time_interval_min
    
    # create data table replacing number of turns by distance run as computed from running wheel diameter
    dat <- select(file, matches(turns_data_string)) * to_distance_factor
    names(dat) <- animal_ids
    
    # vector of Light/Dark phases for individual measurements
    phase <- date_to_phase_arr(tms, night_start, night_duration)
    
    # initial count of intervals before the first phase change
    init_phase_count = first_n_ocurrence(phase)
    
    # index of first phase change normalized to interval number
    init_crop <- ((init_phase_count) %% intervals) + 1
    
    # time of first phase change
    first_phase_change_dt <- tms[init_phase_count + 1]
    
    # prettify the first phase change time to whole minutes and seconds 
    phase_change_time <- night_start
    
    # prettify chron vector of times by subtracting the difference between actual time and prettified time
    phase_change_diff = first_phase_change_dt - phase_change_time
    tms <- tms - as.character(first_phase_change_dt)
    
    # create chron datetime object
    date_time <- chron(dts, tms) 
    
    # create data frame with intervals for aggregation
    int_df <- data.frame(lapply(1:length(intervals), agg_time, init_crop, intervals,  globalValues$records_count))
    
    # rename columns of data frame to following pattern 'I#'
    names_int_df <- vapply(c(1:length(intervals)), function(x) {paste0("I", x)}, character(1))
    names(int_df) <- names_int_df

    # create empty vector to store results of aggregation
    result <- vector("list", length(names(int_df)))  
      
    # aggregate data based on aggregation interval data frame
    # adds column of intervals that will have to be removed later
    for(i in seq_along(names(int_df))) {
      result[[i]] <- aggregate(dat, by = int_df[i], sum)
      # add aggregated phase vector as the first column
      result[[i]][1] <- aggregate(phase, int_df[i], first)[2]
      names(result[[i]])[1] <- "phase"
    }

    # add hour-by-hour data to global results to be accessible from anywhere within program
    globalValues$dat_hour <- cbind(dat, hour = chron::hours(date_time))
    
    # add experimental details to global results to be accessible from anywhere within program
    globalValues$expDetails <- expDetails
    
    return(result)
    
  })
  

  # 1. time plot
  #-----------------------------------------------------------------------------------
  output$timePlot <- renderPlot({
    indd <- match(as.numeric(input$timeAggregation), globalValues$com_fact)
    result <- resultInput()[[indd]]
    
    # put Dark/Light phase into separate vector
    phase <- as.character(unname(unlist(result[1])))
    start_phase = phase[1]
    
    # delete phase column from original data frame
    result[1] <- NULL
    # calculate cumulative sums for cumulative plot
    result_cum <- cumsum(result)
    
    # display normal time course or cumulative sums if the cumulative tickbox is selected
    if(input$cumulative == FALSE) {
      plot_data <- result
    } else {
      plot_data <- result_cum
    }

    # add id column (row index) to the data frame
    plot_data <- add_id_time(plot_data)
    # melt into long format based on id column
    plot_data_melt <- melt(plot_data, id.vars = c("id"))
  
    # create Dark/Light index intervals
    rect_start <- first_n_ocurrence(phase) + 1
    rects <- dark_plot_rect(rect_start, dim(plot_data)[1], (12/globalValues$com_fact[indd]), start_phase = start_phase)

    # plot graph
    graph_options <- c(graph_options, geom_line(size=0.75)) # specific graphing options
    ggplot() %+% plot_data_melt + 
      aes(x = id, y = value, group = variable, colour=variable, linetype = variable) + 
      graph_options + 
      palette +  # colour palette
      xlab("interval") +
      dark_rect_aes(rects) # Dark/Light rectangles to plot in the background


  }, height = graph_height)
   
   
  # 2. group time plot
  #-----------------------------------------------------------------------------------

  output$groupTimePlot <- renderPlot({
    indd <- match(as.numeric(input$timeAggregation), globalValues$com_fact)
    
    # reactive object that is connected to update groups button
    input$update

    result <- resultInput()[[indd]]
    
    # put Dark/Light phase into separate vector
    phase <- as.character(unname(unlist(result[1])))
    start_phase = phase[1]
    
    # delete phase column from original data frame
    result[1] <- NULL
    
    # calculate cumulative sums for cumulative plot
    result_cum <- cumsum(result)
    
    # display normal time course or cumulative sums if the cumulative tickbox is selected
    if(input$cumulative == FALSE) {
      plot_data <- result
    } else {
      plot_data <- result_cum
    }

    # number of dynamically generated inputs for means calculation
    num <- as.integer(input$no_groups)

    isolate({

      # gather contents of groups from dynamically generated textInputs
      # report as a nested list of subject names
      # adding new input generates NULL - is changed to empty string not to generate errors
      input_list <- lapply(1:num, function(x) {input[[paste0("n_input_", x)]]})
      input_list[sapply(input_list, is.null)] <- list("")

      # generate regex expression from nested list, one string per group
      # they have to be comma separated
      columns <- str_group_sel(input_list)

      # amount of groups defined by user
      if(length(columns) == 0) {
        group_count <- 0
      } else {
        group_count <- sum(vapply(1:length(columns), function(x) {dim(select(plot_data, matches(columns[x])))[2]}, integer(1)))
      }

      # if more than one group was specified by user
      if(group_count > 0) {

        # create summarized data frame based on columns specified by the user
        plot_df <- group_data(plot_data, columns)
        
        # prettify names based on following convention 'Group #'
        names(plot_df) <- name_groups(length(columns))
        
        # add id (index) column to the data frame
        plot_df <- add_id_time(plot_df)
        
        # create Dark/Light index intervals
        rect_start <- first_n_ocurrence(phase) + 1
        rects <- dark_plot_rect(rect_start, dim(plot_data)[1], (12/globalValues$com_fact[indd]), start_phase = start_phase)
    
        # melt to long format based on id
        plot_df_melt <- melt(plot_df, id.vars = c("id"))

        # plot graph
        ggplot() %+% 
          plot_df_melt + 
          aes(x = id, y = value, group = variable, color = variable, linetype = variable) +
          dark_rect_aes(rects) +
          scale_x_continuous(breaks = pretty(1:dim(plot_df)[1], n = 20)) + 
          geom_line(size=0.75)  + 
          graph_options + 
          palette + 
          xlab("interval")

      } else {
        
      }
    })
  }, height = graph_height)


  # 3. summary plot
  #-----------------------------------------------------------------------------------
  output$summaryPlot <- renderPlot({
    indd <- match(as.numeric(input$timeAggregation), globalValues$com_fact)
    
    # summarize Dark/Light based on 12-hour aggregation result
    result_dark_light <- resultInput()[[6]] %>%
      summarize_df(c("phase", "variable"), sum_func)

    # create vector which will hold graphical objects
    grobs <- vector("list", 2*length(sum_func))

    for(i in sum_func) { # for every summarizing funciton
      ind <- which(sum_func == i)*2 # create even index
      
      # put individual Dark/Light graphs in odd indices of grob list
      grobs[[ind-1]] <- ggplot(result_dark_light) + 
        aes_string("variable", i) + 
        geom_bar(stat="identity") + 
        facet_wrap(~phase, scales = "free") + 
        graph_options + 
        ylab(paste(i, "distance [m]"))
      
      # put combined Dark/Light graphs in even indices of grob list
      grobs[[ind]] <- select(result_dark_light, phase, variable, which(colnames(result_dark_light) == i)) %>% 
        ggplot() + 
        aes_string("variable", i, fill = "phase") + 
        geom_bar(stat="identity", position=position_dodge()) + 
        graph_options + 
        ylab(paste(i, "distance [m]"))
    }

    # arrange and show the graphs
    do.call("grid.arrange", c(grobs, ncol=2))


  }, height = graph_height*1.2)


  # 4. group summary plot
  #-----------------------------------------------------------------------------------
  output$groupSummaryPlot <- renderPlot({

    # summarize Dark/Light based on 12-hour aggregation result
    plot_data <- resultInput()[[6]]

    # put Dark/Light phase into separate vector
    phase <- as.character(unname(unlist(plot_data[1])))
    
    # delete phase column from original data frame
    plot_data[1] <- NULL
    
    # number of dynamically generated inputs for means calculation
    num <- as.integer(input$no_groups)
    
    # reactive object that is connected to update groups button
    input$update

    isolate({
    # gather contents of groups from dynamically generated textInputs
    # report as a nested list of subject names
    # adding new input generates NULL - is changed to empty string not to generate errors
    input_list <- lapply(1:num, function(x) {input[[paste0("n_input_", x)]]})
    input_list[sapply(input_list, is.null)] <- list("")

    # generate regex expression from nested list, one string per group
    columns <- str_group_sel(input_list)
    
    # amount of groups defined by user
    # they have to be comma separated
    if(length(columns) == 0) {
      group_count <- 0
    } else {
      group_count <- sum(vapply(1:length(columns), function(x) {dim(select(plot_data, matches(columns[x])))[2]}, integer(1)))
    }

    # if more than one group was specified by user
    if(group_count > 0) {
      
      # create summarized data frame based on columns specified by the user
      plot_df <- group_data(plot_data, columns)
      
      # prettify names based on following convention 'Group #'
      names(plot_df) <- name_groups(length(columns))
      
      # create summarized data frame based on groups specified by user for individual phases x specified summarization functions
      plot_df <- summarize_df(cbind.data.frame(plot_df, phase = phase), c("phase", "variable"), sum_func)

      # create vector which will hold graphical objects
      grobs <- vector("list", 2*length(sum_func))

      for(i in sum_func) { # for every summarizing funciton
        ind <- which(sum_func == i)*2 # create even index
        
        # put individual Dark/Light graphs in odd indices of grob list
        grobs[[ind-1]] <- ggplot(plot_df) + 
          aes_string("variable", i) + 
          geom_bar(stat="identity") + 
          facet_wrap(~phase, scales = "free") + 
          graph_options + ylab(paste(i, "distance [m]"))
        
        # put combined Dark/Light graphs in even indices of grob list
        grobs[[ind]] <- select(plot_df, phase, variable, which(colnames(plot_df) == i)) %>%
          ggplot() + 
          aes_string("variable", i, fill = "phase") + 
          geom_bar(stat="identity", position=position_dodge()) + 
          graph_options + 
          ylab(paste(i, "distance [m]"))
      }

      # arrange and show the graphs
      do.call("grid.arrange", c(grobs, ncol=2))
      
    }
    })
  }, height = graph_height)



  # 5. hour plot
  #-----------------------------------------------------------------------------------
  output$hourPlot <- renderPlot({

    # get dat_hour data frame from global values
    # melt into long format based on hours
    # group by hours
    # summarize by summing
    dat_hour_sum <- melt(globalValues$dat_hour, id.vars = "hour") %>%
      group_by(hour, variable) %>%
      summarize(sum = sum(value))

    # additional graph options
    graph_options <- c(graph_options, geom_line(size=0.75))
    
    # plot graph
    ggplot() %+% dat_hour_sum + 
      aes(x = hour, y = sum, group = variable, color = variable, linetype = variable) + 
      graph_options + 
      palette

  }, height = graph_height)

  
  # 6. experiment details
  # -----------------------------------------------------------------------------------
  output$experimentDetails <- renderUI(

    # write experiment details from global value array as HTML
    HTML(paste(globalValues$expDetails, collapse="<br>"))
  )
  
})

# Run the application
shinyApp(ui = ui, server = server)
