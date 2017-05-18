# TODO change computation of mean to normalize to hours
# TODO implement night start

library(zoo)
library(ggplot2)
library(pryr)
library(dplyr)
library(shiny)
library(reshape2)
library(readr)
library(readxl)
library(lubridate)
library(xlsx)
library(stringr)
library(grid)
library(gridExtra)
library(shiny)
library(chron)

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
origin = '1970-01-01 00:00:00 CET'
phase_name_vector <- c("Light", "Dark")
to_distance_factor = round(pi * 22 / 100, digits = 3)
night_start = 18
night_duration = 12
sum_func <- c("sum", "mean", "max", "min")
graph_height = 650
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
palette <- list(scale_linetype_manual(values = c(rep("solid", 8), rep("dashed", 8), rep("twodash", 8))),
                scale_color_manual(values = c(rep(cbPalette, 3))))
graph_options <- list(xlab(""), ylab("distance [m]"), theme(axis.text = element_text(size=8)))


# ************************************************
# function declaration
# ************************************************

agg_time <- function(x, init_crop, intervals, records_count) {
  c(rep(1, each = (init_crop[x] - 1)), rep(2:(records_count/intervals[x]+2), each = intervals[x]))[1:records_count]
}


#
gen_seq_time_init <-function(start_pos, time_seq) {
  return(rep(time_seq[1], (start_pos-1)))
}

#
gen_seq_time_end <- function(start_pos, interval, time_seq,  records_count) {
  return(as.POSIXct(unlist(lapply(0:(records_count/interval), function (x) {rep(time_seq[start_pos+(interval*x)], each = interval)})), origin="1970-01-01 00:00:00")[1:(records_count - (start_pos-1))])
}

gen_agg_parameter <- function(data, init_crop, intervals, len) {
  
  # create empty list where time aggregations will be stored
  agg <- agg <- vector("list", length(intervals))
  time_data <- time(data)
  
  # repeat for all specified time aggregations
  for(i in seq_along(init_crop)) {
    seq1 <- gen_seq_time_init(init_crop[i], time_data) 
    seq2 <- gen_seq_time_end(init_crop[i], intervals[i], time_data, len)
    # !!!! changed to sum
    agg[[i]] <- aggregate(data, c(seq1, seq2), sum)
  }
  
  return(agg)
}

gen_hour_sum <- function(data, fasting_index, initial_start) {
  return(as.data.frame(t(vaplly(0:23, function(x) {colSums(filter(data, hour == x, row_number() < fasting_index & row_number() >= initial_start))}, integer(1)))))
}

# function takes a list and returns number of occurences of first element until it changes
first_n_ocurrence <- function(list) {
  return(grep(list[1], list, invert = TRUE)[1] - 1)
}

date_to_phase <- function (dt, night_start, night_duration) {
  x <- hour(dt)
  if(x >= night_start | x < ((night_start + night_duration) %% 24)) {
    return("Dark")
  } else {
    return("Light")
  }
}

date_to_phase_par <- partial(date_to_phase, night_start = night_start, night_duration = night_duration)

date_to_phase_arr <- function(x, night_start = night_start, night_duration = night_duration) {
  x <- hours(x)
  night_end <- (night_start + night_duration) %% 24
  return(ifelse(x >= night_start | x < night_end, "Dark", "Light"))
} 

sanitize_header_string <- function(string) {
  vapply(string, gsub, pattern = " ", character(1), replace = "_") %>% 
  vapply(gsub, pattern = "-", character(1), replace = "_") %>%
  vapply(gsub, pattern = "\\.", character(1), replace = "") %>%
  return()
}

aligned_time_series <- function(to_time, time_interval_min, total_len, init_len) {
  time1 <- sort(seq(to_time, by = -1*time_interval_min*60, length.out = init_len + 1))
  time2 <- seq(to_time, by = time_interval_min*60, length.out = (total_len - init_len))
  return(c(head(time1, n=-1), time2))
}

aligned_time_series2 <- function(to_time, st, en, time_interval_min, total_len, init_len) {
  by <- time_interval_min/1440
  #time1 <- sort(seq(to_time, st, by = -1*time_interval_min*60, length = init_len + 1))
  #print(seq(st, to_time))
  print(st)
  time2 <- seq(to_time, en, by = tm_int)
  print(time2)
  return(c(head(time1, n=-1), time2))
}

filter_on_vector <- function(x, dat) {filter(dat, phase == x)}

parse_info <- function(string, info) {
  return(gsub(string, "", info[grep(string, info)]) %>% str_trim())
}

group_data <- function(plot_data, columns) {
  df <- do.call(cbind, lapply(1:length(columns), function(x) {
    data.frame(rowMeans(select(plot_data, matches(columns[x])
    )))
  }))
  return(df)
}

dark_plot_rect <- function(initial_phase_no, len, dark_intervals) {
  st <- seq(from = initial_phase_no, to = len, by = 2*dark_intervals) - 0.5
  #print(st)
  en <- seq(from = initial_phase_no + dark_intervals, to = len, by = 2*dark_intervals) - 0.5
  #print(en)
  min <- min(length(st), length(en))
  return(data.frame(start = st[1:min], end = en[1:min]))
}

dark_rect_aes <- function(rects) {
  return(annotate("rect", xmin = rects$start, xmax = rects$end, ymin = -Inf, ymax = Inf, alpha=0.04, fill='blue'))
}

add_id_time <- function(df) {
  df$id <- 1:dim(df)[1]
  #df$time <- rownames(df)
  return(df)
}

add_phase <- function(df) {
  return(cbind.data.frame(df, phase = vapply(time(df), date_to_phase_par, character(1))))
}

name_groups <- function(len) {
  return(vapply(1:len, function (x) {paste("Group ", x)}, character(1)))
}

summarize_df <- function(df, group_vars, functions) {
  df %>% 
    melt() %>%
    group_by_(.dots = group_vars) %>% 
    summarize_each(functions, value) %>%
    ungroup
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

add_to_char_vector <- function(vec, ...) {
  arguments <- c(...)
  for(i in seq_along(arguments)) {
    index <- which(vec == "")[1]
    stopifnot(index <= length(vec))
    vec[index] <- arguments[i]
  }
  return(vec)
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
  
  
  output$fileUploaded <- reactive({
    return(!is.null(resultInput()))
  })

  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  output$download <- downloadHandler(
    
    filename = function() { 
      paste0(format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), "_fwr", ".xlsx") # file name
    },
    
    content = function(file){
      result <- resultInput()
      
      fwr_wb <- createWorkbook()
      
      # put data into individual sheets with one parameter per sheet and time aggregations side by side
      for (i in seq_along(result)) {
        sheet <- createSheet(fwr_wb, sheetName = paste(i, "_hour_agg"))
        header_row <- data.frame(names(result[[i]]))
        names(header_row) <- "date"
        addDataFrame(t(header_row), sheet, col.names = FALSE, startRow = 1)
        addDataFrame(result[[i]], sheet, col.names = FALSE, startRow = 2)
      }
      saveWorkbook(fwr_wb, file)
    }
    
  )
  
  
  resultInput <- reactive({
    
    filename <- input$file1
    ext <- tools::file_ext(filename)[1]
    
    if(is.null(filename)) {
      filename <- "week4.asc"
    } else {
      file.rename(filename$datapath,
                  paste(filename$datapath, ext, sep="."))
      filename <- paste(filename$datapath, ext, sep=".")
    }
  
    col_numbers <- count.fields(filename, sep=",")
    header_row_index <- match(max(col_numbers), col_numbers)
    first_data_row_index <- header_row_index + 1
    last_data_row_index <- 1 + length(col_numbers) - match(max(col_numbers), rev(col_numbers))
    
    header <- 
      scan(filename, what=character(), skip = header_row_index + 1, nlines = 1, sep = ",") %>% 
      sanitize_header_string() %>%
      unname()
    
    info <- read_lines(filename, n_max = header_row_index - 1)
    parse_partial <- partial(parse_info, info = info)
    
    file <- read.table(filename, header = FALSE, sep = ",", col.names = header, fill = TRUE, stringsAsFactors = FALSE, skip = header_row_index + 2)
    
    
    #TODO simplify
    animal_ids <- parse_partial(animal_id_string) %>% sanitize_header_string()
    sampling_intervals <- parse_partial(sampling_interval_string)
    experiment_description <- parse_partial(experiment_description_string)
    start_date <- parse_date_time(parse_partial(start_date_string), order="mdy hms")
    end_date <- parse_date_time(parse_partial(end_date_string), order="mdy hms")
    
    
    # fill experiment details
    expDetails <- add_to_char_vector(vector("character", 50), 
                                     paste(experiment_description_string, experiment_description),
                                     paste(start_date_string, start_date),
                                     paste(end_date_string, end_date),
                                     paste("Number of subjects: ", length(animal_ids)),
                                     paste(animal_id_string, paste(animal_ids, collapse = ", ")),
                                     paste(sampling_interval_string, sampling_intervals[1]),
                                     paste("Number of measurments: ", dim(file)[1]))
    
    dts <- chron(dates. = dates(select(file, matches(turns_date_string))[[1]]))
    tms <- chron(times. = times(select(file, matches(turns_time_string))[[1]]))
    
    globalValues$records_count <- dim(file)[1]
    globalValues$com_fact <- c(1, 2, 3, 4, 6, 12)
    experiment_duration <- as.integer(difftime(end_date, start_date, units = "secs"))
    time_interval_min = parse_time(sampling_intervals[1]) %>% as.integer() / 60
    intervals <- globalValues$com_fact*60 / time_interval_min
    
    
    dat <- select(file, matches(turns_data_string)) * to_distance_factor
    names(dat) <- animal_ids
    
    phase <- date_to_phase_arr(tms, night_start, night_duration)
    init_phase_count = first_n_ocurrence(phase)
    first_phase_change_dt <- tms[init_phase_count + 1]
    phase_change_time <- paste(night_start, "00", "00", sep = ":")
    init_crop <- ((init_phase_count) %% intervals) + 1
    
    tms <- tms - as.character(first_phase_change_dt - phase_change_time)
    date_time <- chron(dts, tms) 
    
    int_df <- data.frame(lapply(1:length(intervals), agg_time, init_crop, intervals,  globalValues$records_count))
    names_int_df <- vapply(c(1:length(intervals)), function(x) {paste0("I", x)}, character(1))
    names(int_df) <- names_int_df

    
    result <- vector("list", length(names(int_df)))  
      
    for(i in seq_along(names(int_df))) {
      result[[i]] <- aggregate(dat, by = int_df[i], sum)
      result[[i]][1] <- aggregate(phase, int_df[i], first)[2]
      names(result[[i]])[1] <- "phase"
    }

    globalValues$dat_hour <- cbind(dat, hour = hours(date_time))
    globalValues$expDetails <- expDetails
    
    return(result)
    
  })
  

  # 1. time plot
  #-----------------------------------------------------------------------------------
  output$timePlot <- renderPlot({
    indd <- match(as.numeric(input$timeAggregation), globalValues$com_fact)
    
    result <- resultInput()[[indd]]
    phase <- as.character(unname(unlist(result[1])))
    result[1] <- NULL
    result_cum <- cumsum(result)
    

    if(input$cumulative == FALSE) {
      plot_data <- result
    } else {
      plot_data <- result_cum
    }

    plot_data <- add_id_time(plot_data)
    plot_data_melt <- melt(plot_data, id.vars = c("id"))
  

    rect_start <- first_n_ocurrence(phase) + 1
    rects <- dark_plot_rect(rect_start, dim(plot_data)[1], (12/globalValues$com_fact[indd]))

    graph_options <- c(graph_options, geom_line(size=0.75))
    ggplot() %+% plot_data_melt + 
      aes(x = id, y = value, group = variable, colour=variable, linetype = variable) + 
      graph_options + 
      palette +  
      xlab("interval") +
      dark_rect_aes(rects)


  }, height = graph_height)
   
   
  # 2. group time plot
  #-----------------------------------------------------------------------------------

  output$groupTimePlot <- renderPlot({
    indd <- match(as.numeric(input$timeAggregation), globalValues$com_fact)
    input$update

    result <- resultInput()[[indd]]
    phase <- as.character(unname(unlist(result[1])))
    result[1] <- NULL
    result_cum <- cumsum(result)
    
    
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
      columns <- str_group_sel(input_list)

      # amount of groups defined by user
      # if no groups are defined yet, display empty graph to avoid errors

      if(length(columns) == 0) {
        group_count <- 0
      } else {
        group_count <- sum(vapply(1:length(columns), function(x) {dim(select(plot_data, matches(columns[x])))[2]}, integer(1)))
      }

      if(group_count > 0) {

        plot_df <- group_data(plot_data, columns)
        names(plot_df) <- name_groups(length(columns))
        plot_df <- add_id_time(plot_df)

        rect_start <- first_n_ocurrence(phase) + 1
        rects <- dark_plot_rect(rect_start, dim(plot_data)[1], (12/globalValues$com_fact[indd]))
    
        plot_df_melt <- melt(plot_df, id.vars = c("id"))

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
    
    result_dark_light <- resultInput()[[6]] %>%
      summarize_df(c("phase", "variable"), sum_func)


    grobs <- vector("list", 2*length(sum_func))

    for(i in sum_func) {
      ind <- which(sum_func == i)*2
      grobs[[ind-1]] <- ggplot(result_dark_light) + 
        aes_string("variable", i) + 
        geom_bar(stat="identity") + 
        facet_wrap(~phase, scales = "free") + 
        graph_options + 
        ylab(paste(i, "distance [m]"))
      grobs[[ind]] <- select(result_dark_light, phase, variable, which(colnames(result_dark_light) == i)) %>%
        ggplot() + 
        aes_string("variable", i, fill = "phase") + 
        geom_bar(stat="identity", position=position_dodge()) + 
        graph_options + 
        ylab(paste(i, "distance [m]"))
    }

    do.call("grid.arrange", c(grobs, ncol=2))


  }, height = graph_height*1.2)


  # 4. group summary plot
  #-----------------------------------------------------------------------------------
  output$groupSummaryPlot <- renderPlot({

    plot_data <- resultInput()[[6]]
    phase <- as.character(unname(unlist(plot_data[1])))
    plot_data[1] <- NULL
    
    # number of dynamically generated inputs for means calculation
    num <- as.integer(input$no_groups)
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
    # if no groups are defined yet, display empty graph to avoid errors
    if(length(columns) == 0) {
      group_count <- 0
    } else {
      group_count <- sum(vapply(1:length(columns), function(x) {dim(select(plot_data, matches(columns[x])))[2]}, integer(1)))
    }

    if(group_count > 0) {

      plot_df <- group_data(plot_data, columns)
      
      names(plot_df) <- name_groups(length(columns))
      print(plot_df)
      plot_df <- summarize_df(cbind.data.frame(plot_df, phase = phase), c("phase", "variable"), sum_func)

      grobs <- vector("list", 2*length(sum_func))

      for(i in sum_func) {
        ind <- which(sum_func == i)*2
        grobs[[ind-1]] <- ggplot(plot_df) + 
          aes_string("variable", i) + 
          geom_bar(stat="identity") + 
          facet_wrap(~phase, scales = "free") + 
          graph_options + ylab(paste(i, "distance [m]"))
        grobs[[ind]] <- select(plot_df, phase, variable, which(colnames(plot_df) == i)) %>%
          ggplot() + 
          aes_string("variable", i, fill = "phase") + 
          geom_bar(stat="identity", position=position_dodge()) + 
          graph_options + 
          ylab(paste(i, "distance [m]"))
      }

      do.call("grid.arrange", c(grobs, ncol=2))
    }
    })
  }, height = graph_height)



  # 5. hour plot
  #-----------------------------------------------------------------------------------
  output$hourPlot <- renderPlot({

    
    dat_hour_sum <- melt(globalValues$dat_hour, id.vars = "hour") %>%
      group_by(hour, variable) %>%
      summarize(sum = sum(value))

    graph_options <- c(graph_options, geom_line(size=0.75))
    ggplot() %+% dat_hour_sum + 
      aes(x = hour, y = sum, group = variable, color = variable, linetype = variable) + 
      graph_options + 
      palette

  }, height = graph_height)

  
  # 6. experiment details
  # -----------------------------------------------------------------------------------
  output$experimentDetails <- renderUI(

    HTML(paste(globalValues$expDetails, collapse="<br>"))
  )
  
})

# Run the application
shinyApp(ui = ui, server = server)
