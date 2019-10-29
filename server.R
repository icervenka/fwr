# TODO implement interval


# imports ------------------------------------------------
library(shiny)
library(shinyWidgets)
library(shinyTime)
library(data.table)
library(xlsx)
library(rJava)
library(stringr)
library(readr)
library(tidyverse)
library(purrr)
library(broom)
library(ggplot2)
library(numbers)
library(reshape2)
library(matrixStats)
library(lubridate)
library(dplyr)

# options ------------------------------------------------
options(shiny.maxRequestSize = 20*1024^2)
options(java.parameters = "-Xmx2048m")

# functions -----------------------------------------------
create_aggregation_vector = function(each, length) {
  vec = c(rep(1:length, each = each, length.out = length))
  return(vec)
}

aggregate_parameter = function(data, time, param) {
  setDT(data)[,.(light = first(light),
                 date_time = first(date_time),
                 value = mean(get(param))),
              by = .(subject, interval = get(time))]
}

parse_group_inputs = function(inp) {
  
  group_list = lapply(1:as.integer(inp$select_no_groups), function(x) {
    inp[[paste0("group_no_", x)]]
  })
  
  group_list[sapply(group_list, is.null)] <- list("")
  group_list <- group_list[group_list != ""]
  
  group_list <- rapply(lapply(group_list, strsplit, ","), str_trim, how = "list") %>%
    lapply(unlist)
  
  if(length(group_list) > 0) {
    group_df = map_dfr(1:length(group_list), function(x) {
      cbind.data.frame(subject = group_list[[x]], group =  paste0("Group", x), stringsAsFactors = FALSE)
    })
  } else {
    group_df = data.frame()
  }
  
  return(group_df)
}

min.mean.sd.max <- function(x) {
  r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

plot_points = function(condition_field, aes_colour = subject) {
  if("1" %in% condition_field) {
    geom_point(aes(colour = {{ aes_colour }} ))
  } else {
    geom_blank()
  }
}

plot_errorbars = function(condition_field, aes_fill = group) {
  if(condition_field == "2") {
    geom_ribbon(aes(ymin=mean-sd, ymax=mean+sd, fill = {{ aes_fill }}), alpha = 0.08)
  } else {
    geom_blank()
  }
}

plot_facets = function(n, formula = "param ~ ." ) {
  if(n > 1) {
    facet_grid(as.formula(formula) ,scales = "free_y", labeller = label_both)
  } else {
    geom_blank()
  }
}

plot_jitter = function(condition_field) {
  if("1" %in% condition_field) {
    geom_jitter(shape = 21, colour = "black", fill = "white", size = 3, width = 0.25)
  } else {
    geom_blank()
  }
}

# server ------------------------------------------------
server <- function(input, output, session) {
  
  theme_set(theme_bw(base_size = 18))
  global_vars = reactiveValues()
  
  observe({
    night_start = input$night_start
    night_end = input$night_end
  })
  
  source("read_input_server.R", local = TRUE)
  
  source("sidebar_items_server.R", local = TRUE)
  
  source("individual_plot_server.R", local = TRUE)
  
  # source("grouped_plot_server.R", local = TRUE)
  # 
  # source("daily_individual_plot_server.R", local = TRUE)
  # 
  # source("daily_grouped_plot_server.R", local = TRUE)
  # 
  # source("hour_plot_server.R", local = TRUE)
  # 
  # source("download_server.R", local = TRUE)
  
}
