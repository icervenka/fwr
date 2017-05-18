
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
graph_options <- list(xlab(""), ylab("distance [m]"), theme(axis.text.x = element_text(size=9)))


# ************************************************
# function declaration
# ************************************************

agg_time <- function(x, records_count) {
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
  df$time <- rownames(df)
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
setwd("E:/Google Drive/programming/fwr/")

filename = "week4.asc"

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


tmp_st <- unlist(strsplit(parse_partial(start_date_string), " "))
tmp_en <- unlist(strsplit(parse_partial(end_date_string), " "))
start_date2 <- chron(tmp_st[1], tmp_st[2])
end_date2 <- chron(tmp_en[1], tmp_en[2])

records_count <- dim(file)[1]
experiment_duration <- as.integer(difftime(end_date, start_date, units = "secs"))
time_interval_min = parse_time(sampling_intervals[1]) %>% as.integer() / 60


dat <- select(file, matches(turns_data_string)) * to_distance_factor
names(dat) <- animal_ids

date <- select(file, matches(turns_date_string))[[1]]
#print(as.chron(date))
time <- select(file, matches(turns_time_string))[[1]]
#print(chron(dates(select(file, matches(turns_date_string))[[1]]), times(select(file, matches(turns_time_string))[[1]])))
#print(times(select(file, matches(turns_time_string))[[1]]))
#, select(file, matches(turns_time_string))[[1]], format = c(dates = "m/d/y", times = "h:m:s")))
date_time <- 
  mapply(paste, date, time, MoreArgs = list(sep = " ")) %>% 
  unname() %>%
  parse_date_time(orders = "mdy hms", tz = "CET")

date_time <- chron(dates(select(file, matches(turns_date_string))[[1]]), times(select(file, matches(turns_time_string))[[1]])) %>%
  format(enclosed = c("", ""))

dts <- chron(dates. = dates(select(file, matches(turns_date_string))[[1]]))
tms <- chron(times. = times(select(file, matches(turns_time_string))[[1]]))

#phase <- vapply(date_time[1:((60/time_interval_min)*12)], date_to_phase, character(1), night_start, night_duration)
phase <- date_to_phase_arr(tms, 18, 12)
#print(phase)

init_phase_count = first_n_ocurrence(phase)
#first_phase_change_dt <- floor_date(date_time[init_phase_count + 1 ], "hour")
#print(first_phase_change_dt)

first_phase_change_dt <- times[init_phase_count + 1 ]
phase_change_time <- "18:00:00"
del_time <- first_phase_change_dt - phase_change_time
tms <- tms - as.character(del_time)

tm <- data.frame(matrix(NA, nrow = records_count, ncol = 2))
names(tm) <- c("date_time", "phase")
tm$date_time <- aligned_time_series2(first_phase_change_dt, start_date2, end_date2, time_interval_min, records_count, init_phase_count)
tm <- mutate(tm, phase = date_to_phase_arr(date_time, night_start, night_duration))


com_fact <- c(1, 2, 3, 4, 6, 12)
intervals <- com_fact*60 / time_interval_min
init_crop <- ((init_phase_count) %% intervals) + 1


dat_zoo <- zoo(dat, order.by = tm$date_time)

result <- gen_agg_parameter(dat_zoo, init_crop, intervals, records_count)

expDetails <- expDetails



days <- as.integer(days(date_time)) - as.integer(days(date_time)[1])
ag <- (hours(date_time) - hours(date_time)[1] + 1) + (24 * days)



int_df <- data.frame(lapply(1:length(intervals), agg_time, records_count))
names(int_df) <- vapply(c(1:length(intervals)), function(x) {paste0("I", x)}, character(1))

result <- vector("list", length(names(int_df)))  

for(i in seq_along(names(int_df))) {
  result[[i]] <- aggregate(dat, by = int_df[i], sum)
}



ggplot(cmp) + aes(interval, sum, group = subject) + geom_line()


cmp <- cbind.data.frame(dat, int_df) %>%
  melt(id.vars = c("I1", "I2", "I3", "I4", "I5", "I6")) %>%
  group_by(variable) %>%
  mutate(cumvalue = cumsum(value)) %>%
  group_by_(.dots = c(subject = "variable", interval = "I4")) %>%
  summarise(sum = sum(value), cumsum = sum(cumvalue))