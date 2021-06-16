readInput = observe({

  file <- input$file1
  ext <- tools::file_ext(file)[1]

  # if file is not uploaded provide temp file to show results
  if(is.null(file)) {
    file <- read_delim("2019-11-01_zierath_fwr_convert.csv", delim = ',',col_types = "ccd")
  } else if(toupper(ext) == "CSV") {
    file.rename(file$datapath,
                paste(file$datapath, ext, sep="."))
    file <- read_delim(paste(file$datapath, ext, sep="."), delim = ',',col_types = "ccd")
  }

  night_start = "18:00:00"
  night_end = "06:00:00"

  data = file
  names(data) = c("subject", "date_time", "distance")
  subject_list = unique(data$subject)
  
  data$date_time = ymd_hms(data$date_time)
  #data = data %>% mutate(light = ifelse(hms::as_hms(date_time) < hms::as_hms(night_start) & hms::as_hms(date_time) > hms::as_hms(night_end), 1, 0))
  data = data %>% mutate(light = ifelse(hms::as_hms(date_time) < hms::as_hms(night_start) & hms::as_hms(date_time) > hms::as_hms(night_end), 1, 0))
  #data = data %>% filter(subject != "Mouse_12") %>% group_by(subject) %>% mutate(interval = 1:n())
  data = data %>% group_by(subject) %>% mutate(interval = row_number())
  
  interval = find_interval(data, subject, date_time, interval)
  if(length(interval) != 1) {
    stop("One of subject time series is not regular. Please update your data and try again")
  } else {
    interval = as.numeric(interval)
  }
  
  global_vars$interval = interval
  
  time_aggregation_values = intersect(seq(interval, 24*60, by = interval), 
                                      c(divisors(12*60)[-1], 1440))
  time_aggregation_repeats = time_aggregation_values / interval
  
  
  data_subject = data %>% dplyr::group_by(subject) %>% nest()
  data_subject = data_subject %>% 
    mutate(first_night_interval = map(data, . %>% dplyr::filter(light == 0) %>% top_n(1, -interval) %>% dplyr::select(interval) %>% as.numeric),
           no_records = map(data, . %>% dplyr::count() %>% as.numeric),
           cropped_records = map2_dbl(.x = data, .y = first_night_interval, function(x, y) {(x %>% dplyr::count() %>% as.numeric) + 1 - y}))
  
  
  min_records = min(data_subject$cropped_records)
  
  data_subject = data_subject %>%
    mutate(cropped = modify2(data, first_night_interval, function(x, y, mm) {
      x %>% dplyr::filter(interval >= y & interval <= (mm + y))
    }, mm = min_records))
  
  aggdf = map_dfc(time_aggregation_repeats, .f = create_aggregation_vector, data_subject$cropped_records[[1]])
  names(aggdf) = paste0("t",time_aggregation_values)
  
  data_long = data_subject %>% select(subject, cropped) %>% unnest(cropped)
  data_agg = cbind.data.frame(data_long, aggdf)
  
  global_vars$data_agg = data_agg
  global_vars$subject_list = subject_list
  global_vars$time_aggregation_values = time_aggregation_values
  global_vars$time_aggregation_repeats = time_aggregation_repeats
})
  