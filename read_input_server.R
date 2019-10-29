readInput = observe({
  
  interval = 5
  # 
  # file <- input$file1
  # ext <- tools::file_ext(file)[1]
  # 
  # # if file is not uploaded provide temp file to show results
  # if(is.null(file)) {
  file <- read_delim("2018-01-17_zierath_fwr_convert.csv", delim = ',',col_types = "ccd")
  # } else if(toupper(ext) == "CSV" | toupper(ext) == "TXT") {
  #   file.rename(file$datapath,
  #               paste(file$datapath, ext, sep="."))
  #   file <- read_delim(paste(file$datapath, ext, sep="."), delim = ',',col_types = "cicdiddddiiiiiiiiddc")
  # }
  #
  
  data = file
  names(data) = c("subject", "date_time", "distance")
  subject_list = unique(data$subject)
  
  data$date_time = dmy_hms(data$date_time)
  data = data %>% mutate(light = ifelse(hms::as_hms(date_time) < night_start & hms::as_hms(date_time) > night_end, 1, 0))
  data = data %>% filter(subject != "Mouse_12") %>% group_by(subject) %>% mutate(interval = 1:n())
  
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