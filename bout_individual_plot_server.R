output$bout_individual_plot <- renderPlot({
  
  input$bout_update
  
  isolate({
    ptm <- proc.time()
    bout_mincount = input$bout_mincount

    aggregated_df = aggregate_parameter(global_vars$data_agg, 
                                        paste0("t", input$bout_aggregation), 
                                        "distance",
                                        "sum")
    print(proc.time() - ptm)
  })

  
  aggregated_df = aggregated_df %>% mutate(day = as.numeric(date(date_time) - first(date(date_time))))
  
  # print(aggregated_df)
  # global_vars$dark_intervals = aggregated_df %>% filter(light == 0) %>% select(interval) %>% unique %>% pull
  # global_vars$light_intervals = aggregated_df %>% filter(light == 1) %>% select(interval) %>% unique %>% pull
  # 
  # aggregated_df = aggregated_df %>%
  #   dplyr::filter(interval %in% input$select_dark | interval %in% input$select_light) #%>%
  #   #group_by(light, subject)
  # 
  # #global_vars$output_df = output_df
  
  bout_aggregated = aggregated_df %>% dplyr::mutate(bout = case_when(value < bout_mincount ~ 0,
                                                   value >= bout_mincount ~ 1,))

  rle_bout = rle(bout_aggregated$bout)
  rle_bout_cumsum = cumsum(rle_bout$values)
  bout_aggregated = bout_aggregated %>% mutate(bid = rep(rle_bout$values * rle_bout_cumsum, rle_bout$lengths)) %>% dplyr::filter(bout != 0)

  bout_summary = bout_aggregated %>% group_by(subject, day, light, bid) %>% summarise(bout_dist = sum(value), bout_length = n())

  bout_length_summary = bout_summary %>% group_by(subject, day, light) %>% summarise(bouts = n())

  # speed_summary = data_agg %>% 
  #   mutate(day = as.numeric(date(date_time) - first(date(date_time))), speed = distance / 5) %>%
  #   group_by(subject, day, light) %>%
  #   summarise(peak_speed = max(speed))
    
  # ptm <- proc.time()
  p1 = bout_summary %>% pivot_longer(cols = contains("bout_")) %>%
    ggplot(aes(x = subject, y = value)) + 
    geom_violin(aes(fill = subject, color = subject), trim = F, alpha = 0.5, adjust=2)  +
    stat_summary(fun.data = mean.sd,
                 geom="pointrange", color="black", fill="white", shape=21) +
    plot_facets(2, "name ~ light") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  p2 = bout_length_summary %>% ggplot(aes(x = subject, y = bouts, fill = subject)) +
    stat_summary(fun.data = min.mean.sd.max,  geom = "boxplot") +
    plot_jitter(input$display_points) + 
    plot_facets(2, "~ light") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # print(proc.time() - ptm)
  # p3 = speed_summary %>%
  #   ggplot(aes(x = subject, y = peak_speed, fill = subject)) + geom_boxplot() + facet_grid(~light)

  grid.arrange(p1, p2, ncol = 1)
})

output$bout_individual_plot_render <- renderUI({
  plotOutput("bout_individual_plot", height = input$plot_height*2, width = input$plot_width)
})