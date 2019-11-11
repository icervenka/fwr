output$work_individual_plot <- renderPlot({
  int_min = global_vars$interval / 60
  
  aggregated_df = aggregate_parameter(data_agg, 
                                      paste0("t", int_min), 
                                      "distance",
                                      "sum")
  
  bout_mincount = 5
  aggregated_df = aggregated_df %>% mutate(day = as.numeric(date(date_time) - first(date(date_time))))
  
  bout_aggregated = aggregated_df %>% dplyr::mutate(bout = case_when(value < bout_mincount ~ 0,
                                                                     value >= bout_mincount ~ 1,))
  
  rle_bout = rle(bout_aggregated$bout)
  rle_bout_cumsum = cumsum(rle_bout$values)
  bout_aggregated = bout_aggregated %>% mutate(bid = rep(rle_bout$values * rle_bout_cumsum, rle_bout$lengths)) %>% dplyr::filter(bout != 0)
  bout_summary = bout_aggregated %>% group_by(subject, day, light, bid) %>% summarise(bout_dist = sum(value), bout_length = n())
  
  bout_summary %>% pivot_longer(cols = contains("bout_")) %>% 
    ggplot(aes(x = subject, y = value)) + geom_violin(aes(fill = subject, color = subject), trim = F, alpha = 0.5, adjust=2)  +
    stat_summary(fun.data = mean.sd, 
                 geom="pointrange", color="black", fill="white", shape=21) +
    facet_grid(name ~ light, scales = "free_y", labeller = label_both)
  
  bout_summary %>% pivot_longer(cols = contains("bout_")) %>% 
    ggplot(aes(x = subject, y = value)) + geom_violin(aes(fill = subject, color = subject), trim = F)  +
    stat_summary(fun.data = min.mean.sd.max,  geom = "boxplot", width = 0.1)  +
    facet_grid(name ~ light, scales = "free_y")
  
  
  bout_length_summary = bout_summary %>% group_by(subject, day, light) %>% summarise(bouts = n())
  
  
  bout_length_summary %>% ggplot(aes(x = subject, y = bouts, fill = subject)) + geom_boxplot() + facet_wrap(~light)
  
  
  bout_summary_2 = bout_summary %>% group_by(subject, day, light) %>% summarise(dist_mean = mean(bout_dist), 
                                                                                sd_mean = sd(bout_dist), 
                                                                                length_mean = mean(bout_length),
                                                                                sd_length = sd(bout_length),
                                                                                bouts = n())
  
  bout_summary %>% 
    ggplot(aes(x = subject, y = bout_dist, fill = subject)) + geom_boxplot() + facet_wrap(~light)
  
})

output$workt_individual_plot_render <- renderUI({
  plotOutput("work_individual_plot", height = input$plot_height, width = input$plot_width)
})