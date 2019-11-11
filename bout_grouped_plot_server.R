output$bout_grouped_plot <- renderPlot({
  
  input$bout_update
  
  group_df = parse_group_inputs(input)
  
  isolate({
    bout_mincount = input$bout_mincount
    
  })
  
  
  
  if(dim(group_df)[1] > 0) {
    isolate({
      aggregated_df = aggregate_parameter(global_vars$data_agg, 
                                          paste0("t", input$bout_aggregation), 
                                          "distance",
                                          input$aggregate_by)
      
      
      #global_vars$max_display_interval = max(aggregated_df$interval)
    })
    aggregated_df = aggregated_df %>% mutate(day = as.numeric(date(date_time) - first(date(date_time))))
    bout_aggregated = aggregated_df %>% dplyr::mutate(bout = case_when(value < bout_mincount ~ 0,
                                                                       value >= bout_mincount ~ 1,))
    
    rle_bout = rle(bout_aggregated$bout)
    rle_bout_cumsum = cumsum(rle_bout$values)
    bout_aggregated = bout_aggregated %>% mutate(bid = rep(rle_bout$values * rle_bout_cumsum, rle_bout$lengths)) %>% dplyr::filter(bout != 0)
    
    group_aggregated_df = merge(bout_aggregated, group_df, by = "subject")
    group_aggregated_df = group_aggregated_df %>% group_by(group, light) %>% mutate(split_interval = row_number())
    
    
    bout_summary = group_aggregated_df %>% group_by(group, day, light, bid) %>% summarise(bout_dist = sum(value), bout_length = n())
    print(bout_summary)
    
    bout_length_summary = group_aggregated_df %>% group_by(group, day, light) %>% summarise(bouts = n())
    print(bout_length_summary)
    
    p1 = bout_summary %>% pivot_longer(cols = contains("bout_")) %>%
      ggplot(aes(x = group, y = value)) + 
      geom_violin(aes(fill = group, color = group), trim = F, alpha = 0.5, adjust=2)  +
      stat_summary(fun.data = mean.sd,
                   geom="pointrange", color="black", fill="white", shape=21) +
      plot_facets(2, "name ~ light") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    p2 = bout_length_summary %>% ggplot(aes(x = group, y = bouts, fill = group)) +
      stat_summary(fun.data = min.mean.sd.max,  geom = "boxplot") +
      plot_jitter(input$display_points) + 
      plot_facets(2, "~ light") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    grid.arrange(p1, p2, ncol = 1)
  }
})

output$bout_grouped_plot_render <- renderUI({
  plotOutput("bout_grouped_plot", height = input$plot_height*2, width = input$plot_width)
})