output$daily_grouped_plot <- renderPlot({
  
  # TODO works, but there is some json error Input to asJSON(keep_vec_names=TRUE) is a named vector.
  # TODO remove fasting-refeeding
  # select checkboxes for days and nights for aggregation
  group_df = parse_group_inputs(input)
  
  if(dim(group_df)[1] > 0) {
    
    aggregated_df = aggregate_parameter(global_vars$data_agg, 
                                        "t720", 
                                        "distance",
                                        input$aggregate_by)
    
    group_aggregated_df = merge(aggregated_df, group_df, by = "subject")
    group_aggregated_df = group_aggregated_df %>% group_by(subject, light) %>% mutate(split_interval = row_number())
    
    global_vars$dark_intervals = group_aggregated_df %>% ungroup %>% filter(light == 0) %>% select(interval) %>% unique %>% pull
    global_vars$light_intervals = group_aggregated_df %>% ungroup %>% filter(light == 1) %>% select(interval) %>% unique %>% pull
    
    group_aggregated_df %>%
      dplyr::filter(interval %in% input$select_dark | interval %in% input$select_light) %>%
      group_by(light, group) %>% 
      ggplot(aes(x = group,y = value, fill = group)) + 
      stat_summary(fun.data = min.mean.sd.max,  geom = "boxplot") +
      plot_jitter(input$display_points) + 
      plot_facets(2, "~ light") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(y = "Distance [m]")
  }
})

output$daily_grouped_plot_render <- renderUI({
  plotOutput("daily_grouped_plot", height = input$plot_height, width = input$plot_width)
})

