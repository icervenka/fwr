output$daily_individual_plot <- renderPlot({

  aggregated_df = aggregate_parameter(global_vars$data_agg, 
                                      "t720", 
                                      "distance",
                                      "sum")

  global_vars$dark_intervals = aggregated_df %>% filter(light == 0) %>% select(interval) %>% unique %>% pull
  global_vars$light_intervals = aggregated_df %>% filter(light == 1) %>% select(interval) %>% unique %>% pull

  output_df = aggregated_df %>%
    dplyr::filter(interval %in% input$select_dark | interval %in% input$select_light) %>%
    group_by(light, subject)
  
  global_vars$output_df = output_df
  
  output_df %>% 
    ggplot(aes(x = subject,y = value, fill = subject)) + 
    stat_summary(fun.data = min.mean.sd.max,  geom = "boxplot") +
    plot_jitter(input$display_points) + 
    plot_facets(2, "~ light") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(y = "Distance [m]")
})


output$daily_individual_plot_render <- renderUI({
  plotOutput("daily_individual_plot", height = input$plot_height, width = input$plot_width)
})