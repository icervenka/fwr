output$individual_plot <- renderPlot({

  aggregated_df = aggregate_parameter(global_vars$data_agg, 
                                      paste0("t", input$select_aggregation), 
                                                "distance")
  
  global_vars$max_display_interval = max(aggregated_df$interval)
  
  aggregated_df %>%
    dplyr::filter(subject %in% input$select_subjects) %>%
    dplyr::filter(interval >= input$display_interval[1] & interval <= input$display_interval[2]) %>%
    ggplot(aes(x = interval, y = value, color = subject)) + 
    geom_tile(data = . %>% filter(subject == input$select_subjects[1]) %>% filter(light != 1),
              aes(x = (!light)*interval, y = 0 , width = 1, height = Inf),
              fill = "grey50", alpha = 0.2, inherit.aes = F) +
    geom_line() + 
    plot_points(input$display_points) + 
    plot_facets(input$facet_light, formula = "~light") + 
    labs(y = "Average distance [cm]")
})

output$individual_plot_render <- renderUI({
  plotOutput("individual_plot", height = input$plot_height, width = input$plot_width)
})
