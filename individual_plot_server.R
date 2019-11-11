output$individual_plot <- renderPlot({

  aggregated_df = aggregate_parameter(global_vars$data_agg, 
                                      paste0("t", input$select_aggregation), 
                                      "distance",
                                      input$aggregate_by)
  split_aggregated_df = aggregated_df %>% group_by(subject, light) %>% mutate(split_interval = row_number())
  
  global_vars$max_display_interval = max(aggregated_df$interval)
  
  p1 = aggregated_df %>%
    dplyr::filter(subject %in% input$select_subjects) %>%
    dplyr::filter(interval >= input$display_interval[1] & interval <= input$display_interval[2]) %>%
    ggplot(aes(x = interval, y = value, color = subject)) +
    geom_tile(data = . %>% filter(subject == input$select_subjects[1]) %>% filter(light != 1),
              aes(x = (!light)*interval, y = 0 , width = 1, height = Inf),
              fill = "grey50", alpha = 0.2, inherit.aes = F) +
    geom_line() +
    plot_points(input$display_points) +
    labs(y = "Distance [m]")

  p2 = split_aggregated_df %>%
    dplyr::filter(subject %in% input$select_subjects) %>%
    dplyr::filter(interval >= input$display_interval[1] & interval <= input$display_interval[2]) %>%
    ggplot(aes(x = split_interval, y = value, color = subject)) +
    geom_line() +
    plot_points(input$display_points) +
    plot_facets(2, formula = "~light") +
    labs(y = "Distance [m]")

    grid.arrange(p1, p2, ncol = 1)
})

output$individual_plot_render <- renderUI({
  plotOutput("individual_plot", height = input$plot_height*2, width = input$plot_width)
})
