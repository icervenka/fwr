output$hour_grouped_plot <- renderPlot({
  
  group_df = parse_group_inputs(input)
  
  if(dim(group_df)[1] > 0) {
  
    aggregated_df = aggregate_parameter(global_vars$data_agg, 
                                        "t60", 
                                        "distance",
                                        input$aggregate_by)
    
    print(aggregated_df)
    setDT(aggregated_df)[, "hour" := hour(ymd_hms(date_time))]
    setDT(aggregated_df)[, hour := (hour+input$shift_zt)%%24]
    
    group_aggregated_df = merge(aggregated_df, group_df, by = "subject")

    group_aggregated_df %>%
      group_by(hour, group) %>%
      summarise(mean = mean(value), sd = sd(value), light = dplyr::first(light))  %>%
      ggplot(aes(x = hour, y = mean)) +
      geom_tile(data = . %>% filter(group == "Group1" & light != 1),
                aes(x = (!light)*hour, y = 0 , width = 1, height = Inf),
                fill = "grey50", alpha = 0.2, inherit.aes = F) +
      geom_line(aes(color = group)) +
      plot_points(input$display_points, group) +
      plot_errorbars(input$display_errorbars) + 
      labs(y = "Distance [m]")
  }
})


output$hour_grouped_plot_render <- renderUI({
  plotOutput("hour_grouped_plot", height = input$plot_height, width = input$plot_width)
})