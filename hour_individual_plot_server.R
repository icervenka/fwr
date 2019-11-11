output$hour_individual_plot <- renderPlot({
  
  aggregated_df = aggregate_parameter(global_vars$data_agg, 
                                      "t60", 
                                      "distance",
                                      input$aggregate_by)
  
  setDT(aggregated_df)[, "hour" := hour(ymd_hms(date_time))]
  setDT(aggregated_df)[, hour := (hour+input$shift_zt)%%24]
  
  aggregated_df$param = factor(aggregated_df$param, levels = unique(aggregated_df$param))

  aggregated_df %>%
    dplyr::filter(subject %in% input$select_subjects) %>%
    group_by(hour, subject) %>%
    summarise(mean = mean(value), sd = sd(value), light = dplyr::first(light))  %>%
    ggplot(aes(x = hour, y = mean)) +
    geom_tile(data = . %>% filter(subject == input$select_subjects[1] & light != 1),
              aes(x = (!light)*hour, y = 0 , width = 1, height = Inf),
              fill = "grey50", alpha = 0.2, inherit.aes = F) +
    geom_line(aes(color = subject)) +
    plot_points(input$display_points) +
    plot_errorbars(input$display_errorbars, subject)
})


output$hour_individual_plot_render <- renderUI({
  plotOutput("hour_individual_plot", height = input$plot_height, width = input$plot_width)
})