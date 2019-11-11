output$group_plot <- renderPlot({
  # TODO error when groups contain the same animals
  
  
  
  
  group_df = parse_group_inputs(input)
  
  if(dim(group_df)[1] > 0) {
    aggregated_df = aggregate_parameter(global_vars$data_agg, 
                                        paste0("t", input$select_aggregation), 
                                        "distance",
                                        input$aggregate_by)
    
    
    group_aggregated_df = merge(aggregated_df, group_df, by = "subject")
    group_aggregated_df = group_aggregated_df %>% group_by(subject, light) %>% mutate(split_interval = row_number())
    
    # pvals = map_dfr(1:max(group_aggregated_df$interval), function(x) {
    #   int_df = group_aggregated_df %>% dplyr::filter(interval == x)
    #   cbind.data.frame(interval = x, tidy(pairwise.t.test(int_df$parameter, int_df$group, p.adjust.method = "none")))
    # })
    # pvals = pvals %>% dplyr::mutate(p.adj = p.adjust(p.value, method = "BH"))
    
    global_vars$max_display_interval = max(aggregated_df$interval)
    
    p1 = group_aggregated_df %>%
      group_by(interval, light, group) %>% 
      summarise(mean = mean(value), sd = sd(value)) %>%
      dplyr::filter(interval >= input$display_interval[1] & interval <= input$display_interval[2]) %>%
      ggplot(aes(x = interval, y = mean)) + 
      geom_tile(data = . %>% filter(group == "Group1") %>% filter(light != 1),
                aes(x = (!light)*interval, y = 0 , width = 1, height = Inf),
                fill = "grey50", alpha = 0.2, inherit.aes = F) +
      geom_line(aes(colour = group)) + 
      plot_points(input$display_points, group) +
      plot_errorbars(input$display_errorbars) + 
      labs(y = "Distance [m]")
    
    p2 = group_aggregated_df %>%
      group_by(split_interval, interval, light, group) %>% 
      summarise(mean = mean(value), sd = sd(value)) %>%
      dplyr::filter(interval >= input$display_interval[1] & interval <= input$display_interval[2]) %>%
      ggplot(aes(x = split_interval, y = mean)) + 
      geom_line(aes(colour = group)) +
      plot_facets(2, formula = "~light") +
      plot_points(input$display_points, group) +
      plot_errorbars(input$display_errorbars) + 
      labs(y = "Distance [m]")
    
    grid.arrange(p1, p2, ncol = 1)
  }
})

output$group_plot_render <- renderUI({
  plotOutput("group_plot", height = input$plot_height*2, width = input$plot_width)
})