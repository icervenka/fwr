output$file_input <- renderUI({
  fileInput("file1", "Upload your file")
})

output$night_start <- renderUI({
  timeInput("night_start", "Night start", value = strptime("18:00:00", "%T"))
})

output$night_end <- renderUI({
  timeInput("night_end", "Night end", value = strptime("06:00:00", "%T"))
})

output$select_aggregation <- renderUI({
  shinyWidgets::sliderTextInput("select_aggregation", "Select aggregation [min]",
                                choices = global_vars$time_aggregation_values %>% as.character, selected = "60")
})

output$aggregate_by <- renderUI({
  radioButtons("aggregate_by", label = "Aggregate by",
               choices = list("Mean" = "mean", "Sum" = "sum"), 
               selected = "mean")
})

output$display_interval <- renderUI({
  sliderInput("display_interval", label = "Display intervals", min = 1, 
              max = global_vars$max_display_interval, value = c(0, global_vars$max_display_interval), step = 1)
})

output$select_cumulative <- renderUI({
  radioButtons("select_cumulative", label = "Plot",
               choices = list("Interval data" = 1, "Cumulative data" = 2), 
               selected = 1)
})

output$select_subjects <- renderUI({
  # Create the checkboxes and select them all by default
  pickerInput(
    inputId = "select_subjects", 
    label = "Select subjects", 
    choices = global_vars$subject_list, 
    selected = global_vars$subject_list[1:2],
    options = list(
      `actions-box` = TRUE, 
      size = 15,
      `selected-text-format` = "count > 4"
    ), 
    multiple = TRUE
  )
})

output$shift_zt <- renderUI({
  sliderInput("shift_zt", label = "Shift Zeitgeber 0 to:", min = 0, 
              max = 23, value = 6, step = 1)
})

output$select_no_groups <- renderUI({
  # Create the checkboxes and select them all by default
  numericInput("select_no_groups", label = "Select number of groups", value = 1, min = 1, max = length(global_vars$subject_list)/2)
})

output$display_groups <- renderUI({
  # Create the checkboxes and select them all by default
  if(is.integer(input$select_no_groups)) {
    map(1:as.integer(input$select_no_groups), function(i) {
      isolate(
        pickerInput(
          inputId = paste0("group_no_", i), 
          label = paste0("Group: ", i), 
          choices = global_vars$subject_list, 
          selected = input[[paste0("group_no_", as.character(i))]],
          options = list(
            `actions-box` = TRUE, 
            size = 15,
            `selected-text-format` = "count > 4"
          ), 
          multiple = TRUE
        )
      )
    })
  }
})

output$display_points <- renderUI({
  # Create the checkboxes and select them all by default
  checkboxGroupInput("display_points", "Display additional", 
                     choices  = c("Display points" = 1))
})

output$display_errorbars <- renderUI({
  # Create the checkboxes and select them all by default
  radioButtons("display_errorbars", "Display error bars",
               choices  = c("none" = 1, "SD" = 2),
               selected = 1)
})

output$display_statistics <- renderUI({
  radioButtons("display_statistics", label = "Display statistics",
               choices = list("none" = 1, "p-values" = 2, "adjusted p-values (BH)" = 3), 
               selected = 1)
})

output$select_dark <- renderUI({
  pickerInput(
    inputId = "select_dark", 
    label = "Include dark intervals", 
    choices = global_vars$dark_intervals,
    selected = global_vars$dark_intervals, 
    options = list(
      `actions-box` = TRUE, 
      size = 15,
      `selected-text-format` = "count > 15"
    ), 
    multiple = TRUE
  )
})

output$select_light <- renderUI({
  pickerInput(
    inputId = "select_light", 
    label = "Include light intervals", 
    choices = global_vars$light_intervals, 
    selected = global_vars$light_intervals, 
    options = list(
      `actions-box` = TRUE, 
      size = 15,
      `selected-text-format` = "count > 15"
    ), 
    multiple = TRUE
  )
})

output$download_view <- renderUI({
  downloadButton("download_current_view", label = "Download this view as csv", icon = icon("download"))
})

output$download_current_view <- downloadHandler(
  filename = "daily_view.csv",
  content = function(file) {
    data = global_vars$output_df
    write.csv(data, file)
  }
)

output$bout_mincount <- renderUI({
  numericInput("bout_mincount", label = "Bout minimum count", value = 5)
})

output$bout_aggregation <- renderUI({
  shinyWidgets::sliderTextInput("bout_aggregation", "Select min bout length [min]",
                                choices = global_vars$time_aggregation_values %>% as.character, selected = "60")
})

output$bout_update <- renderUI({
  actionButton("bout_update", "Update bout parameters")
})

output$plot_width <- renderUI({
  sliderInput("plot_width", label = "Plot width [px]", min = 1000, 
              max = 2000, value = 1500)
})

output$plot_height <- renderUI({
  sliderInput("plot_height", label = "Plot height [px]", min = 250, 
              max = 1000, value = 500)
})