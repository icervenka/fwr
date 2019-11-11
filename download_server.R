output$download <- renderUI({
  
  default_aggregations = c(10, 30, 60, 180, 720)
  
  fluidRow(
    map(1:5, function(x) {
      selectInput(paste0("select_aggregation_",x), paste0(label = "Select aggregation ", x, " [min]"), 
                  choices = global_vars$time_aggregation_values, 
                  selected = default_aggregations[x])
    }),
    downloadButton("download_xlsx", label = "Download", icon = icon("download"))
  )
})

output$download_xlsx <- downloadHandler(
  
  filename = function() {
    paste0(format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), "_fwr", ".xlsx")
  },
  
  content = function(file) {
    
    tagg = map(1:5, function(x) {
      paste0("t",input[[paste0("select_aggregation_", x)]])
    })
    
    wb<-createWorkbook(type="xlsx")
    
    walk(tagg, function(p) {
      sheet <- createSheet(wb, sheetName = p)
      
      aggregated_df = aggregate_parameter(data_agg, 
                                          p, 
                                          "distance",
                                          input$aggregate_by)
      
      export_df = dcast(aggregated_df, interval + light + date_time ~ subject, value.var = "value")
      addDataFrame(export_df, sheet, startRow=1, startColumn=1, row.names=FALSE)
    })
    saveWorkbook(wb, file)
  }
)