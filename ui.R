ui <- fluidPage(
  titlePanel("FWR-VIS"),
  
  # tags$head(
  #   tags$style(
  #     HTML(
  #       ".checkbox-inline { 
  #                   margin-left: 0px;
  #                   margin-right: 30px;
  #         }
  #        .checkbox-inline+.checkbox-inline {
  #                   margin-left: 0px;
  #                   margin-right: 30px;
  #         }"
  #     )
  #   ) 
  # ),
  
  sidebarLayout(
    sidebarPanel(
      "",
      
      uiOutput("file_input"),
      
      conditionalPanel(
        condition = "input.tabs1 != 'Download'",
      
      uiOutput("night_start"),
      
      uiOutput("night_end"),
      tags$hr(),
      ),
      
      conditionalPanel(
        condition = "input.tabs1 == 'Series Individual' |             
                     input.tabs1 == 'Hour Individual'",
                      
        uiOutput("select_subjects")
      ),
      
      conditionalPanel(
        condition = "input.tabs1 == 'Series Individual' | 
                     input.tabs1 == 'Series Grouped'",
        
        uiOutput("select_aggregation")
        
      ),
      
      conditionalPanel(
        condition = "input.tabs1 == 'Series Individual' | 
                     input.tabs1 == 'Series Grouped'",

        uiOutput("display_interval")
        
      ),
      
      
      conditionalPanel(
        condition = "input.tabs1 == 'Bout Individual' |
                     input.tabs1 == 'Bout Grouped'",
        
        uiOutput("bout_aggregation"),
        
        uiOutput("bout_mincount"),
        
        uiOutput('bout_update'),
        
        tags$hr(),
      ),
      
      conditionalPanel(
        condition = "input.tabs1 == 'Series Individual' | 
                     input.tabs1 == 'Series Grouped' | 
                     input.tabs1 == 'Daily Individual' | 
                     input.tabs1 == 'Daily Grouped' | 
                     input.tabs1 == 'Hour Individual' |
                     input.tabs1 == 'Hour Grouped'",
        
        uiOutput("aggregate_by"),
        
      ),
      
      conditionalPanel(
        condition = "input.tabs1 == 'Daily Individual' | 
                     input.tabs1 == 'Daily Grouped'",
        
        uiOutput("select_dark"),
        
        uiOutput("select_light")
        
      ),
    
      conditionalPanel(
        condition = "input.tabs1 == 'Hour Individual' |
                     input.tabs1 == 'Hour Grouped'",
        
        uiOutput("shift_zt")
      ),
      
      conditionalPanel(
        condition = "input.tabs1 == 'Series Individual' | 
                     input.tabs1 == 'Series Grouped'",
        
        uiOutput("select_cumulative")
        
      ),
      
      conditionalPanel(
        condition = "input.tabs1 == 'Series Individual' | 
                     input.tabs1 == 'Series Grouped' | 
                     input.tabs1 == 'Daily Individual' | 
                     input.tabs1 == 'Daily Grouped' | 
                     input.tabs1 == 'Bout Individual' |
                     input.tabs1 == 'Bout Grouped' |
                     input.tabs1 == 'Hour Individual' |
                     input.tabs1 == 'Hour Grouped'",
        
        uiOutput("display_points")
        
      ),
      
      conditionalPanel(
        condition = "input.tabs1 == 'Series Grouped' | 
                     input.tabs1 == 'Daily Grouped' |
                     input.tabs1 == 'Hour Grouped' |
                     input.tabs1 == 'Bout Grouped'",
        
        uiOutput('select_no_groups'),
        
        uiOutput('display_groups')
        
      ),
      
      conditionalPanel(
        condition = "input.tabs1 == 'Hour Individual' |
                     input.tabs1 == 'Series Grouped' | 
                     input.tabs1 == 'Hour Grouped'",
        
        uiOutput("display_errorbars"),
        
        # uiOutput("display_statistics")
        
      ),
      
      conditionalPanel(
        condition = "input.tabs1 == 'Daily Individual' | 
                     input.tabs1 == 'Daily Grouped'",
        
        uiOutput("download_view")
        
      ),

      conditionalPanel(
        condition = "input.tabs1 != 'Download'",
        
        tags$hr(),
        
        uiOutput("plot_width"),
        
        uiOutput("plot_height")
      ),
      
      conditionalPanel(
        condition = "input.tabs1 == 'Download'",      
        p("Due to the rather computationally intensive calculations of all possible aggregations and their subsequent export xlsx format,
         it is currently only possible to select 5 aggregation intervals."),
        h4("The request take several seconds to process after clicking the download button")
      ),
      width = 2
    ),
    mainPanel(
      "",
      
      tabsetPanel(
        id = "tabs1",
        type = "pills",
        
        tabPanel("Series Individual",
                 uiOutput("individual_plot_render")),
        
        tabPanel("Daily Individual",
                 uiOutput("daily_individual_plot_render")),
        
        tabPanel("Bout Individual",
                 uiOutput("bout_individual_plot_render")),
        
        tabPanel("Hour Individual",
                 uiOutput("hour_individual_plot_render")),
        
        tabPanel("Series Grouped",
                 uiOutput("group_plot_render")),
        
        tabPanel("Daily Grouped",
                 uiOutput("daily_grouped_plot_render")),
        
        tabPanel("Bout Grouped",
                 uiOutput("bout_grouped_plot_render")),
        
        tabPanel("Hour Grouped",
                 uiOutput("hour_grouped_plot_render")),
        
        # tabPanel("Circadian",
        #          source("temp_ui.R", local = TRUE)[1]),
        # 
        # tabPanel("Sleep",
        #          source("temp_ui.R", local = TRUE)[1]),
        # 
        # tabPanel("Movement",
        #          source("temp_ui.R", local = TRUE)[1]),
        # 
        # tabPanel("Food",
        #          source("temp_ui.R", local = TRUE)[1]),
        # 
        # tabPanel("Drink",
        #          source("temp_ui.R", local = TRUE)[1]),
        # 
        tabPanel("Download", 
                 uiOutput("download"))
        
      ),
      width = 10
    )
  )
)
