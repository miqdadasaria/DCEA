library("shiny")
library("tidyverse")

source("baseline.R")
source("atkinson.R")

shinyServer(function(input, output) {
 
  ##### baseline distribution server functions  ####
  raw_baseline_data = reactive({
    if (is.null(input$baseline_data_file)){
      filename="data/england_regions_sample_distribution.csv"
    } else {
      filename=input$baseline_data_file$datapath
    }
    read_csv(filename)
  })
  
  baseline_data = reactive({
    process_baseline_data(raw_baseline_data(),input$input_vars)
  })
  
  output$download_sample_data = downloadHandler(
    filename = "england_regions_sample_distribution.csv",
    content = function(file) {
      results = read_csv("data/england_regions_sample_distribution.csv")
      write_csv(results,file)
    }
  )
  
  output$baseline_input_vars = renderUI(
    selectInput("input_vars", 
                "Split data by following dimensions:",
                setdiff(colnames(raw_baseline_data()),c("START_AGE","END_AGE","POPULATION","MORTALITY_RATE","HRQL_SCORE")), 
                multiple=TRUE)                      
  )
  
  output$baseline_plot = renderPlot({
    withProgress(message = paste0('Updating baseline health distribution plot'),{
      baseline_health_distribution_plot(baseline_data(),input$input_vars)
    })
  })
  
  output$raw_baseline_input_data = renderDataTable({
    withProgress(message = 'Loading raw baseline input data table',{
      table = raw_baseline_data()
      datatable(table,
                style = 'bootstrap',
                rownames = FALSE,
                colnames = gsub("_"," ",colnames(table)),
                options = list(pageLength = 18, autoWidth = TRUE, dom='ftrpi')) %>% formatRound(columns=c('MORTALITY_RATE', 'HRQL_SCORE'), digits=4)
    })
  })
  
  output$baseline_distribution_data = renderDataTable({
    withProgress(message = 'Loading baseline health distribution data table',{
      table = baseline_data()
      datatable(table,
                style = 'bootstrap',
                rownames = FALSE,
                colnames = gsub("_"," ",colnames(table)),
                options = list(pageLength = 18, autoWidth = TRUE, dom='ftrpi')) %>% formatRound(columns=c('QALE'), digits=4)
    })
  })
  
  output$baseline_inequality_summary = renderDataTable({
    withProgress(message = 'Loading baseline health distribution data table',{
      table = inequality_summary(baseline_data(),input$input_vars)
      datatable(table,
                style = 'bootstrap',
                rownames = FALSE,
                colnames = gsub("_"," ",colnames(table)),
                options = list(pageLength = 18, autoWidth = TRUE, dom='ftrpi')) %>% formatRound(columns=c("Value"), digits=3)
    })
  })
  
  ##### baseline distribution server functions  ####
  nhb_data = reactive({
    if (is.null(input$nhb_data_file)){
      filename="data/nrt_net_health_benefits_sample_distribution.csv"
    } else {
      filename=input$nhb_data_file$datapath
    }
    read_csv(filename)
  })
  
  output$download_sample_nhb_data = downloadHandler(
    filename = "nrt_net_health_benefits_sample_distribution.csv",
    content = function(file) {
      results = read_csv("data/nrt_net_health_benefits_sample_distribution.csv")
      write_csv(results,file)
    }
  )
  
  output$raw_nhb_input_data = renderDataTable({
    withProgress(message = 'Loading raw NHB data table',{
      table = nhb_data()
      datatable(table,
                style = 'bootstrap',
                rownames = FALSE,
                colnames = gsub("_"," ",colnames(table)),
                options = list(pageLength = 18, autoWidth = TRUE, dom='ftrpi')) 
    })
  })
  
  output$atkinson_ede_table = renderDataTable({
    withProgress(message = 'Loading Atkinson EDE results table',{
      table = display_atkinson_ede_table(nhb_data())
      datatable(table,
                style = 'bootstrap',
                rownames = FALSE,
                colnames = gsub("_"," ",colnames(table)),
                options = list(pageLength = 18, autoWidth = TRUE, dom='ftrpi')) 
    })
  })
  
  output$atkinson_ede_plot = renderPlot({
    withProgress(message = paste0('Updating Atkinson EDE plot'),{
      plot_atkinson_ede(nhb_data())
    })
  })
  
})