library("shiny")
library("tidyverse")

source("baseline.R")

shinyServer(function(input, output) {
  
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
                setdiff(colnames(raw_baseline_data()),c("START_AGE","END_AGE","POPULATION","MORTALITY_RATE","MORBIDITY_RATE")), 
                selected = setdiff(colnames(raw_baseline_data()),c("START_AGE","END_AGE","POPULATION","MORTALITY_RATE","MORBIDITY_RATE")), 
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
                options = list(pageLength = 18, autoWidth = TRUE, dom='ftrpi')) %>% formatRound(columns=c('MORTALITY_RATE', 'MORBIDITY_RATE'), digits=4)
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
      table = inequality_summary(baseline_data())
      datatable(table,
                style = 'bootstrap',
                rownames = FALSE,
                colnames = gsub("_"," ",colnames(table)),
                options = list(pageLength = 18, autoWidth = TRUE, dom='ftrpi')) %>% formatRound(columns=c("Value"), digits=3)
    })
  })
  
})