library("shiny")
library("tidyverse")

source("baseline.R")
source("swf.R")
source("intervention.R")


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
  
  
  ##### intervention distribution server functions  ####
  raw_intervention_data = reactive({
    if (is.null(input$intervention_data_file)){
      filename="data/nrt_net_health_benefits_universal_distribution.csv"
    } else {
      filename=input$intervention_data_file$datapath
    }
    read_csv(filename)
  })
  
  output$download_sample_intervention_data = downloadHandler(
    filename = "nrt_net_health_benefits_universal_distribution.csv",
    content = function(file) {
      results = read_csv("data/nrt_net_health_benefits_universal_distribution.csv")
      write_csv(results,file)
    }
  )
  
  output$intervention_plot = renderPlot({
    withProgress(message = paste0('Updating intervention distribution plot'),{
      plot_intervention(raw_intervention_data(), input$total_intervention_cost, input$marginal_productivity_health, input$opportunity_cost_scenario)
    })
  })
  
  output$raw_intervention_input_data = renderDataTable({
    withProgress(message = 'Loading raw intervention data table',{
      table = raw_intervention_data()
      datatable(table,
                style = 'bootstrap',
                rownames = FALSE,
                colnames = gsub("_"," ",colnames(table)),
                options = list(pageLength = 18, autoWidth = TRUE, dom='ftrpi')) 
    })
  })
  
  output$intervention_health_dist = renderDataTable({
    withProgress(message = 'Loading intervention health distribution table',{
      table = calculate_post_intervention_distribution(raw_intervention_data(), input$total_intervention_cost, input$marginal_productivity_health, input$opportunity_cost_scenario)
      datatable(table,
                style = 'bootstrap',
                rownames = FALSE,
                colnames = gsub("_"," ",colnames(table)),
                options = list(pageLength = 18, autoWidth = TRUE, dom='ftrpi')) 
    })
  })
  
  output$download_post_intervention_data = downloadHandler(
    filename = "post_intervention_health_distribution.csv",
    content = function(file) {
      results = calculate_post_intervention_distribution(raw_intervention_data(), input$total_intervention_cost, input$marginal_productivity_health, input$opportunity_cost_scenario)
      write_csv(results,file)
    }
  )
  
  ##### social welfare function server functions  ####
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
  
  output$baseline_decision_name = renderUI(
    selectInput("baseline_decision", 
                "Set baseline decision to:",
                gsub("_"," ",gsub("DECISION_","",colnames(nhb_data()%>%select(starts_with("DECISION"))))))                      
  )
  
  output$dominance_table = renderDataTable({
    withProgress(message = 'Loading dominance table',{
      table = display_dominance_table(nhb_data(),input$baseline_decision)
      datatable(table,
                style = 'bootstrap',
                rownames = FALSE,
                colnames = gsub("_"," ",colnames(table)),
                options = list(pageLength = 16, autoWidth = TRUE, dom='ftrpi')) 
    })
  })
  
  output$atkinson_ede_table = renderDataTable({
    withProgress(message = 'Loading Atkinson EDE results table',{
      table = display_ede_table(nhb_data(),"Atkinson",input$baseline_decision)
      datatable(table,
                style = 'bootstrap',
                rownames = FALSE,
                colnames = gsub("_"," ",colnames(table)),
                options = list(pageLength = 18, autoWidth = TRUE, dom='ftrpi')) 
    })
  })
  
  output$atkinson_ede_plot = renderPlot({
    withProgress(message = paste0('Updating Atkinson EDE plot'),{
      plot_ede(nhb_data(),"Atkinson",input$baseline_decision)
    })
  })
  
  output$gini_ede_table = renderDataTable({
    withProgress(message = 'Loading Extended Gini EDE results table',{
      table = display_ede_table(nhb_data(),"Extended Gini",input$baseline_decision)
      datatable(table,
                style = 'bootstrap',
                rownames = FALSE,
                colnames = gsub("_"," ",colnames(table)),
                options = list(pageLength = 18, autoWidth = TRUE, dom='ftrpi')) 
    })
  })
  
  output$gini_ede_plot = renderPlot({
    withProgress(message = paste0('Updating Extended Gini EDE plot'),{
      plot_ede(nhb_data(),"Extended Gini",input$baseline_decision)
    })
  })
  
  output$kolm_ede_table = renderDataTable({
    withProgress(message = 'Loading Extended Kolm EDE results table',{
      table = display_ede_table(nhb_data(),"Kolm",input$baseline_decision)
      datatable(table,
                style = 'bootstrap',
                rownames = FALSE,
                colnames = gsub("_"," ",colnames(table)),
                options = list(pageLength = 18, autoWidth = TRUE, dom='ftrpi')) 
    })
  })
  
  output$kolm_ede_plot = renderPlot({
    withProgress(message = paste0('Updating Kolm EDE plot'),{
      plot_ede(nhb_data(),"Kolm",input$baseline_decision)
    })
  })
  
  output$equity_impact_plane_plot = renderPlot({
    withProgress(message = paste0('Updating Equity Impact Plane plot'),{
      plot_equity_impact_plane(nhb_data(),input$index,input$inequity_aversion,input$baseline_decision)
    })
  })
  
})