# Shiny ui for management and hospital performance
# 
# Author: Miqdad Asaria
# Date: 11/10/2018
########################################################

library("shiny")
library("DT")
source("baseline.R")

shinyUI(
  navbarPage(theme = "sandstone.css",
             "Distributional Cost-Effectiveness Analysis",
             
             ##### baseline distribution UI panel ####
             
             tabPanel("Generate Baseline Health Distribution",
                      
                      sidebarPanel(
                        h3("Create baseline distribution"),

                        tags$div(
                          HTML("<small>
                               <p>This application takes input in CSV format containing the age specific mortality, 
                               morbidity and population data split by the population characteristics across 
                               wish you want to analyse equity. The CSV file must contain five columns named: 
                               START_AGE, END_AGE, POPULATION, MORTALITY_RATE and HRQL_SCORE. Additional columns
                               should be added to capture each of the characteristics you want to be able to spilt 
                               the population by.</small>")
                        ),      
                        
                        fileInput('baseline_data_file', 'Choose CSV File',
                                  accept=c('text/csv', 
                                           'text/comma-separated-values,
                                           text/plain', 
                                           '.csv')),
                        tags$div(
                          HTML("<small>
                               An example data file for England allowing you to split data by socioeconomic status
                               and region can be downloaded below to help understand the input data format. If no input
                               data file is selected this is the data that is used to generate the default health 
                               distributions shown here.</small>")
                          ),
                        
                        downloadButton("download_sample_data", "Download Sample Dataset in CSV format"),
                        
                        tags$div(
                          HTML("<small><p>
                               <p>Select the variables from the dataset that you want to use to generate population subgroups.</small>")
                          ),
                        
                        uiOutput("baseline_input_vars"),
                        
                        tags$div(
                          HTML("<small>
                          <p>This site was produced by <a href='https://github.com/miqdadasaria'>Miqdad Asaria</a> 
                          <p>Source code can be found <a href='https://github.com/miqdadasaria/DCEA'>here</a>.</small>")
                        )
                      ),
                      mainPanel(
                        tabsetPanel(id="tabset",
                                    tabPanel("Health Distribution Plot", plotOutput("baseline_plot")),
                                    tabPanel("QALE by Age Tables", div(dataTableOutput("baseline_distribution_data"), style = "font-size:70%")),
                                    tabPanel("Inequality Summary", div(dataTableOutput("baseline_inequality_summary"), style = "font-size:70%")),
                                    tabPanel("Raw input data", div(dataTableOutput("raw_baseline_input_data"), style = "font-size:70%"))
                                    # ,tabPanel("Health Distribution Table", div(dataTableOutput("baseline_table"), style = "font-size:70%")),
                                    
                        )
                        
                      )
             ),
             
             # end baseline distribution UI panel
             

             
             ##### notes UI panel ####
             tabPanel("Notes", tags$div(HTML("<p>&nbsp;<p>
                                             
                                             <dl class='dl-horizontal'>
                                             <dt>Baseline distribution</dt> 
                                             <dd>blah, blah, blah...</dd>
                                             <p>
                                             
                                             <dt>Atkinson index</dt>
                                             <dd>blah, blah, blah...</dd>
                                             <p>
                                             </dl>"))
                      
             )
             # end notes UI panel
             )
             )