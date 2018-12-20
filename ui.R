# Shiny ui for management and hospital performance
# 
# Author: Miqdad Asaria
# Date: 11/10/2018
########################################################

library("shiny")
library("DT")

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
                               which you want to analyse equity. The CSV file must contain five columns named: 
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
                               distributions shown here.<p></small>")
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
             
             ##### intervention distribution UI panel ####
             tabPanel("Generate Intervention Health Distribution",
                      
                      sidebarPanel(
                        h3("Create intervention distribution"),
                        
                        tags$div(
                          HTML("<small>
                               <p>This application takes input in CSV format containing the population, baseline health levels, 
                               incremental health benefit and opportunity costs split by the population characteristics across 
                               which you want to analyse equity. The CSV file must contain four columns named: 
                               POPULATION, BASELINE, INCREMENTAL_HEALTH_BENEFIT and OPPORTUNITY_COST_WEIGHTS. Additional columns
                               should be added to capture each of the characteristics you want to be able to spilt 
                               the population by.</small>")
                          ),      
                        
                        fileInput('intervention_data_file', 'Choose CSV File',
                                  accept=c('text/csv', 
                                           'text/comma-separated-values,
                                           text/plain', 
                                           '.csv')),
                        tags$div(
                          HTML("<small>
                               An example data file for England allowing you to split data by socioeconomic status
                               and region can be downloaded below to help understand the input data format. If no input
                               data file is selected this is the data that is used to generate the default health 
                               distributions shown here.<p></small>")
                          ),
                        
                        downloadButton("download_sample_intervention_data", "Download Sample Dataset in CSV format"),
                        
                        numericInput("marginal_productivity_health", "Marginal productivity of the health system (cost per HALY)", 12000, min=0),
                        
                        numericInput("total_intervention_cost", "Total cost of intervention", 309705410),
                        
                        selectInput("opportunity_cost_scenario", 
                                    "Set opportunity cost distribution to:",
                                    list("As given in input" = "given",
                                         "Equaly distributed" = "equal",
                                         "All on least healthy" = "poor",
                                         "All on most healthy" = "rich"), 
                                    selected="given"),  
                        
                        downloadButton("download_post_intervention_data", "Download Post Intervention Health Distribution"),
                        
                        tags$div(
                          HTML("<small>
                               <p>This site was produced by <a href='https://github.com/miqdadasaria'>Miqdad Asaria</a> 
                               <p>Source code can be found <a href='https://github.com/miqdadasaria/DCEA'>here</a>.</small>")
                          )
                          ),
                      mainPanel(
                        tabsetPanel(id="tabset",
                                    tabPanel("Health distribution plot", plotOutput("intervention_plot")),
                                    tabPanel("Health distribution data", div(dataTableOutput("intervention_health_dist"), style = "font-size:70%")),
                                    tabPanel("Raw input data", div(dataTableOutput("raw_intervention_input_data"), style = "font-size:70%"))
                        )
                        
                      )
                        ),
             
             # end intervention distribution UI panel
             
             
             
             ##### Atkinson UI panel ####
             tabPanel("Evaluate Using Social Welfare Function",
                      
                      sidebarPanel(
                        h3("Evaluate polcies using SWF"),
                        
                        tags$div(
                          HTML("<small>
                               <p>This application takes input in CSV format containing the net health benefits 
                               resulting from alternative health policies split by the population characteristics across 
                               wish you want to analyse equity. The CSV file should contain a column titled 
                               POPULATION, a column for each of the equity relevant characteristics you want to be able 
                               to spilt the analysis by and a column detailing the net health benefits produced by each 
                               policy to be evaluated. The columns containing net health benefits related to each 
                               policy should be named POLICY_[POLICY_NAME] where [POLICY_NAME] describes the policy.
                               The first policy listed in the file will be taken as the baseline against which other
                               policies will be compared.</small>")
                          ),      
                        
                        fileInput('nhb_data_file', 'Choose CSV File',
                                  accept=c('text/csv', 
                                           'text/comma-separated-values,
                                           text/plain', 
                                           '.csv')),
                        tags$div(
                          HTML("<small>
                               An example data file for the NHS nicotine replacement therapy programme allowing you 
                               to split data by socioeconomic status and region can be downloaded below to help 
                               understand the input data format. If no input data file is selected this is the data 
                               that is used to generate the default results shown here.<p></small>")
                          ),
                        
                        downloadButton("download_sample_nhb_data", "Download Sample NHB Dataset in CSV format"),
                        
                        uiOutput("baseline_policy_name"),
                        
                        tags$div(
                          HTML("<small><p>
                               <p>This site was produced by <a href='https://github.com/miqdadasaria'>Miqdad Asaria</a> 
                               <p>Source code can be found <a href='https://github.com/miqdadasaria/DCEA'>here</a>.</small>")
                          )
                          ),
                      mainPanel(
                        tabsetPanel(id="tabset_atkinson",
                                    tabPanel("Atkinson EDE Plot", plotOutput("atkinson_ede_plot")),
                                    tabPanel("Atkinson EDE Table", div(dataTableOutput("atkinson_ede_table"), style = "font-size:70%")),
                                    tabPanel("Gini EDE Plot", plotOutput("gini_ede_plot")),
                                    tabPanel("Gini EDE Table", div(dataTableOutput("gini_ede_table"), style = "font-size:70%")),
                                    tabPanel("Kolm EDE Plot", plotOutput("kolm_ede_plot")),
                                    tabPanel("Kolm EDE Table", div(dataTableOutput("kolm_ede_table"), style = "font-size:70%")),
                                    tabPanel("Equity Impact Plane", 
                                             plotOutput("equity_impact_plane_plot"),
                                             selectInput("index", "Social Welfare Function", choices = c("Atkinson","Extended Gini","Kolm")),
                                             sliderInput("inequity_aversion", "Inequity aversion parameter", min=0, max=15, value=10, step=0.005)
                                             ),
                                    tabPanel("Raw input data", div(dataTableOutput("raw_nhb_input_data"), style = "font-size:70%"))
                        )
                        
                      )
                        ) ,      
             # end Atkinson UI panel
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