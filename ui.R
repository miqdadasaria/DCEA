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
                          <p>This site was produced by <a href='http://www.lse.ac.uk/lse-health/people/miqdad-asaria'>Miqdad Asaria</a> 
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
                                         "Equally distributed" = "equal",
                                         "All on least healthy" = "poor",
                                         "All on most healthy" = "rich"), 
                                    selected="given"),  
                        
                        downloadButton("download_post_intervention_data", "Download Post Intervention Health Distribution"),
                        
                        tags$div(
                          HTML("<small>
                               <p>This site was produced by <a href='http://www.lse.ac.uk/lse-health/people/miqdad-asaria'>Miqdad Asaria</a> 
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
             
             
             
             ##### SWF UI panel ####
             tabPanel("Evaluate Using Social Welfare Function",
                      
                      sidebarPanel(
                        h3("Evaluate decisions using SWF"),
                        
                        tags$div(
                          HTML("<small>
                               <p>This application takes input in CSV format containing the net health benefits 
                               resulting from alternative health decisions split by the population characteristics across 
                               wish you want to analyse equity. The CSV file should contain a column titled 
                               POPULATION, a column for each of the equity relevant characteristics you want to be able 
                               to spilt the analysis by and a column detailing the net health benefits produced by each 
                               decision to be evaluated. The columns containing net health benefits related to each 
                               decision should be named DECISION_[DECISION_NAME] where [DECISION_NAME] describes the decision.
                               The first decision listed in the file will be taken as the baseline against which other
                               decisions will be compared.</small>")
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
                        
                        uiOutput("baseline_decision_name"),
                        
                        tags$div(
                          HTML("<small><p>
                               <p>This site was produced by <a href='http://www.lse.ac.uk/lse-health/people/miqdad-asaria'>Miqdad Asaria</a> 
                               <p>Source code can be found <a href='https://github.com/miqdadasaria/DCEA'>here</a>.</small>")
                          )
                          ),
                      mainPanel(
                        tabsetPanel(id="tabset_swf",
                                    tabPanel("Dominance Analysis",
                                             div(dataTableOutput("dominance_table"), style = "font-size:70%"),
                                             HTML("<small><ol><li>Pareto Dominance:   At least one group is healthier, and no group less healthy.</li>
                                                          <li>Total Health Superiority:   Total health is higher, summing across all groups (i.e. the option lies to the North of the equity impact plane)</li>
                                                          <li>Lorenz Superiority:  Relative inequality is lower (i.e. the option lies to the West of the equity impact plane)</li>
                                                          <li>First Order Stochastic Dominance:  The option is better, if one accepts Pareto and anonymity (group identity does not matter).</li>
                                                          <li>Generalised Lorenz Dominance:  The option is better, if one accepts Pareto, anonymity, and the principle of health transfers (a hypothetical transfer of health from healthier to less healthy people ought to lead to a more equal health outcome).</li>
                                                          <li>Second Order Generalized Lorenz:  The option is better, if one accepts, Pareto, anonymity, health transfers, and the principle of downside positional transfer sensitivity (a transfer of health from one group to a less healthy group reduces inequality more when it occurs further down the distribution).</li>
                                                          </ol></small>")),
                                    tabPanel("Atkinson EDE Plot", plotOutput("atkinson_ede_plot"), 
                                             HTML("<small><ol><li>The equally distributed equivalent (EDE) plot is a way of assessing trade-offs between efficiency and equity by determining the degree of concern for equity (\"inequity aversion\") required to consider one decision better than another.</li>
                                                          <li>The point at which two lines cross shows the inequity aversion value required to consider one decision better than another, and the x-axis intercept shows the inequity aversion value required to consider that decision better than the baseline comparator decision.</li>
                                                          <li>The y-axis intercept shows the efficiency impact of the decision i.e. net health benefit (NHB) in health-adjusted life-years (HALYs).  When the inequity aversion parameter is zero, equity-weighted NHB is the same as unweighted NHB.</li>
                                                          <li>The equity impact of the decision is equity-weighted NHB minus unweighted NHB.  Equity-improving decisions (compared with the baseline) have an upward slope, since the equity improvement is valued more highly as inequity aversion increases; and equity-harming decisions have a downward slope.</li>
                                                          <li>Equity-weighted NHB is measured in terms of equity-weighted HALYs that have been standardised to make them comparable with ordinary HALYs. An equity-weighted HALY score of 1 represents a HALY that is equally distributed between all equity-relevant population groups.  An unequally distributed HALY can then be given a score above 1 if it reduces inequality and below 1 if it increases inequality, with the size of the differential depending on the value of the inequity aversion parameter.</li>
                                                          </ol></small>")),
                                    tabPanel("Atkinson EDE Table", div(dataTableOutput("atkinson_ede_table"), style = "font-size:70%")),
                                    tabPanel("Gini EDE Plot", plotOutput("gini_ede_plot"), 
                                             HTML("<small><ol><li>The equally distributed equivalent (EDE) plot is a way of assessing trade-offs between efficiency and equity by determining the degree of concern for equity (\"inequity aversion\") required to consider one decision better than another.</li>
                                                  <li>The point at which two lines cross shows the inequity aversion value required to consider one decision better than another, and the x-axis intercept shows the inequity aversion value required to consider that decision better than the baseline comparator decision.</li>
                                                  <li>The y-axis intercept shows the efficiency impact of the decision i.e. net health benefit (NHB) in health-adjusted life-years (HALYs).  When the inequity aversion parameter is one, equity-weighted NHB is the same as unweighted NHB.</li>
                                                  <li>The equity impact of the decision is equity-weighted NHB minus unweighted NHB.  Equity-improving decisions (compared with the baseline) have an upward slope, since the equity improvement is valued more highly as inequity aversion increases; and equity-harming decisions have a downward slope.</li>
                                                  <li>Equity-weighted NHB is measured in terms of equity-weighted HALYs that have been standardised to make them comparable with ordinary HALYs. An equity-weighted HALY score of 1 represents a HALY that is equally distributed between all equity-relevant population groups.  An unequally distributed HALY can then be given a score above 1 if it reduces inequality and below 1 if it increases inequality, with the size of the differential depending on the value of the inequity aversion parameter.</li>
                                                  <li>A \u03B7 parameter of 2 corresponds to the conventional Gini index.  If distribution A generalized Lorenz dominates (GLD) distribution B, then, using the extended Gini family of social welfare functions, the welfare (EDEH) generated by A is greater than that generated by B for any value of the parameter above 1. If A second order GLD B, then the EDEH of A is greater than that of B for any parameter value above 2. If A third order GLD B, then the EDEH of A is greater than that of B for any parameter value above 3.</li>
                                            </ol></small>")),
                                    tabPanel("Gini EDE Table", div(dataTableOutput("gini_ede_table"), style = "font-size:70%")),
                                    tabPanel("Kolm EDE Plot", plotOutput("kolm_ede_plot"), 
                                             HTML("<small><ol><li>The equally distributed equivalent (EDE) plot is a way of assessing trade-offs between efficiency and equity by determining the degree of concern for equity (\"inequity aversion\") required to consider one decision better than another.</li>
                                                  <li>The point at which two lines cross shows the inequity aversion value required to consider one decision better than another, and the x-axis intercept shows the inequity aversion value required to consider that decision better than the baseline comparator decision.</li>
                                                  <li>The y-axis intercept shows the efficiency impact of the decision i.e. net health benefit (NHB) in health-adjusted life-years (HALYs).  When the inequity aversion parameter is zero, equity-weighted NHB is the same as unweighted NHB.</li>
                                                  <li>The equity impact of the decision is equity-weighted NHB minus unweighted NHB.  Equity-improving decisions (compared with the baseline) have an upward slope, since the equity improvement is valued more highly as inequity aversion increases; and equity-harming decisions have a downward slope.</li>
                                                  <li>Equity-weighted NHB is measured in terms of equity-weighted HALYs that have been standardised to make them comparable with ordinary HALYs. An equity-weighted HALY score of 1 represents a HALY that is equally distributed between all equity-relevant population groups.  An unequally distributed HALY can then be given a score above 1 if it reduces inequality and below 1 if it increases inequality, with the size of the differential depending on the value of the inequity aversion parameter.</li>
                                                  </ol></small>")),
                                    tabPanel("Kolm EDE Table", div(dataTableOutput("kolm_ede_table"), style = "font-size:70%")),
                                    tabPanel("Equity Impact Plane", 
                                             plotOutput("equity_impact_plane_plot"),
                                             selectInput("index", "Social Welfare Function", choices = c("Atkinson","Extended Gini","Kolm")),
                                             sliderInput("inequity_aversion", "Inequity aversion parameter", min=0, max=15, value=10, step=0.005),
                                             HTML("<small><ol><li>The equity impact plane is a way of visualising trade-offs between efficiency impact (on the y-axis) and equity impact (on the x-axis).  The value of the equity impact depends on the chosen value of the inequity aversion parameter.</li>
                                                  <li>This version of the plane measures both efficiency and equity impact in comparable units of population-level health-adjusted life-years (HALYs).  This allows decisions to be compared using a simple, fixed 135 degree equity indifference line. Points to the right of the line are better than the baseline decision, and points to the left are worse.  The higher the inequity aversion parameter, the greater the value of the equity impact.</li>
                                                  <li>Efficiency impact is unweighted net health benefit (NHB).  Equity impact is equity-weighted NHB minus unweighted NHB.</li<
                                                  </ol></small>")
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
                                             <dd></dd>
                                             <p>
                                             
                                             <dt>Atkinson index</dt>
                                             <dd></dd>
                                             <p>
                                             </dl>"))
                      
             )
             # end notes UI panel
             )
             )