library(tidyverse)
library(ggpubr)

calculate_post_intervention_distribution = function(intervention_data, total_intervention_cost, marginal_productivity_health, opportunity_cost_scenario){
  num_subgroups = nrow(intervention_data)
  poor = which.min(intervention_data$BASELINE)
  rich = which.max(intervention_data$BASELINE)
  
  if(marginal_productivity_health!=0 & total_intervention_cost!=0){
    if(opportunity_cost_scenario=="given"){
      weights = (intervention_data$POPULATION*intervention_data$OPPORTUNITY_COST_WEIGHTS)/sum(intervention_data$POPULATION*intervention_data$OPPORTUNITY_COST_WEIGHTS)
    } else if(opportunity_cost_scenario=="equal"){
      weights = intervention_data$POPULATION/sum(intervention_data$POPULATION)
    } else if(opportunity_cost_scenario=="poor"){
      weights = rep(0,times=num_subgroups)
      weights[poor] = 1
    } else if(opportunity_cost_scenario=="rich"){
      weights = rep(0,times=num_subgroups)
      weights[rich] = 1
    }
    health_opportunity_cost = weights * (total_intervention_cost/marginal_productivity_health)
  } else {
    health_opportunity_cost = rep(0,times=num_subgroups)
  }
  
  post_intervention_data = bind_cols(intervention_data,"HEALTH_OPPORTUNITY_COST"=health_opportunity_cost) %>%
    mutate(NET_HEALTH_BENEFIT=INCREMENTAL_HEALTH_BENEFIT-(HEALTH_OPPORTUNITY_COST/POPULATION),
           POST_INTERVENTION=BASELINE+NET_HEALTH_BENEFIT,
           INCREMENTAL_HEALTH_BENEFIT=INCREMENTAL_HEALTH_BENEFIT*POPULATION,
           NET_HEALTH_BENEFIT=NET_HEALTH_BENEFIT*POPULATION) %>%
    arrange(BASELINE) 
  
  return(post_intervention_data)
}

plot_intervention = function(intervention_data, total_intervention_cost, marginal_productivity_health, opportunity_cost_scenario){

  graph_data = calculate_post_intervention_distribution(intervention_data, total_intervention_cost, marginal_productivity_health, opportunity_cost_scenario)
  
  subgroup_vars = setdiff(colnames(intervention_data),c("BASELINE","INCREMENTAL_HEALTH_BENEFIT","HEALTH_OPPORTUNITY_COST","NET_HEALTH_BENEFIT","POST_INTERVENTION","POPULATION","OPPORTUNITY_COST_WEIGHTS"))
  
  graph_data = graph_data %>%
    mutate(RIGHT = cumsum(POPULATION),
           LEFT = RIGHT-POPULATION,
           RIGHT = RIGHT/sum(POPULATION),
           LEFT = LEFT/sum(POPULATION)) %>%
    unite("LABEL",subgroup_vars,sep =":") %>%
    select(-c(POPULATION,OPPORTUNITY_COST_WEIGHTS)) %>%
    gather(key="variable",value="HALE",-c(LABEL,LEFT,RIGHT)) %>%
    mutate(variable=gsub("_"," ",variable), variable=factor(variable,
                                                            levels=c("BASELINE","POST INTERVENTION","INCREMENTAL HEALTH BENEFIT","HEALTH OPPORTUNITY COST","NET HEALTH BENEFIT"),
                                                            labels=c("BASELINE","POST INTERVENTION","INCREMENTAL HEALTH BENEFIT","HEALTH OPPORTUNITY COST","NET HEALTH BENEFIT")))
  
  plot1 = ggplot(graph_data %>% filter(variable %in% c("BASELINE","POST INTERVENTION")), aes(label=LABEL)) +
    geom_rect(aes(xmin=LEFT, xmax=RIGHT, ymin=0, ymax=HALE), colour="white", fill="#556B2F") +
    xlab("Proportion of Population") +
    ylab("Health Adjusted Life Expectancy at Birth") +
    geom_text(aes(x=(LEFT+RIGHT)/2, y=HALE/2), angle=90, colour="white", fontface="bold") + 
    facet_wrap(variable~.) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.title = element_blank(),
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          legend.position="none",
          text=element_text(family = "Roboto", colour = "#3e3f3a"))
  
  plot2 = ggplot(graph_data %>% filter(!(variable %in% c("BASELINE","POST INTERVENTION"))), aes(label=LABEL)) +
    geom_rect(aes(xmin=LEFT, xmax=RIGHT, ymin=0, ymax=HALE), colour="white", fill="#556B2F") +
    xlab("Proportion of Population") +
    ylab("Population Health Adjusted Life Years") +
    geom_text(aes(x=(LEFT+RIGHT)/2, y=HALE/2), angle=90, colour="white", fontface="bold") + 
    facet_wrap(variable~.) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.title = element_blank(),
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          legend.position="none",
          text=element_text(family = "Roboto", colour = "#3e3f3a"))
  
  plot = ggarrange(plot1, plot2,
                   ncol = 1, nrow = 2)
  
  return(plot)
}
