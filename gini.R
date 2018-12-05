library("tidyverse")

calculate_gini_ede = function(NHB,POPULATION){
  results = data_frame()
  CUMULATIVE_POPULATION = cumsum(POPULATION)
  TOTAL_POPULATION = sum(POPULATION)
  POPULATION_FRACTION = POPULATION/TOTAL_POPULATION
  CUMULATIVE_POPULATION_FRACTION = CUMULATIVE_POPULATION/TOTAL_POPULATION

  MEAN_QALE = weighted.mean(NHB,POPULATION)
  for(e in seq(0.5,5,0.5)){
    GINI_WEIGHTS = CUMULATIVE_POPULATION
      GINI_WEIGHTS[1] = POPULATION_FRACTION[1] - (1^e - (1-CUMULATIVE_POPULATION_FRACTION[1])^e)
    for (i in 2:length(GINI_WEIGHTS)) {
      GINI_WEIGHTS[i] = POPULATION_FRACTION[i] - ((1-CUMULATIVE_POPULATION_FRACTION[i-1])^e - (1-CUMULATIVE_POPULATION_FRACTION[i])^e)
    }
    GINI_MEAN_QALE = t(NHB) %*% GINI_WEIGHTS
    ede = MEAN_QALE - GINI_MEAN_QALE
    results[1,paste("e",e,sep="_")] = ede
  }
  return(results)
}

calculate_gini_ede_table = function(nhb_data){
  results=nhb_data %>% 
    select(POPULATION,starts_with("POLICY_")) %>%
    gather(POLICY,NHB,-POPULATION) %>%
    group_by(POLICY) %>%
    do(calculate_gini_ede(.$NHB,.$POPULATION)) %>%
    ungroup() %>%
    mutate(POLICY=gsub("POLICY_","",POLICY),
           POLICY=gsub("_"," ",POLICY))
  
  return(results)
}

display_gini_ede_table = function(nhb_data){
  ede_table = calculate_gini_ede_table(nhb_data) %>%
    gather(E,EDE,-POLICY) %>%
    separate(E,c("x","e"),sep="_") %>%
    mutate('Gini \u03B7'=as.double(e)) %>%
    select(-c(x,e)) %>%
    mutate(EDE=round(EDE,6)) %>%
    spread(POLICY,EDE)

  return(ede_table)
}

plot_gini_ede = function(nhb_data){
  graph_data = calculate_gini_ede_table(nhb_data)
  baseline = graph_data[1,1]$POLICY
  diff_data = t(t(graph_data[-1]) - t(graph_data[-1])[,1])
  graph_data = bind_cols(graph_data[,1],as_data_frame(diff_data)) %>%
    filter(POLICY!=baseline) %>%
    gather(E,EDE,-POLICY) %>%
    separate(E,c("x","e"),sep="_") %>%
    mutate(e=as.integer(e))
    
  plot = ggplot(graph_data) +
    geom_line(aes(x=e, y=EDE, group=POLICY, colour=POLICY)) + 
    ylab(paste("Gini EDE QALE compared to",baseline)) +
    xlab("Gini inequality aversion (\u03B7)") +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.title = element_blank(),
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          text=element_text(family = "Roboto", colour = "#3e3f3a"))
  
  return(plot)
}