library("tidyverse")

calculate_atkinson_ede = function(NHB,POPULATION){
  results = data_frame()
  for(e in seq(0,20,2)){
    atkinson=(NHB^(1-e))*POPULATION
    ede = (sum(atkinson)/sum(POPULATION))^(1/(1-e))
    results[1,paste("e",e,sep="_")] = ede
  }
  return(results)
}

calculate_atkinson_ede_table = function(nhb_data){
  results=nhb_data %>% 
    select(POPULATION,starts_with("POLICY_")) %>%
    gather(POLICY,NHB,-POPULATION) %>%
    group_by(POLICY) %>%
    do(calculate_atkinson_ede(.$NHB,.$POPULATION)) %>%
    ungroup() %>%
    mutate(POLICY=gsub("POLICY_","",POLICY),
           POLICY=gsub("_"," ",POLICY))
  
  return(results)
}

display_atkinson_ede_table = function(nhb_data){
  ede_table = calculate_atkinson_ede_table(nhb_data) %>%
    gather(E,EDE,-POLICY) %>%
    separate(E,c("x","e"),sep="_") %>%
    mutate('Atkinson \u03B5'=as.integer(e)) %>%
    select(-c(x,e)) %>%
    mutate(EDE=round(EDE,6)) %>%
    spread(POLICY,EDE)

  return(ede_table)
}

plot_atkinson_ede = function(nhb_data){
  graph_data = calculate_atkinson_ede_table(nhb_data)
  baseline = graph_data[1,1]$POLICY
  diff_data = t(t(graph_data[-1]) - t(graph_data[-1])[,1])
  graph_data = bind_cols(graph_data[,1],as_data_frame(diff_data)) %>%
    filter(POLICY!=baseline) %>%
    gather(E,EDE,-POLICY) %>%
    separate(E,c("x","e"),sep="_") %>%
    mutate(e=as.integer(e))
    
  plot = ggplot(graph_data) +
    geom_line(aes(x=e, y=EDE, group=POLICY, colour=POLICY)) + 
    ylab(paste("Atkinson EDE QALE compared to",baseline)) +
    xlab("Atkinson inequality aversion (\u03B5)") +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.title = element_blank(),
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          text=element_text(family = "Roboto", colour = "#3e3f3a"))
  
  return(plot)
}