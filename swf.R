library("tidyverse")

source("gini.R")
source("atkinson.R")
source("kolm.R")

calculate_ede_table = function(nhb_data, index){
  results=nhb_data %>% 
    select(POPULATION,starts_with("POLICY_")) %>%
    gather(POLICY,NHB,-POPULATION) %>%
    group_by(POLICY) 
  
  if(index=="Extended Gini"){
    results = results %>%
      do(calculate_extended_gini_ede_table(.$NHB,.$POPULATION))
  } else if(index=="Atkinson"){
    results = results %>%
      do(calculate_atkinson_ede_table(.$NHB,.$POPULATION))
  } else if(index=="Kolm"){
    results = results %>%
      do(calculate_kolm_ede_table(.$NHB,.$POPULATION))
  } 
  
  results = results %>%
    ungroup() %>%
    mutate(POLICY=gsub("POLICY_","",POLICY),
           POLICY=gsub("_"," ",POLICY))
  
  return(results)
}

display_ede_table = function(nhb_data, index){
  ede_table = calculate_ede_table(nhb_data, index) %>%
    gather(E,EDE,-POLICY) %>%
    separate(E,c("x","e"),sep="_") 
  
  if(index=="Extended Gini"){
    ede_table = ede_table %>%
      mutate('Extended Gini \u03B7'=as.double(e)) 
  } else if(index=="Atkinson"){
    ede_table = ede_table %>%    
      mutate('Atkinson \u03B5'=as.double(e))
  } else if(index=="Kolm"){
    ede_table = ede_table %>%    
      mutate('Kolm \u03B1'=as.double(e))
  } 
  
  ede_table = ede_table %>%
    select(-c(x,e)) %>%
    mutate(EDE=round(EDE,6)) %>%
    spread(POLICY,EDE)
  
  return(ede_table)
}

plot_ede = function(nhb_data, index){
  graph_data = calculate_ede_table(nhb_data, index)
  baseline = graph_data[1,1]$POLICY
  diff_data = t(t(graph_data[-1]) - t(graph_data[-1])[,1])
  graph_data = bind_cols(graph_data[,1],as_data_frame(diff_data)) %>%
    filter(POLICY!=baseline) %>%
    gather(E,EDE,-POLICY) %>%
    separate(E,c("x","e"),sep="_") %>%
    mutate(e=as.double(e))
  
  if(index=="Extended Gini"){
    inequality_aversion = "\u03B7"
  } else if(index=="Atkinson"){
    inequality_aversion = "\u03B5"
  } else if(index=="Kolm"){
    inequality_aversion = "\u03B1"
  }
  
  plot = ggplot(graph_data) +
    geom_line(aes(x=e, y=EDE*sum(nhb_data$POPULATION), group=POLICY, colour=POLICY), size=2) + 
    ylab(paste(index,"EDE population QALYs compared to",baseline)) +
    xlab(paste0(index," inequity aversion (",inequality_aversion,")")) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.title = element_blank(),
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          text=element_text(family = "Roboto", colour = "#3e3f3a"))
  
  return(plot)
}

plot_equity_impact_plane = function(nhb_data, index, e){
  results=nhb_data %>% 
    select(POPULATION,starts_with("POLICY_")) %>%
    gather(POLICY,NHB,-POPULATION) %>%
    group_by(POLICY) 
  
  if(index=="Extended Gini"){
    results = results %>%
      summarise(EDE=calculate_extended_gini_ede(NHB,POPULATION,e),NHB=weighted.mean(NHB,POPULATION))
  } else if(index=="Atkinson"){
    results = results %>%
        summarise(EDE=calculate_atkinson_ede(NHB,POPULATION,e),NHB=weighted.mean(NHB,POPULATION))
  } else if(index=="Kolm"){
    results = results %>%
      summarise(EDE=calculate_kolm_ede(NHB,POPULATION,e),NHB=weighted.mean(NHB,POPULATION))
  } 
  
  results = results %>%
    ungroup() %>%
    mutate(POLICY=gsub("POLICY_","",POLICY),
           POLICY=gsub("_"," ",POLICY))
  
  results$EDE = (results$EDE - results$EDE[1])*sum(nhb_data$POPULATION)
  results$NHB = (results$NHB - results$NHB[1])*sum(nhb_data$POPULATION)
  baseline = results$POLICY[1]
  
  if(index=="Extended Gini"){
    inequality_aversion = "\u03B7"
  } else if(index=="Atkinson"){
    inequality_aversion = "\u03B5"
  } else if(index=="Kolm"){
    inequality_aversion = "\u03B1"
  }
  
  plot = ggplot(results%>%filter(POLICY!=baseline)) +
    geom_point(aes(x=EDE, y=NHB, group=POLICY, colour=POLICY), size=3) + 
    ylab(paste("Health Impact population QALYs incremental to",baseline)) +
    xlab(paste0("Equity Impact EDE population QALYs incremental to ",baseline," using ",index," index (",inequality_aversion,"=",e,")")) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.title = element_blank(),
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          text=element_text(family = "Roboto", colour = "#3e3f3a"))
  
  return(plot)
}