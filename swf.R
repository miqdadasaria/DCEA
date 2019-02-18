library("tidyverse")

source("gini.R")
source("atkinson.R")
source("kolm.R")

calculate_ede_table = function(nhb_data, index, baseline){
  results=nhb_data %>% 
    select(POPULATION,starts_with("DECISION_")) %>%
    gather(DECISION,NHB,-POPULATION) %>%
    group_by(DECISION) 
  
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
    mutate(DECISION=gsub("DECISION_","",DECISION),
           DECISION=gsub("_"," ",DECISION))
  
  results = results %>%
    mutate(r=ifelse(DECISION==baseline,0,1)) %>%
    arrange(r) %>%
    select(-r)
  
  return(results)
}

display_ede_table = function(nhb_data, index, baseline){
  ede_table = calculate_ede_table(nhb_data, index, baseline) %>%
    gather(E,EDE,-DECISION) %>%
    separate(E,c("x","e"),sep="_") 
  
  total_population = sum(nhb_data$POPULATION)
  
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
    mutate(EDE=round(EDE*total_population,0)) %>%
    spread(DECISION,EDE)
  
  return(ede_table)
}

plot_ede = function(nhb_data, index, baseline){
  graph_data = calculate_ede_table(nhb_data, index, baseline)
  diff_data = t(t(graph_data[-1]) - t(graph_data[-1])[,1])
  graph_data = bind_cols(graph_data[,1],as_data_frame(diff_data)) %>%
    filter(DECISION!=baseline) %>%
    gather(E,EDE,-DECISION) %>%
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
    geom_line(aes(x=e, y=EDE*sum(nhb_data$POPULATION), group=DECISION, colour=DECISION), size=2) + 
    ggtitle(paste("Equity weighted NHB compared with",baseline)) +
    ylab(paste("Equity weighted HALYs")) +
    xlab(paste0(index," inequity aversion (",inequality_aversion,")")) +
    geom_hline(yintercept=0, colour="darkgrey") +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.title = element_text(lineheight=.8, face="bold", hjust=0.5),
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          text=element_text(family = "Roboto", colour = "#3e3f3a"))
  
  return(plot)
}

plot_equity_impact_plane = function(nhb_data, index, e, baseline){
  results=nhb_data %>% 
    select(POPULATION,starts_with("DECISION_")) %>%
    gather(DECISION,NHB,-POPULATION) %>%
    group_by(DECISION) 
  
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
    mutate(DECISION=gsub("DECISION_","",DECISION),
           DECISION=gsub("_"," ",DECISION))
  
  results = results%>%
    mutate(r=ifelse(DECISION==baseline,0,1)) %>%
    arrange(r) %>%
    select(-r)
  
  results$EDE = ((results$EDE-results$NHB) - (results$EDE[1]-results$NHB[1]))*sum(nhb_data$POPULATION)
  results$NHB = (results$NHB - results$NHB[1])*sum(nhb_data$POPULATION)
  baseline = results$DECISION[1]
  
  if(index=="Extended Gini"){
    inequality_aversion = "\u03B7"
  } else if(index=="Atkinson"){
    inequality_aversion = "\u03B5"
  } else if(index=="Kolm"){
    inequality_aversion = "\u03B1"
  }
  
  maximum = max(abs(c(results$EDE,results$NHB)))
  
  plot = ggplot(results%>%filter(DECISION!=baseline)) +
    geom_point(aes(x=EDE, y=NHB, group=DECISION, colour=DECISION), size=3) +
    geom_vline(xintercept=0, colour="grey") +
    geom_hline(yintercept=0, colour="grey") +
    geom_abline(intercept=0, slope=-1, linetype = "dashed") +
    ggtitle(paste("Health equity impact plane compared with",baseline)) +
    ylab(paste("Health Impact (HALYs)")) +
    xlab(paste0("Equity Impact (HALYs weighted using ",index," index ",inequality_aversion,"=",e,")")) +
    xlim(-1*maximum,maximum) +
    ylim(-1*maximum,maximum) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.title = element_text(lineheight=.8, face="bold", hjust=0.5),
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          text=element_text(family = "Roboto", colour = "#3e3f3a"))
  
  return(plot)
}