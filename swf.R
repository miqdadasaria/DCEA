library("tidyverse")

source("gini.R")
source("atkinson.R")

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
      mutate('Atkinson \u03B5'=as.integer(e))
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
    mutate(e=as.integer(e))
  
  if(index=="Extended Gini"){
    inequality_aversion = "\u03B7"
  } else if(index=="Atkinson"){
    inequality_aversion = "\u03B5"
  }
  
  plot = ggplot(graph_data) +
    geom_line(aes(x=e, y=EDE, group=POLICY, colour=POLICY)) + 
    ylab(paste(index,"EDE QALE compared to",baseline)) +
    xlab(paste0(index," inequity aversion (",inequality_aversion,")")) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.title = element_blank(),
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          text=element_text(family = "Roboto", colour = "#3e3f3a"))
  
  return(plot)
}