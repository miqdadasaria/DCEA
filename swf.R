library("tidyverse")

source("gini.R")
source("atkinson.R")
source("kolm.R")

so_gen_lorenz_dominance = function(lorenz_data){
  zeros = rep(0,ncol(lorenz_data))
  names(zeros) = names(lorenz_data)
  data = bind_rows(zeros,lorenz_data)
  data$cum_baseline = 0
  data$cum_intervention = 0
  
  if(nrow(data)>1){
    for (i in 2:nrow(data)) {
      cum_baseline = 0
      cum_intervention = 0
      for (j in 2:i) {
        cum_baseline = cum_baseline +data$baseline[j]*(data$POPULATION[i]-data$POPULATION[j-1])
        cum_intervention = cum_intervention +data$intervention[j]*(data$POPULATION[i]-data$POPULATION[j-1])
      }
      data$cum_baseline[i]=cum_baseline
      data$cum_intervention[i]=cum_intervention
    }
  }
  results = data %>% mutate(baseline=cum_baseline, intervention=cum_intervention) %>% select(-cum_baseline,-cum_intervention) %>% filter(POPULATION>0)
  return(results)
}

calculate_dominance_table = function(dominace_data){
  
  baseline = colnames(dominace_data)[2]
  intervention = colnames(dominace_data)[3]
  
  paraeto_dominance = suppressWarnings(
      dominace_data %>% 
      mutate(PARAETO = ifelse(.data[[baseline]]>.data[[intervention]],0,ifelse(.data[[intervention]]>.data[[baseline]],1,NA))) %>% 
      summarise(minimum=min(PARAETO,na.rm=TRUE),maximum=max(PARAETO,na.rm=TRUE)) %>%
      mutate(result=if(!is.infinite(minimum)){minimum+maximum}else{NA}, paraeto_dominance=if(is.na(result) | result==1){"No Dominance"} else if(result==2){"Dominates"} else{"Dominanted"}) %>%
      select(paraeto_dominance)) 
  
  total_population = sum(dominace_data$POPULATION)
  mean_health_baseline = weighted.mean(dominace_data[[baseline]],dominace_data$POPULATION)
  mean_health_intervention = weighted.mean(dominace_data[[intervention]],dominace_data$POPULATION)
  lorenz_dominance = suppressWarnings(
    dominace_data %>% 
    arrange(.data[[baseline]]) %>% 
    mutate(
           POPULATION=POPULATION/total_population,
           baseline = .data[[baseline]] * POPULATION / mean_health_baseline,
           baseline = cumsum(baseline),
           intervention = .data[[intervention]] * POPULATION / mean_health_intervention,
           intervention = cumsum(intervention),
           POPULATION=cumsum(POPULATION),
           LORENZ = ifelse(baseline>intervention,0,ifelse(intervention>baseline,1,NA))
           ) %>%
    summarise(minimum=min(LORENZ,na.rm=TRUE),maximum=max(LORENZ,na.rm=TRUE)) %>%
    mutate(result=if(!is.infinite(minimum)){minimum+maximum}else{NA}, lorenz_dominance=if(is.na(result) | result==1){"No Dominance"} else if(result==2){"Dominates"} else{"Dominanted"}) %>%
    select(lorenz_dominance))
  
  gen_lorenz_dominance = suppressWarnings(
    dominace_data %>% 
    arrange(.data[[baseline]]) %>% 
    mutate(
      POPULATION=POPULATION/total_population,
      baseline = .data[[baseline]] * POPULATION,
      baseline = cumsum(baseline),
      intervention = .data[[intervention]] * POPULATION,
      intervention = cumsum(intervention),
      POPULATION=cumsum(POPULATION),
      GEN_LORENZ = ifelse(baseline>intervention,0,ifelse(intervention>baseline,1,NA))
    ) %>%
    summarise(minimum=min(GEN_LORENZ,na.rm=TRUE),maximum=max(GEN_LORENZ,na.rm=TRUE)) %>%
    mutate(result=if(!is.infinite(minimum)){minimum+maximum}else{NA}, gen_lorenz_dominance=if(is.na(result) | result==1){"No Dominance"} else if(result==2){"Dominates"} else{"Dominanted"}) %>%
    select(gen_lorenz_dominance))
  
  so_gen_lorenz_dominance = suppressWarnings(dominace_data %>% 
                                            arrange(.data[[baseline]]) %>% 
                                            mutate(
                                              POPULATION=POPULATION/total_population,
                                              baseline = .data[[baseline]] * POPULATION,
                                              intervention = .data[[intervention]] * POPULATION,
                                              POPULATION=cumsum(POPULATION)) %>%
                                              so_gen_lorenz_dominance() %>%
                                              mutate(SO_GEN_LORENZ = ifelse(baseline>intervention,0,ifelse(intervention>baseline,1,NA))) %>%
                                            summarise(minimum=min(SO_GEN_LORENZ,na.rm=TRUE),maximum=max(SO_GEN_LORENZ,na.rm=TRUE)) %>%
                                            mutate(result=if(!is.infinite(minimum)){minimum+maximum}else{NA}, so_gen_lorenz_dominance=if(is.na(result) | result==1){"No Dominance"} else if(result==2){"Dominates"} else{"Dominanted"}) %>%
                                            select(so_gen_lorenz_dominance))
  
  results = bind_cols(
        paraeto_dominance,
         lorenz_dominance,
         gen_lorenz_dominance,
         so_gen_lorenz_dominance) %>% 
    mutate(intervention=intervention) %>%
    select(`Decision Option`=intervention,
            Pareto=paraeto_dominance,
            Lorenz=lorenz_dominance,
            `Generalized Lorenz`=gen_lorenz_dominance,
            `Second Order Generalized Lorenz`=so_gen_lorenz_dominance)
  
  return(results)
}

display_dominance_table = function(nhb_data, baseline){
  
  data = nhb_data %>% select("POPULATION",starts_with("DECISION"))
  names(data) = gsub("_"," ",gsub("DECISION_","",names(data)))
  
  results_list = list()
  
  for (decision in seq_along(data)) {
    dominace_data = data %>% select("POPULATION",baseline,decision)
    if (ncol(dominace_data)==3) {
      results_list[[paste0("decision:",decision)]] = calculate_dominance_table(dominace_data)
    }
  }
  
  results = bind_rows(results_list)
  
  return(results)
}


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
          plot.margin = unit(c(1, 1, 1, 1), "lines"))
  
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
          plot.margin = unit(c(1, 1, 1, 1), "lines"))
  
  return(plot)
}