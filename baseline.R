library("tidyverse")

calculate_qale = function(baseline_data_subgroup){
  qale = baseline_data_subgroup %>% 
    mutate(n=1+END_AGE-START_AGE, a=0.5, M=MORTALITY_RATE, 
           q=n*M/(1+(1-a)*n*M), 
           l=1, L=n, T=1, e=1, u=HRQL_SCORE, Y=1, 
           N=1,H=1) %>% 
    arrange(START_AGE)
    
    N=nrow(qale)
    qale$q[N] = 1
    
    qale$l[1] = 1
    for (i in 2:N) {
      qale$l[i] = (1-qale$q[i-1])*qale$l[i-1]
    }
    
    qale$L[N]=qale$n[N]*qale$a[N]*qale$l[N]
    for (i in 1:(N-1)) {
      qale$L[i] = qale$n[i]*(qale$l[i+1]+(qale$a[i]*(qale$l[i]-qale$l[i+1])))
    }
    
    qale$T[N] = qale$L[N]
    for (i in (N-1):1) {
      qale$T[i] = qale$T[i+1] + qale$L[i]
    }
    
    qale$e = qale$T/qale$l
    
    qale$Y = qale$L*qale$u

    qale$N[N] = qale$Y[N]
    for (i in (N-1):1) {
      qale$N[i] = qale$N[i+1] + qale$Y[i]
    }
    
    qale$H = qale$N/qale$l
      
    
  return(qale %>% select(START_AGE,END_AGE,QALE=H))
}

baseline_health_distribution = function(baseline_data, input_vars){
   qale_distribution = baseline_data %>%
    group_by_(.dots=input_vars) %>%
    do(calculate_qale(.)) %>%
    ungroup() %>%
    select_(.dots=c("START_AGE","END_AGE",input_vars,"QALE"))
  
  return(qale_distribution)
}

process_baseline_data = function(input_data, input_vars){

    group_vars = c("START_AGE","END_AGE",input_vars)
    aggregated_data = input_data %>% 
      group_by_(.dots=group_vars) %>%
      summarise( 
        MORTALITY_RATE=weighted.mean(MORTALITY_RATE,POPULATION),
        HRQL_SCORE=weighted.mean(HRQL_SCORE,POPULATION),
        POPULATION=sum(POPULATION)
      ) %>%
      ungroup() %>%
      select_(.dots=c("START_AGE","END_AGE",input_vars,"POPULATION","MORTALITY_RATE","HRQL_SCORE"))

  qale_distribution = baseline_health_distribution(aggregated_data, input_vars)
  
  processed_data = inner_join(aggregated_data, qale_distribution) %>%
    select_(.dots=c("START_AGE","END_AGE",input_vars,"POPULATION","QALE")) %>%
    arrange_(.dots=c(input_vars,"START_AGE"))
  
  return(processed_data)
}

baseline_health_distribution_plot = function(baseline_qale_distribution, input_vars){
  graph_data = 
    if(length(input_vars)==0){
      baseline_qale_distribution %>%
      mutate(TOTAL_POPULATION=sum(POPULATION),LABEL="whole population")
    } else {
      baseline_qale_distribution %>%
      group_by_(.dots=input_vars) %>%
      summarise(TOTAL_POPULATION=sum(POPULATION)) %>% 
      ungroup() %>%
      right_join(baseline_qale_distribution) %>%
      unite("LABEL",input_vars,sep =":")
    } 
  
  graph_data = graph_data %>%
      filter(START_AGE==0) %>%
      arrange(QALE) %>%
      mutate(RIGHT = cumsum(TOTAL_POPULATION),
             LEFT = RIGHT-TOTAL_POPULATION,
             RIGHT = RIGHT/sum(TOTAL_POPULATION),
             LEFT = LEFT/sum(TOTAL_POPULATION))

  plot = ggplot(graph_data, aes(label=LABEL)) +
    geom_rect(aes(xmin=LEFT, xmax=RIGHT, ymin=0, ymax=QALE), colour="white", fill="#556B2F") +
    xlab("Proportion of Population") +
    ylab("Quality Adjusted Life Expectancy at Birth") +
    geom_text(aes(x=(LEFT+RIGHT)/2, y=QALE/2), angle=90, colour="white", fontface="bold") + 
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.title = element_blank(),
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          legend.position="none",
          text=element_text(family = "Roboto", colour = "#3e3f3a"))
  
  return(plot)
}

inequality_summary = function(baseline_qale_distribution, input_vars){
  qale_at_birth = 
    if(length(input_vars)==0){
        baseline_qale_distribution %>%
        mutate(TOTAL_POPULATION=sum(POPULATION))
      } else {
        baseline_qale_distribution %>%
        group_by_(.dots=input_vars) %>%
        summarise(TOTAL_POPULATION=sum(POPULATION)) %>% 
        ungroup() %>%
        right_join(baseline_qale_distribution)
      }
  
  qale_at_birth = qale_at_birth %>%
      filter(START_AGE==0) %>%
      arrange(QALE) %>%
      mutate(RIGHT = cumsum(TOTAL_POPULATION),
           LEFT = RIGHT-TOTAL_POPULATION,
           MID = (RIGHT+LEFT)/(2*sum(TOTAL_POPULATION)),
           POP_FRACTION = TOTAL_POPULATION/sum(TOTAL_POPULATION),
           POP_QALE = POP_FRACTION * QALE) 
  
  absolutue_gap = max(qale_at_birth$QALE) - min(qale_at_birth$QALE)
  relative_gap = (max(qale_at_birth$QALE) / min(qale_at_birth$QALE)) - 1
  SII = coef(lm(QALE~MID,qale_at_birth))[2]
  RII = SII/sum(qale_at_birth$POP_QALE)
  
  results = data_frame("Indicator" = c("Absolute Gap","Relative Gap","SII","RII"),"Value"=c(absolutue_gap, relative_gap, SII, RII))
  return(results)
}