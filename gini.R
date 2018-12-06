library("tidyverse")

calculate_extended_gini_ede = function(NHB,POPULATION,e){
  CUMULATIVE_POPULATION = cumsum(POPULATION)
  TOTAL_POPULATION = sum(POPULATION)
  POPULATION_FRACTION = POPULATION/TOTAL_POPULATION
  CUMULATIVE_POPULATION_FRACTION = CUMULATIVE_POPULATION/TOTAL_POPULATION
  MEAN_QALE = weighted.mean(NHB,POPULATION)
  GINI_WEIGHTS = CUMULATIVE_POPULATION
  GINI_WEIGHTS[1] = POPULATION_FRACTION[1] - (1^e - (1-CUMULATIVE_POPULATION_FRACTION[1])^e)
  for (i in 2:length(GINI_WEIGHTS)) {
    GINI_WEIGHTS[i] = POPULATION_FRACTION[i] - ((1-CUMULATIVE_POPULATION_FRACTION[i-1])^e - (1-CUMULATIVE_POPULATION_FRACTION[i])^e)
  }
  GINI_MEAN_QALE = t(NHB) %*% GINI_WEIGHTS
  ede = MEAN_QALE - GINI_MEAN_QALE
  
  return(ede)
}

calculate_extended_gini_ede_table = function(NHB,POPULATION){
  results = data_frame()
  for(e in seq(1,5,0.5)){
     results[1,paste("e",e,sep="_")] = calculate_extended_gini_ede(NHB,POPULATION,e)
  }
  return(results)
}


