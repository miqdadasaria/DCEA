library("tidyverse")

calculate_kolm_ede = function(NHB,POPULATION,e){
  ede = (-1/e)*log(weighted.mean(exp(-e * NHB),POPULATION))
  return(ede)
}

calculate_kolm_ede_table = function(NHB,POPULATION){
  results = data_frame()
  for(e in seq(0.0125,0.125,0.0125)){
    results[1,paste("e",e,sep="_")] = calculate_kolm_ede(NHB,POPULATION,e)
  }
  return(results)
}

