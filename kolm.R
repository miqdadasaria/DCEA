library("tidyverse")

calculate_kolm_ede = function(NHB,POPULATION,e){
  if(e==0){
    ede = weighted.mean(NHB,POPULATION)
  } else {
    ede = (-1/e)*log(weighted.mean(exp(-e * NHB),POPULATION))
  }
  return(ede)
}

calculate_kolm_ede_table = function(NHB,POPULATION){
  results = data_frame()
  for(e in seq(0.0,0.2,0.02)){
    results[1,paste("e",e,sep="_")] = calculate_kolm_ede(NHB,POPULATION,e)
  }
  return(results)
}

