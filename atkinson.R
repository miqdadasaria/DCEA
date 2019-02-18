library("tidyverse")

calculate_atkinson_ede = function(NHB,POPULATION,e){
  if(e==1){
    ede = prod(NHB^(POPULATION/sum(POPULATION)))
  } else {
    ede = (sum((NHB^(1-e))*POPULATION)/sum(POPULATION))^(1/(1-e))
  }
  return(ede)
}

calculate_atkinson_ede_table = function(NHB,POPULATION){
  results = data_frame()
  for(e in seq(0,15,1)){
    results[1,paste("e",e,sep="_")] = calculate_atkinson_ede(NHB,POPULATION,e)
  }
  return(results)
}

