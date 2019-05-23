getRandomResult <- function(x){
  recorded_close_test <- final_return[[6]]
  predicted_close_test <- final_return[[8]]
  
  budget <- init_budget
  stocks_held <- 0
  
  for(i in 1:length(predicted_close_test)){
    currentAssetPrice <- recorded_close_test[i]
    randomChoice <- runif(1, -1, 1)
    
    if(randomChoice > 0.25){
      # buy
      assetsToBuy <- as.integer(budget / currentAssetPrice)
      stocks_held <- stocks_held + assetsToBuy
      budget <- budget - (currentAssetPrice * assetsToBuy)
    }
    else if (randomChoice < -0.25) {
      # sell
      budget <- budget + (currentAssetPrice * stocks_held)
      stocks_held <- 0
    }
  }
  
  dump <- stocks_held * recorded_close_test[length(recorded_close_test)]
  final_budget <- budget + dump
  final_profit <- ((final_budget - init_budget) / init_budget) * 100
  return(final_profit)
}
