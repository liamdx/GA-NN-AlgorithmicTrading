library(GA)
library(SpatialEpi)

recorded_close <- final_return[[6]]
# MACD with recorded values for training segment and predicted values for test
# If above 0 sell signal, if less than 0, buy signal
macd_open <- MACD(append(final_return[[1]], final_return[[7]]), nFast = 3, nSlow = 7, nSig = 3)[test_split_index:n_instances]
# Again, used the recorded values for training segment of the data and predictions for test
# Rsi, greater than 70 is a sell signal, less than 30 is a buy signal
rsi_open <- RSI(append(final_return[[1]], final_return[[7]]), n = 3)[test_split_index:n_instances]

predicted_fitness <- function(x){
  weights <- normalize(x[1:2])
  budget<-init_budget
  stocks_held <- 0
  last_stock_buy_price <- 0
  
  macd_open_weight <- weights[1]
  rsi_open_weight <- weights[2]
  
  
  markup <- x[3]
  buyCutoff <- x[4]
  sellCutoff <- x[5]
  rsiBuyCutoff <- x[6] * 100
  rsiSellCutoff <- x[7] * 100
  macdBuyCutoff <- x[8]
  macdSellCutoff <- x[9]
  
  # For loop simulates each day of trading
  for(i in 1:length(macd_open)){
    # if greater than 0.5 buy, if less than -0.5 sell
    decision_threshold <- 0
    # macd open
    if(macd_open[i] >= macdBuyCutoff){
      strength_multiplier <- macd_open[i] / max(macd_open)
      decision_threshold <- decision_threshold - (1 * strength_multiplier) * macd_open_weight
    }
    else if(macd_open[i] <= -(macdSellCutoff)){
      strength_multiplier <- abs(macd_open[i]) / abs(min(macd_open))
      decision_threshold <- decision_threshold + (1 * strength_multiplier) * macd_open_weight
    }
    
    # rsi open
    if(rsi_open[i] >= rsiBuyCutoff){
      strength_multiplier <- (rsi_open[i] - rsiBuyCutoff) / (100 - rsiBuyCutoff)
      decision_threshold <- decision_threshold - (1 * strength_multiplier) * rsi_open_weight
    }
    else if(rsi_open[i] <= rsiSellCutoff){
      strength_multiplier <- (rsiSellCutoff-rsi_open[i]) / (rsiSellCutoff)
      decision_threshold <- decision_threshold + (1 * strength_multiplier) * rsi_open_weight
    }
    
    # Make decision
    if(decision_threshold >= buyCutoff){
      # buy signal, get the actual price
      currentAssetPrice <- recorded_close[i]
      
      if(budget > currentAssetPrice){
        # Make number of assets bought proportional to the signal strength
        buyMultiplier <- ((decision_threshold - 0.25) / 75) * 100
        maxAssets <- as.integer(budget / currentAssetPrice)
        assetsToBuy <- as.integer(maxAssets * buyMultiplier)
        # buy the stocks
        stocks_held <- stocks_held + assetsToBuy
        budget <- budget - (currentAssetPrice * assetsToBuy)
        last_stock_buy_price <- currentAssetPrice
      }
    }
    else if(decision_threshold <= -(sellCutoff))
    {
      if(stocks_held > 0){
        currentAssetPrice <- recorded_close[i]
        # does the current asset price meet the markup requirement?
        if(currentAssetPrice > last_stock_buy_price  * (1 + markup)){
          # Sell signal, again, proportional to strength of the signal
          sellMultiplier <- ((abs(decision_threshold) - 0.25) / 75) * 100
          assetsToSell <- as.integer(stocks_held * sellMultiplier)
          budget <- budget + (currentAssetPrice * assetsToSell)
          stocks_held <- stocks_held - assetsToSell
        }
      }
    }
  }
  # any assets we still have at the end of the training period
  dump <- stocks_held * recorded_close[length(recorded_close)]
  return(budget + dump)
}

getGaResults <- function(x){
  #  0.01260358 0.6873197 0.7127867 0.2865611 0.5537719 0.2578408 0.9994718 0.7861834 0.5642601
  
  # predicted_fitness(c(0.012,0.68,0.71, 0.28, 0.55, 0.25,0.98, 0.78, 0.56))
  GA<-ga(type='real-valued', fitness=predicted_fitness, lower=rep(0,9), upper=rep(1,9),
         popSize = popSize, maxiter=generations,keepBest=TRUE)
  GA@solution
  final_budget <- GA@fitnessValue
  final_profit <- ((final_budget - init_budget) / init_budget) * 100
  return(final_profit)
}
