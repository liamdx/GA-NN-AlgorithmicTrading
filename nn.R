myStocks<-c("NVDA")
getSymbols(myStocks, src="yahoo", from="2018-06-01", to="2018-09-01")

# Get values we are interested in
nvda_close <- coredata(NVDA$NVDA.Close)[,1]
nvda_close_l1 <- Lag(nvda_close, lag_01)
nvda_close_l2 <- Lag(nvda_close, lag_02)
nvda_close_l3 <- Lag(nvda_close, lag_03)
nvda_close_l4 <- Lag(nvda_close, lag_04)
nvda_open <- coredata(NVDA$NVDA.Open)[,1]
nvda_open_l1 <- Lag(nvda_open, lag_01)
nvda_open_l2 <- Lag(nvda_open, lag_02)
nvda_open_l3 <- Lag(nvda_open, lag_03)
nvda_open_l4 <- Lag(nvda_open, lag_04)

# Create dataframe and add lags
nvda_lagged <- data.frame(nvda_close, nvda_open, 
                          nvda_close_l1, nvda_close_l2, nvda_close_l3, nvda_close_l4, 
                          nvda_open_l1, nvda_open_l2, nvda_open_l3, nvda_open_l4)

nvda_lagged <- nvda_lagged[complete.cases(nvda_lagged),]
scaled_nvda <- scale(nvda_lagged, scale=TRUE, center = TRUE)
scale_parameters <- attributes(scaled_nvda)[3:4]

# Reformat the data for training
colnames(scaled_nvda) <- paste(c('a','b','c','d','e','f','g','h','i','j'))
rownames(scaled_nvda) <- NULL

# Test Train Split
n_instances <- length(scaled_nvda[,1])
test_split_index <- as.integer((n_instances / 100) * train_test_percentage)


getPredictions <- function(x){
  train_data <- as.ts(scaled_nvda[1:test_split_index-1,])
  test_data <- as.ts(scaled_nvda[test_split_index:n_instances,])
  
  # Neural net for open / close prices
  f <- as.formula("a + b ~ c + d + e + f + g + h + i + j")
  current_nn <- neuralnet(f, data=train_data,  hidden=nn_layers, threshold = threshold, stepmax=stepmax)
  
  # current nn train results
  nn_train_predictions <- compute(current_nn, train_data)
  nn_train_results = nn_train_predictions$net.result
  
  # current nn test results
  nn_test_predictions <- compute(current_nn, test_data)
  nn_test_results = nn_test_predictions$net.result
  
  # Plot train and test accuracy
  plot(train_data[,1], lwd=4, col=1, main="NVDA Training Set", ylim=c(-3,3),ylab = "Price",  panel.first = grid())
  lines(train_data[,2],lwd=3.5 ,col=2)
  lines(nn_train_results[,1],lwd = 2.5, col=3)
  lines(nn_train_results[,2],lwd=2, col=4)
  legend("bottomright",  c("Recorded Close", "Recorded Open", "NN Close", "NN Open"), cex=1.0, fill=1:4)
  
  plot(test_data[,1], lwd=4, col=1, main="NVDA Test Set", ylim=c(-3,3), ylab="Price", panel.first = grid())
  lines(test_data[,2],lwd=3.5 ,col=2)
  lines(nn_test_results[,1],lwd = 2.5, col=3)
  lines(nn_test_results[,2],lwd=2, col=4)
  legend("bottomright", c("Recorded Close", "Recorded Open", "NN Close", "NN Open"), cex=1.0, fill=1:4)
  
  
  r_open_train <- coredata(NVDA$NVDA.Open)[,1][1:test_split_index-1]
  r_close_train <- coredata(NVDA$NVDA.Close)[,1][1:test_split_index-1]
  
  r_open_test <- coredata(NVDA$NVDA.Open)[,1][test_split_index:n_instances]
  r_close_test <- coredata(NVDA$NVDA.Close)[,1][test_split_index:n_instances]
  
  # rescale
  nn_open_train <- nn_train_results[,2] * scale_parameters[[2]][2] + scale_parameters[[1]][2]
  nn_close_train <- nn_train_results[,1] * scale_parameters[[2]][1] + scale_parameters[[1]][1]
  nn_open_test <- nn_test_results[,2] * scale_parameters[[2]][2] + scale_parameters[[1]][2]
  nn_close_test <- nn_test_results[,1] * scale_parameters[[2]][1] + scale_parameters[[1]][1]
  
  f_return <- list(r_open_train, r_close_train, 
                   nn_open_train, nn_close_train,
                   r_open_test, r_close_test,
                   nn_open_test, nn_close_test)
  
  return(f_return)
}
