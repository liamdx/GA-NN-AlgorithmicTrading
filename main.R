# Liam Devlin 20/03/2019
# Algorithmic Trading 

library(quantmod)
library(neuralnet)
library(Metrics)
library(GA)
library(SpatialEpi)
source("nn.r")
source("ga_pred.r")
source("ma.r")
source("random.r")

# Keep runs consistent
set.seed(1111)


# Neural Network predictions
print("Making time-series predictions")
# neural network parameters
threshold <- 0.006
nn_layers <- c(8,5,2)
# nn_layers <- c(7,5,2)
stepmax <- 1000000
train_test_percentage <- 80
# Lag values
lag_01 <- 1
lag_02 <- 2
lag_03 <- 3
lag_04 <- 4
# run nn
final_return <- getPredictions()

# Plot Nvidia as Timeseries
plot(nvda_open, col=3, main="Open and Close prices of NVDA Asset", xlab="Time",ylab="Price", panel.first = grid(), cex=0.5)
lines(nvda_open,lwd=3.5 ,col=3)
points(nvda_close, col=2)
lines(nvda_close, lwd=3.5 ,col=2)
months= index(NVDA)


# Trading Section
print("Trading on predictions")
# Trading parameters
init_budget <- 10000
# GA Parameters
popSize <- 2000
generations <- 500
# Run Trading GA
gaResult <- getGaResults()
print(paste("GA return:", gaResult))


# Moving average (for comparison)
print("Moving Average")
maResult <- getMovingAverageResult()
print(paste("Moving Average return:", maResult))


# Random Comparison
print("Random")
randomResult <- 0
# Aggregate 1000 Random results
for(i in 1:1000){
  randomResult <- randomResult + getRandomResult()
}
randomResult <- randomResult / 1000
print(paste("Random Decision Average return:", randomResult))

colors_vec = c("red","green","blue")
methods <- c("GA", "Moving Average", "Random")
results <- c(gaResult, maResult, randomResult)
barplot(results,  main = "Performance of Trading Methods", col = colors_vec, names.arg = methods, xlab = "Methods", ylab = "Return (%)", , panel.first = grid())