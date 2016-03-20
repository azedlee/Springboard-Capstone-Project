#Install and Run e1071
library(e1071)

#Input CSV into variable
inputData <- SkillCraft1_Dataset
str(inputData)

#Random number generator to create trainldx and testldx
set.seed(1234)
completeSet <- sample(nrow(inputData), 0.7*nrow(inputData), replace = FALSE)
trainldx <- inputData[completeSet,]
testldx <- inputData[-completeSet,]

#Create trainY and testY - LeagueIndex requires to be tested against
trainY <- trainldx$LeagueIndex
testY <- testldx$LeagueIndex

#Create trainX and testX - columns to test against trainY and testY
trainX <- trainldx[-2]
testX <- testldx[-2]

#Create an array to check Cost
costArray <- seq(0.3, 0.31, by = 0.01)

#Create an array to check Gamma
gammaArray <- seq(0.01, 0.011, by = 0.001)

RMSE_Length <- length(gammaArray)

#Save results for testRMSE
testRMSE <- data.frame("Gamma" = numeric(length(gammaArray)), "Cost" = numeric(length(costArray)), "RMSE" = numeric(RMSE_Length), stringsAsFactors = FALSE)

#For loop to check for best cost
for(count in 1:(length(costArray)))
{
  svm_model <- svm(trainY ~ ., data = cbind(trainX, trainY), kernel = "radial", cost = costArray[count], gamma = 1, type = "eps-regression", epsilon = 1)
  predictedY <- predict(svm_model, testX)
  evaluate_RMSE <- sqrt(mean((testY - predictedY)^2))
  testRMSE$Cost[count] <- costArray[count]
  testRMSE$RMSE[count] <- evaluate_RMSE
}

#For loop to check for best Gamma
for(count in 1:length(gammaArray))
{
  svm_model <- svm(trainY ~ ., data = cbind(trainX, trainY), kernel = "radial", gamma = gammaArray[count], cost = 0.3, type = "eps-regression", epsilon = 1)
  predictedY <- predict(svm_model, testX)
  evaluate_RMSE <- sqrt(mean((testY - predictedY)^2))
  testRMSE$Gamma[count] <- gammaArray[count]
  testRMSE$Cost[count] <- 0.3
  testRMSE$RMSE[count] <- evaluate_RMSE
}

#Optimal Cost and Gamma
optimalCost <- 0.3
optimalGamma <- 0.011

#Testing for trainRMSE
trainRMSE <- data.frame("Gamma" = numeric(length(gammaArray)), "Cost" = numeric(length(costArray)), "TrainRMSE" = numeric(RMSE_Length), stringsAsFactors = FALSE)

for(count in 1:RMSE_Length)
{
  svm_model <- svm(trainY ~ ., data = cbind(trainX, trainY), kernel = "radial", cost = optimalCost, gamma = optimalGamma, type = "eps-regression", epsilon = 1)
  predictedY <- predict(svm_model, trainX)
  evaluate_RMSE <- sqrt(mean((trainY - predictedY)^2))
  trainRMSE$Gamma[count] <- optimalGamma
  trainRMSE$Cost[count] <- optimalCost
  trainRMSE$TrainRMSE[count] <- evaluate_RMSE
}

#Creating 5 equally size folds
#folds <- cut(seq(1, nrow(trainldx)), breaks = 5, labels = FALSE)
folds <- round(seq(1, nrow(trainldx), by = nrow(trainldx)/5))

#Calculate CVerror
CVerror <- vector("list", 5)
for(i in 1:5)
{
  svm_model <- svm(x = trainX[unlist(folds[[-i]])], y = trainY[unlist(folds[[-i]])], gamma = optimalGamma, cost = optimalCost, kernel = "radial", type = "eps-regression", epsilon = 1)
  predictedY <- predict(svm_model, testX)
  CVerror[i] <- sqrt(mean((trainY - predictedY)^2))
}
cvRMSE <- (CVerror[1] + CVerror[2] + CVerror[3] + CVerror[4] + CVerror[5])/5

##Action Points##
#vi) Generate a plot with three lines each visualizing trainRMSE, testRMSE and cvRMSE.
#Commands to use are plot(...), lines(..) etc.

