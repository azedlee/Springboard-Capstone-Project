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
costArray <- seq(0.25, 0.35, by = 0.01)

#Create an array to check Gamma
gammaArray <- seq(0.005, 0.015, by = 0.001)

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
  svm_model <- svm(trainY ~ ., data = cbind(trainX, trainY), kernel = "radial", gamma = gammaArray[count], cost = costArray[count], type = "eps-regression", epsilon = 1)
  predictedY <- predict(svm_model, testX)
  evaluate_RMSE <- sqrt(mean((testY - predictedY)^2))
  testRMSE$Gamma[count] <- gammaArray[count]
  testRMSE$Cost[count] <- costArray[count]
  testRMSE$RMSE[count] <- evaluate_RMSE
}

#Optimal Cost and Gamma
optimalCost <- 0.32
optimalGamma <- 0.012

#Testing for trainRMSE
trainRMSE <- data.frame("Gamma" = numeric(length(gammaArray)), "Cost" = numeric(length(costArray)), "TrainRMSE" = numeric(RMSE_Length), stringsAsFactors = FALSE)

for(count in 1:RMSE_Length)
{
  svm_model <- svm(trainY ~ ., data = cbind(trainX, trainY), kernel = "radial", gamma = gammaArray[count], cost = costArray[count], type = "eps-regression", epsilon = 1)
  predictedY <- predict(svm_model, trainX)
  evaluate_RMSE <- sqrt(mean((trainY - predictedY)^2))
  trainRMSE$Gamma[count] <- gammaArray[count]
  trainRMSE$Cost[count] <- costArray[count]
  trainRMSE$TrainRMSE[count] <- evaluate_RMSE
}

#Creating 5 equally size folds
initialCut <- cut(seq(1, nrow(trainldx)), breaks = 5, labels = FALSE)
for(i in 1:5)
{
  for(j in length(initialCut))
  {
    if(initialCut[j] == i)
    {
      folds[[i]] <- initialCut[j]
    }
  }
}
#folds[[i]] <- which(initialCut[j] == i, arr.ind = TRUE)
folds_cut <- round(seq(1, nrow(trainldx), by = nrow(trainldx)/5))
folds <- vector("list", 5)
folds[[1]] <- seq(folds_cut[1], folds_cut[2] - 1, by = 1)
folds[[2]] <- seq(folds_cut[2], folds_cut[3] - 1, by = 1)
folds[[3]] <- seq(folds_cut[3], folds_cut[4] - 1, by = 1)
folds[[4]] <- seq(folds_cut[4], folds_cut[5] - 1, by = 1)
folds[[5]] <- seq(folds_cut[5], 2376, by = 1)

#Calculate CVerror
CVerror <- vector("list", 5)
for(i in 1:5)
{
  svm_model <- svm(trainY[folds[[i]]] ~ ., data = cbind(trainX[folds[[i]]], trainY[folds[[i]]]), gamma = optimalGamma, cost = optimalCost, kernel = "radial", type = "eps-regression", epsilon = 1)
  predictedY <- predict(svm_model, testX)
  CVerror[[i]] <- sqrt(mean((trainY - predictedY)^2))
}
cvRMSE <- (CVerror[[1]] + CVerror[[2]] + CVerror[3] + CVerror[4] + CVerror[5])/5

##Action Points##
#v) Implement fold 5 cross-validation in a for loop and also store CV Error for each choice of cost & gamma and we choose the cost and gamma that gives the least CV Error.
#Create a list called folds of length 5 where folds[[1]] has say the indices of first fold and so forth.
#Example of dropping one fold in the for loop for i=1 is like say…svm(x =  trainX[ unlist(folds[[-i]]), ], y=trainY[unlist(folds[[-i]]),]  , …gamma[j], cost = optimalCost)
#Also note, the cvError is the average of the 5 errors obtained after predicting for each fold.
#vi) Generate a plot with three lines each visualizing trainRMSE, testRMSE and cvRMSE.
#Commands to use are plot(...), lines(..) etc.
#Ensure trainRMSE, testRMSE and cvRMSE are computed on same choices of gamma & cost. Widen gamma and cost choices from 2 entries to slightly longer array of say size 5 or 10 etc.


