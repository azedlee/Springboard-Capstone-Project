#Install and Run e1071
library(e1071)
library(dplyr)
library(tidyr)

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

#Add in data set into folds
set.seed(123)
split(trainX, sample(rep(1:15, 0.2*nrow(trainX))))

#Create an array to check Gamma
gammaArray = c(0.05, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65)

#For loop to check for best Gamma
RMSE_Result <- data.frame("Gamma" = numeric(15), "RMSE" = numeric(15), stringsAsFactors = FALSE)
for(gammaArray in 1:15)
{
  svm_model <- svm(GameID ~ ., data = trainX, kernel = "radial", gamma = gammaArray, type = "eps-regression", epsilon = 1, cross = 5)
  predictedY <- predict(svm_model, testX)
  evaluate_RMSE <- sqrt(mean((testY - predictedY)^2))
  RMSE_Result$Gamma <- gammaArray
  RMSE_Result$RMSE <- evaluate_RMSE
}

###Action Points###
#i) Ensure testX, testY are subsetted correctly and are of same number of rows.
#ii) Create an array Gamma = c(0.05, 1, 5, 10 ,15,20 ,25 ,30 ,35 ,40 ,45, 50, 55 ,60 ,65)
#iii) Do for loop through gamma and obtain cross-validated RMSE for each gamma.
#Use parameter cross=5, type = ‘eps-regression’,  epsilon = 1
#iv) After the loop, choose the gamma which gives you the least cross-validated error.
