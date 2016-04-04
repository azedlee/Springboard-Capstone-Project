#Libraries
library(e1071)
library(dplyr)
library(tidyr)
library(ggplot2)

#Input CSV into variable
inputData <- SkillCraft1_Dataset
str(inputData)

#Add in mean(age) to LeagueIndex = 8
inputData$Age <- as.numeric(as.character(inputData$Age)) #Convert Column from Factor to Numeric
mean_avg <- round(mean(inputData$Age, na.rm = TRUE)) #Round values
inputData$Age <- ifelse(is.na(inputData$Age) == TRUE, mean_avg, inputData$Age)

#Change HoursPerWeek and TotalHours for LeagueIndex = 8 by 2013 minus release date
inputData$TotalHours <- as.numeric(as.character(inputData$TotalHours)) #Convert Column from Factor to Numeric
inputData$HoursPerWeek <- as.numeric(as.character(inputData$HoursPerWeek)) #Convert Column from Factor to Numeric
years_released <- 2013 - 2010
total_game_hours <- 8 * 20 * 12 * years_released # 8 Hours per day, 20 days per month, 12 months per year, 3 years
total_game_hours_per_week <- 8 * 20 / 4 # 8 hours per day, 20 days per month, divided by 4 weeks in a month
inputData$TotalHours <- ifelse(is.na(inputData$TotalHours) == TRUE, total_game_hours, inputData$TotalHours)
inputData$HoursPerWeek <- ifelse(is.na(inputData$HoursPerWeek) == TRUE, total_game_hours_per_week, inputData$HoursPerWeek)

#Manipulated TotalHours data longer than years_released
inputData$TotalHours <- ifelse(inputData$TotalHours >= 9000, inputData$TotalHours / 10, inputData$TotalHours)
inputData$TotalHours <- ifelse(inputData$TotalHours >= 100000, inputData$TotalHours / 100, inputData$TotalHours)

#Change variables with timestamps into minute: SelectByHotkeys, UniqueHotKeys, MinimapAttacks, MiniMapRightClicks, NumberOfPACs
##TotalMapExplored, WorkersMade, UniqueUnitsMade, ComplexUnitsMade, ComplexAbilitiesUsed
per_timestamp <- 88.5
per_minute <- 60
inputData$SelectByHotkeys <- inputData$SelectByHotkeys * per_timestamp * per_minute
inputData$AssignToHotkeys <- inputData$AssignToHotkeys * per_timestamp * per_minute
inputData$MinimapAttacks <- inputData$MinimapAttacks * per_timestamp * per_minute
inputData$MinimapRightClicks <- inputData$MinimapRightClicks * per_timestamp * per_minute
inputData$NumberOfPACs <- inputData$NumberOfPACs * per_timestamp * per_minute
inputData$WorkersMade <- inputData$WorkersMade * per_timestamp * per_minute
inputData$ComplexUnitsMade <- inputData$ComplexUnitsMade * per_timestamp * per_minute
inputData$ComplexAbilitiesUsed <- inputData$ComplexAbilitiesUsed * per_timestamp * per_minute

#Save inputData after Data Wrangling for Regression Analysis
inputData_reg <- inputData

#Change LeagueIndex Numbers to Actual Rank Names
inputData$LeagueIndex <- as.character(inputData$LeagueIndex)
inputData$LeagueIndex[inputData$LeagueIndex == "1"] <- "Bronze"
inputData$LeagueIndex[inputData$LeagueIndex == "2"] <- "Silver"
inputData$LeagueIndex[inputData$LeagueIndex == "3"] <- "Gold"
inputData$LeagueIndex[inputData$LeagueIndex == "4"] <- "Platinum"
inputData$LeagueIndex[inputData$LeagueIndex == "5"] <- "Diamond"
inputData$LeagueIndex[inputData$LeagueIndex == "6"] <- "Master"
inputData$LeagueIndex[inputData$LeagueIndex == "7"] <- "GrandMaster"
inputData$LeagueIndex[inputData$LeagueIndex == "8"] <- "Professional"
inputData$LeagueIndex <- factor(inputData$LeagueIndex, levels = c("Bronze", "Silver", 
                                                                  "Gold", "Platinum", 
                                                                  "Diamond", "Master", 
                                                                  "GrandMaster", "Professional")) #Manually order levels

#Create ggplots for Exploratory Data Analysis
##APM vs Ranks
ggplot(data = inputData, aes(x = LeagueIndex, y = APM, group = LeagueIndex)) +
  geom_boxplot(aes(color = LeagueIndex)) +
  scale_x_discrete(breaks = c(1, 2, 3, 4, 5, 6, 7, 8), 
    labels = c("Bronze", "Silver", "Gold", "Platinum", "Diamond", "Master", "GrandMaster", "Professional")) +
  ggtitle("APM vs Ranks") +
  labs(x = "Ranks", y = "APM")
ggsave('APM vs Ranks.png')

##Hours Per Week vs APM
ggplot(data = inputData, aes(x = HoursPerWeek, y = APM)) +
  geom_point(aes(color = LeagueIndex), size = 10, stat = "summary", fun.y = median) +
  scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
  ggtitle("Hours Per Week vs APM") +
  labs(x = "Hours Per Week", y = "Median APM")
ggsave('APM vs Hours Per Week.png')

##Median Total Hours
ggplot(data = subset(inputData, LeagueIndex != "Professional"), 
                     aes(x = LeagueIndex, y = TotalHours, fill = LeagueIndex)) +
  geom_bar(stat = "summary", fun.y = median) +
  ggtitle("Total Hours vs Ranks") +
  labs(x = "Ranks", y = "Total Hours")
ggsave('Total Hours vs Ranks.png')

##SelectByHotkey, Assign to Hotkey, Total Unique Hotkey vs Ranks
ggplot(data = inputData, aes(x = LeagueIndex, y = SelectByHotkeys, fill = LeagueIndex)) +
  geom_bar(stat = "summary", fun.y = median) +
  ggtitle("Selected by Hotkey per Minute in Ranks") +
  labs(x = "Ranks", y = "Selected by Hotkey per Minute")
ggsave('Selected by Hotkey per Minute in Ranks.png')

ggplot(data = inputData, aes(x = LeagueIndex, y = AssignToHotkeys, fill = LeagueIndex)) +
  geom_bar(stat = "summary", fun.y = median) +
  ggtitle("Assign to Hotkey per Minute in Ranks") +
  labs(x = "Ranks", y = "Assign to Hotkey per Minute")
ggsave('Assign to Hotkey per Minute in Ranks.png')

ggplot(data = inputData, aes(x = LeagueIndex, y = UniqueHotkeys, fill = LeagueIndex)) +
  geom_bar(stat = "summary", fun.y = median) +
  ggtitle("Unique Hotkeys in Ranks") +
  labs(x = "Ranks", y = "Total Unique Hotkeys")
ggsave('Total Unique Hotkeys in Ranks.png')

##Select by Hotkey vs Assign to Hotkey
ggplot(data = inputData, aes(x = AssignToHotkeys, y = SelectByHotkeys)) +
  geom_point(aes(color = LeagueIndex), size = 5) +
  geom_smooth(method = "lm", color = "red") +
  ggtitle("Selected by Hotkey vs Assign to Hotkey per Minute") +
  labs(x = "Assign to Hotkey per Minute", y = "Selected by Hotkey per Minute")
ggsave('Selected by Hotkey vs Assign to Hotkey per Minute.png')

##Minimap Right Clicks vs Minimap Attacks
ggplot(data = inputData, aes(x = MinimapRightClicks, y = MinimapAttacks)) +
  geom_point(aes(color = LeagueIndex), size = 4) +
  ggtitle("Minimap Right Clicks vs Minimap Attacks") +
  labs(x = "Minimap Right Clicks per Minute", y = "Minimap Attacks per Minute")
ggsave('Minimap Right Clicks vs Minimap Attacks.png')

##Number of PACS vs Ranks
ggplot(data = inputData, aes(x = LeagueIndex, y = NumberOfPACs, fill = LeagueIndex)) +
  geom_bar(stat = "summary", fun.y = median) +
  ggtitle("Number of PACs per Minute vs Ranks") +
  labs(y = "Number of PACs per Minute", x = "Ranks")
ggsave('Number of PACs per Minute vs Ranks.png')

##Time Gap (ms) Between PACs vs Ranks
ggplot(data = inputData, aes(x = LeagueIndex, y = ActionsInPAC, fill = LeagueIndex)) +
  geom_bar(stat = "summary", fun.y = median) +
  ggtitle("Actions in PAC per Minute vs Ranks") +
  labs(y = "Actions in PAC per Minute", x = "Ranks")
ggsave('Number of PACs per Minute vs Ranks.png')

##Median Actions in PAC vs Median Number of PACs (colored by Ranks)
LI_aip_nop <- inputData %>%
  group_by(LeagueIndex) %>%
  summarise(median_aip = median(as.numeric(ActionsInPAC)),
            median_nop = median(as.numeric(NumberOfPACs)),
            n = n()) %>%
  ungroup() %>%
  arrange(LeagueIndex)

ggplot(data = LI_aip_nop, aes(x = median_nop, y = median_aip)) +
  geom_point(aes(color = LeagueIndex), size = 10, stat = "summary", fun.y = median) +
  ggtitle("Actions in PAC vs Number of PACs in Ranks") +
  labs(x = "Median Number of PACs per Minute", y = "Median Actions in PAC per Minute")
ggsave("Actions in PAC vs Number of PACs in Ranks.png")

##APM vs PACs
LI_apm_nop <- inputData %>%
  group_by(LeagueIndex) %>%
  summarise(median_apm = median(APM),
            median_nop = median(NumberOfPACs),
            n = n()) %>%
  arrange(LeagueIndex)
ggplot(data = LI_apm_nop, aes(x = median_apm, y = median_nop)) + 
  geom_point(aes(color = LeagueIndex), size = 10, stat = "summary", fun.y = median) +
  geom_smooth(color = "Blue") +
  ggtitle("Actions per Minute vs Perception Action Cycles per Minute") +
  labs(x = "Median Actions per Minute", y = "Median Number of Perception Action Cycles per Minute")
ggsave("Actions per Minute vs Perception Action Cycles per Minute.png")

LI_apm_nop_mean <- inputData %>%
  group_by(LeagueIndex) %>%
  summarise(mean_apm = mean(APM),
            mean_nop = mean(NumberOfPACs),
            n = n()) %>%
  arrange(LeagueIndex)
ggplot(data = LI_apm_nop_mean, aes(x = mean_apm, y = mean_nop)) + 
  geom_point(aes(color = LeagueIndex), size = 10, stat = "summary", fun.y = median) + 
  geom_smooth(color = "Blue") +
  ggtitle("Actions per Minute vs Perception Action Cycles per Minute") +
  labs(x = "Mean Actions per Minute", y = "Mean Number of Perception Action Cycles per Minute")
ggsave("Actions per Minute vs Perception Action Cycles per Minute.png")

cor.test(inputData$APM, inputData$NumberOfPACs, method = "p")
cor.test(LI_apm_nop$median_apm, LI_apm_nop$median_nop, method = "p")
cor.test(LI_apm_nop_mean$mean_apm, LI_apm_nop_mean$mean_nop, method = "p")

#Random number generator to create trainldx and testldx
set.seed(1234)
completeSet <- sample(nrow(inputData), 0.7*nrow(inputData), replace = FALSE)
trainldx <- inputData_reg[completeSet,]
testldx <- inputData_reg[-completeSet,]

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
for(count in 1:RMSE_Length)
{
  svm_model <- svm(trainY ~ ., data = cbind(trainX, trainY), kernel = "radial", cost = costArray[count], gamma = 1, type = "eps-regression", epsilon = 1)
  predictedY <- predict(svm_model, testX)
  evaluate_RMSE <- sqrt(mean((testY - predictedY)^2))
  testRMSE$Cost[count] <- costArray[count]
  testRMSE$RMSE[count] <- evaluate_RMSE
}

#For loop to check for best Gamma
for(count in 1:RMSE_Length)
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

#Caret package k-fold Cross Validation
library(caret)
library(kernlab)

fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)
grid <- expand.grid(C = seq(0.25, 0.35, 0.01))
fit.registered <- train(trainY~., data = cbind(trainX, trainY), trControl = fitControl,
                        method = "svmRadialCost", tuneLength = 9, preProc = c("center", "scale"), tuneGrid = grid)
prediction.registered <- predict(fit.registered, testX)
cv_RMSE <- sqrt(mean((prediction.registered - testY)^2))
cv_RMSE

#Train RMSE vs Test RMSE vs k-fold CV Error plot
ggplot(fit.registered) +
  geom_line(aes(color = "Red"), size = 2) +
  geom_line(aes(x = trainRMSE$Cost, y = trainRMSE$TrainRMSE, color = "Green"), size = 2) +
  geom_line(aes(x = testRMSE$Cost, y = testRMSE$RMSE, color = "Blue"), size = 2) +
  scale_color_manual("Legend Title \n", labels = c("Test RMSE", "Train RMSE", "k-fold RMSE"), values = c("Blue", "Red", "Green")) +
  ggtitle("Train RMSE vs Test RMSE vs 5-fold CV Error RMSE") +
  labs(x = "Cost", y = "RMSE")
ggsave("Train RMSE vs Test RMSE vs 5-fold CV Error plot.png")
