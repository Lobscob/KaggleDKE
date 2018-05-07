##===========================================INIT==============================================
library(corrplot)
library(caret)
setwd("/Users/wweb/Google_Drive/Kaggle")

sample <- read.csv("sample_sol.csv")
testOG <- read.csv("test.csv")
trainOG <- read.csv("train.csv")

#===========================================DELETING OUTLIERS=======================================
deleteOutliers <- function(x,data){
  # delete outliers using boxplot
  plot <- boxplot(x,plot=FALSE)
  newData <- data[which(x> plot$stats[1] & x < plot$stats[5]),]
  
  return(newData)
}

newTrain<-trainOG
newTrain<-deleteOutliers(newTrain$n_non_stop_unique_tokens,newTrain)
newTrain<-newTrain[newTrain$shares < 800000,]

summary(newTrain)
#=========================================MAKING THE LINEAR REGRESSION============================
cortotal <- cor(newTrain)

#make the corplot
corrplot(cortotal,method= "square")

#make test and train set
index <- sample(1:nrow(newTrain),round(0.75*nrow(newTrain)))
train <- newTrain[index,]
test <- newTrain[-index,]

#check nullhypothesis
lm.training=lm(shares~., data=newTrain)
print(summary(lm.training))

#make the multiple regression and plot it
lm.mytrain=lm(shares ~ n_tokens_title + num_hrefs + num_self_hrefs + num_imgs + average_token_length + data_channel_is_entertainment + kw_min_avg + kw_max_avg + kw_avg_avg + global_subjectivity, data = train)

plot(lm.mytrain)
#================================PREDICTING ON (extracted from train) TEST SET===================
#lm.mytrain$residuals

actual <- test$shares
predict <- predict(lm.mytrain, test)

RMSE <- sqrt( mean( (actual - predict)^2) ) 

residualTest <- (actual - predict)

plot(predict,residualTest)
abline(h=0, col ="red", lty="dashed")

print(RMSE)
#=====================PREDICTING ON (original) TEST SET AND PUTTING RESULT IN DF=======================
endResult <- predict(lm.mytrain,testOG)
#print(endResult)

#make the dataframe format
resultData <- data.frame(endResult)
resultData$id <- 1:nrow(resultData) 
resultData <- resultData[c("id", "endResult")]
colnames(resultData) <- c("id", "shares")

#change all negative values to the mean (mean = 1671)
resultData$shares[resultData$shares < 0] = 1671
#print(resultData)

#save it
write.csv(resultData, file = "result.csv", row.names=FALSE)


































