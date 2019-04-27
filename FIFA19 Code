library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)
library(gbm)
library(tidyverse)

#load the data 
fifa <- read.csv("fifa_normalized_without_overall.csv")

# radomize the entire data set to eliminate sample order bias 
fifa <- fifa[sample(nrow(fifa)),]
fifa<- fifa[c(-1)]
str(fifa)
#names(fifa)[2]<- "Market.Value"
#data particion 
set.seed(1)
train.size <- nrow(fifa)*.60
train.df <- fifa[c(1:train.size),]
valid.df <- fifa[c((nrow(train.df)+1):nrow(fifa)),]
print(nrow(train.df))
print(nrow(valid.df))

#regressor tree 
Fifa.tree.regressor <- rpart(Market.Value ~ ., data = train.df, method = "anova")
summary(Fifa.tree.regressor)

#plot of the regressor tree 
prp(Fifa.tree.regressor, type = 1, extra = 1, split.font = 1, varlen = -10) 

#length of the tree 
length(Fifa.tree.regressor$frame$var[Fifa.tree.regressor$frame$var == "<leaf>"])

# cpredictions on the training data 
Fifa.Tree.Regressor.pred.train <- predict(Fifa.tree.regressor,train.df,type = "vector")

# prediction vs actual values 
data.frame(
  "Predicted" = Fifa.Tree.Regressor.pred.train[1:20],
  "Actual_Values" = train.df$Market.Value[1:20])

# cpredictions on the validation data 
Fifa.Tree.Regressor.pred.valid <- predict(Fifa.tree.regressor,valid.df,type = "vector")

# prediction vs actual values 
data.frame(
  "Predicted" = Fifa.Tree.Regressor.pred.valid[1:20],
  "Actual_Values" = valid.df$Market.Value[1:20])

######################################## Second Best Model Fit #####################################
#basic forrest 
basic.forrrest<- randomForest(Market.Value ~.,data = train.df, ntree = 1000, importance = TRUE) 
## random forest
fifa.random_forrest <- randomForest(Market.Value ~ New.LCB+New.LS+New.GKPositioning+New.Reactions+New.LDM+
                                      New.LB+New.LF+New.LM+New.StandingTackle+New.LWB+New.Markiing+NEW.LAM+
                                      New.GKHandling+New.GKDiving+New.GKReflexes+New.Finishing+New.StandingTackle+
                                      Position+New.Interceptions+New.LW+New.HeadingAccuracy+New.LCM+New.BallControl+
                                      New.Strength+New.LongPassing+New.Positioning+New.ShortPassing+New.Aggression+
                                      New.Vision+New.International.Reputation,data = train.df, ntree = 1000, importance = TRUE) 
#summary of random forrest regresor 
# R-Squared: 89.60
summary(fifa.random_forrest)
which.min(fifa.random_forrest$mse)

#plot of the error rate of the random forrest
plot(randomForest(Market.Value ~ New.LCB+New.LS+New.GKPositioning+New.Reactions+New.LDM+
                    New.LB+New.LF+New.LM+New.StandingTackle+New.LWB+New.Markiing+NEW.LAM+
                    New.GKHandling+New.GKDiving+New.GKReflexes+New.Finishing+New.StandingTackle+
                    Position+New.Interceptions+New.LW+New.HeadingAccuracy+New.LCM+New.BallControl+
                    New.Strength+New.LongPassing+New.Positioning+New.ShortPassing+New.Aggression+
                    New.Vision+New.International.Reputation,data = train.df, ntree = 1000, importance = TRUE),
     main= "Feature Importance of Random Forrest")
grid()
## variable importance plot
varImpPlot(fifa.random_forrest, type = 1)

# training set predictions 
fifa.random_forrest.train.pred <- predict(fifa.random_forrest, train.df)
summary(fifa.random_forrest.train.pred)
# prediction vs actual values 
data.frame(
  "Predicted" = fifa.random_forrest.train.pred[1:20],
  "Actual_Values" = train.df$Market.Value[1:20])

# valid set predictions 
fifa.random_forrest.valid.pred <- predict(fifa.random_forrest, valid.df)
data.frame(
  "Predicted" = fifa.random_forrest.valid.pred [1:20],
  "Actual_Values" = valid.df$Market.Value[1:20])


#RMSE of validation set
RMSE_FIFA_VALID<- sqrt(mean((fifa.random_forrest.train.pred - valid.df$Market.Value)^2))
print(RMSE_FIFA_VALID)

#MAE of validation set
MAE_FIFA_VALID<- mean(abs(fifa.random_forrest.train.pred - valid.df$Market.Value))
print(MAE_FIFA_VALID)


######################################## Best Model Fit #####################################
#boosting 
set.seed(1)
new.fifa<- as.tibble(fifa)
New_Fifa<- new.fifa %>% select(Market.Value,New.LCB,New.LS,New.GKPositioning,New.Reactions,New.LDM,
                            New.LB,New.LF,New.LM,New.StandingTackle,New.LWB,New.Markiing,NEW.LAM,
                            New.GKHandling,New.GKDiving,New.GKReflexes,New.Finishing,New.StandingTackle,
                            Position,New.Interceptions,New.LW,New.HeadingAccuracy,New.LCM,New.BallControl,
                            New.Strength,New.LongPassing,New.Positioning,New.ShortPassing,New.Aggression,
                            New.Vision,New.International.Reputation)
train.size <- nrow(fifa)*.60
train.df <- New_Fifa[c(1:train.size),]
valid.df <- New_Fifa[c((nrow(train.df)+1):nrow(fifa)),]

#gradient boosted model for regression tree 
#This is the equivalent of boosted classification tree using the function boosting() 
# from the library(adabag) package 
boost<- gbm(Market.Value ~ ., distribution = "gaussian", data = train.df, n.trees = 2000,
             shrinkage = .01, interaction.depth = 10, cv.folds = 3)
noptimal_trees<- gbm.perf(boost, method = "cv")
print(noptimal_trees)
#plot of importance plot 
summary(boost)
#validation set predictions
pred <- predict(boost, valid.df, n.trees = noptimal_trees)
#comparisons with actual values 
data.frame(
  "Predicted" = pred[1:20],
  "Actual_Values" = valid.df$Market.Value[1:20])

#RMSE of validation set
RMSE_FIFA_VALID<- sqrt(mean((pred - valid.df$Market.Value)^2))
print(RMSE_FIFA_VALID)

#MAE of validation set
MAE_FIFA_VALID<- mean(abs(pred - valid.df$Market.Value))
print(MAE_FIFA_VALID)

#Plot of dependence plot of the most influencial variables 
plot(boost, i = "New.Reactions", lwd = 3)
plot(boost, i = "New.LCB", lwd = 3)
plot(boost, i = "New.LF", lwd = 3)
plot(boost, i = "New.LM", lwd = 3)
plot(boost, i = "New.BallControl", lwd = 3)
plot(boost, i = "New.LS", lwd = 3)
plot(boost, i = "New.GKPositioning", lwd = 3)
plot(boost, i = "New.ShortPassing", lwd = 3)
plot(boost, i = "New.LDM", lwd = 3)
plot(boost, i = "New.GKDiving", lwd = 3)
plot(boost, i = "New.GKHandling", lwd = 3)
plot(boost, i = "NEW.LAM", lwd = 3)
plot(boost, i = "New.StandingTackle", lwd = 3)
plot(boost, i = "New.GKReflexes", lwd = 3)
plot(boost, i = "New.LB", lwd = 3)

# Validation set analysis 
n.trees = seq(from=1 ,to=2000, by=1) 
pred <- predict(boost, valid.df, n.trees = n.trees)
dim(pred)
test_error<- with(valid.df,apply((pred-valid.df$Market.Value)^2,2,mean))
plot(n.trees, test_error, pch=5, col="blue",xlab="Number of Trees",
     ylab="Test Mean Squared Error", main = "Perfomance of Boosting on Validation Set")
#add line to the min test error found on from the 1000 tested random forrests 
abline(h = min(test_error),col="green", lwd = 3)
legend("topright",c("Minimum Test error Line for Random Forests"),col="green",lty=1,lwd=1)
