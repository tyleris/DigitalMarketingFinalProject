# Load data

rm(list=ls())
setwd("~/Dropbox (Personal)/-MIT Classes/Digital Marketing and social media analytics/Group Project Materials/DigitalMarketingFinalProject")

library(ggplot2)
library(reshape2)
library(plyr)
library(dplyr)
library(gridExtra)

custData <- read.csv('High_Note_Data_Imputed.csv')
summary(custData)

custData[is.na(custData$songsListenedPM),]
#############
############### predictive models
##############

# Baseline model age, male, songslistened

# Drop all period 2 columns
drop <- c('delta2_friend_cnt', 'delta2_avg_friend_age','delta2_avg_friend_male','delta2_friend_country_cnt','delta2_subscriber_friend_cnt','delta2_songslisted','delta2_lovedTracks','delta2_posts','delta2_playlists', 'delta2_good_country')
temp <- custData[, !(names(custData) %in% drop)]

# Create test and train data
library('caTools')
split = sample.split(temp$adopter, SplitRatio = 0.7)
train <- subset(temp, split == TRUE)
test <- subset(temp, split == FALSE)

###### logistic reg
lm = glm(adopter ~ ., data= train, family="binomial")
summary(lm)

pred1 = predict(lm, type="response")

# Check the predictions at a threshold of 0.5
table(Train$Republican, pred1 >= 0.5)

# Save a confusion matrix to a variable in order to inspect its properties
mod1confusionMatrix = table(Train$Republican, pred1 >= 0.5)

# Accuracy of model 1
sum(diag(mod1confusionMatrix)) / sum(mod1confusionMatrix)
# Sensitivity of model 1 (True positive rate )
mod1confusionMatrix[2,2] / sum(mod1confusionMatrix[2,])
# Specificity of model 1 (True negative rate)
mod1confusionMatrix[1,1] / sum(mod1confusionMatrix[1,])

#ROC and AUC
library(ROCR)

# CART (training set)
PredictROC = predict(lm)

# Let's see what this looks like
PredictROC

# We want the second column - this is the probability of class 1
PredictROC = PredictROC[,2]

# Commands to generate ROC curve just like those for Logistic Regression

pred = prediction(PredictROC, train$adopt)
perf = performance(pred, "tpr", "fpr")
plot(perf)

# And you can compute the AUC
as.numeric(performance(pred, "auc")@y.values)


########## decision tree
install.packages('rpart')
install.packages('rpart.plot')
library(rpart)
library(rpart.plot)

cart <- rpart(adopt ~., data = train)  

# Plot
prp(cart, compress = TRUE, tweak = 2)

# ROC 
PredictCART = predict(cart, type = "class")

# We want the second column - this is the probability of class 1
PredictROC = PredictROC[,2]

# Commands to generate ROC curve just like those for Logistic Regression
pred = prediction(PredictROC, train$adopt)
perf = performance(pred, "tpr", "fpr")
plot(perf)

# And you can compute the AUC
as.numeric(performance(pred, "auc")@y.values)

########################
######### random forest
###########################

install.packages("randomForest")
library(randomForest)

rf = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = StevensTrain, ntree=500, nodesize=25)

# ROC for Random Forest (training set)
PredictROC = predict(rf, type="prob")

# 



# xgboost

###################
# Causal model (diff in diff, instrumental variables, ...)
###################

