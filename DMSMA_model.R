# Digital Marketing and Social Media Analytics 
# Final group project
# Jacob Hallac
# Tyler Ibbotson-Sindelar
# George Mu

# startup
# reset environment
rm(list=ls())
setwd("~/Dropbox (Personal)/-MIT Classes/Digital Marketing and social media analytics/Group Project Materials/DigitalMarketingFinalProject")

library(ggplot2)
library(reshape2)
library(plyr)
library(dplyr)
library(gridExtra)

############
# functions
############

saveTable <- function(fileName, data){
  tg = gridExtra::tableGrob(data)
  h = grid::convertHeight(sum(tg$heights), "in", TRUE)
  w = grid::convertWidth(sum(tg$widths), "in", TRUE)
  ggplot2::ggsave(fileName, tg, width=w, height=h)  
}

############
# Load Data
##########
customerData = read.csv('High_Note_data.csv')

# are there duplicate net_users?
ft <- table(customerData$net_user)
sum(ft > 1)
#yes, 1?

# remove non-numeric id column
drop = names(customerData) %in%  c('net_user')
customerData <- customerData[, !drop]

################
##### summarize data 
################

summary(customerData)

# very few adopters
ggplot(customerData, aes(x= customerData$adopter)) + geom_histogram()
ggsave(filename = 'adoption rate.pdf',device = 'pdf')
sum(customerData$adopter == 1) / sum(customerData$adopter == 0) 

# histograms of all columns

# Reshape data for facet wrap
d <- melt(customerData, id.vars = "net_user")
d[1:200,] #check out a sample

ggplot(d, aes(x= value)) + facet_grid(variable ~ ., scales = "free_x", space = 'free_y') + geom_histogram(binwidth = 1)
ggsave(filename = 'allColumns.pdf', device = 'pdf', height = 44, width = 8.5)
# compare adopters to non-adopters

d <- melt(numericalColumns, id.vars = c("net_user","adopter"))
d[1:200,] #check out a sample
ggplot(d, aes(x= value, fill=as.factor(adopter))) + facet_wrap(~variable, scales = "free_x", space = 'free') + geom_histogram()

# Look at individual column comparisons
ggplot(customerData, aes(x= age, fill = as.factor(adopter))) + 
  geom_histogram(alpha= 0.3, position = 'identity')

# Trying to look at songs listened histogram 
ggplot(customerData, aes(x= songsListened)) + 
  geom_histogram(alpha= 0.3, position = 'identity', aes(y=..count../sum(..count..)))
  
# Table of stats by adopter non adopter
adopt <- customerData[customerData$adopter == 1,]
nonadopt <- customerData[customerData$adopter == 0,]
adoptMeans <- apply(adopt, 2, function(x) round(mean(x, na.rm = TRUE),2))
nonadoptMeans <- apply(nonadopt, 2, function(x) round(mean(x, na.rm = TRUE),2))
df<-data.frame(adoptMeans, nonadoptMeans)
saveTable('adopterVnonadopterStats.pdf', df)

# songs listened
ggplot(customerData, aes(x= adopter, y = songsListened)) + geom_bar(stat = 'summary', fun.y = "mean")
ggplot(customerData, aes(y = songsListened)) + geom_hist(stat = 'summary', fun.y = "mean")

# corr matrix
include <- names(customerData) %in% c("adopter", "friend_cnt", "avg_friend_age", "songsListened", "lovedTracks", "posts", "playlists", "shouts")
cormat <- round(cor(customerData[,include]), 2)

#######################
####### feature engineering
########################

# Create new usage per month features
customerData$playlistsPM = customerData$playlists / customerData$tenure
customerData$shoutsPM = customerData$shouts / customerData$tenure
customerData$postsPM = customerData$posts / customerData$tenure
customerData$lovedTracksPM = customerData$lovedTracks / customerData$tenure
customerData$songsListenedPM = customerData$songsListened / customerData$tenure

#######################
###### cleaning  
#######################

# % of missing values
pMiss <- function(x){round(sum(is.na(x))/length(x),2)}
df <- data.frame(percentMissing = sort(apply(customerData,2,pMiss),decreasing = FALSE))
saveTable('percentOfRowMissing.pdf', df)

sum(is.na(customerData$age))/length(customerData$age)

# Plot missing values by row
library(VIM)

aggr_plot <- aggr(customerData, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=TRUE, cex.axis=.4, prop=TRUE, combined=TRUE, bars=TRUE, gap=3, ylab=c("Histogram of missing data","Pattern"))

# impute missing values
library(mice)

# drop delta2_shouts
customerData$delta2_shouts = NULL

# add male na as new level - probably due to people choosing not specify gender
# bad idea because then can't calc avg male
#customerData[is.na(customerData$male),] = 3
#table(customerData$male)

# add good country na as new level
table(customerData$good_country)
customerData$good_country[is.na(customerData$good_country)] = 4
table(customerData$good_country)

# What percent rows have no NAs
sum(complete.cases(customerData))/length(customerData$adopter)
# 77%

# What percent have only age missing?
keep <- !(names(customerData) %in% c('age'))
sum(!complete.cases(customerData) & complete.cases(customerData[,keep])) / length(customerData$adopter)
# 9%

# so ~14% have other missing values too
md.pattern(customerData)

#Histogram of NAs per row
d <- apply(customerData, 1, function(x) sum(is.na(x)))
round(prop.table(table(d)),2)
df <- data.frame(missing_value_count=d)
a <- ggplot(df, aes(df$missing_value_count)) + geom_histogram()

ggsave('hist of missing values', plot = a, device = 'png')

# Compare people with missing data to those that dont have it
keep <- !(names(customerData) %in% c('age')) # exclude age because so many missing values
complete <- customerData[complete.cases(customerData[,keep]),]
missing <- customerData[!complete.cases(customerData[,keep]),]

# Create two different datasets of rows with all data vs rows without all data
c <- apply(complete, 2, function(x) round(mean(x, na.rm = TRUE),2))
m <- apply(missing, 2, function(x) round(mean(x, na.rm = TRUE),2))



df<-data.frame(complete_rows=c,missing_rows=m)
saveTable('tableMissingCompare.pdf', df)

# consider dropping age b/c 15% missing, but we know highly informative, so keep
# missing values seem to be driven by age and gender not being input, and then low friend counts

# impute all cols
temp <- mice(customerData)
custDataImp <- complete(temp, 1)

# Reshape data so period is a column

#############
############### predictive models
##############

# Baseline model age, male, songslistened

# Drop all period 2 columns
drop <- c('delta2_friend_cnt', 'delta2_avg_friend_age','delta2_avg_friend_male','delta2_friend_country_cnt','delta2_subscriber_friend_cnt','delta2_songslisted','delta2_lovedTracks','delta2_posts','delta2_playlists', 'delta2_good_country')
temp <- customerData[, !(names(customerData) %in% drop)]

# Create test and train data
split = sample.split(temp$adopter, SplitRatio = 0.7)
train <- subset(temp, split == TRUE)
test <- subset(temp, split == FALSE)

###### logistic reg
lm = glm(adopter~., data=train, family="binomial")
summary(lm)

pred1 = predict(mod1, type="response")

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



########## decision tree
library(rpart)
library(rpart.plot)

cart1 <- rpart(tenure ~., data = train)  
prp(cart1, compress = TRUE, tweak = 2)


# random forest

# 



# xgboost

###################
# Causal model (diff in diff, instrumental variables, ...)
###################

