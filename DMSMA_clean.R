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
#ft <- table(customerData$net_user)
#sum(ft > 1)
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

# Look for outliers

# Reshape data for facet wrap
d <- melt(customerData, id.vars = "net_user")
d[1:200,] #check out a sample

ggplot(d, aes(x= value)) + facet_grid(variable ~ ., scales = "free", space = 'free') + geom_histogram(binwidth = 1)
ggsave(filename = 'allColumns.pdf', device = 'pdf', height = 44, width = 8.5)
# compare adopters to non-adopters

d <- melt(numericalColumns, id.vars = c("net_user","adopter"))
d[1:200,] #check out a sample
ggplot(d, aes(x= value, fill=as.factor(adopter))) + facet_wrap(~variable, scales = "free_x", space = 'free') + geom_histogram()

# Look at individual column comparisons
ggplot(customerData, aes(x= age, fill = as.factor(adopter))) + 
  geom_histogram(alpha= 0.3, position = 'identity')

# Trying to look at songs listened histogram 

col = 'songsListened'
ggplot(customerData, aes(x= customerData[,col])) + geom_histogram()
ggsave(filename = paste('hist_', col, '.pdf', sep = '') , device = 'pdf')

# Trying to look at songs listened histogram 

ggplot(customerData, aes(x= friend_cnt)) + geom_histogram()



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

customerData$playlistsPM = customerData$playlists / (customerData$tenure + 1)
customerData$shoutsPM = customerData$shouts / (customerData$tenure + 1)
customerData$postsPM = customerData$posts / (customerData$tenure + 1)
customerData$lovedTracksPM = customerData$lovedTracks / (customerData$tenure + 1)
customerData$songsListenedPM = customerData$songsListened / (customerData$tenure + 1)

incl <- c('playlistsPM', 'shoutsPM', 'postsPM', 'lovedTracksPM', 'songsListenedPM')

#check
incl <- c('tenure','playlists', 'playlistsPM','shouts', 'shoutsPM','posts', 'postsPM','lovedTracks', 'lovedTracksPM', 'songsListened', 'songsListenedPM')
customerData[,incl]

#check for inf values
customerData[customerData$tenure == 0,incl]

# Subscriber friends / total friends

customerData$subscriberFriendRatio <- customerData$subscriber_friend_cnt / customerData$friend_cnt
customerData$subscriberFriendRatio[customerData$friend_cnt == 0] <- 0 

# Create age or gender missing, age and gender missing column

ageAndGen <- is.na(customerData$age)  & is.na(customerData$male)
ageOrGen <- is.na(customerData$age) | is.na(customerData$male)

customerData$missingAgeAndGender <- customerData$age
customerData$missingAgeAndGender[ageAndGen] <- 1
customerData$missingAgeAndGender[!ageAndGen] <- 0

customerData$missingAgeOrGender <- customerData$age
customerData$missingAgeOrGender[ageOrGen] <- 1
customerData$missingAgeOrGender[!ageOrGen] <- 0

# check
incl <- c('missingAgeOrGender', 'missingAgeAndGender', 'age', 'male')
customerData[,incl]

##############################
########### missing value summary stats
################################

# % of missing values
pMiss <- function(x){round(sum(is.na(x))/length(x),2)}
df <- data.frame(percentMissing = sort(apply(customerData,2,pMiss),decreasing = TRUE))
saveTable('percentOfRowMissing.pdf', df)

sum(is.na(customerData$age))/length(customerData$age)

# Plot missing values by row
library(VIM)

aggr_plot <- aggr(customerData, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=TRUE, cex.axis=.4, prop=TRUE, combined=TRUE, bars=TRUE, gap=3, ylab=c("Histogram of missing data","Pattern"))
grid.arr

ggsave(filename = 'missing value distributions.pdf', device = pdf)

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

# missing values seem to be driven by age and gender not being input, and then low friend counts

#######################
###### Handle missing value
#######################

# drop delta2_shouts
customerData$delta2_shouts = NULL

# add male na as new level - probably due to people choosing not specify gender
# bad idea because then can't calc avg male
#customerData[is.na(customerData$male),] = 3
#table(customerData$male)

# add good country na as new level
table(customerData$good_country)
customerData$good_country[is.na(customerData$good_country)] = 3
table(customerData$good_country)

# consider dropping age b/c 15% missing, but we know highly informative, so keep

# impute missing values
library(mice)

# impute all cols
customerDataImp <- mice(customerData, m=1)
custDataImp <- complete(customerDataImp, 1)

# test
apply(custDataImp,2,function(x) sum(is.na(x)))

# test quality of imputation

# export
write.csv(custDataImp, 'High_Note_Data_Imputed.csv')

# Reshape data so time period is a column (maybe...)

