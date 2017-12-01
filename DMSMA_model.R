# Digital Marketing and Social Media Analytics 
# Final group project
# Jacob Hallac
# Tyler Ibbotson-Sindelar
# George Mu

# Load Data

customerData = read.csv('High_Note_data.csv')

################
##### summarize data 
################

summary(customerData)

# very few adopters
ggplot(customerData, aes(x= customerData$adopter)) + geom_histogram()
sum(customerData$adopter == 1) / sum(customerData$adopter == 0) 

# histograms of all columns
#pdf("Rplots.pdf")
#dev.off() 

library(ggplot2)
library(reshape2)
library(plyr)
library(dplyr)

# Reshape data for facet wrap
d <- melt(customerData, id.vars = "net_user")
d[1:200,] #check out a sample

ggplot(d, aes(x= value)) + facet_wrap(~variable, scales = "free_x") + geom_histogram()

# compare adopters to non-adopters

d <- melt(numericalColumns, id.vars = c("net_user","adopter"))
d[1:200,] #check out a sample
ggplot(d, aes(x= value, fill=as.factor(adopter))) + facet_wrap(~variable, scales = "free_x") + geom_histogram()


# Look individually
ggplot(customerData, aes(x= age, fill = as.factor(adopter))) + 
  geom_histogram(alpha= 0.3, position = 'identity')

# Table of stats by adopter non adopter
library(purrr)
customerData %>% split(.$adopter) %>% map(summary)

tapply(customerData$age, customerData$adopter, summary)

#######################
###### Basic cleaning
#######################

# are there duplicate net_users?
ft <- count(customerData$net_user)
ft[ft[,2] > 1,] #yes, nine #NAME? ids


# Reshape data so period is a column

# Compare predictive values of indvidual features

# Build additional features

# Basic predictive (correlation) models

# Causal model (diff in diff, instrumental variables, ...)





