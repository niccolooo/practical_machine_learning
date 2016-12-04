# ASSIGNMENT 
library(ggplot2)

trainSet <- read.csv("~/R_wd/coursera/practical_ML/assignment/pml-training.csv")
testSet <- read.csv("~/R_wd/coursera/practical_ML/assignment/pml-testing.csv")

#PREPROCESSING
trainSet$num_window <- as.factor(trainSet$num_window)
testSet$num_window <- as.factor(testSet$num_window)

#EXPLORATION

#1. get subsamples of each classe to look for patterns

clA <- subset(trainSet, classe == "A")
clB <- subset(trainSet, classe == "B")
clC <- subset(trainSet, classe == "C")
clD <- subset(trainSet, classe == "D")
clE <- subset(trainSet, classe == "E")

qplot(clA$X, clA$raw_timestamp_part_1, colour = clA$num_window, data = clA)
qplot(clA$raw_timestamp_part_1, clA$yaw_belt, colour = clA$user_name, data = clA)
qplot(clA$X, clA$raw_timestamp_part_1, colour = clA$user_name, data = clA)



clA_levels = levels(clA$num_window)
aTS <- subset(clA, clA$num_window == 2)