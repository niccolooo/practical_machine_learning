# ASSIGNMENT 
library(ggplot2)
library(caret)

set.seed(9812)

raw_data <- read.csv("~/R_wd/coursera/practical_ML/practical_machine_learning/pml-training.csv")

raw_newdata <- read.csv("~/R_wd/coursera/practical_ML/practical_machine_learning/pml-testing.csv")

#PREPROCESSING
raw_data$num_window <- as.factor(raw_data$num_window)
raw_newdata$num_window <- as.factor(raw_newdata$num_window)

clean_data <- raw_data[complete.cases(raw_data), ]
clean_newdata <- raw_newdata[ , colSums(is.na(raw_newdata)) == 0]

complete_data <- clean_data[, intersect(names(clean_newdata), names(clean_data))]
complete_data$classe <- clean_data$classe

inBuild = createDataPartition(complete_data$classe, p = 0.7)[[1]]
validation <- complete_data[-inBuild,]
buildData <- complete_data[inBuild,]

inTrain <- createDataPartition(buildData$classe, p=0.7)[[1]]
training <- buildData[inTrain,]
testing <- buildData[ -inTrain,]

#FEATURE SELECTION
# ref: http://www.simafore.com/blog/bid/61220/How-to-perform-feature-selection-for-predictive-analytics
# ref: http://gerardnico.com/wiki/data_mining/stepwise_regression
# ref: https://www.udacity.com/course/viewer#!/c-ud262/l-627968607/m-601008601

# 0. remove user_name raw_timestamp_part_1 raw_timestamp_part_2   cvtd_timestamp new_window
training$X = NULL
training$user_name = NULL
training$raw_timestamp_part_1  = NULL
training$raw_timestamp_part_2 = NULL
training$cvtd_timestamp = NULL
training$new_window = NULL
training$num_window = NULL

# 1. compute correlation matrix and remove variables whose correlation is > 0.5
corMat <- cor(training[, 1:(dim(training)[2]-1)])
highlyCor <- findCorrelation(corMat, cutoff = 0.5)
training <- training[, -highlyCor]
message("Removing " , length(highlyCor), " variables whose correlation is greater than 0.5")

# 2. establish baseline with random forest run with all variables
fitN <- randomForest(classe ~ ., data = training)
predN <- predict(fitN, testing)
message( "all-feature acc : " , ClassAcc(predN, testing$classe))     

# 3. establish baseline with random forest run with a single variable each time
var_indices <- 1:(length(training)-1)
accuracies <- vector(,dim(training)[2])
for(i in 1:length(var_indices)){
       ss <- training[c(var_indices[i])]; ss$classe <- training$classe;
       mod <- randomForest(classe ~ ., data = ss )
       pred <- predict(mod, testing)
       acc <- ClassAcc(pred, testing$classe)
       message( names(training)[var_indices[i]], " acc : " , acc )     
       accuracies[i] <- acc;
}

best_acc <- max(accuracies) # accuracy
current_set <- which.max(accuracies) # set of best indices at step i
to_add <- 0 # index to add to improve solution
open_set <- c(var_indices) # set of indices to explore
exit_cond = FALSE

while(length(open_set > 1 || to_add != 0  || exit_cond == FALSE)){
     
     open_set <- open_set[-current_set] # remove current_set from open_set
     
     for( i in 1:length(open_set)){
          index <- open_set[i] # pick index from open_set
          temp_set <- c(current_set, index) # define set as current set + index
          temp_data <- training[,temp_set]; temp_data$classe <- training$classe # define new dataset 
          mod <- randomForest(classe ~ ., data = temp_data)    # fit model and predict on test
          pred <- predict(mod, testing) #predict
          acc <- ClassAcc(pred, testing$classe) # compute accuracy
          message("accuracy of temp_set" , temp_set , " : ", acc)
          if(acc > best_acc){
               best_acc = acc; 
               to_add = index;
          }
     }  
     
     if(to_add == 0){
          exit_condition = TRUE;
     }else{
          #update current_set
          current_set <- c(current_set, to_add)          
     }
}
