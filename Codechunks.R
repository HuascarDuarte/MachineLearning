## Load the training data
File <- "/Users/Huascar/MachineLearning/data/pml-training.csv"
Data <- read.csv(File, stringsAsFactors = FALSE)

##Remove features (kurtosis, swewness, avg, max, etc) without data
##Also remove user name, new window and timestamps columns
removeCols <- c(1:6, 12:36, 50:59, 69:83, 87:101, 103:112, 125:139, 141:150)
myData <- Data[,-removeCols]
rm(Data) ## clean workspace

##Make some character variables into Factors
##myData$user_name <- as.factor(myData$user_name)
##myData$num_window <- as.factor(myData$num_window)
myData$classe <- as.factor(myData$classe)

##Subset data into train/test data
inTrain<- createDataPartition(y=myData$classe, 
                              p=0.7, 
                              list=FALSE)
myTrain <- myData[inTrain,]
myTest <- myData[-inTrain,]

##Lets create a random forest model based on num_window variable
fit <- train(classe ~ num_window, 
             method="rf", 
             data = myTrain)

##Check accurary in data
confusionMatrix(predict(fit), myTrain$classe)

##Check accuracy in test data
confusionMatrix(predict(fit, newdata=myTest), myTest$classe)

## Load the testing data
File2 <- "/Users/Huascar/MachineLearning/data/pml-testing.csv"
testData <- read.csv(File2, stringsAsFactors = FALSE)

##Apply same data celansing as to the train data
##Remove features (kurtosis, swewness, avg, max, etc) without data
##Also remove user name, new window and timestamps columns
removeCols <- c(1:6, 12:36, 50:59, 69:83, 87:101, 103:112, 125:139, 141:150)
testData <- testData[,-removeCols]


##Make some character variables into Factors
##myData$user_name <- as.factor(myData$user_name)
##myData$num_window <- as.factor(myData$num_window)
testData$problem_id <- as.factor(testData$problem_id)

##Apply predictions
answers <- predict(fit, newdata=testData)

##Function to create submission files
pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

##Create files
pml_write_files(answers)

