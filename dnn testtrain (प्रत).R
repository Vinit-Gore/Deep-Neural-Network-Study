# Create Vector of Column Max and Min Values
wholedata <- read.csv("Dnn/data/DDSM-OPT-TRAIN.csv",header = TRUE)
sc = 1 # start attribute column number 
ec = 9 # end attribute column number
maxs <- apply(wholedata[,sc:ec], 2, max)
mins <- apply(wholedata[,sc:ec], 2, min)
# Use scale() and convert the resulting matrix to a data frame
scaled.data <- as.data.frame(scale(wholedata[,sc:ec],center = mins, scale = maxs - mins))
# Convert target column from Yes/No to 1/0
#target = as.numeric(wholedata$target)-1
target = wholedata$target
data = cbind(scaled.data,target)
train = data



testdata <- read.csv("Dnn/data/DDSM-OPT-TEST.csv",header = TRUE)
sc = 1 # start attribute column number 
ec = 9 # end attribute column number
maxs <- apply(testdata[,sc:ec], 2, max)
mins <- apply(testdata[,sc:ec], 2, min)
# Use scale() and convert the resulting matrix to a data frame
scaled.test.data <- as.data.frame(scale(testdata[,sc:ec],center = mins, scale = maxs - mins))
# Convert target column from Yes/No to 1/0
#target = as.numeric(wholedata$target)-1
target = testdata$target
test = cbind(scaled.test.data,target)





set.seed(101)
## Create Split (any column is fine)
#library(caTools)
#split = sample.split(data$target, SplitRatio = 0.70)
## Split based off of split Boolean Vector
#train = subset(data, split == TRUE)
#test = subset(data, split == FALSE)

f <- as.formula(paste('target ~',paste(names(scaled.data),collapse=' + ')))
print(f)

library(neuralnet)
nn <- neuralnet(f,train,hidden=c(1,5),linear.output=FALSE, lifesign = "full")
plot(nn)
# Compute Predictions off Test Set
predicted.nn.values <- compute(nn,test[,sc:ec])

predicted.nn.values$net.result <- sapply(predicted.nn.values$net.result,round,digits=0)
cm <- table(test$target,predicted.nn.values$net.result)
print(cm)
accuracy <- function(cm) {
  cat("Accuracy =",(100*(cm[1]+cm[4]) / (cm[1]+cm[2]+cm[3]+cm[4])) )
}
accuracy(cm)
sensitivity = cm[4] / (cm[2]+cm[4])
cat("Sensitivity =",sensitivity ,"\n")
specificity = cm[1] / (cm[1]+cm[3])
cat("Specificity =",specificity ,"\n")
Youdens.J = sensitivity + specificity -1
cat("Youden's J =",Youdens.J) #informedness

library(pROC)
rocc <- roc(test$target, predicted.nn.values$net.result)
auc(rocc)
plot(rocc,legacy.axes=TRUE)


