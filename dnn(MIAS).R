wholedata <- read.csv("Dnn-DWM/data/wholedata.csv",header = TRUE)
#wholedata <- subset(wholedata, select = -a16) # column a16 has only one value across all samples
sc = 2 # Start attribute Column number 
ec = 31 # End attribute Column number
wholedata <- data.matrix(wholedata)
# Create Vector of Column Max and Min Values
maxs <- apply(wholedata[,sc:ec], 2, max)
mins <- apply(wholedata[,sc:ec], 2, min)
# Use scale() and convert the resulting matrix to a data frame
scaled.data <- as.data.frame(scale(wholedata[,sc:ec],center = mins, scale = maxs - mins))
# Convert target column from 2/1 to 1/0 where 1 = poisonous, 0 = edible
target = wholedata[,1]-1
data = cbind(target,scaled.data)



set.seed(101)
# Create Split (any column is fine)
library(caTools)
split = sample.split(data$target, SplitRatio = 0.79)
# Split based off of split Boolean Vector
train = subset(data, split == TRUE)
test = subset(data, split == FALSE)

f <- as.formula(paste('target ~',paste(names(scaled.data),collapse=' + ')))
print(f)

library(neuralnet)
nn <- neuralnet(f,train,hidden=c(10,5),linear.output=FALSE)
plot(nn)
# Compute Predictions off Test Set
predicted.nn.values <- compute(nn,test[,sc:ec])

predicted.nn.values$net.result <- sapply(predicted.nn.values$net.result,round,digits=0)
cm <- table(test$target,predicted = predicted.nn.values$net.result)
print(cm)

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
cat("Accuracy =",((cm[1]+cm[4]) / (cm[1]+cm[2]+cm[3]+cm[4]))*100 ,"\n")
