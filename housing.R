# jonathan nash kaggle
# regression on boston housing data set

# libraries
library(corrplot)
library(randomForest)

# read in data
train <- read.csv("train.csv")
test <- read.csv("test.csv")
orig.train <- train
orig.test <- test

# na
train.clean <- train[,colSums(is.na(train))<nrow(train)]
nums <- unlist(lapply(train.clean, is.numeric))  

# correlation
train.nums <- train[,nums]
correlated <- which(cor(train.nums)[,38] > 0.5)
corrplot(cor(train.nums), method="circle")

train.x <- train.nums[,correlated]
train.x <- train.x[,-ncol(train.x)]
train.y <- train.nums$SalePrice

# random forest
rf <- randomForest(x = train.x, 
                   y = train.y, 
                   importance = TRUE, 
                   ntree = 900)
rf

# test
test.x <- test[,colnames(train.x)]
rownames(test.x) <- 1:1459
which(is.na(test.x))
test.x[is.na(test.x)] <- 0

# predict
pred <- predict(rf, test.x)
result <- data.frame(test, SalePrice = pred)

# output
submit <- result[,c("Id","SalePrice")]
write.csv(submit, file = "predictions_housing_jhsn.csv", row.names = F)










