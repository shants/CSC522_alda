library(rpart)
library(rpart.plot)
library(caret)
rm(list = ls())
mushrooms <- read.csv("./mushrooms.csv")
str(mushrooms)
nrow(mushrooms) - sum(complete.cases(mushrooms))
mushrooms$veil.type <- NULL

set.seed(12345) # for reproducibility
train <- sample(1:nrow(mushrooms),size = ceiling(0.80*nrow(mushrooms)),replace = FALSE)
# training set
mushrooms_train <- mushrooms[train,]
# test set
mushrooms_test <- mushrooms[-train,]
tree <- rpart(class~.,
              data=mushrooms_train,
              method = "class")

cp.optim <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
tree <- prune(tree, cp=cp.optim)
pred <- predict(object=tree,mushrooms_test[-1],type="class")
t <- table(mushrooms_test$class,pred)
confusionMatrix(t)
