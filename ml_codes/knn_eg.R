library(class)
data("iris")
str(iris)
head(iris)
table(iris$Species)
set.seed(9850)
gp <- runif(nrow(iris))
iris <- iris[order(gp),]
summary(iris[,c(1,2,3,4)])
normalize <- function(x){
  return( (x-min(x))/(max(x)-min(x)) )
}

iris_n <- as.data.frame(lapply(iris[,c(1,2,3,4)], normalize))
iris_train <- iris_n[1:129,]
iris_test <- iris_n[130:150,]
iris_train_target <- iris[1:129,5]
iris_test_target <- iris[130:150,5]

require(class)
m1 <- knn(train = iris_train, test = iris_test, cl=iris_train_target, k =13)
m1
table(m1, iris_test_target)
