###########################
# ALDA: hw2.R 
# Instructor: Dr. Thomas Price
# Mention your team details here
# Darpan Dodiya <dpdodiya@ncsu.edu>
# Shantanu Sharma <ssharm34@ncsu.edu>
# Shrijeet Jagadishchandra Joshi <sjoshi22@ncsu.edu>
#
############################

require(caret)
require(rpart)


calculate_distance_matrix <- function(train_matrix, test_matrix, method_name){
  # NOTE: This function has already been implemented for you.
  # DO NOT modifiy this function.
  
  # INPUT:
  # Input: train_matrix: type: matrix n_sentences x sentence_length,
  # where n_sentences is total # training rows (100 in the dataset supplied to you) and
  # sentence_length is the total # features (100 in the dataset supplied to you).
  # Input: test_matrix: type: matrix of size 50 x 100 (i.e, 50 rows, 100 features)
  # Input: method_name: type: string, can be one of the following values: ('calculate_euclidean', 'calculate_cosine')
  
  # OUTPUT:
  # output: a 50 x 100 matrix of type double, containing the distance/similarity calculation using method_name between 
  # every row in test to every row in train 
  # This function has already been implemented for you. It takes the data matrix and method name, outputs the distance
  # matrix based on the method name.

  distance_matrix = matrix(0L, nrow = nrow(test_matrix), ncol = nrow(train_matrix))
  # the looping logic for pairwise distances is already provided for you
    for(i in seq(1, nrow(test_matrix))){
      for(j in seq(1, nrow(train_matrix))){
        distance_matrix[i,j] <- do.call(method_name, list(unlist(test_matrix[i,]), unlist(train_matrix[j,])))
      }
    }
  return(distance_matrix)
}

calculate_euclidean <- function(p, q) {
  # Input: p, q are vectors of size 1 x 100, each representing a row (i.e., a sentence) from the original dataset.
  # output: a single value of type double, containing the euclidean distance between the vectors p and q
  # Write code here to calculate the euclidean distance between pair of vectors p and q
  x <- sqrt(sum((p - q) ^ 2))
  return(x)
}

calculate_cosine <- function(p, q) {
  # Input: p, q are vectors of size 1 x 100, each representing a row (i.e., a sentence) from the original dataset.
  # output: a single value of type double, containing the cosine distance between the vectors p and q
  # Write code here to calculate the cosine distance between pair of vectors p and q
  return((p%*%q)/(sqrt(sum(p^2))*sqrt(sum(q^2))))
  
}

knn_classifier <- function(x_train, y_train, x_test, distance_method, k){
  # You will be IMPLEMENTING a KNN Classifier here
  
  # Build a distance matrix by computing the distance between every test sentence 
  # (row in training TF-IDF matrix) and training sentence (row in test TF-IDF matrix).
  # Use the above calculate_distance_matrix function to calculate this distance matrix (code already given to you).
  # You can re-use the calculate_euclidean and calculate_cosine methods from HW1 here.
  # Once the distance matrix is computed, for each row in the distance matrix, calculate the 'k' nearest neighbors
  # and return the most frequently occurring class from these 'k' nearest neighbors.
  
  # INPUT:
  # x_train: TF-IDF matrix with dimensions: (number_training_sentences x number_features)
  # y_train: Vector with length number_training_sentences of type factor - refers to the class labels
  # x_test: TF-IDF matrix with dimensions: (number_test_sentences x number_features)
  # k: integer, represents the 'k' to consider in the knn classifier
  # distance_method: String, can be of type ('calcualte_euclidean' or 'calculate_cosine')
  # OUTPUT:
  # A vector of predictions of length = number of sentences in y_test and of type factor.
  
  # NOTE 1: Don't normalize the data before calculating the distance matrix
  
  # NOTE 2: For cosine, remember, you are calculating similarity, not distance. As a result, K nearest neighbors 
  # k values with highest values from the distance_matrix, not lowest. 
  # For euclidean, you are calculating distance, so you need to consider the k lowest values. 
  
  # NOTE 3:
  # In case of conflicts, choose the class with lower numerical value
  # E.g.: in 5NN, if you have 2 NN of class 1, 2 NN of class 2, and 1 NN of class 3, there is a conflict b/w class 1 and class 2
  # In this case, you will choose class 1. 
  
  # NOTE 4:
  # You are not allowed to use predefined knn-based packages/functions. Using them will result in automatic zero.
  # Allowed packages: R base, utils
  
  #begin code 
  
  #calculation of distance matrix
  dm <- calculate_distance_matrix(x_train, x_test, distance_method)
  ans<- c() # final ans 
  
  if (distance_method == 'calculate_euclidean'){
    for(i in seq(1, nrow(x_test))){
      r <- dm[i,]
      r<-as.matrix(r)
      d <- data.frame(r,y_train)
      d<- d[order(d$r),]
      d<-d[1:k,]
      #print(d)
      d_c <- d$y_train
      tbl <- as.data.frame(table(d_c))
      v<- as.matrix(tbl[order(-tbl[2],tbl[1]),])
      ans <- c(ans,v[1,1])
      
    }
    return(as.factor(ans))  
  }else{
    #distance method is 'calculate_cosine'
    # look at note 2 
    for(i in seq(1, nrow(x_test))){
      r <- dm[i,]
      r<-as.matrix(r)
      d <- data.frame(r,y_train)
      d<- d[order(-d$r),]
      d<-d[1:k,]
      #print(d)
      d_c <- d$y_train
      tbl <- as.data.frame(table(d_c))
      v<- as.matrix(tbl[order(-tbl[2],tbl[1]),])
      ans <- c(ans,v[1,1])
    }
    return (as.factor(ans))
  }
}


knn_classifier_confidence <- function(x_train, y_train, x_test, distance_method='calculate_cosine', k){
  # You will be trying to build a modified KNN classifier using the paper given in the HW
  # While most of the implementation is similar to the KNN classifier, there is one additional step you need to do.
  # Read the HW PDF for more details about this method
  
  # INPUT:
  # x_train: TF-IDF matrix with dimensions: (number_training_sentences x number_features)
  # y_train: Vector with length number_training_sentences of type factor - refers to the class labels
  # x_test: TF-IDF matrix with dimensions: (number_test_sentences x number_features)
  # k: integer, represents the 'k' to consider in the knn classifier
  # distance_method: String, can be of type ( 'calculate_cosine')
  
  # OUTPUT:
  # A vector of predictions of length = number of sentences in y_test and of type factor.
  
  # Read the NOTES from comments under knn_classifier.
  # Allowed packages: R base, utils
  dm <- calculate_distance_matrix(x_train, x_test, distance_method)
  ans<- c() # final ans 

  for(i in seq(1, nrow(x_test))){
    d_dist <-c()
    d_class <- c()
    r <- dm[i,]
    r <- as.matrix(r)
    d <- data.frame(r,y_train)
    lab_vals <- unique(d$y_train)
    d<- d[order(-d$r),]
    d<-d[1:k,]
    v1<-c()
    v2<-c()
    for(j in lab_vals){
      d1 <- d[d$y_train==j,]
      conf = sum(d1$r)/sum(d$r)
      v1<-c(v1,conf)
      v2<-c(v2,j)
    }
    d<-data.frame(v1,v2)
    p<-d[order(-d$v1)[1],2]
    ans <- c(ans,p)
  }
  return (as.factor(ans))
}


dtree <- function(x_train, y_train, x_test){
  set.seed(123)
  # You will build a CART decision tree, then use the tuned model to predict class values for a test dataset.
  
  # INPUT:
  # x_train: TF-IDF matrix with dimensions: (number_training_sentences x number_features)
  # y_train: Vector with length number_training_sentences of type factor - refers to the class labels
  # x_test: TF-IDF matrix with dimensions: (number_test_sentences x number_features)
  
  # OUTPUT:
  # A vector of predictions of length = number of sentences in y_test and of type factor.
  
  # Allowed packages: rpart, R Base, utils
  
  # HINT1: Make sure to read the documentation for the rpart package. Check out the 'rpart' and 'predict' functions.
  
  # HINT2: I've given you attributes and class labels as separate variables. Do you need to combine them 
  # into a data frame for rpart?
  df <- data.frame(x_train, y_train)
  names(df)[length(names(df))]<-"Class" 
  
  #print(df)
  m <- rpart(Class ~ ., data = df,method = "class")
  p <- predict(m, x_test,type="class")
  #print(p)
  return(as.factor(p))
}


dtree_cv <- function(x_train, y_train, x_test, n_folds){
  set.seed(123)
  # You will build a decision tree and tune its parameters using n-fold crossvalidation on the *training* dataset,
  # then use the tuned model to predict class values for a test dataset.
  
  # INPUT:
  # x_train: TF-IDF matrix with dimensions: (number_training_sentences x number_features)
  # y_train: Vector with length number_training_sentences of type factor - refers to the class labels
  # x_test: TF-IDF matrix with dimensions: (number_test_sentences x number_features)
  # n_folds: integer, refers to the number of folds for n-fold cross validation
  
  # OUTPUT:
  # A vector of predictions of length = number of sentences in y_test and of type factor.
  
  # Allowed packages: rpart, caret, R Base, utils
  
  # HINT1: Make sure to read the documentation for the caret package. Check out the 'train' and 'trainControl' functions.
  #data_ctrl <- trainControl(method = "cv", number = n_folds)
  df <- data.frame(x_train, y_train)
  names(df)[length(names(df))]<-"Class" 
  t<- trainControl(method = "cv", number = n_folds)
  m1 <- train(Class ~ . , data = df, method = "rpart",
              tuneLength=20,
              trControl = t)
  
  p <- predict(m1,x_test,type="raw")
  return(as.factor(p))  
}


calculate_accuracy <- function(y_pred, y_true){
  # Given the following:
  
  # INPUT:
  # y_pred: predicted class labels (vector, each value of type factor)
  # y_true: ground truth class labels (vector, each value of type factor)
  
  # OUTPUT:
  # a list in the following order: [confusion matrix, overall accuracy], where confusion matrix is of class "table"
  # (see Figure 2 in the PDF for an example Confusion Matrix)
  # and overall accuracy is on a scale of 0-1 of type double
  # overall class accuracy = accuracy of all the classes
  
  # confusion matrix should have Prediction to the left, and Reference on the top.
  t <- table(y_pred,y_true)
  #print(t)
  cm<-confusionMatrix(y_pred, y_true)
  #print(cm)
  acc <- sum(diag(t))/sum(t)
  op <- list(confmat = cm$table, accuracy=cm$overall['Accuracy'])
  #op <- list(confmat=t, accuracy=acc)
  return (op)
}

