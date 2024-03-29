########
# HW3 
# Instructor: Dr. Thomas Price
# 
# @author: Krishna Gadiraju/kgadira
# 
# Darpan Dodiya <dpdodiya@ncsu.edu>
# Shantanu Sharma <ssharm34@ncsu.edu>
# Shrijeet Jagadishchandra Joshi <sjoshi22@ncsu.edu>
#########

# Write code for regression here
alda_regression <- function(x_train, x_test, y_train, regression_type){
  # Perform regression (linear/ridge/lasso)
  
  # Inputs:
    # x_train: training data frame(19 variables, x1-x19)
    # x_test: test data frame(19 variables, x1-x19)
    # y_train: dependent variable, training data (vector, continous type)
    # regression_type: specifies type of regression, string variable, can be of type 'linear', 'ridge' or 'lasso'
  
  # General Information:
    # Instructions for specific regression types:
      # linear: no cross validation
      # ridge: use 10-fold cross validation to determine optimal lambda
      # lasso: use 10-fold cross validation to determine optimal lambda
  
  # Output:
    # A list with two elements, first element = model generated, second element = predictions on test data (vector) 
  
  # allowed packages: R-base, glmnet
  
  # Function hints: Read the documentation for the functions glmnet, cv.glmnet, predict
  # Ridge and Lasso regression hints: Lambda is the hyperparameter
  if(regression_type == 'linear'){ 
    # ~ 2-3 lines of code
    # write code for building a linear regression model using x_train, y_train
    # can you use glmnet to do simple linear regression as well?
    # Explore away!  
    # Hint: Think of what the lambda value means for linear regression without regularization.
     # make predictions
    
    fit=glmnet(x_train,y_train, lambda = 0)
    # predict using the model
    result = predict(fit, x_test)
    return(list(fit,c(result)))
    
    
  }else if(regression_type == 'ridge'){
    # ~ 2-3 lines of code
    # write code for ridge regression here
    # 10 fold cross validation, with mse (mean squared error) as the measure
    # the hyperparameter you are tuning here is lambda
    #cv.glmnet
    
    fit=cv.glmnet(x_train,y_train, alpha = 0, nfolds = 10)
    # make predictions
    # predict on x_test using the model that gives least MSE
    result = predict(fit,x_test, s = fit$lambda.min )
    
    return(list(fit,c(result)))
    
    
  }else{
    # ~ 2-3 lines of code
    # write code for lasso regression here
    # 10 fold cross validation, with mse (mean squared error) as the measure
    # the hyperparameter you are tuning here is lambda

    # predict on x_test using the model that gives least MSE
    
    fit=cv.glmnet(x_train,y_train, alpha = 1, nfolds = 10)
    # make predictions
    
    # predict on x_test using the model that gives least MSE
    result = predict(fit,x_test, s = fit$lambda.min )
    
    return(list(fit,c(result)))
    
  }
  
}

calculate_rmse <- function(y_true, y_pred){
  # DO NOT modify this code. TA has already given you code for this
  # You have already been provided this code to calculate RMSE
  
  # Inputs:
  # y_true: ground truth dependent variable values, of type vector
  # y_pred: prediction outcomes from any regression method, with the same length as y_true
  
  # Outputs:
  # a single value of type double, with the RMSE value
  return(sqrt(sum(y_true - y_pred)^2/length(y_true)))
}

regression_compare_rmse <- function(y_test, linear_regression_prediction, ridge_prediction, lasso_prediction){
  # ~ 8-10 lines of code
  # Calculate the rmse for each of the regression methods: 'linear', 'ridge', 'lasso'
  # Return the best method and its RMSE (i.e., method with least RMSE)
  
  # Inputs:
  # y_test: ground truth dependent variable from test data (vector)
  # linear_regression_prediction: predictions from linear regression (vector)
  # ridge_regression_prediction: predictions from ridge regression (vector)
  # lasso_regression_prediction: predictions from lasso regression (vector)
  
  # Returns:
  # list of three values:
  # First value, of type string, with the name of the best method
  #  'linear' if linear_regression_prediction is best
  #  'ridge' if ridge_prediction is best
  #  'lasso' if lasso_regression is best
  # Second value, of type double, with the corresponding RMSE of the best method (do not round off)
  # third value is a vector of RMSE values, in the following order: c(linear regression's RMSE, ridge regression's RMSE, lasso's RMSE)
  
  # Allowed packages: R-base
  # You are given the implementation for calculate_rmse (see above) 
  #y_test, linear_regression_prediction, ridge_prediction, lasso_prediction
  linearRMSE = calculate_rmse(y_test , linear_regression_prediction)
  ridgeRMSE = calculate_rmse(y_test , ridge_prediction)
  lassoRMSE = calculate_rmse(y_test , lasso_prediction)
  
  # View(list("linear",linearRMSE,c(linearRMSE,ridgeRMSE , lassoRMSE) ))
  
  if(linearRMSE < ridgeRMSE && linearRMSE < lassoRMSE)
  {
    return(list("linear",linearRMSE,c(linearRMSE,ridgeRMSE , lassoRMSE) ))
    
  }
  else if(ridgeRMSE < linearRMSE && ridgeRMSE < lassoRMSE)
  {
    return(list("ridge",ridgeRMSE,c(linearRMSE,ridgeRMSE , lassoRMSE) ))

  }
  else if(lassoRMSE < linearRMSE && lassoRMSE < ridgeRMSE)
  {
    return(list("lasso",lassoRMSE,c(linearRMSE,ridgeRMSE , lassoRMSE) ))

  }
  #return(list("lasso",lassoRMSE,c(linearRMSE,ridgeRMSE , lassoRMSE) ))
  
  
  
}


alda_svm <- function(x_train, x_test, y_train, kernel_name){
  # Perform classification using support vector machines (linear/radial/sigmoid)
  
  # Inputs:
    # x_train: training data frame(4 variables, x1-x4)
    # x_test: test data frame(4 variables, x1-x4)
    # y_train: dependent variable, training data (factor)
    # kernel_name: specifies type of SVM kernel, string variable, can be of type 'linear', 'radial' or 'sigmoid' or 'polynomial'

  # General information
    # Both training data and test data have already been scaled - so you don't need to scale it once again.
  
  # Kernel specific information: using 10-fold cross-validation, perform hyperparameter tuning for each kernel as shown below:
    # Linear: 
      # 'cost' parameter: for the following values: c(0.01, 0.1, 1, 10)
    # radial: 
      # 'cost' parameter: for the following values: c(0.01, 0.1, 1, 10), 
      # 'gamma' parameter: for the following values: c(0.05, 0.5, 1, 2)
    # polynomial:
      # 'cost' parameter: for the following values: c(0.01, 0.1, 1, 10), 
      # 'gamma' parameter: for the following values: c(0.05, 0.5, 1, 2)
      # 'degree' parameter: for the following values: c(1,2,3)
    # sigmoid:
      # 'cost' parameter: for the following values: c(0.01, 0.1, 1, 10), 
      # 'gamma' parameter: for the following values: c(0.05, 0.5, 1, 2)
  
  # Output:
    # A list with two elements, first element = model generated, second element = predictions on test data (factor) 
  
  # Word of caution:
    # Make sure that you pick the best parameters after tuning
  
  # allowed packages: R-base, e1071
  
  # Hints: See if you can use the 'tune' function in e1071 for cross validation
   if(kernel_name == "radial"){
    # ~1-2 lines 
     svm.tune <- tune(svm, train.x=x_train, train.y=y_train, 
                       kernel="radial", ranges=list(cost=c(0.01, 0.1, 1, 10), gamma=c(0.05, 0.5, 1, 2), scale=FALSE))
     
     #print(summary(svm.tune))
     
     svm_after_tuned <- svm(x_train, y_train, kernel="radial", 
                            cost=svm.tune$best.parameters$cost, gamma=svm.tune$best.parameters$gamma, scale=FALSE)
     
     #print(summary(svm_after_tuned))
     
     return(list(svm_after_tuned, predict(svm_after_tuned, x_test)))
     
  }else if(kernel_name == 'polynomial'){
    #~1-2 lines
    svm.tune <- tune(svm, train.x=x_train, train.y=y_train, kernel="polynomial", 
                     ranges=list(cost=c(0.01, 0.1, 1, 10), gamma=c(0.05, 0.5, 1, 2), degree=c(1,2,3), scale=FALSE))
    
    svm_after_tuned <- svm(x_train, y_train, kernel="polynomial", cost=svm.tune$best.parameters$cost, 
                           gamma=svm.tune$best.parameters$gamma, degree=svm.tune$best.parameters$degree, scale=FALSE)
    
    return(list(svm_after_tuned, predict(svm_after_tuned, x_test)))
    
  }else if(kernel_name == 'sigmoid'){
    #~1-2 lines
    svm.tune <- tune(svm, train.x=x_train, train.y=y_train, kernel="sigmoid", 
                     ranges=list(cost=c(0.01, 0.1, 1, 10), gamma=c(0.05, 0.5, 1, 2), scale=FALSE))
    
    svm_after_tuned <- svm(x_train, y_train, kernel="sigmoid", cost=svm.tune$best.parameters$cost, 
                           gamma=svm.tune$best.parameters$gamma, scale=FALSE)
    
    return(list(svm_after_tuned, predict(svm_after_tuned, x_test)))
    
  }else{ # default linear kernel
    #~1-2 lines
    svm.tune <- tune(svm, train.x=x_train, train.y=y_train, kernel="linear", 
                     ranges=list(cost=c(0.01, 0.1, 1, 10), scale=FALSE))
    
    svm_after_tuned <- svm(x_train, y_train, kernel="linear", cost=svm.tune$best.parameters$cost, scale=FALSE)
    
    return(list(svm_after_tuned, predict(svm_after_tuned, x_test, scale=FALSE)))
  }
  
}


classification_compare_accuracy <- function(y_test, linear_kernel_prediction, radial_kernel_prediction, 
                                            polynomial_kernel_prediction, sigmoid_kernel_prediction){
  # ~ 6-10 lines of code
  # Calculate the accuracy for each of the classification methods: 
    # 'svm-linear': linear kernel SVM
    # 'svm-radial': radial kernel SVM
    # 'svm-poly': polynomial kernel SVM
    # 'svm-sigmoid': sigmoid kernel SVM 
  # Return the best method and its accuracy (i.e., method with highest accuracy)
  
  # Inputs:
    # y_test: ground truth dependent variable from test data (factor)
    # linear_kernel_prediction: predictions from linear kernel SVM (factor)
    # radial_kernel_prediction: predictions from radial kernel SVM (factor)
    # polynomial_kernel_prediction: predictions from polynomial kernel SVM (factor)
    # sigmoid_kernel_prediction: predictions from sigmoid kernel SVM (factor)
    
  # Returns:
  # list of three values:
    # First value, of type string, with the name of the best method, sould be:
      # 'svm-linear' if linear_kernel_prediction is best
      # 'svm-radial' if radial_kernel_prediction is best
      # 'svm-poly' if polynomial_kernel_prediction is best
      # 'svm-sigmoid' if sigmoid_kernel_prediction is best
    # Second value, of type double, with the corresponding overall accuracy of the best method (on a scale of 100, do not round off)
    # third value, a vector with the overall accuracies of all methods in this order: c(linear-svm's accuracy, radial-svm's accuracy, poly-svm's accuracy, sigmoid-svm's accuracy)
  # Allowed packages: R-base
  # Note that I asked you to implement accuracy calculation - do not use a library for this
  
  cm = as.matrix(table(linear_kernel_prediction,y_test))
  svm_linear_accuracy = (100 * sum(diag(cm)))/sum(cm)
  best_accu = list(svm_linear_accuracy, 'svm-linear')
 
  cm = as.matrix(table(radial_kernel_prediction,y_test))
  svm_radial_accuracy = (100 * sum(diag(cm)))/sum(cm)
  if(svm_radial_accuracy > best_accu[1])
    best_accu = list(svm_radial_accuracy, 'svm-radial')
  
  cm = as.matrix(table(polynomial_kernel_prediction,y_test))
  svm_poly_accuracy = (100 * sum(diag(cm)))/sum(cm)
  if(svm_poly_accuracy > best_accu[1])
    best_accu = list(svm_poly_accuracy, 'svm-poly')
  
  cm = as.matrix(table(sigmoid_kernel_prediction,y_test))
  svm_sigmoid_accuracy = (100 * sum(diag(cm)))/sum(cm)
  if(svm_sigmoid_accuracy > best_accu[1])
    best_accu = list(svm_sigmoid_accuracy, 'svm-sigmoid')
  
  return(list(best_accu[[2]], best_accu[[1]], c(svm_linear_accuracy, svm_radial_accuracy, svm_poly_accuracy, svm_sigmoid_accuracy)))
}

