
#############################################################
########## Classification performance measurements ##########
#############################################################

get_perf_mes <- function(prediction, validation){
  #require(pROC)
  mat <- table("Predictions"= prediction,  "Actual" = validation)
  pred <- as.numeric(prediction)
  category <- as.numeric(as.factor(validation))
  accuracy <- sum(diag(mat)) / sum(mat)
  precision <- diag(mat) / rowSums(mat)
  recall <- diag(mat) / colSums(mat)
  roc_obj <- multiclass.roc(category, pred)
  output <- list('Confusion matrix' = mat, 
                 'accuracy' = accuracy, 
                 'precision' = precision, 
                 'recall' = recall, 
                 roc_obj)
  return(output)
}



get_performance <- function(prediction, validation){
  output <- get_perf_mes(prediction, validation)
  for (i in 1:length(output)) {
    print(output[i])
  }
}


#############################################################
################# Replace weights with Y/N ##################
#############################################################


convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
}
