validate <- function(testcol,pred){ 
  
  cm = as.matrix(table(Actual = testcol, Predicted = pred))
  cm.perc = round(prop.table(as.matrix(table(Actual = testcol, Predicted = pred)))*100,0)
  cat("Confusion Matrices\n")
  cat("  \n")
  print(cm)
  cat("  \n")
  print(cm.perc)
  cat("  \n")
  
  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  diag = diag(cm) # number of correctly classified instances per class 
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the actual classes
  q = colsums / n # distribution of instances over the predicted classes
  
  
  accuracy = sum(diag) / n
  cat("Accuracy \n")
  cat("  \n")
  print(accuracy) 
  cat("  \n")
  
  precision = diag / colsums # fraction of correct predictions for each class
  recall = diag / rowsums  # Also sensivity, fraction of instances of a class that were correctly predicted
  
  cat("Precision \n" )
  cat("  \n")
  print(precision)
  cat("  \n" )
  
  cat("Recall \n" )
  cat("  \n")
  print(recall)
  cat("  \n" )
}