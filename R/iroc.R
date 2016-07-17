#' Calculate sensitivity, specitifity, accuracy and AUC of the ROC.
#'
#' Calculate sensitivity, specitifity, accuracy and AUC of the ROC.
#' And the 0.95 CI of AUC were calculated by DeLong's method.
#' @author Yi Li, \email{liyistat@@gmail.com}
#' @param testy A vector containing the true class labels.
#' @param yhat A numeric vector containing  prediction probabilities.
#' @return result  A list of sensitivity, specifity, accuracy, AUC and 0.95 CI of AUC.
#' @export
#' @examples
#' #Simulate true prediction labels
#' testy<-rep(c(0,1),500)
#' #Simulate prediction probabilities
#' yhat<-runif(1000, min = 0, max = 1)
#' #Calculate the sensitivity, specifity, accuracy in topleft of the ROC curve
#' #Calculate AUC and 0.95 CI of AUC.
#' pred.result(testy,yhat)

pred.result<-function(testy,yhat){
  #optimal threshold
  result.roc<- pROC::roc(testy,yhat)
  result.coords <- pROC::coords(result.roc, "best", best.method="closest.topleft", ret=c("threshold","sensitivity","specificity", "accuracy"))
  predict<-factor(ifelse(yhat > result.coords[1], 1, 0))
  table<-table(predict, testy)
  threshold<-result.coords[1]
  sensitivity<-result.coords[2]
  specificity<-result.coords[3]
  accuracy<-result.coords[4]
  pred <- ROCR::prediction(yhat, testy)
  auc<-attributes(ROCR::performance(pred, 'auc'))$y.values[[1]]
  ci<-pROC::ci.auc(result.roc)
  result1<-cbind(threshold,sensitivity,specificity,accuracy,auc)
  row.names(result1)<-NULL
  result<-list(result1,ci)
  return(result)
}

#'Calculate the performance for the ROC curve
#' @param testy A vector containing the true class labels.
#' @param yhat A numeric vector containing  prediction probabilities.
#' @return Performance for the ROC curve
#' @export
perf<-function(testy,yhat){
  pred <- ROCR::prediction(yhat, testy);
  perf <- ROCR::performance(pred,"tpr","fpr");
  return(perf)
}

#' Plot the ROC curve
#'
#' Plot the ROC curve and show the AUC of ROC.
#' The 0.95 CI of AUC were calculated by DeLong's method.
#' @author Yi Li, \email{liyistat@@gmail.com}
#' @param testy A vector containing the true class labels.
#' @param yhat A numeric vector containing  prediction probabilities.
#' @return Figure of the ROC curve.
#' @export
#' @examples
#' #Simulate true prediction labels
#' testy<-rep(c(0,1),500)
#' #Simulate prediction probabilities
#' yhat<-runif(1000, min = 0, max = 1)
#' #Plot the ROC curve and show the AUC and its CI
#' #Print the prediction performance of the topleft point
#' iroc(testy, yhat)

iroc<-function(testy,yhat){
  title<-paste("Plot the ROC curve")
  ROCR::plot(perf(testy,yhat),colorize = T, lwd = 2, main=title)
  abline(0, 1, lty = 8, col = "light blue")
  auc<-pred.result(testy,yhat)[[1]][5]
  auc_ci<-pred.result(testy,yhat)[[2]]
  nameauc<-paste("AUC",round(auc,4),sep=" = ")
  ci<-paste(round(auc_ci[1],4),round(auc_ci[3],4),sep=" - ")
  nameci<-paste("95% CI:", ci)
  legend(0.5,0.2,c(nameauc, nameci), cex=1.1, box.lty = 2,box.col = "white")
  points(1-pred.result(testy ,yhat)[[1]][3],pred.result(testy ,yhat)[[1]][2],pch = 20, cex = 2,col = "red3")
  text(1-pred.result(testy ,yhat)[[1]][3]-0.08,pred.result(testy ,yhat)[[1]][2]+0.06,"closest.topleft",cex = .9)
  print("Prediction performance of the closest topleft point:")
  result<-pred.result(testy,yhat)[[1]][-5]
  names(result)<-c("threshold", "sensitivity", "specificity", "accuracy")
  return(result)
  }
