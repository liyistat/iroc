#' Calculate sensitivity, specitifity, accuracy and AUC of the ROC.
#'
#' Calculate sensitivity, specitifity, accuracy and AUC of the ROC.
#' And the 0.95 CI of AUC were calculated by DeLong's method.
#' @param testy  A numeric vector containing  prediction probabilities.
#' @param yhat  A vector containing the true class labels.
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
  sen<-result.coords[2]
  spc<-result.coords[3]
  acc<-result.coords[4]
  pred <- ROCR::prediction(yhat, testy)
  auc<-attributes(ROCR::performance(pred, 'auc'))$y.values[[1]]
  ci<-pROC::ci.auc(result.roc)
  result1<-cbind(sen,spc,acc,auc)
  result<-list(result1,ci)
  return(result)
}


#'Calculate the performance for the ROC curve
#' @param testy A numeric vector containing  prediction probabilities.
#' @param yhat A vector containing the true class labels.
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
#' @param testy A numeric vector containing  prediction probabilities.
#' @param yhat A vector containing the true class labels.
#' @return The figure of the ROC curve.
#' @export
#' @examples
#' #Simulate true prediction labels
#' testy<-rep(c(0,1),500)
#' #Simulate prediction probabilities
#' yhat<-runif(1000, min = 0, max = 1)
#' #Plot the ROC curve and show the AUC and its CI
#' iroc(testy, yhat)


iroc<-function(testy,yhat){
  title<-paste("Plot ROC curve")
  ROCR::plot(perf(testy,yhat),colorize = T, lwd = 2, main = title)
  abline(0, 1, lty = 8, col = "light blue")
  auc<-pred.result(testy,yhat)[[1]][4]
  auc_ci<-pred.result(testy,yhat)[[2]]
  nameauc<-paste("AUC",round(auc,4),sep=" = ")
  ci<-paste(round(auc_ci[1],4),round(auc_ci[3],4),sep=" - ")
  nameci<-paste("95% CI:", ci)
  legend("topleft",c(nameauc, nameci), cex=1)
}
