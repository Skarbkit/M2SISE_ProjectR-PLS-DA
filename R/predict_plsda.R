
#' PLSDA predictions
#'
#' @description
#' Applies PLSDA model to a newdata set
#'
#' @param ObjectPLSDA
#' a PLSDA model (ObjectPLSDA of class \code{plsda})
#' @param newdata
#' a matrix with newdata values (predictors)
#' @param type
#' indicate the type of prediction 'class' or 'posterior'
#' @return
#' PLS-DA results (an object of class \code{plsdares})
#'
#' @details
#' See examples in help for \code{\link{plsda}} function.
#'#' @examples
#' library(plsda)
#' d <- plsda.split_sample(iris)
#' model <- plsda(Species~.,d$train)
#' predict.plsda(model, d$test, type ="class")
#' predict.plsda(model, d$test, type ="posterior")



predict.plsda<-function(ObjectPLSDA,newdata,type){
  
  # New data contrôle de cohérence
  if (class(ObjectPLSDA)=="PLSDA") {
    
    # tranform characters variables in factor
    newdata[sapply(newdata, is.character)] <- lapply(newdata[sapply(newdata, is.character)],as.factor)
    
    x<-t(apply(newdata,1,function(x){x-(ObjectPLSDA$Xmeans)}))
    ncx<-ncol(x)
    coef<-ObjectPLSDA$plsda.coef
    
    # PROBABILITES D'APPARTENANCE AUX CLASSES
    prob<-t(apply((as.matrix(X) %*% coef[-(ncx+1),]),1,function(x){x+coef[ncx+1,]}))
    
    #ON calculele l'exponentiel normalisé
    prob<-t(apply(prob,1,function(x){exp(x)/sum(exp(x))}))
   
   
    if (type == "class") {
      # predicted classes
      l.max=apply(prob,1,which.max)
      return(sapply(l.max,function(x)ObjectPLSDA$level[x]))
    }else if (type == "posterior") {
      return(prob)
    }
  }else{
    stop("The class of the object is not PLSDA")
  }
}
