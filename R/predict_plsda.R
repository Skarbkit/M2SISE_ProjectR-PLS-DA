
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
#' predict.plsda(model, d$test, type ="class")
#' predict.plsda(model, d$test, type ="posterior")




predict.plsda<-function(ObjectPLSDA,newdata,type="class"){
  
  # New data contrôle de cohérence
  if (class(ObjectPLSDA)=="PLSDA") {
    
      if (missing(newdata) || is.null(newdata)){
        newdata<-ObjectPLSDA$X
        }
    
    # RÃ©cupÃ©ration des moyennes
    means <- colMeans(ObjectPLSDA)

    x<-t(apply(newdata,1,function(x){x-means}))
    ncx<-ncol(x)
    coef<-ObjectPLSDA$plsda.coef
    
    
    # PROBABILITES D'APPARTENANCE AUX CLASSES
    #On calcule les scores de Y 
    yscore <- as.matrix(X) %*% coef[-(ncx+1),]
    y<-t(apply(yscore,1,function(x){x+coef[ncx+1,]}))
    
    #ON calculele l'exponentiel normalisé(sofmax)
    prob<-t(apply(y,1,function(x){exp(x)/sum(exp(x))}))
   
   
    if (type == "class") {
      # predicted classes
      l.max=apply(prob,1,which.max)
      return(sapply(l.max,function(x) ObjectPLSDA$level[x]))
    }else if (type == "posterior") {
      return(prob)
    }
  }else{
    stop("The class of the object is not PLSDA")
  }
}
