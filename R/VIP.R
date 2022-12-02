

plsda.vip<-function(objectPLSDA,threshold=0.8){
  
  #from the object PLSDA
  #r2 <- as.matrix(objectPLSDA$coef)
  x <- as.matrix(objectPLSDA$X)
  W<-as.matrix(objectPLSDA$x.loadings)
  
  #colSum of correlations
  Sqr2 <- colSums(r2)
  #Sum of all correlations
  Sr2 <- sum(Sqr2)
  
  #VIP
  Sr2b<- sapply(1:ncol(x),function(x){sum(Sqr2*((W^2)[x,]))})
  vip<- sqrt((ncol(x)/Sr2)*Sr2b)
  
  #Colnames
  names(vip)<-colnames(x)
  
  #VIP if vip>treshold
  varnames<-names(vip)[vip>threshold]
  #if only 1 vip with treshold, select minimum 2
  if(length(varnames)<2){
    varnames <- sort(vip, decreasing = T)
    varnames <- names(vip)[1:2]
  }
  
  #df with only vip
  df_var <- as.data.frame(objectPLSDA$X)
  df_var <- df_var[,colnames(x) %in% res]
  
  
  object <- list("newX"=df_var,
                 "name"= varnames,
                 "vip"=vip,
                 "r2"=r2,
                 "threshold" = threshold
  )
  
  class(object) <- "VIP"
  return(object)
}