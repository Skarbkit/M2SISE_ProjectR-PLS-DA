# fit 

#library(mixOmics)
#library(pls)

pls.fit <- function(formula, data, ncomp=2, center=T, reduce=F){
  
  # Check input values 
  
  # formula
  if(inherits(formula, "formula")==FALSE){stop("Error : input formula isn't a formula object !")}
  # data
  if(is.data.frame(data)==FALSE){stop("Error : input data isn't a dataframe object !")}
  
  # A modifier avec la formule (selon les composantes choisis)
  NbXcol = length(attributes(terms(formula, data = data))$term.labels)
  #Nbcol = ncol(data)
  
  # ncomp
  if(is.numeric(ncomp)==FALSE){stop("Error : input ncomp isn't a numeric object !")}
  if(as.integer(ncomp)!=ncomp){stop("Error : input ncomp isn't an integer !")}
  if(ncomp<1){stop("Error : input ncomp cannot be negative !")}
  if(ncomp>NbXcol){stop("Error : input ncomp must be less than the number of descriptive variables included in the formula !")}
  
  # center
  if(!is.logical(center)){"Error : input center isn't a logical value !"}
  if(!is.logical(reduce)){"Error : input reduce isn't a logical value !"}
  
  
  # 
  Xcolnames = attributes(terms(formula, data = data))$term.labels
  ycolname = toString(formula[[2]])
  
  # Compare colnames from formula with the data's column names
  
  if(!all(Xcolnames%in%colnames(data))){stop("Error : Descriptives colnames in formula doesn't match with colnames in the data !")}
  if(!ycolname%in%colnames(data)){stop("Error : Target column in formula doesn't match with colnames in the data !")}
  
  X = data[,Xcolnames]
  y = data[,ycolname]
  
  # Fit the model 
  nepals.res = nepals(X, ncomp, center=T, reduce=T)
  
  # Return 
  
}

