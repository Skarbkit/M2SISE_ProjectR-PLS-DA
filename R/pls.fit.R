### nipalspls.fit.R: Nipals PLS fit algorithm for tall data.
###
### Implements an adapted version of the `’algorithme PLS1 NIPALS' described in
###   Diplôme PostGrade en Statistique - La régression PLS (2004) from 
###   Séverine Vancolen supervised by Yadolah Dodge
### 
###   Ressources included in Methods and formulas for x- 
###   and y-statistics in Partial Least Squares Regression from support.minitab.com



#' @title Nipals PLS (M2 SISE R Project)
#'
#' @description Fits a dataset with the Nipals PLS algorithm.
#'
#'
#' @param formula a formula for the PLS regression.  \code{NA}s and \code{Inf}s are
#' not allowed.
#' @param data a matrix of observations.  \code{NA}s and \code{Inf}s are not
#' allowed.
#' @param ncomp the number of components to be used in the modelling.
#' @param center logical, If \code{TRUE}, center descriptive variables. 
#' Default is to perform mean centering.
#' @param reduce logical.  If \code{TRUE} descriptive variables are reduce.  
#' Default is to reduce by SD of each variable in the data matrix.

#' @keywords regression multivariate
#' @export

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
  
  n = nrow(data)
  p = NbXcol
  
  # Fit the model 
  nipals.res = nipals(X, ncomp, center=center, reduce=reduce)
  
  
  # Attention il faudrait centrer réduire X dans la fit avant le nipals pour avoir 
  # les bons résultats (donc changer les paramètres d'entrée de la nipals et faire les 
  # calculs dans la fit)
  
  # Initialize x results matrix 
  x.residuals = X
  x.loadings = nipals.res$loadings
  x.scores = nipals.res$scores
  x.weights = nipals.res$contrib
  
  # x.residuals contient la variance des dimensions prédites qui n'est pas expliqué par le 
  # modèle pls (nipals pour nous). Les individus avec des résidus relativement large sont 
  # des anomalies (outliers) qui indique qu'ils ne sont pas bien expliqué par le modèle.
  
  for (j in 1:ncomp){ # ncomp pour la matrice residuals
    for (i in 1:n){
      x.residuals[i,j] = x.residuals[i,j] - x.scores[i,] %*% x.loadings[j,]
    }
  }
  
  # r, number of response on the class
  r = nlevels(y)
  
  y.dm = dummies(y)
  
  # Initialize y results matrix
  y.loadings = matrix(0, r, ncomp)
  y.scores = matrix(0, n, ncomp)
  
  
  for (i in 1:r){
    y.loadings[i,] = t(t(x.scores) %*% y.dm[,i])
    
  }
  #y.scores = y.dm[,1]%*%y.loadings
   # Prend la première colonne pour avoir un cas binaire
  # for (i in 1:r){
  #   y.scores[,i] = y.dm%*%y.loadings[,i]
  # }
   # Pas certain sur les y.scores, on a une colonne y 0/1 donc la moitié des scores est rempli de 0
  
  #y.pred = x.scores %*% t(y.loadings) # > 0 égale à 1 donc valeur positive, la pred est ok
  

  # Coefficients values
  df_pls <- data.frame(nipals.res$scores,y)
  lo<-nnet::multinom(y~.,df_pls,trace=F)
  
  # Gets coefficients from multinomial log-linear 
  coefs = data.frame(coef(lo))
  
  # Add first class coefficients with empty values
  coefs = rbind(rep(0, ncol(coefs)), coefs)
  row.names(coefs)[rownames(coefs) == "1"] = levels(y)[1]

  # Return 
  
  #class S3
  res.PLS = list("X" = X,
                  "y" = y.dm, # binaire pour l'instant mais à changer 
                  "x.scores" = x.scores,
                  "y.scores" = y.scores,
                  "x.loadings" = x.loadings,
                  "y.loadings" = y.loadings,
                  "x.weights" = x.weights,
                  "ynames" = colnames(y),
                  "Xnames" = Xnames,
                  "N_comp" = ncomp,
                  "coefs" = coefs
  )
  class(res.PLS) <- "PLSDA"
  return(res.PLS)
}




# Print PLS

#' print.pls from pls.fit
#'
#' @description
#' Print Coefficients and y.loadings from PLSDA Object
#'
#' @param PLSDA a PLSDA object to print
#'
#' @export
#'
print.pls = function(PLSDA){
  res = rbind(PLSDA$intercept, PLSDA$coef)
  y.loadings = PLSDA$y.loadings
  
  cat("Coefficients : \n")
  print(res)
  print("\n")
  cat("Y Loadings : \n")
  print(y.loadings)
}

# Summary PLS

#' summary.pls from pls.fit
#'
#' @description
#' Summary of a PLSDA Object
#'
#' @param PLSDA a PLSDA object to be summarized
#'
#' @export
#'

summary.pls = function(PLSDA){
  # a compléter
}
