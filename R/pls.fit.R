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
  
  n = nrow(data)
  p = NbXcol
  
  # Fit the model 
  nipals.res = nipals(X, ncomp, center=T, reduce=T)
  
  
  # Attention il faudrait centrer réduire X dans la fit avant le nipals pour avoir 
  # les bons résultats (donc changer les paramètres d'entrée de la nipals et faire les 
  # calculs dans la fit)
  
  # Initialize x results matrix 
  x.residuals = X
  x.loadings = nipals.res$loadings
  x.scores = nipals.res$scores
  
  # x.residuals contient la variance des dimensions prédites qui n'est pas expliqué par le 
  # modèle pls (nipals pour nous). Les individus avec des résidus relativement large sont 
  # des anomalies (outliers) qui indique qu'ils ne sont pas bien expliqué par le modèle.
  
  for (j in 1:ncomp){ # ncomp pour la matrice residuals
    for (i in 1:n){
      x.residuals[i,j] = x.residuals[i,j] - x.scores[i,] %*% x.loadings[j,]
    }
  }
  
  # r, number of response, 1 in our case where y has only one column
  r = 1
  
  # Initialize y results matrix
  y.loadings = matrix(0, r, ncomp)
  y.loadings = t(t(x.scores) %*% y.dm[,1]) # Prend la première colonne pour avoir un cas binaire
  y.scores = matrix(0, n, ncomp)
  y.scores = y.dm[,1]%*%y.loadings # Pas certain sur les y.scores, on a une colonne y 0/1 donc la moitié des scores est rempli de 0
  
  y.pred = x.scores %*% t(y.loadings) # > 0 égale à 1 donc valeur positive, la pred est ok
  
  # x.weights = matrix(0, p, ncomp)
  # 
  # for (j in 1:p){
  #   x.weights[j,] = 
  # }
  
  # Return 
  
  #class S3
  res.PLS <- list("X" = X,
                   "y" = y.dm[,1],
                   "x.scores" = x.scores,
                   "y.scores" = y.scores,
                   "x.loadings" = x.loadings,
                   "y.loadings" = y.loadings,
                   "ynames" = colnames(y),
                   "Xnames" = Xnames,
                   "N_comp" = ncomp,
                  # Faudrait rajouter les poids qui apparemment sont utilisés dans les calculs de 
                  # la nipals à l'origine
                  # j'ai besoin des poids de X pour calculer les coeff 
                  #"coef" = coef,
  )
  class(res.PLS) <- "PLSDA"
  return(res.PLS)
}

