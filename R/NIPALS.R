#' This function performs NIPALS algorithm, i.e. the singular-value
#' decomposition (SVD) of a data table that can contain missing values.
#' 
#' The NIPALS algorithm (Non-linear Iterative Partial Least Squares) has been
#' developed by H. Wold at first for PCA and later-on for PLS. It is the most
#' commonly used method for calculating the principal components of a data set.
#' It gives more numerically accurate results when compared with the SVD of the
#' covariance matrix, but is slower to calculate.
#' 
#' This algorithm allows to realize SVD with missing data, without having to
#' delete the rows with missing data or to estimate the missing data.


nipals <- function (X, ncomp = 2, center = T, reduce = F)
{
  X = as.matrix(X)
  #-# Checklist to see if the function can run #-#
  #~ Matrix is numeric
  if (!is.numeric(X))
    stop ("Matrix is not numeric")
  #~ Col not void
  if (any(colSums(!is.na(X)) == 0) | any(rowSums(!is.na(X)) == 0 ))
    stop("Some rows or columns are void", 
         "Function can't run properly", call)

  #-# Function center reduce -#
  if(center & reduce){ X <- apply(X,2,function(x) return((x-mean(x))/sd(x))) 
  } else if (center & !reduce){ X <- apply(X,2,function(x) return(x-mean(x)))
  } else if (!center & reduce){ X <- apply(X,2,function(x) return(x/sd(x))) 
  }
  
  
  #-# Preparation objects #-#
  n = nrow(X)
  p = ncol(X)
  nc = ncomp
  X.old = X
  Tm = matrix(0, n, nc) # scores
  Ph = matrix(0, p, nc) # loadings
  eigvals = rep(0, nc) # eigenvalues
  
  
  #-# Iterative process #-#
  
  #### Only for dataset with no missing values (for now)###
  
  for (h in 1:nc)
  {
    T.new = X.old[,1]
    ph.old = rep(1, p)
    ph.new = rep(1, p)
    iter = 1
    repeat
    {
      ph.new = t(X.old) %*% T.new / sum(T.new^2)
      ph.new = ph.new / sqrt(sum(ph.new^2))
      T.new = X.old %*% ph.new
      
      ph.aux = sum((ph.new - ph.old)^2)
      ph.old = ph.new
      # check convergence
      if (ph.aux < 1e-06 || iter==100) break
      iter = iter + 1
    }
    Tm[,h] = T.new
    Ph[,h] = ph.new
    X.new = X.old - T.new %*% t(ph.new)
    X.old = X.new
    eigvals[h] = sum(T.new^2) / (n-1)
  }
  dimnames(Tm) = list(rownames(X), paste(rep("t",nc), 1:nc, sep=""))
  dimnames(Ph) = list(colnames(X), paste(rep("p",nc), 1:nc, sep=""))
  
  # =======================================================#
  # =======================================================#
  
  #-# Eigenvalues #-#
  eig.perc = 100 * eigvals / p
  eigs = data.frame(values=eigvals, percentage=eig.perc, cumulative=cumsum(eig.perc))
  rownames(eigs) = paste(rep("v",nc), 1:nc, sep="")
  
  cor.sco = cor(X, Tm)
  
  #-# Individuals contribution #-#
  ConInd = (100/n) * Tm^2 %*% diag(1/eigvals)
  dimnames(ConInd) = list(rownames(X), paste(rep("ctr",nc),1:nc,sep=".") )
  
  #-# Results #- 
  res = list(values = eigs, 
             scores = Tm, 
             loadings = Ph, 
             cor.xt = cor.sco, 
             contrib = ConInd 
             )
  
  class(res) = "nipals"
  return(res)
}



##################################################


#- test que je supprimerai à la fin -#

res <- nipals(iris[,1:4],ncomp=2)
summary(res)
head(res$loadings)

X= iris[,1:4]
X_c<-apply(X,2,function(x) return(x-mean(x)))
X_cr<-apply(X,2,function(x) return((x-mean(x))/sd(x)))
X_cr


X = as.matrix(X)

center = T
reduce = F


if(center & reduce){ print('1') 
    } else if (center & !reduce){ print('2')
    } else if (!center & reduce){ print('3') 
    }