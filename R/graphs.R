#res <- nipals(iris[,1:4],ncomp=2)
#summary(res)
#head(res$loadings)

library(ggplot2)
library(tidyverse)
library(ggforce)
library(plotly)

#### Scatter Plot reduction dimension avec NIPALS #####
#X <- res$scores
#Y <- iris[,5]

#data <- data.frame(x1=X[,1], x2=X[,2], y=Y)
#data$y <- factor(data$y)
#ggplot(data, aes(x=x1, y=x2, color=y)) +
#  geom_point() +
#  xlab("dimension 1") +
 # ylab("dimension 2") +
#  ggtitle("Clusters") +
#  geom_mark_ellipse(aes(color = y), expand = unit(0.5,"mm"))

### Cercle correlation  ###

#loadings <-data.frame(res$loadings)
#ggplot(loadings, aes(x=p1, y=p2)) +
#  geom_point() +
#  xlim(-1, 1) +
#  ylim(-1, 1)


circle.plot <- function(PLSDA){
  ## Plot circle of correlations between variables
  # get correlations
  cor_tx = PLSDA$x.loadings[,1]
  cor_ty = PLSDA$x.loadings[,2]
  X=PLSDA$X
  cor1=res$cor_tx*sqrt(eigen(cor(scale(res$X)))$values[1])
  cor2=res$cor_ty*sqrt(eigen(cor(scale(res$X)))$values[2])
  
  
  # points for generating circle
  z = seq(0, 2*pi, l=100)
  # open plot
  plot(cos(z), sin(z), type="l", 
       main=expression(bold("Circle of Correlations on  ") * bold(list(t[1],t[2]))), 
       ylim=c(-1.1,1.1), xlim=c(-1.2,1.2),
       xlab=expression("PLS-component  " * t[1]), 
       ylab=expression("PLS-component  " * t[2]), 
       cex.main=1, cex.axis=.8, col="grey")
  # adding lines
  abline(h=seq(-1,1,.25), v=seq(-1,1,.25), col="grey", lty=3)
  abline(h=0, v=0, col="grey", lwd=2)
  # variables
  points(cor1[,1], cor1[,2], pch=20, col=rep("blue",nrow(cor1)))
  text(cor1[,1], cor1[,2], labels=rownames(cor1), pos=2, 
       col=rep("blue",nrow(cor1)), cex=.8)
  
}


#' PLOT Individuals
#'
#' @description 
#' Show individuals on factorial plan
#'
#' @param PLSDA 
#' an object PLSDA
#'
#' @param Axe1 
#' the number of the component on x axis
#'  
#' @param Axe2 
#' the number of the component on y axis
#' @returns 
#'  Individuals plots for PLSDA
#' @export
#'

individuals_plot <- function(PLSDA, Axe1 = 1, Axe2 = 2){
  
  # Get the number of component choosed in the pls fit
  n_comp = PLSDA$N_comp
  
  if(Axe1 > n_comp | Axe_2 > n_comp){
    
    print("La valeur de l'axe est supérieure au nombre de composants")
  }else 
    {
      x= PLSDA$x.scores[,Axe1]
      y= PLSDA$x.scores[,Axe2]
   
    ind <- plot_ly(x = x, y =y , color = PLSDA$y, type = "scatter", mode = "markers")
    
    ind <- ind %>% layout(
      title = "Individuals",
      legend=list(title=list(text='Color')),
      xaxis = list(title=paste0("Comp ",Axe1)),
      yaxis = list(title=paste0("Comp ",Axe2))
    )
    
    return(ind)
  }
}
