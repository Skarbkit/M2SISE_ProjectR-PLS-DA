res <- nipals(iris[,1:4],ncomp=2)
summary(res)
head(res$loadings)

library(ggplot2)
library(tidyverse)
library(ggforce)

#### Scatter Plot reduction dimension avec NIPALS #####
X <- res$scores
Y <- iris[,5]

data <- data.frame(x1=X[,1], x2=X[,2], y=Y)
data$y <- factor(data$y)
ggplot(data, aes(x=x1, y=x2, color=y)) +
  geom_point() +
  xlab("dimension 1") +
  ylab("dimension 2") +
  ggtitle("Clusters") +
  geom_mark_ellipse(aes(color = y), expand = unit(0.5,"mm"))

### Cercle correlation ?? ###

loadings <-data.frame(res$loadings)
ggplot(loadings, aes(x=p1, y=p2)) +
  geom_point() +
  xlim(-1, 1) +
  ylim(-1, 1)

### 
