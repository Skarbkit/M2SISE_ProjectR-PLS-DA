# PLSDA Regression (Partial Least Square Discriminant Analysis) 

### TABLE OF CONTENTS
-   [Project Description](#project-description)
    -   [PLSDA Definition](#plsda-definition)
    -   [NIPALS Algorithm](#nipals-algorithm)
-  [Navigating the Shiny App](#navigating-the-shiny-app)

### PROJECT DESCRIPTION
----------------------
This project is part of our cursus in Data Science at the University of Lyon 2, Master 2 SISE.  The main objective is to develop an R package and Shiny application capable of realizing a PLSDA. 
The user can download the R package directly from Git or execute the Shiny App (we will see how later) 


#### PLSDA DEFINITION 
----------------------
From the mixOmics website we find the following definition :  
"PLS was designed with a canonical (exploratory) approach and a regression (explanatory) approach in mind. Partial Least Squares – Discriminant Analysis (PLS-DA) was hence developed to allow the powerful PLS algorithm to be used for classification [1, 2]. It performs very similarly to PLS, just that the response vector y contains categorical vectors rather than continuous vectors. PLS-DA has the same advantages that PLS does, such that it operates efficiently over large dataframes and is not negatively influenced by collinearity."

#### NIPALS ALGORITHM
---------------------
From statistics4u.com : 
"The NIPALS Algorithm ("Nonlinear Iterative vartial Least Squares") has been developed by H. Wold at first for PCA and later-on for PLS. It is the most commonly used method for calculating the principal components of a data set. It gives more numerically accurate results when compared with the SVD of the covariance matrix, but is slower to calculate."

### NAVIGATING THE SHINY APP

To connect to the Shiny App, enter Rstudio and type this line
```sh
runGitHub("reponame","username")
```

