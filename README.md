# PLSDA Regression (Partial Least Square Discriminant Analysis) 

### TABLE OF CONTENTS
-   [Project Description](#project-description)
    -   [PLSDA Definition](#plsda-definition)
    -   [NIPALS Algorithm](#nipals-algorithm)
-  [Navigating the Shiny App](#navigating-the-shiny-app)
    -   [Uploading and visualizing new data](#uploading-and-visualizing-new-data-in-app)
    -   [Splitting the data in app](#splitting-the-data-in-app)
    -   [Fitting the model-in-app](#fitting-the-model-in-app)
    -   [Testing the model and predicting-in-app](#testing-the-model-and-predicting-in-app)
    -   [](#)
-  [Using the package PLSDA](#using-the-package-plsda)
    -   [Installing the package](#installing-the-package)
    -   [Preparing the data](#preparing-the-data)
    -   [Fitting the model](#fitting-the-model)
    -   [Testing the model and predicting](#testing-the-model-and-predicting)
    -   [Determining the VIP](#determining-the-vip)
    -   [Graphics Options](#graphics-options)
    -   [Help](#help)
       
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
-----------------------------

The following tutorial is also available in video form here : // Si on fait une vidéo //

To connect to the Shiny App, enter Rstudio and type these lines
```sh
install.packages("shiny")
install.packages("shinydashboard")
library(shiny)
shiny::runGitHub('M2SISE_ProjectR-PLS-DA', 'Skarbkit', ref='main') #je regarderai après pourquoi ça fonctionne pas
```
#### UPLOADING AND VISUALIZING NEW DATA IN APP
----------------------------------------
On the top section of the app you can import a csv file by clicking the "Browse button". You can then choose the separator, the Header and the display (which is shown below)
<img width="500" alt="Shiny_Upload_data" src="https://i.imgur.com/f6Yoeei.jpg">
<br/>

#### SPLITTING THE DATA IN APP
-------------------------------------


### USING THE PACKAGE PLSDA 
-----------------------------------
To install the package you need to download it from GitHub
```sh
library(devtools)
install_github("Skarbkit/PLSDA")
```

Then when it is successfully installed, you need to load it
```sh
library(PLSDA)
```
To test the following functions we will use the dataset 'iris' (available in R)
```sh
summary(iris)
```
#### PREPARING THE DATA
-------------------------------
The version of our packages does not accept missing values for now. You need to clean it before using it.
With the function split_train_test() you can separate your data set into two with a treshold of p.

```sh
data_split <- split_train_test(data = iris, class=c("Species"),p=0.67) 
summary(data_split)
```

<img width="700" alt="Capture d’écran 2021-11-29 à 15 59 47" src="https://i.imgur.com/8hGZQsK.jpg">
<br/>

#### FITTING THE MODEL
------------------------------------
```sh
model1 <- pls.fit(formula = Species ~ . , data = iris , ncomp = 2 , center = TRUE , reduce = FALSE )
print(model)
summary(model)
```
<img width="700" alt="Capture d’écran 2021-11-29 à 15 59 47" src="https://i.imgur.com/8hGZQsK.jpg">
<br/>

pls.fit() uses functions dummies() and nipals() when creating the model. Both are available in the package to use separately

```sh
dummies(iris$Species)
```
<img width="700" alt="Capture d’écran 2021-11-29 à 15 59 47" src="https://i.imgur.com/8hGZQsK.jpg">
<br/>

```sh
nipals(X = iris[,1:4], ncopmp = 2 , center = TRUE , reduce = FALSE)
```
<img width="700" alt="Capture d’écran 2021-11-29 à 15 59 47" src="https://i.imgur.com/8hGZQsK.jpg">
<br/>



#### TESTING THE MODEL AND PREDICTING
------------------------------------
```sh
predict.plsda(ObjectPLSDA = model1 , newdata = , type="class") #A COMPLETER
```
<img width="700" alt="Capture d’écran 2021-11-29 à 15 59 47" src="https://i.imgur.com/8hGZQsK.jpg">
<br/>

#### DETERMINING THE VIP
------------------------------------

```sh
plsda.vip(objectPLSDA = model1 , treshold = 0.8 ) 
```

<img width="700" alt="Capture d’écran 2021-11-29 à 15 59 47" src="https://i.imgur.com/8hGZQsK.jpg">
<br/>

#### GRAPHICS OPTIONS
------------------------------------
 The package comes with graphics functions.
```sh
 
```

#### HELP
-----------------------------------------
Don't forget to use the command help(<function>) to open a help window for each function.


