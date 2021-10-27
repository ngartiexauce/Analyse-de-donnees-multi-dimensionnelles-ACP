---
title: "Projet RDSM"
author: "Luweh Adjim Ngarti Exaucé & Laamoumri Yassine & Agbo Merveille"
date: "28/12/2020"
output: html_document
---

```{r setup, include=FALSE, warning=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r message=FALSE,warning=FALSE}
library(tidyverse)
library(rmarkdown)
library(corrplot)
library(factoextra)
library(rmarkdown)
library(FactoMineR)

```

## Question 1 : Introduction

### a) 
Il faut faire une ACP normée car les variables  ont des unités différentes
comme par  exemple : milligrammes, microgrammes, grammes, kilocalories.<br>
Cela nous permettra d'étudier la notion de corrélation entre les variables.<br>


### b)
Tout d'abord on charge nos données : 
```{r message=FALSE}
load('data.baby.Rdata')
data_baby_base <- data.baby.comp
data_baby_reduit <- data.baby2.comp
```

Puis on effectue une ACP non normée


```{r}
pca.R <- PCA(data_baby_reduit, scale.unit = FALSE)
```

 La 1er dimension représente environ 94,54% de l'inertie tandis que la dimension 2 seulement 3,47%. L'information portée par la dimension 2 est insignifiante. De plus, on voit que la variable VitA_mcg est extrêmement correlée avec la dimension 1,ce qui justifie sa position

```{r}
dimdesc(pca.R)$Dim.1
```

On voit que le coefficient de corrlation entre VitA_mcg et la dimension 1 est de 0.999 

## Question 2

### a)
```{r}
res.PCA<-PCA(data_baby_reduit,scale.unit=TRUE,graph=TRUE)

corrplot(cor(data_baby_reduit),method="number",number.cex=0.7)
```

VitB1_mg ,VitB2_mg,VitB3_mg sont fortement correlée entre eux. Il en est de même pour Fat_g, VitE_mg et VitB9_mcg.

### b)

```{r}
valeurs_propres<-get_eigenvalue(res.PCA)
valeurs_propres
fviz_eig(res.PCA,addlabels=TRUE,ylim=c(0,50))
```

La 1ere dimension explique environ 49% de la variabilité des variables.Les deux premières dimension expliquent environ 72% de la variabilité des variables. Avec la régle de Kaiser, on garde les dimensions ayant une valeur propre supérieur à 1. Dans notre cas, on selectionne nos deux premières dimensions.

### c)

```{r}
plot.PCA(res.PCA,axes=c(1,2),choix='var',title="Graphe des variables de l'ACP")
plot.PCA(res.PCA,axes=c(1,3),choix='var',title="Graphe des variables de l'ACP")
plot.PCA(res.PCA,axes=c(1,4),choix='var',title="Graphe des variables de l'ACP")
plot.PCA(res.PCA,axes=c(2,3),choix='var',title="Graphe des variables de l'ACP")
plot.PCA(res.PCA,axes=c(2,4),choix='var',title="Graphe des variables de l'ACP")
plot.PCA(res.PCA,axes=c(3,4),choix='var',title="Graphe des variables de l'ACP")
```

La meilleure représentation est obtenue en conservant les dimensions 1 et 2. Pratiquement toutes les variables sont bien représentées puisque l'écart type des variables se rapproche de 1. On le voit bien sur ce graphique car le cercle de correlation est de rayon 1.
Cependant, les variables VitB6_mg, VitC_mg sont moins bien représentées.

## Question 3

### a)

```{r}
princomp.R <- princomp(data_baby_reduit,cor=TRUE)

loadings <- princomp.R$loadings

ind <- get_pca_ind(res.PCA)

ind$coord[c(237, 218, 216, 144, 141, 140, 149, 340, 125),]
```

### b)
```{r}
fviz_pca_ind(res.PCA)
```

Commentaire


