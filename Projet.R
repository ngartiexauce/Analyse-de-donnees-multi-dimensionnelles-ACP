
library(tidyverse)
library(rmarkdown)
library(Factoshiny)
library(corrplot)
library(factoextra)
library(dplyr)
#Chemin dossier

setwd("C:/Users/yassi/Downloads/RDSM")


#Chargement des donn?es


load('data.baby.Rdata')
data_baby_base <- data.baby.comp
data_baby_reduit <- data.baby2.comp
#Questions 

# Introduction

#Question 1

#a) Il faut faire une ACP norm?e car les variables  ont des unit?s
# exemple : milligrammes, microgrammes, grammes, kilocalories


#b)


summary(data_baby_base)
summary(data_baby_reduit)




pca.R <- PCA(data_baby_reduit, scale.unit = FALSE)


# La 1er dimension repr?sente environ 94,54% de l'inertie tandis que la dimension 2 seulement 3,47%
#L'information port?e par la dimension 2 est insignifiante
# On voit que la variable VitA_mcg est extr?mement corr?l?e avec la dimension 1
#Ce qui justifie sa position

res.PCA<-PCA(data_baby_reduit,scale.unit=FALSE)
dimdesc(res.PCA)$Dim.1
# On voit que le coefficient de corrlation entre VitA_mcg et la dimension 1 est de 0.999 



#Question 2

#a)
res.PCA<-PCA(data_baby_reduit,scale.unit=TRUE,graph=TRUE)

corrplot(cor(data_baby_reduit),method="number",number.cex=0.7)

#VitB1_mg ,VitB2_mg,VitB3_mg sont fortement corr?l?e entre eux

#De même pour Fat_g, VitE_mg et VitB9_mcg


summary(res.PCA)

#b
valeurs_propres<-get_eigenvalue(res.PCA)
valeurs_propres
fviz_eig(res.PCA,addlabels=TRUE,ylim=c(0,50))


# La 1er dimension explique environ 49% de la variabilit? des variables
#Les deux premi?res dimension expliquent environ 72% de la variabilit? des variables
#Avec la r?gle de Kaiser, on garde les dimensions ayant une valeur propre sup?rieur ? 1
# Dans notre cas, on selectionne nos deux premi?res dimensions.


#c

plot.PCA(res.PCA,axes=c(1,2),choix='var',title="Graphe des variables de l'ACP")
plot.PCA(res.PCA,axes=c(1,3),choix='var',title="Graphe des variables de l'ACP")
plot.PCA(res.PCA,axes=c(1,4),choix='var',title="Graphe des variables de l'ACP")
plot.PCA(res.PCA,axes=c(2,3),choix='var',title="Graphe des variables de l'ACP")
plot.PCA(res.PCA,axes=c(2,4),choix='var',title="Graphe des variables de l'ACP")
plot.PCA(res.PCA,axes=c(3,4),choix='var',title="Graphe des variables de l'ACP")



#La meilleure represennt?e est obtenue en conservant l?s dimensions 1 et 2
#Pratiquement toutes les variables sont bien repr?sent?
#puisque l'?cart type des variables se rapproche de 1
#On le voit bien sur ce graphique car le cercle de corr?lation est de rayon 1
#Cependant, les variables VitB6_mg, VitC_mg sont moins bien repr?sent?es




# Question 3

#a

princomp.R <- princomp(data_baby_reduit,cor=TRUE)

loadings <- princomp.R$loadings

ind <- get_pca_ind(res.PCA)
ind$coord
ind$coord[c(237, 218, 216, 144, 141, 140, 149, 340, 125),]


#b 
fviz_pca_ind(res.PCA)


#Les individus 216 et 218 sont literalement coll?s ? la premi?re dimension 
#ce qui veut dire que leurs contribution ? la deuxi?me dimension est faible
#L'individu 216 a la valeur sur C la plus elev?e donc c'est lui qui contribue
#le plus ? la premi?re dimension
#respectivement -2.5066857 et -1.8455244

ind$coord[c(237, 218, 216, 144, 141, 140, 149, 340, 125),]

#c
#En revanche la position du point 144  est decal?e des deux dimensions
#Donc sa contributions pour les deux dimensions est non neglig?able


#d
#Calculons la variance 
#1
var_1<-var(ind$coord[,1])
var_1

#2 
var_2<- valeurs_propres[1]
var_2


#e
#Contribution premi?re dimension
fviz_contrib(res.PCA, choice = "ind", sort.val="desc", top=20,axes=1)

#contribution deuxi?me dimension
fviz_contrib(res.PCA, choice = "ind", sort.val="desc", top=20,axes=2)

#contribution ? la construction du plan
fviz_contrib(res.PCA, choice = "ind", sort.val="desc", top=20,axes=1:2)

#f Position sur le premier plan
plot.PCA(res.PCA,select='contrib  20',habillage='contrib',title="Graphe des individus de l'ACP")




# interprétaLa dimension 1 oppose des individus tels que 144 (à droite du graphe, caractérisés par une coordonnée fortement positive sur l’axe) à des individus caractérisés par une coordonnée fortement négative sur l’axe (à gauche du graphe).

#Le groupe auquel l’individu 144 appartient (caractérisés par une coordonnée positive sur l’axe) partage :
  
#  de fortes valeurs pour les variables VitB1_mg, VitB3_mg, VitB2_mg, Fiber_g, VitB6_mg et Protein_g (de la plus extrême à la moins extrême).
#de faibles valeurs pour les variables VitC_mg et VitA_mcg (de la plus extrême à la moins extrême).
#Le groupe 2 (caractérisés par une coordonnée positive sur l’axe) partage :
  
#  de fortes valeurs pour des variables telles que Fat_g, Sugar_g, VitB12_mcg, VitA_mcg, VitB9_mcg, VitE_mg, VitC_mg, Protein_g, VitB6_mg et VitB2_mg (de la plus extrême à la moins extrême).
#de faibles valeurs pour la variable Fiber_g.
#Le groupe 3 (caractérisés par une coordonnées négative sur l’axe) partage :
  
#  de faibles valeurs pour des variables telles que Protein_g, Fat_g, VitB9_mcg, VitB6_mg, VitE_mg, Sugar_g, VitB12_mcg, VitA_mcg, VitC_mg et VitB2_mg (de la plus extrême à la moins extrême).
#La dimension 2 oppose des individus tels que 144 (en haut du graphe, caractérisés par une coordonnées fortement positive sur l’axe) à des individus caractérisés par une coordonnée fortement négative sur l’axe (en bas du graphe).

# le groupe auquel l’individu 144 appartient (caractérisés par une coordonnée positive sur l’axe) partage :
# 
#   de fortes valeurs pour les variables vitb1_mg, vitb3_mg, vitb2_mg, fiber_g, vitb6_mg et protein_g (de la plus extrême à la moins extrême).
# de faibles valeurs pour les variables vitc_mg et vita_mcg (de la plus extrême à la moins extrême).
# le groupe 2 (caractérisés par une coordonnées négative sur l’axe) partage :
# 
#   de fortes valeurs pour des variables telles que fat_g, sugar_g, vitb12_mcg, vita_mcg, vitb9_mcg, vite_mg, vitc_mg, protein_g, vitb6_mg et vitb2_mg (de la plus extrême à la moins extrême).
# de faibles valeurs pour la variable fiber_g.
# tion à modifier à notre sauce



#



#################Question 4############################♦


#Question 2

#a)

res.PCA<-PCA(data_baby_reduit,quanti.sup=c(10,11,12,13),graph=FALSE)
plot.PCA(res.PCA,choix='var',title="Graphe des variables de l'ACP")
plot.PCA(res.PCA,select='contrib  20',habillage='contrib',title="Graphe des individus de l'ACP")


corrplot(cor(data_baby_reduit),method="number",number.cex=0.7)

#VitB1_mg ,VitB2_mg,VitB3_mg sont fortement corr?l?e entre eux

#De même pour Fat_g, VitE_mg et VitB9_mcg


summary(res.PCA)

#b
valeurs_propres<-get_eigenvalue(res.PCA)
valeurs_propres
fviz_eig(res.PCA,addlabels=TRUE,ylim=c(0,50))


# La 1er dimension explique environ 49% de la variabilit? des variables
#Les deux premi?res dimension expliquent environ 72% de la variabilit? des variables
#Avec la r?gle de Kaiser, on garde les dimensions ayant une valeur propre sup?rieur ? 1
# Dans notre cas, on selectionne nos deux premi?res dimensions.


#c

plot.PCA(res.PCA,axes=c(1,2),choix='var',title="Graphe des variables de l'ACP")
plot.PCA(res.PCA,axes=c(1,3),choix='var',title="Graphe des variables de l'ACP")
plot.PCA(res.PCA,axes=c(1,4),choix='var',title="Graphe des variables de l'ACP")
plot.PCA(res.PCA,axes=c(2,3),choix='var',title="Graphe des variables de l'ACP")
plot.PCA(res.PCA,axes=c(2,4),choix='var',title="Graphe des variables de l'ACP")
plot.PCA(res.PCA,axes=c(3,4),choix='var',title="Graphe des variables de l'ACP")



#La meilleure represennt?e est obtenue en conservant l?s dimensions 1 et 2
#Pratiquement toutes les variables sont bien repr?sent?
#puisque l'?cart type des variables se rapproche de 1
#On le voit bien sur ce graphique car le cercle de corr?lation est de rayon 1
#Cependant, les variables VitB6_mg, VitC_mg sont moins bien repr?sent?es




# Question 3

#a

princomp.R <- princomp(data_baby_reduit,cor=TRUE)

loadings <- princomp.R$loadings

ind <- get_pca_ind(res.PCA)
ind$coord
ind$coord[c(237, 218, 216, 144, 141, 140, 149, 340, 125),]


#b 
fviz_pca_ind(res.PCA)


#Les individus 216 et 218 sont literalement coll?s ? la premi?re dimension 
#ce qui veut dire que leurs contribution ? la deuxi?me dimension est faible
#L'individu 216 a la valeur sur C la plus elev?e donc c'est lui qui contribue
#le plus ? la premi?re dimension
#respectivement -2.5066857 et -1.8455244

ind$coord[c(237, 218, 216, 144, 141, 140, 149, 340, 125),]

#c
#En revanche la position du point 144  est decal?e des deux dimensions
#Donc sa contributions pour les deux dimensions est non neglig?able


#d
#Calculons la variance 
#1
var_1<-var(ind$coord[,1])
var_1

#2 
var_2<- valeurs_propres[1]
var_2


#e
#Contribution premi?re dimension
fviz_contrib(res.PCA, choice = "ind", sort.val="desc", top=20,axes=1)

#contribution deuxi?me dimension
fviz_contrib(res.PCA, choice = "ind", sort.val="desc", top=20,axes=2)

#contribution ? la construction du plan
fviz_contrib(res.PCA, choice = "ind", sort.val="desc", top=20,axes=1:2)

#f Position sur le premier plan
plot.PCA(res.PCA,select='contrib  20',habillage='contrib',title="Graphe des individus de l'ACP")






# 
# La dimension 1 oppose des individus tels que 145 (à droite du graphe, caractérisés par une coordonnée fortement positive sur l’axe) à des individus caractérisés par une coordonnée fortement négative sur l’axe (à gauche du graphe).
# 
# Le groupe auquel l’individu 145 appartient (caractérisés par une coordonnée positive sur l’axe) partage :
#   
#   de fortes valeurs pour les variables VitB1_mg, VitB2_mg, VitB3_mg, Fiber_g, VitB6_mg et Protein_g (de la plus extrême à la moins extrême).
# de faibles valeurs pour les variables VitC_mg et VitA_mcg (de la plus extrême à la moins extrême).
# Le groupe 2 (caractérisés par une coordonnée positive sur l’axe) partage :
#   
#   de fortes valeurs pour des variables telles que Fat_g, VitB12_mcg, Sugar_g, VitA_mcg, VitB9_mcg, VitC_mg, VitE_mg, Protein_g, VitB6_mg et VitB2_mg (de la plus extrême à la moins extrême).
# de faibles valeurs pour la variable Fiber_g.
# Le groupe 3 (caractérisés par une coordonnées négative sur l’axe) partage :
#   
#   de faibles valeurs pour des variables telles que Fat_g, VitB9_mcg, Protein_g, VitE_mg, VitB6_mg, VitB12_mcg, Sugar_g, VitA_mcg, VitC_mg et VitB2_mg (de la plus extrême à la moins extrême).
# La dimension 2 oppose des individus tels que 145 (en haut du graphe, caractérisés par une coordonnées fortement positive sur l’axe) à des individus caractérisés par une coordonnée fortement négative sur l’axe (en bas du graphe).
# 
# Le groupe 1 (caractérisés par une coordonnée positive sur l’axe) partage :
#   
#   de faibles valeurs pour des variables telles que Fat_g, VitB9_mcg, Protein_g, VitE_mg, VitB6_mg, VitB12_mcg, Sugar_g, VitA_mcg, VitC_mg et VitB2_mg (de la plus extrême à la moins extrême).
# Le groupe auquel l’individu 145 appartient (caractérisés par une coordonnée positive sur l’axe) partage :
#   
#   de fortes valeurs pour les variables VitB1_mg, VitB2_mg, VitB3_mg, Fiber_g, VitB6_mg et Protein_g (de la plus extrême à la moins extrême).
# de faibles valeurs pour les variables VitC_mg et VitA_mcg (de la plus extrême à la moins extrême).
# Le groupe 3 (caractérisés par une coordonnées négative sur l’axe) partage :
#   
#   de fortes valeurs pour des variables telles que Fat_g, VitB12_mcg, Sugar_g, VitA_mcg, VitB9_mcg, VitC_mg, VitE_mg, Protein_g, VitB6_mg et VitB2_mg (de la plus extrême à la moins extrême).
# de faibles valeurs pour la variable Fiber_g.



# 
# Filtrez les données pour sélectionner tous les plats ayant une coordonnée :
#   • Supérieure à 3 sur le premier axe
# • Négative sur le deuxième axe.
# b. Commentez cet ensemble de plats par rapport à la base complète des 362 plats
# c. Reprenez l’ACP en mettant ces plats en inactifs. Commentez l’interprétation du nouveau plan factoriel
# (1,2).



tab_coord <- filter(as.data.frame(res.PCA$ind$coord),res.PCA$ind$coord[,1]>3, res.PCA$ind$coord[,2]<0)

indices <- as.integer(row.names(tab_coord))


data_baby_q5 <- slice(data_baby_reduit,-c(indices))



#b)


#Question 2

#a)

res.PCA<-PCA(data_baby_q5,graph=FALSE)
plot.PCA(res.PCA,choix='var',title="Graphe des variables de l'ACP")
plot.PCA(res.PCA,select='contrib  20',habillage='contrib',title="Graphe des individus de l'ACP")


corrplot(cor(data_baby_reduit),method="number",number.cex=0.7)

#VitB1_mg ,VitB2_mg,VitB3_mg sont fortement corr?l?e entre eux

#De même pour Fat_g, VitE_mg et VitB9_mcg


summary(res.PCA)

#b
valeurs_propres<-get_eigenvalue(res.PCA)
valeurs_propres
fviz_eig(res.PCA,addlabels=TRUE,ylim=c(0,50))


# La 1er dimension explique environ 49% de la variabilit? des variables
#Les deux premi?res dimension expliquent environ 72% de la variabilit? des variables
#Avec la r?gle de Kaiser, on garde les dimensions ayant une valeur propre sup?rieur ? 1
# Dans notre cas, on selectionne nos deux premi?res dimensions.


#c

plot.PCA(res.PCA,axes=c(1,2),choix='var',title="Graphe des variables de l'ACP")
plot.PCA(res.PCA,axes=c(1,3),choix='var',title="Graphe des variables de l'ACP")
plot.PCA(res.PCA,axes=c(1,4),choix='var',title="Graphe des variables de l'ACP")
plot.PCA(res.PCA,axes=c(2,3),choix='var',title="Graphe des variables de l'ACP")
plot.PCA(res.PCA,axes=c(2,4),choix='var',title="Graphe des variables de l'ACP")
plot.PCA(res.PCA,axes=c(3,4),choix='var',title="Graphe des variables de l'ACP")



#La meilleure represennt?e est obtenue en conservant l?s dimensions 1 et 2
#Pratiquement toutes les variables sont bien repr?sent?
#puisque l'?cart type des variables se rapproche de 1
#On le voit bien sur ce graphique car le cercle de corr?lation est de rayon 1
#Cependant, les variables VitB6_mg, VitC_mg sont moins bien repr?sent?es




# Question 3

#a

princomp.R <- princomp(data_baby_q5,cor=FALSE)

loadings <- princomp.R$loadings

ind <- get_pca_ind(res.PCA)



#b 
fviz_pca_ind(res.PCA)


#Les individus 216 et 218 sont literalement coll?s ? la premi?re dimension 
#ce qui veut dire que leurs contribution ? la deuxi?me dimension est faible
#L'individu 216 a la valeur sur C la plus elev?e donc c'est lui qui contribue
#le plus ? la premi?re dimension
#respectivement -2.5066857 et -1.8455244


#c
#En revanche la position du point 144  est decal?e des deux dimensions
#Donc sa contributions pour les deux dimensions est non neglig?able


#d
#Calculons la variance 
#1
var_1<-var(ind$coord[,1])
var_1

#2 
var_2<- valeurs_propres[1]
var_2


#e
#Contribution premi?re dimension
fviz_contrib(res.PCA, choice = "ind", sort.val="desc", top=20,axes=1)

#contribution deuxi?me dimension
fviz_contrib(res.PCA, choice = "ind", sort.val="desc", top=20,axes=2)

#contribution ? la construction du plan
fviz_contrib(res.PCA, choice = "ind", sort.val="desc", top=20,axes=1:2)

#f Position sur le premier plan
plot.PCA(res.PCA,select='contrib  20',habillage='contrib',title="Graphe des individus de l'ACP")






PCAshiny(data.baby2.comp)

