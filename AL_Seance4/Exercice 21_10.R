
# Exercice falcultatif pour le 21 octobre 2021
# a envoyer par mail : arno.muller@ined.fr


##########################
# Package              ####

library(TraMineR)       # Analyse de séquences
library(tidyverse)      # Gestion des données
library(haven)          # Ouverture données format SAS

##########################


##########################
# Lecture des donnees  ####


# Merci de n'utiliser ces données QUE dans le cadre de ce cours

# Donnees Peuplement et dépeuplement de Paris
# l'enquête a été effectué en 1986 et est une enquête RETROSPECTIVE 


setwd("D:/IDUP/1_Analyse_Sequence/Fichiers")
PDP = read_sas("PDP_LOG.sas7bdat")




# Objectif             ####

# Le fichier en entrée est un fichier INDIVIDU*ANNEE, renseigné à partir du moment 
# où l'individu dispose d'un logement INDIVIDU. On souhaite effectuer une analyse 
# de séquences, visant à regrouper des individus ayant une trajectoire résidentielle 
# relativement similaire, en termes de statut d'occupation. 

# Cette analyse se fera de 25 à 50 ans. 

# La définition du fichier de séquences est plus aisée à partir d'un fichier 
# individuel, recensant, sous la forme d'un vecteur le statut d'occupation 
# à chaque âge. 



##########################




##########################
# EXERCICE 1           ####


# Construire pour cette analyse un fichier INDIVIDU (large), renseignant les différents 
# statuts d'occupation de 25 à 50 ans. On conservera dans ce fichier toutes les 
# variables individuelles fixes :
# - la situation matrimoniale désigne la situation au moment de l'enquête et est donc fixe
# - l'origine sociale
# - l'année de naissance


# Dans le cas où des individus ne serait pas observer dès leur 25ans, considerer
# qu'ils habitent encore chez leurs parents : SO_LOG == "Parents"


str(PDP)
table(PDP$SO_LOG)
table(PDP$SITMAT)
table(PDP$ORIG_SOC)
table(PDP$Age)



# Création de la variable Année de Naissance

agemax <- PDP %>% 
  arrange(IDENT,Age) %>% 
  group_by(IDENT) %>% 
  slice_tail() %>% 
  select(IDENT,Age) %>% 
  mutate(ANNAIS = 1986 - Age ) %>% 
  select(IDENT, ANNAIS)





##########################

##########################
# EXERCICE 2          ####

# QUESTION 1 ####

#	Définition des données de séquences



# QUESTION 2 ####


#	Production d'un chronogramme selon l'origine sociale


# QUESTION 3 ####


#	Production de tapis selon l'origine sociale, de manière séparée pour les 
# individus selon l'état résidentiel initial



# QUESTION 4 ####


#	Calculer les grandeurs suivantes par individu : Nombre de transitions, 
# Durée passée dans chaque état, Entropie longitudinale




# QUESTION 5 ####

#### Indicateurs globaux

## Quelle est la séquence la plus commune ?




## Quelle est la séquence des états modaux 



## Est ce que les personnes passent le même temps dans les status d'occupation
# selon leur origine sociale ?



## Quelle est la probabilité de devenir propriétaire quand on a habité chez ses
# parent l'année précédente ?



## A quel age les individus partagent t'ils le plus le même statuts d'occupation
# A quel age les individus ont ils le statut d'occupation le plus hétérogène ?
# lire graphiquement


##########################









