
# Exercice falcultatif pour le 21 octobre 2021
# a envoyer par mail : arno.muller@ined.fr


##########################
# Package              ####

library(TraMineR)       # Analyse de s�quences
library(tidyverse)      # Gestion des donn�es
library(haven)          # Ouverture donn�es format SAS

##########################


##########################
# Lecture des donnees  ####


# Merci de n'utiliser ces donn�es QUE dans le cadre de ce cours

# Donnees Peuplement et d�peuplement de Paris
# l'enqu�te a �t� effectu� en 1986 et est une enqu�te RETROSPECTIVE 


setwd("D:/IDUP/1_Analyse_Sequence/Fichiers")
PDP = read_sas("PDP_LOG.sas7bdat")




# Objectif             ####

# Le fichier en entr�e est un fichier INDIVIDU*ANNEE, renseign� � partir du moment 
# o� l'individu dispose d'un logement INDIVIDU. On souhaite effectuer une analyse 
# de s�quences, visant � regrouper des individus ayant une trajectoire r�sidentielle 
# relativement similaire, en termes de statut d'occupation. 

# Cette analyse se fera de 25 � 50 ans. 

# La d�finition du fichier de s�quences est plus ais�e � partir d'un fichier 
# individuel, recensant, sous la forme d'un vecteur le statut d'occupation 
# � chaque �ge. 



##########################




##########################
# EXERCICE 1           ####


# Construire pour cette analyse un fichier INDIVIDU (large), renseignant les diff�rents 
# statuts d'occupation de 25 � 50 ans. On conservera dans ce fichier toutes les 
# variables individuelles fixes :
# - la situation matrimoniale d�signe la situation au moment de l'enqu�te et est donc fixe
# - l'origine sociale
# - l'ann�e de naissance


# Dans le cas o� des individus ne serait pas observer d�s leur 25ans, considerer
# qu'ils habitent encore chez leurs parents : SO_LOG == "Parents"


str(PDP)
table(PDP$SO_LOG)
table(PDP$SITMAT)
table(PDP$ORIG_SOC)
table(PDP$Age)



# Cr�ation de la variable Ann�e de Naissance

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

#	D�finition des donn�es de s�quences



# QUESTION 2 ####


#	Production d'un chronogramme selon l'origine sociale


# QUESTION 3 ####


#	Production de tapis selon l'origine sociale, de mani�re s�par�e pour les 
# individus selon l'�tat r�sidentiel initial



# QUESTION 4 ####


#	Calculer les grandeurs suivantes par individu : Nombre de transitions, 
# Dur�e pass�e dans chaque �tat, Entropie longitudinale




# QUESTION 5 ####

#### Indicateurs globaux

## Quelle est la s�quence la plus commune ?




## Quelle est la s�quence des �tats modaux 



## Est ce que les personnes passent le m�me temps dans les status d'occupation
# selon leur origine sociale ?



## Quelle est la probabilit� de devenir propri�taire quand on a habit� chez ses
# parent l'ann�e pr�c�dente ?



## A quel age les individus partagent t'ils le plus le m�me statuts d'occupation
# A quel age les individus ont ils le statut d'occupation le plus h�t�rog�ne ?
# lire graphiquement


##########################









