
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

hop <- read.csv2("PDP_IND_YEAR.csv")



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
# - l'année de naissance (a construire, si vous n'y arrivez pas sautez l'étape)


# Dans le cas où des individus ne serait pas observer dès leur 25ans, considerer
# qu'ils habitent encore chez leurs parents : SO_LOG == "Parents"


str(PDP)
table(PDP$SO_LOG)
table(PDP$SITMAT)
table(PDP$ORIG_SOC)
table(PDP$Age)



Z <- PDP %>% 
  
  # Variable à selectionner 
  select(IDENT,SO_LOG,Age,ORIG_SOC,SITMAT) %>% 
  
  # Garder que les âges entre 25 et 50ans
  filter(Age<51 & Age>24) %>%  
  
  # Passage en fichier LARGE, pour qu'on observe tous les individus sur les mêmes âge
  pivot_wider(names_from = Age,
              names_prefix = "AGE",
              names_sort = TRUE,
              values_from = SO_LOG) %>% 
  # On a des NA pour les personnes qu'on a pas observé dès 25ans.
  # On repasse en long pour faciliter le codage de ces NA en habite avec leurs parents
  
  pivot_longer(cols = c(AGE25:AGE50),
               names_to = "Age",
               names_prefix = "AGE",
               values_to = "SO_LOG") %>% 
  
  # Recodage NA SO-LOG en "Parents"
  mutate(SO_LOG = ifelse(is.na(SO_LOG) ,"Parents", SO_LOG )) %>% 
  
  
  # On revient au format individuel
  pivot_wider(names_from = Age,
              names_prefix = "AGE",
              names_sort = TRUE,
              values_from = SO_LOG)
  



# Création de la variable Année de Naissance

agemax <- PDP %>% 
  arrange(IDENT,Age) %>% 
  group_by(IDENT) %>% 
  slice_tail() %>% 
  select(IDENT,Age) %>% 
  mutate(ANNAIS = 1986 - Age ) %>% 
  select(IDENT, ANNAIS)

Z <- Z %>% 
  left_join(agemax, by = "IDENT") %>% 
  select(IDENT, ANNAIS, ORIG_SOC, SITMAT, everything())




##########################

##########################
# EXERCICE 2          ####

# QUESTION 1 ####

#	Définition des données de séquences


names(table(PDP$SO_LOG))

# Définition de l'ALPHABET
SO.alphabet = c("Autre","HLM","Locataire autre","Locataire prive","Loge employeur",
                "LOI 48","ND,NSP","Parents","Proprietaire")   
# Ou
SO.alphabet = names(table(PDP$SO_LOG))


# Label court (pour graph)
SO.scodes=c("AUT", "HLM", "LOC_AUT", "LOC_PRI", "LOG_EMP", "LOI48", "NSP", "PAR", "PROPRI")


# OBJET DE SEQUENCE
SO.seq=seqdef(Z, 5:30, 
              alphabet = SO.alphabet, 
              states = SO.scodes,
              labels = SO.alphabet)


# QUESTION 2 ####


#	Production d'un chronogramme selon l'origine sociale


seqdplot(SO.seq,
         group = Z$ORIG_SOC,
         with.legend = TRUE,
         use.layout = TRUE,
         legend.prop = 0.15)


# QUESTION 3 ####


#	Production de tapis selon l'origine sociale, de manière séparée pour les 
# individus selon l'état résidentiel initial


seqIplot(SO.seq,
         sortv = "from.start",
         group = Z$ORIG_SOC,
         with.legend = TRUE,
         use.layout = TRUE,
         legend.prop = 0.15)


# Pour export et affichage
pdf(file="test2.pdf")
seqIplot(SO.seq,
         sortv = "from.start",
         group = Z$ORIG_SOC,
         with.legend = TRUE,
         use.layout = TRUE,
         legend.prop = 0.15)
dev.off()


# QUESTION 4 ####


#	Calculer les grandeurs suivantes par individu : Nombre de transitions, 
# Durée passée dans chaque état, Entropie longitudinale


# Nombre de transition
seqtransn(SO.seq)

## Durée passée dans chaque état
seqistatd(SO.seq)[1:10,]

# Entropie Longitudinale
seqient(SO.seq)
hist(seqient(SO.seq))


# QUESTION 5 ####

#### Indicateurs globaux

## Quelle est la séquence la plus commune ?
seqtab(SO.seq)
# Visualisation des 10 séquences les plus fréquentes
seqfplot(SO.seq)


## Quelle est la séquence des états modaux 
seqmodst(SO.seq)
seqmsplot(SO.seq)


## Est ce que les personnes passent le même temps dans les status d'occupation
# selon leur origine sociale ?

seqmtplot(SO.seq)

# Selon l'origine sociale
seqmtplot(SO.seq, group = Z$ORIG_SOC)



## Quelle est la probabilité de devenir propriétaire quand on a habité chez ses
# parent l'année précédente ?

transition <- seqtrate(SO.seq)
round(transition,2)



## A quel age les individus partagent t'ils le plus le même statuts d'occupation
# A quel age les individus ont ils le statut d'occupation le plus hétérogène ?
seqHtplot(SO.seq)



##########################









