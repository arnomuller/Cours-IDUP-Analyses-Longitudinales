
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

hop <- read.csv2("PDP_IND_YEAR.csv")



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
# - l'ann�e de naissance (a construire, si vous n'y arrivez pas sautez l'�tape)


# Dans le cas o� des individus ne serait pas observer d�s leur 25ans, considerer
# qu'ils habitent encore chez leurs parents : SO_LOG == "Parents"


str(PDP)
table(PDP$SO_LOG)
table(PDP$SITMAT)
table(PDP$ORIG_SOC)
table(PDP$Age)



Z <- PDP %>% 
  
  # Variable � selectionner 
  select(IDENT,SO_LOG,Age,ORIG_SOC,SITMAT) %>% 
  
  # Garder que les �ges entre 25 et 50ans
  filter(Age<51 & Age>24) %>%  
  
  # Passage en fichier LARGE, pour qu'on observe tous les individus sur les m�mes �ge
  pivot_wider(names_from = Age,
              names_prefix = "AGE",
              names_sort = TRUE,
              values_from = SO_LOG) %>% 
  # On a des NA pour les personnes qu'on a pas observ� d�s 25ans.
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
  



# Cr�ation de la variable Ann�e de Naissance

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

#	D�finition des donn�es de s�quences


names(table(PDP$SO_LOG))

# D�finition de l'ALPHABET
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


#	Production de tapis selon l'origine sociale, de mani�re s�par�e pour les 
# individus selon l'�tat r�sidentiel initial


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
# Dur�e pass�e dans chaque �tat, Entropie longitudinale


# Nombre de transition
seqtransn(SO.seq)

## Dur�e pass�e dans chaque �tat
seqistatd(SO.seq)[1:10,]

# Entropie Longitudinale
seqient(SO.seq)
hist(seqient(SO.seq))


# QUESTION 5 ####

#### Indicateurs globaux

## Quelle est la s�quence la plus commune ?
seqtab(SO.seq)
# Visualisation des 10 s�quences les plus fr�quentes
seqfplot(SO.seq)


## Quelle est la s�quence des �tats modaux 
seqmodst(SO.seq)
seqmsplot(SO.seq)


## Est ce que les personnes passent le m�me temps dans les status d'occupation
# selon leur origine sociale ?

seqmtplot(SO.seq)

# Selon l'origine sociale
seqmtplot(SO.seq, group = Z$ORIG_SOC)



## Quelle est la probabilit� de devenir propri�taire quand on a habit� chez ses
# parent l'ann�e pr�c�dente ?

transition <- seqtrate(SO.seq)
round(transition,2)



## A quel age les individus partagent t'ils le plus le m�me statuts d'occupation
# A quel age les individus ont ils le statut d'occupation le plus h�t�rog�ne ?
seqHtplot(SO.seq)



##########################









