#######################################
######   ANALYSE LONGITUDINALE   ######
######          Seance 2         ######
#######################################


#######################
### PACKAGE            ####
library(tidyverse)
library(haven)   # Lecture de fichiers SAS
library(gmodels) # Fonction de tableaux croises analogue a SAS
library(GGally)  # Graphique odd ratio
#######################


########################
### IMPORTER DONNEE     ####


Titanic <- read.table("D:/IDUP/1_Analyse_Sequence/TP1_Titanic/Data/titanic.txt",
                      sep = ";",
                      header = TRUE)

class(Titanic) # type d'objet
str(Titanic)

########################


########################
## STAT DESC            ####



#Je veux le nombre de survivants et le nombre de valeurs manquantes,
# ainsi que le total
addmargins(table(Titanic$class, useNA = "always"))



########################

########################
## FILTER()             ####

# Je veux creer une base de donnée où je ne garde que les personnes de
# strictement moins d'un an
# CTRL + M
age_chelou <- Titanic %>% 
  filter(age < 1) 




########################

########################
## TITANIC2             ####

# & pour et 
# | pour ou

Titanic2 <- Titanic %>% 
  filter(age >= 1) %>%  
  mutate(enfant = ifelse(age < 18, "Enfant", "Adulte"), # si age inférieur à 18 alors enfant = 1, sinon enfant = 0
         enfant = as.factor(enfant),
         sexe = ifelse(gender == "male","Homme","Femme"), 
         classe = as.factor(ifelse(class == "1st",1,
                                   ifelse(class == "2nd",2,
                                          ifelse(class == "3rd",3, 0 )))), 
         surv = as.factor(ifelse(survived == "yes", 1, 0)))

########################


########################
## SELECT()             ####
# Une sauvegarde        ####
Testanic <- Titanic2

# Fonctionnement        ####

# Je souhaite garder uniquement les variables qui nous intéresse
# on utilise la fonction select()
# elle permet également de réagencer les colonnes de ma base de données




Testanic <- Testanic %>% 
  select(age, sexe, classe, enfant)

table(Testanic$surv)


# Reagencement          ####

# Mais là l'ordre me plait pas trop, je veux que enfant soit à coté de age
# sauf que j'ai la flemme de réecrire toutes les variables pour les réagencer
# la fonction everything() est la solution


Testanic <- Testanic %>% 
  select(age, enfant, everything())
# ne supprime aucune variable, mais tiens compte de l'ordre de celles notifiées


# Selection inversee    ####

# je peux aussi vouloir selectionner toutes les variables sauf une (ici je veux pas l'âge)
Testanic <- Testanic %>% 
  select(-age)


# Veritable selection   ####

Titanic <- Titanic2 %>% 
  select(surv,age,enfant,sexe,classe)


str(Titanic)
# toujours préféré les variables à character pour les analyses
Titanic$sexe <- as.factor(Titanic$gender)







########################


########################
#####  EXERCICES        ####

# Question 1        ####
#A partir d un tableau croise :  
  
#	Quelle est la probabilite d avoir survecu, pour un enfant ? pour un adulte ?  
# Quel est le rapport de risque de survivre pour un enfant par rapport a un adulte ?  
# Quel est l odds ratio d un enfant vs un adulte ?  

# Methode 1
table(Titanic$age,Titanic$surv)
prop.table(table(Titanic$age,Titanic$surv),1)
prop.table(table(Titanic$age,Titanic$surv),2)

# Methode 2
CrossTable(Titanic$age,Titanic$surv)

# Question 1 bis    ####

# Refaire pour classe

table(Titanic$classe,Titanic$surv)
prop.table(table(Titanic$classe,Titanic$surv),2) # Pourcentage colonne
prop.table(table(Titanic$classe,Titanic$surv),1) # Pourcentage ligne
CrossTable(Titanic$classe,Titanic$surv)

### Table du chi2   ####

# Y a t-il independance significative entre la survie et les variable

# Enfant
tab1 <- table(Titanic$enfant, Titanic$surv)
chisq.test(tab1)

# Classe
tab2 <- table(Titanic$classe, Titanic$surv)
chisq.test(tab2)

# Sexe
chisq.test(Titanic$sexe, Titanic$surv)


# Question 2        ####

# Modeliser la probabilité de deceder, en fonction du sexe, de la classe et de l age. 
# On prendra en modalites de reference : enfant - homme - equipage


# Question 3        ####

# Calculer la probabilite qu un enfant de sexe masculin de seconde classe ait survecu au naufrage
# Comparer le résultat a celui de la question 1.




# Question 4        ####


# Entrer une interaction entre l age et la classe, et commenter les resultats




