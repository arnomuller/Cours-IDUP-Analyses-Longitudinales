#######################################
######   ANALYSE LONGITUDINALE   ######
######          Seance 2         ######
#######################################


#######################
### PACKAGE            ####
library(haven)   # Lecture de fichiers SAS
library(gmodels) # Fonction de tableaux croises analogue a SAS
library(GGally)  # Graphique odd ratio
library(tidyverse)
#######################


########################
### IMPORTER DONNEE     ####

Titanic <- read.table("Chemin/titanic.txt",
                      sep = ";", 
                      header = TRUE)

class(Titanic) # information sur le type d'objet
str(Titanic) # information sur les variables qui compose la base

########################


########################
### MANIPULER VARIABLE  ####


## TABLES    ####

# Survivants ####
#?table()
table(Titanic$survived) # Sans NA
table(Titanic$survived, useNA = "always") # Avec NA
tab <- table(Titanic$survived, useNA = "always") #tab = notre table des survivants
tab # la meme

#Incrustation de tab dans la fonction addmargins() qui permet de montrer les totaux
addmargins(tab)
addmargins(table(Titanic$survived, useNA = "always")) # Meme chose

# Sexe       ####

addmargins(table(Titanic$gender, useNA = "always"))

# Class      ####

addmargins(table(Titanic$class, useNA = "always"))

# Age        ####
addmargins(table(Titanic$age, useNA = "always"))
hist(Titanic$age)

########################
## FILTER    ####

# On ne garde que les personne de moins d'un an

library(tidyverse)
age_chelou <- Titanic %>%
  filter(age < 1)



########################
## TITANIC2  ####

Titanic2 <- Titanic %>% #Titanic2 est base sur des transformations de Titanic
  filter(age >= 1) %>%  
  # Nous ne voulons que les observations ou l'age est superieur ou egal a 1
  # Les NAs sont supprimes dans le processus (vu qu'elles ne sont pas sup a 1)
  mutate(enfant = ifelse(age < 18, 1, 0), 
         #if age < 18, alors enfant = 1 , sinon pour les autres enfants = 0
         enfant = as.factor(enfant),
         # On transforme la variable numeric enfant en variable categorielle
         sexe = ifelse(gender == "male","Homme","Femme"), # traduction
         classe = as.factor(ifelse(class == "1st",1,
                                   ifelse(class == "2nd",2,
                                          ifelse(class == "3rd",3, 0 )))),
         surv = as.factor(ifelse(survived == "yes", 1, 0)))

# Pour classe : si class = 1st, alors classe = 1, mais si class = 2nd, alors 
# classe = 2, mais si class = 3rd alors classe = 3, mais si class ne rentre 
# dans aucune des categories, alors toutes les autres observations prennent
# la valeur 0 : ce sont les membres de l'equipage.
# et on transforme directement cette variable en une variable categorielle, 
# grace a as.factor()


addmargins(table(Titanic2$enfant, useNA = "always"))
addmargins(table(Titanic2$sexe, useNA = "always"))
addmargins(table(Titanic2$classe, useNA = "always"))
addmargins(table(Titanic2$surv, useNA = "always"))

########################


########################
## SELECT()  ####
# Une sauvegarde ####


Testanic <- Titanic2

# J'evite de toucher à Titanic2 pour ne pas avoir à relancer tout le code à cause d'une erreur


# Fonctionnement ####

# Je souhaite garder uniquement les variables qui nous interesse
# on utilise la fonction select()
# elle permet egalement de reagencer les colonnes de ma base de donnees


Testanic <- Testanic %>% 
  select(age, sexe, courgette, enfant)

table(Testanic$surv)


# Reagencement   ####

# Mais la l'ordre me plait pas trop, je veux que enfant soit a cote de age
# sauf que j'ai la flemme de reecrire toutes les variables pour les reagencer
# la fonction everything() est la solution


Testanic <- Testanic %>% 
  select(age, enfant, everything())
# ne supprime aucune variable, mais tiens compte de l'ordre de celles notifiees


# Selection inversee ####

# je peux aussi vouloir selectionner toutes les variables sauf une (ici je veux pas l age)
Testanic <- Testanic %>% 
  select(-age)


# Veritable selection ####

Titanic <- Titanic2 %>% 
  select(surv,age,enfant,sexe,classe)

str(Titanic)
# toujours preferer les variables facot a character pour les analyses
Titanic$sexe <- as.factor(Titanic$sexe)



# Enfant pas tres clair finalement : 

Titanic$enfant2 = factor(Titanic$enfant,labels=c("Adulte","Enfant"))
# je change directement le noms des modalites dans l'ordre de leur apparition

table(Titanic$enfant)
table(Titanic2$enfant)


########################


########################
## TABLEAUX CROISES ####





#### Question 1 ####

# A partir d un tableau croise :  

#	Quelle est la probabilite d avoir survecu, pour un enfant ? pour un adulte ?  
# Quel est le rapport de risque de survivre pour un enfant par rapport a un adulte ?  
#	Quel est l odds ratio d un enfant vs un adulte ?  

# Methode 1

table(Titanic$enfant,Titanic$surv)

prop.table(table(Titanic$enfant,Titanic$surv),2) # Pourcentage colonne
prop.table(table(Titanic$enfant,Titanic$surv),1) # Pourcentage ligne


# Methode 2
#library(gmodels)
CrossTable(Titanic$enfant,Titanic$surv)


# 	Quelle est la probabilitÃ© d avoir survecu, pour un enfant ? pour un adulte ? ####

# Parmis les enfants 78 ont survecu sur un total de 111+78
78/(111+78)
# 0.4126984
# c'est egal au % ligne des enfants survivants

# Du coup pour les adultes = 0.31


#	Quel est le rapport de risque de survivre pour un enfant par rapport a un adulte ?  ####

# RR <- risque de survivre chez les enfant / risque de survivre chez les adultes
0.413 / 0.311
# 1,32 fois plus de chance de survivre chez les enfants que chez les adultes


#	Quel est l odds ratio d un enfant vs un adulte ?  ####

# OR <- (risque de survivre chez les enfant / risque de mourir enfant) / 
# (risque de survivre chez les adultes / risque de survivre adultes)

(0.413/0.587) / (0.311/0.689)
(0.413/(1-0.413)) / (0.311/(1-0.311))

# 1,55 fois plus de chance de survivre plutot que de mourir chez les enfants par rapport aux adultes


########################

#### Question 1 bis ####

# Refaire pour classe

table(Titanic$classe,Titanic$surv)
prop.table(table(Titanic$classe,Titanic$surv),2) # Pourcentage colonne
prop.table(table(Titanic$classe,Titanic$surv),1) # Pourcentage ligne
CrossTable(Titanic$classe,Titanic$surv)


#	Quelle est la probabilite d avoir survecu ####

# pour 1er classe

0.619

# pour 2eme classe

0.407

# pour 3eme classe

0.252

# pour Ã©quipage

0.237

# RR equipage par rapport a 1ere classe  ####

0.619 / 0.237
# 2.611814 fois plus de chance de survivre 


# OR equipage par rapport a 1ere classe ####

(0.237/(1-0.237)) / (0.619/(1-0.619))

# 0.1911869 fois PLUS de chance de survivre plutot que de mourir chez les equipage par rapport aux 1ere classe

1/0.1911

# 5.232862 fois MOINS de chance de survivre


########################

#### Table du chi2 ####
# Y a t-il independance entre la survie et les variables
# Significatives ?

tab1 <- table(Titanic$enfant, Titanic$surv)
chisq.test(tab1)

tab2 <- table(Titanic$classe, Titanic$surv)
chisq.test(tab2)

tab3 <- table(Titanic$sexe, Titanic$surv)
chisq.test(tab3)

########################

#### Question 2 ####

# Modeliser la probabilie de deceder, en fonction du sexe, de la classe et de l age. 
# On prendra en modalites de reference : enfant - homme - equipage

Titanic$enfant <- relevel(Titanic$enfant, "1")
Titanic$enfant2 <- relevel(Titanic$enfant2, "Enfant")
Titanic$sexe <- relevel(Titanic$sexe, "Homme")

reg = glm(surv ~ enfant2 + sexe + classe, data=Titanic, family=binomial(link=logit))
summary(reg)

exp(reg$coefficients)


# Element supp ####

coef(reg)
confint(reg)
exp(coef(reg))
drop1(reg,test="Chisq")
?drop1

# Graphique
ggcoef(reg)
ggcoef(reg,exponentiate=T)


#### Question 3 ####

# Calculer la probabilite qu un enfant de sexe masculin de seconde classe ait survecu au naufrage
# Comparer le resultat a celui de la question 1.


exp(-0.1996)/(1+exp(-0.1996))
# ou ?
exp(-0.1996+(-0.8959))/(1+exp(-0.1996+(-0.8959)))



#### Question 4 ####

# Entrer une interaction entre l age et la classe, et commenter les resultats


reg = glm(surv ~ enfant2 +sexe*classe, data=Titanic, family=binomial(link=logit))
summary(reg)
drop1(reg,test="Chisq")
drop1(reg,~enfant2+sexe+classe,test="Chisq")













