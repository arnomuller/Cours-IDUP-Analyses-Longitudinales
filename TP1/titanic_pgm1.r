library(haven)   # Lecture de fichiers SAS
library(gmodels) # Fonction de tableaux croisés analogue à SAS
library(GGally)  # Graphique odd ratio


# Répertoire de stockage
Rep = "C:/Users/bringe/Desktop/Projets EXTERIEUR/IDUP/FICHIERS/Titanic/"


# Lecture Fichier
Titanic = read_sas(paste0(Rep,"titanic.sas7bdat"))

# Mise sous forme FACTEUR
Titanic$sexe    = factor(Titanic$sexe,labels=c("Homme","Femme"))
Titanic$surv    = factor(Titanic$surv,labels=c("Décédé","Vivant"))
Titanic$age     = factor(Titanic$age,labels=c("Enfant","Adulte"))
Titanic$classe  = factor(Titanic$classe,labels=c("Equipage","1e classe","2eme classe","3eme classe"))


# Question 1 

# Méthode 1
table(Titanic$age,Titanic$surv)
prop.table(table(Titanic$age,Titanic$surv),1)
prop.table(table(Titanic$age,Titanic$surv),2)

# Méthode 2
CrossTable(Titanic$age,Titanic$surv)



# QUESTION 2 
glm(surv ~ age + sexe + classe, data=Titanic, family=binomial(link=logit))

reg = glm(surv ~ age + sexe + classe, data=Titanic, family=binomial(link=logit))
summary(reg)
Z=summary(reg)

# Changement de reférence
reg2 = glm(surv ~ age + relevel(sexe,"Femme") + classe, data=Titanic, family=binomial(link=logit))
summary(reg2)


# Eléments supplémentaires
coef(reg)
confint(reg)
exp(coef(reg))
drop1(reg,test="Chisq")

# Graphique
ggcoef(reg)
ggcoef(reg,exponentiate=T)


# QUESTION 4
reg = glm(surv ~ age +sexe*classe, data=Titanic, family=binomial(link=logit))
drop1(reg,test="Chisq")
drop1(reg,~age+sexe+classe,test="Chisq")

      