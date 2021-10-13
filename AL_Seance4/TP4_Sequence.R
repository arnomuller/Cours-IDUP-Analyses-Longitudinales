
library(tidyverse)


# Package à télécharger
# install.package("TraMineR)
library(TraMineR)

# Données d'exemple fournie par le package
data(mvad)
# ou :
data(actcal)

# Liste des éléments de l'alphabet
names(mvad)
# Colonne 17 à 86 = les variables de séquences 
# Une variable par mois entre juillet 1993 et juin 1999

seqstatl(mvad[, 17:86])
# 6 états possibles :
# Emploi
# Further Education
# Higher Education
# Sans emploi
# Etude
# Apprentissage


### Définition des données de séquence

# Alphabet : états possible
mvad.alphabet=c("employment", "FE", "HE", "joblessness", "school","training")
# Labels long
mvad.labels=c("employment", "further education", "higher education","joblessness", "school", "training")
# Label court (pour graph)
mvad.scodes=c("EM", "FE", "HE", "JL", "SC", "TR")

# Définition de la séquence :
?seqdef
mvad.seq=seqdef(mvad, 17:86, 
                alphabet = mvad.alphabet, 
                states = mvad.scodes,
                labels = mvad.labels, 
                xtstep = 6)

# Je peux vérifier l'alphabet
alphabet(mvad.seq)
summary(mvad.seq)
# Un résumé, on voit l'info colors : ici généré automatiquement mais j'imagine
# qu'on peut les changer à la main


#### Indicateur individuel

## longueur de la sequence 
seqlength(mvad.seq)


## number of transitions
mvad.trans <- seqtransn(mvad.seq)
#les 10 premiers
mvad.trans[1:10]




## Durée passée dans chaque état
seqistatd(mvad.seq)[1:10,]



#### Indicateur globaux

## Sequence modale
# renvoi les 10 trajectoires plus fréquentes
seqtab(mvad.seq)
# Visualisation des 10 séquences les plus fréquentes
seqfplot(mvad.seq)
?seqtab



## Séquence des états modaux 
# renvoi la séquence faites des états modaux à chaque temps.
seqmodst(mvad.seq)
?seqmodst
seqmsplot(mvad.seq)



## Durée MOYENNE passée dans chaque état
apply(seqistatd(mvad.seq),2,mean)
seqmtplot(mvad.seq)
# Possible de voir par groupe :
# exemple male
seqmtplot(mvad.seq, group = mvad$male)
# Interpréter



## Taux de transition entre états
transition <- seqtrate(mvad.seq)
round(transition,2)
?seqtrate


### Entropie transversale
seqHtplot(mvad.seq)


## Entropie longitudinale
seqient(mvad.seq)
hist(seqient(mvad.seq))



#### TAPIS

# Visualisation de la trajectoire des 10 premiers individus
seqiplot(mvad.seq,
         border = NA,
         use.layout = TRUE,
         with.legend = TRUE,
         legend.prop = 0.15,
         cex.axis = 0.6)
?seqiplot

# Visualisation de l'ensemble des séquences, classées par état de départ 
seqIplot(mvad.seq, 
         sortv = "from.start",
         with.legend = TRUE,
         use.layout = TRUE,
         legend.prop = 0.15)
# from.end pour trier par la fin.

seqIplot(mvad.seq, 
         sortv = "from.end",
         with.legend = TRUE,
         use.layout = TRUE,
         legend.prop = 0.15)


# Par groupe : 
seqIplot(mvad.seq, 
         group = mvad$male,
         sortv = "from.start",
         with.legend = TRUE,
         use.layout = TRUE,
         legend.prop = 0.15)
?seqIplot




#### LE CHRONOGRAMME

seqdplot(mvad.seq,
         with.legend = TRUE,
         use.layout = TRUE,
         legend.prop = 0.15)
?seqdplot

# groupe selon sexe
seqdplot(mvad.seq, 
         group = mvad$male,
         with.legend = TRUE,
         use.layout = TRUE,
         legend.prop = 0.15)




