
library(tidyverse)


# Package � t�l�charger
# install.package("TraMineR)
library(TraMineR)

# Donn�es d'exemple fournie par le package
data(mvad)
# ou :
data(actcal)

# Liste des �l�ments de l'alphabet
names(mvad)
# Colonne 17 � 86 = les variables de s�quences 
# Une variable par mois entre juillet 1993 et juin 1999

seqstatl(mvad[, 17:86])
# 6 �tats possibles :
# Emploi
# Further Education
# Higher Education
# Sans emploi
# Etude
# Apprentissage


### D�finition des donn�es de s�quence

# Alphabet : �tats possible
mvad.alphabet=c("employment", "FE", "HE", "joblessness", "school","training")
# Labels long
mvad.labels=c("employment", "further education", "higher education","joblessness", "school", "training")
# Label court (pour graph)
mvad.scodes=c("EM", "FE", "HE", "JL", "SC", "TR")

# D�finition de la s�quence :
?seqdef
mvad.seq=seqdef(mvad, 17:86, 
                alphabet = mvad.alphabet, 
                states = mvad.scodes,
                labels = mvad.labels, 
                xtstep = 6)

# Je peux v�rifier l'alphabet
alphabet(mvad.seq)
summary(mvad.seq)
# Un r�sum�, on voit l'info colors : ici g�n�r� automatiquement mais j'imagine
# qu'on peut les changer � la main


#### Indicateur individuel

## longueur de la sequence 
seqlength(mvad.seq)


## number of transitions
mvad.trans <- seqtransn(mvad.seq)
#les 10 premiers
mvad.trans[1:10]




## Dur�e pass�e dans chaque �tat
seqistatd(mvad.seq)[1:10,]



#### Indicateur globaux

## Sequence modale
# renvoi les 10 trajectoires plus fr�quentes
seqtab(mvad.seq)
# Visualisation des 10 s�quences les plus fr�quentes
seqfplot(mvad.seq)
?seqtab



## S�quence des �tats modaux 
# renvoi la s�quence faites des �tats modaux � chaque temps.
seqmodst(mvad.seq)
?seqmodst
seqmsplot(mvad.seq)



## Dur�e MOYENNE pass�e dans chaque �tat
apply(seqistatd(mvad.seq),2,mean)
seqmtplot(mvad.seq)
# Possible de voir par groupe :
# exemple male
seqmtplot(mvad.seq, group = mvad$male)
# Interpr�ter



## Taux de transition entre �tats
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

# Visualisation de l'ensemble des s�quences, class�es par �tat de d�part 
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




