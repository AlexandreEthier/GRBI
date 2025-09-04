setwd("c:\\Users\\alexe\\OneDrive\\PROG\\GRBI\\GRBI")

# Chargement des librairies####

library(dplyr)
library(tidyr)
library(ggplot2)

# Chargement du jeu de données####

#Fichier Excel (.xlsx) Activite_depressage a été transformé en fichier texte (.txt)
#Bien s'assurer de séparer le fichier «Occurence» et «Activité»

occu <- read.table(file = "Data_GRBI_depressage_OCCU.txt", header = TRUE, fill = TRUE)

colnames(occu) #Renommer la colonne «Duree (sec)» avec un titre sans espace
occu <- occu %>% 
  rename(duree_sec = "duree")

occu <- occu %>% 
  dplyr::select(-X.sec.)#Permet d'enlever la colonne X.sec., créée par l'espace dans le header.
                        #Induisait un problème dans l'alignement des colonnes
occu

act <- read.table(file = "Data_GRBI_depressage_ACTIVITE.txt", header = TRUE, fill =TRUE)
act

str(act)
str(occu)



#TRI ET SÉLECTION####
# On veut tout d'abord séparer les deux classes en AM et PM



# SNR 




# Attribution d'une des trois classes de SNR pour chaque période



#ANALYSE 1 - Fenêtre d'activité vocale####
#Nécessaire d'obtenir les VP pour l'activité vocale (Fichier OCCU)

#Analyse préliminaire pour voir le patron journalier

sun_class <- seq(from = 1, to = 4000)








#ANALYSE 2 - Métriques de performance####































