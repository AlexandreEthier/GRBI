setwd("c:\\Users\\alexe\\OneDrive\\PROG\\GRBI\\GRBI")

# Chargement des librairies####

install.packages("lubridate")

library(lubridate)
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

act_AM <- dplyr::filter(act, Periode == "AM")
act_AM        



act_PM <- dplyr::filter(act, Periode == "PM")#Activité PM
act_PM


# SNR 




# Attribution d'une des trois classes de SNR pour chaque période



#ANALYSE 1 - Fenêtre d'activité vocale####
#Nécessaire d'obtenir les VP pour l'activité vocale (Fichier OCCU)

#Analyse préliminaire pour voir le patron journalier

H_duree_AM <- act_AM %>% 
  group_by(Heure_debut, duree) %>% #Groupe l'heure de début (hh:mm) avec la durée de l'enregistrement (en s) 
  summarise(Heure_debut,duree, start_time_cor2018)
H_duree_AM

# Transformer temps(hh:mm) en secondes, additionner la colonne «start_time_cor2018)  

hms_AM <- lubridate::hms(H_duree_AM$Heure_debut) # Transformation de l'heure de début en HMS (Heure, minutes, secondes)
hms_AM

sec_AM <- period_to_seconds(hms_AM) # Transformation du HMS en secondes
sec_AM

min(sec_AM) #12660
max(sec_AM) #18000
  
# Jointure à la table H_duree_AM

H_duree_AM <- cbind(H_duree_AM, Heure_deb_sec = sec_AM)

#Addition des deux colonnes sec_AM et start_time_cor2018 pour avoir le temps (en secondes)

debut_start <- H_duree_AM[c(3,4)] #Sélection des colonnes qui sont intéressantes pour faire rowSums

as.numeric(debut_start$start_time_cor2018)
as.numeric(debut_start$Heure_deb_sec)

class(debut_start$Heure_deb_sec)
class(debut_start$start_time_cor2018)

sec_voc_AM <- rowSums(debut_start) #Temps(secondes) des vocalises


#On veut créer des "classes" de temps en fct. de l'heure du lever/coucher de soleil (qui correspondra au temps 0)

sun_vec <-  #Avec le min et max des secondes, positionner un axe pour lequel le lever (AM) et coucher (PM) correspond à 0
  
  







#ANALYSE 2 - Métriques de performance####































