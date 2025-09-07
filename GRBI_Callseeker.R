setwd("c:\\Users\\alexe\\OneDrive\\PROG\\GRBI\\GRBI") #Set working directory

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
occu

act <- read.table(file = "Data_GRBI_depressage_ACT.txt", header = TRUE, fill =TRUE)
act

str(act)
str(occu)

#TRI ET SÉLECTION####
# On veut tout d'abord séparer les deux classes en AM et PM

act_AM <- dplyr::filter(act, Periode == "AM")
act_AM        

act_PM <- dplyr::filter(act, Periode == "PM")#Activité PM
act_PM

#Sélection des blocs


# SNR 

# Voir «seewave» package (Marchal et al., 2022)


# Attribution d'une des trois classes de SNR pour chaque période



#ANALYSE 1 - Fenêtre d'activité vocale####
#Nécessaire d'obtenir les VP pour l'activité vocale (Fichier ACT)

#Analyse préliminaire pour voir le patron journalier

H_duree_AM <- act_AM %>% 
  group_by(Heure_debut, duree_sec) %>% #Groupe l'heure de début (hh:mm) avec la durée de l'enregistrement (en s) 
  summarise(Heure_debut,duree_sec, start_time_cor2018)
H_duree_AM

H_duree_PM <- act_PM %>% #Même chose pour la période PM
  group_by(Heure_debut, duree_sec) %>% 
  summarise(Heure_debut, duree_sec, start_time_cor2018)
H_duree_PM

# Transformer temps(hh:mm) en secondes, additionner la colonne «start_time_cor2018)  

hms_AM <- lubridate::hms(H_duree_AM$Heure_debut) # Transformation de l'heure de début en HMS (Heure, minutes, secondes)
hms_PM <- lubridate::hms(H_duree_PM$Heure_debut)

sec_AM <- period_to_seconds(hms_AM) # Transformation du HMS en secondes
sec_PM <- period_to_seconds(hms_PM)
  
# Jointure à la table H_duree_AM

H_duree_AM <- cbind(H_duree_AM, Heure_deb_sec = sec_AM)
H_duree_PM <- cbind(H_duree_PM, Heure_deb_sec = sec_PM)

#Addition des deux colonnes sec_AM et start_time_cor2018 pour avoir le temps (en secondes)

debut_start_AM <- H_duree_AM[c(3,4)]  #Sélection des colonnes qui sont intéressantes pour faire rowSums
debut_start_PM <- H_duree_PM[c(3,4)]

as.numeric(debut_start_AM$start_time_cor2018, debut_start_PM$start_time_cor2018)
as.numeric(debut_start_AM$Heure_deb_sec, debut_start_PM$Heure_deb_sec)

class(c(debut_start_AM$Heure_deb_sec, debut_start_PM$Heure_deb_sec))
class(c(debut_start_AM$start_time_cor2018, debut_start_PM$start_time_cor2018))

sec_voc_AM <- rowSums(debut_start_AM) #Temps(secondes) des vocalises (Heure début + voc)
sec_voc_PM <- rowSums(debut_start_PM)

debut_start_AM <- cbind(debut_start_AM, sec_voc_AM) #Ajout de la colonne du temps des vocalises
debut_start_PM <- cbind(debut_start_PM, sec_voc_PM)

#On veut créer des "classes" de temps en fct. de l'heure du lever/coucher de soleil (qui correspondra au temps 0)
#Établir le temps du lever/coucher de soleil en secondes -> Rapporter sur 0
#Séquence vectorielle de -x à +x en fct. du soleil







#ANALYSE 2 - Métriques de performance####