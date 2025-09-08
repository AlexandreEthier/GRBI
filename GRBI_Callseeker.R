setwd("c:\\Users\\alexe\\OneDrive\\PROG\\GRBI\\GRBI") #Set working directory

# Chargement des librairies####

install.packages("lubridate")
install.packages("car")
install.packages("hms")

library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)

# Chargement du jeu de données####

#Fichier Excel (.xlsx) Activite_depressage a été transformé en fichier texte (.txt)
#Bien s'assurer de séparer le fichier «Occurence» et «Activité»

act <- read.table(file = "Data_GRBI_depressage_ACT.txt", header = TRUE, fill =TRUE) %>% #Jeu de données mère
   dplyr::select(-Latitude, -Longitude, -Visite, -end_time, -type, -filtre_20dB, -end_time_cor2018) %>% 
  rename(voc_start = "start_time_cor2018")  #Renommé start_time_cor2018
  
str(act)


#Jeu de données météo

meteo <- read.table(file = "Data_GRBI_depressage_METEO.txt", header = TRUE, fill = TRUE) %>% #Permet d'aller chercher les heures de lever et coucher de soleil
  dplyr::select(-Heure, -Temperature, -Vit_vent, -Hauteur_prec)
  

#TRI ET SÉLECTION####

#Exclusion des parcelles traitées
act <- act %>% 
  filter(Annee == "2018"|Annee == "2019"|Traitement == "Temoin")

# Séparer les deux classes en AM et PM

act_AM <- dplyr::filter(act, Periode == "AM")

sun_AM <- meteo %>% #Date + Lever de soleil
  select(-Coucher_soleil)


act_PM <- dplyr::filter(act, Periode == "PM")

sun_PM <- meteo %>% #Date + Coucher de soleil
  select(-Lever_soleil)

# SNR 

# Voir «seewave» package (Marchal et al., 2022)


# Attribution d'une des trois classes de SNR pour chaque période



#ANALYSE 1 - Fenêtre d'activité vocale####
#Nécessaire d'obtenir les VP pour l'activité vocale (Fichier ACT)

#Analyse préliminaire pour voir le patron journalier - (c) Olivier Renaud

hms_debut <- lubridate::hms(act_AM$Heure_debut) #Transformation de l'heure du début en période HMS
debut_sec <- period_to_seconds(hms_debut)

sun_hm <- lubridate::hm(sun_AM$Lever_soleil)
sun_AM_sec <- period_to_seconds(sun_hm)

sun_AM <- sun_AM %>% #Rajoute une colonne de l'heure du lever de soleil en secondes
  cbind(sun_AM_sec)


act_AM <- act_AM %>% 
  cbind(debut_sec) %>% #Ajout du début de l'enregistrement, en secondes
  mutate(Heure_voc = debut_sec + voc_start) %>%  #Heure_voc correspond à l'heure d'une vocalise, en secondes
  left_join(sun_AM, by = "Date") #Jointure attributaire de l'heure du lever de soleil en fonction de la date









