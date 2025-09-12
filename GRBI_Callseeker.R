setwd("c:\\Users\\alexe\\OneDrive\\PROG\\GRBI\\GRBI") #Set working directory


# Chargement des librairies####

install.packages("lubridate")
install.packages("car")
install.packages("hms")
install.packages("data.table")

library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)

# Chargement du jeu de données####

#Fichier Excel (.xlsx) Activite_depressage a été transformé en fichier texte (.txt)
#Bien s'assurer de séparer le fichier «Occurence» et «Activité»

act <- read.table(file = "Data_GRBI_depressage_ACT.txt", header = TRUE, fill =TRUE) %>% #Jeu de données mère
   dplyr::select(-Latitude, -Longitude, -Visite,-start_time, -end_time, -type, -filtre_20dB, -end_time_cor2018) %>% 
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

act_AM <- act %>% 
  dplyr::filter(Periode == "AM", duree_sec == 3600) #Filtre seulement les enregistrements qui ont la même durée d'analysé (3600s ou 1h)

sun_AM <- meteo %>% #Date + Lever de soleil
  select(-Coucher_soleil)

act_PM <- act %>% 
  dplyr::filter(Periode == "PM", duree_sec == 3600)

sun_PM <- meteo %>% #Date + Coucher de soleil
  select(-Lever_soleil)

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

voc_sun_AM <- round((act_AM$Heure_voc - act_AM$sun_AM_sec)/60, digits = 0) # Heure de la vocalise (en secondes) - Lever de soleil (en secondes), ramené en minutes
voc_sun_AM
  
act_AM <- act_AM %>% 
  cbind(voc_sun_AM)

tab_voc <- table(act_AM$voc_sun_AM)
print(tab_voc)

n_voc <- as.data.frame(tab_voc) #Me permet de trouver la fréquence de vocalisations par minutes
n_voc

plot(Freq ~ Var1, data = n_voc) #Graphique préliminaire pour observer les tendances
                                #Pic de vocalises [-50;-40].

#Il faut maintenant regrouper les observations par 5 min.

str(n_voc) #On doit transformer Var1 en numeric
n_voc$temps <- as.numeric(levels(n_voc$Var1))[n_voc$Var1]

n_voc #Intervalle de n_voc: [-72; 39] -> Nous allons déterminer l'intervalle comme suit: [-75; +40]

breaks <- seq(from = -75, to = 40, by = 5)

n_voc_par5 <- cut(n_voc$temps, breaks = breaks, right = FALSE) #Création des intervalles
n_voc_par5

n_voc <- n_voc %>% #Ajout des intervalles sur le dataframe n_voc
  cbind(n_voc_par5)
n_voc

n_voc$freq_cum_par5 <- ave(n_voc$Freq, n_voc$n_voc_par5, FUN = sum) # Calcul de la somme des vocalises dans mon intervalle de 5 minutes
                                                                    #La fonction "ave" me permet de réaliser une fonction sur des valeurs en fonction d'une catégorie d'un même dataframe

tab_freq_par_5 <- as.data.frame(distinct(n_voc, n_voc_par5, freq_cum_par5)) #Sélection des valeurs uniques. La somme de la fréquence des intervalles a été généré à plusieurs reprises; il s'agit de choisir les valeurs uniques
tab_freq_par_5

plot(freq_cum_par5 ~ n_voc_par5, data = tab_freq_par_5) #Graphique préliminaire de la fréquence de vocalises par intervalle de 5 minutes précédant et suivant le lever de soleil

# PROCHAINE SÉANCE:

#1 - Renommer les variables + cleanup de l'environnement
#2 - Dédoubler les lignes de codes ici pour représenter PM
#3 Mise en page des graphiques (pour le 1er octobre)






