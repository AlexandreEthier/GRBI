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
   select(-Latitude, -Longitude, -Visite,-start_time, -end_time, -type, -filtre_20dB, -end_time_cor2018) %>% #Drop les colonnes non-poertinentes
   rename(voc_start = "start_time_cor2018")  #Renommé start_time_cor2018
  

#Jeu de données météo

meteo <- read.table(file = "Data_GRBI_depressage_METEO.txt", header = TRUE, fill = TRUE) %>% #Heures de lever et coucher de soleil
  select(-Heure, -Temperature, -Vit_vent, -Haut_prec) #Drop des colonnes non-pertinentes
  

#TRI ET SÉLECTION####

#Exclusion des parcelles traitées
act <- act %>% 
  filter(Annee == "2018"|Annee == "2019"|Traitement == "Temoin")

# Séparer les deux classes en AM et PM

act_AM <- act %>% 
  filter(Periode == "AM", duree_sec == 3600) #Filtre seulement les enregistrements qui ont la même durée d'analysé (3600s ou 1h)

sun_AM <- meteo %>% #Date + Lever de soleil
  select(-Coucher_soleil)

act_PM <- act %>% 
  filter(Periode == "PM", duree_sec == 3600)

sun_PM <- meteo %>% #Date + Coucher de soleil
  select(-Lever_soleil)

#ANALYSE 1 - Fenêtre d'activité vocale####

#Nécessaire d'obtenir les VP pour l'activité vocale (Fichier ACT)

#Analyse préliminaire pour voir le patron journalier - (crédit partiel) Olivier Renaud

#MATIN

hms_debut_AM <- lubridate::hms(act_AM$Heure_debut) #Transformation de l'heure du début en période HH: mm: ss
s_debut_AM <- period_to_seconds(hms_debut_AM)

hms_sun_AM <- lubridate::hm(sun_AM$Lever_soleil) #Transformation similaire pour l'heure du lever de soleil
s_sun_AM <- period_to_seconds(hms_sun_AM)

sun_AM <- sun_AM %>% #Rajoute une colonne de l'heure du lever de soleil en secondes
  cbind(s_sun_AM)

act_AM <- act_AM %>% 
  cbind(s_debut_AM) %>% #Ajout du début de l'enregistrement, en secondes
  mutate(Heure_voc = s_debut_AM + voc_start) %>%  #Heure_voc correspond à l'heure d'une vocalise, en secondes
  left_join(sun_AM, by = "Date") #Jointure attributaire de l'heure du lever de soleil en fonction de la date

voc_sun_AM <- round((act_AM$Heure_voc - act_AM$s_sun_AM)/60, digits = 0) # Heure de la vocalise (en secondes) - Lever de soleil (en secondes), ramené en minutes
voc_sun_AM
  
act_AM <- act_AM %>% 
  cbind(voc_sun_AM)

tab_voc_AM <- table(act_AM$voc_sun_AM)
print(tab_voc_AM)

n_voc_AM <- as.data.frame(tab_voc_AM) %>%  #Me permet de trouver la fréquence de vocalisations par minutes p/r au lever/coucher de soleil
  rename(sun_relatif = "Var1")
n_voc_AM

plot(Freq ~ sun_relatif, data = n_voc_AM) #Graphique préliminaire pour observer les tendances
                                #Pic de vocalises [-50;-40].

#Il faut maintenant regrouper les observations par 5 min.

str(n_voc_AM) #On doit transformer Var1 en numeric
n_voc_AM$relatif_par5 <- as.numeric(levels(n_voc_AM$sun_relatif))[n_voc_AM$sun_relatif]

n_voc_AM #Intervalle de n_voc: [-72; 39] -> Nous allons déterminer l'intervalle comme suit: [-75; +40]

breaks_AM <- seq(from = -75, to = 40, by = 5)

n_voc_AM_par5 <- cut(n_voc_AM$relatif_par5, breaks = breaks_AM, right = FALSE) #Création des intervalles
n_voc_AM_par5

n_voc_AM <- n_voc_AM %>% #Ajout des intervalles sur le dataframe n_voc
  cbind(n_voc_AM_par5)
n_voc_AM

n_voc_AM$freq_cum_par5 <- ave(n_voc_AM$Freq, n_voc_AM$n_voc_AM_par5, FUN = sum) # Calcul de la somme des vocalises dans mon intervalle de 5 minutes
                                                                                #La fonction "ave" me permet de réaliser une fonction sur des valeurs en fonction d'une catégorie d'un même dataframe

freq_par5_AM <- as.data.frame(distinct(n_voc_AM, n_voc_AM_par5, freq_cum_par5)) #Sélection des valeurs uniques. La somme de la fréquence des intervalles a été généré à plusieurs reprises; il s'agit de choisir les valeurs uniques

freq_cum_par5 <- freq_par5_AM$freq_cum_par5

class(abs)
as.factor(abs)


plot(freq_par5_AM$n_voc_AM_par5, freq_par5_AM$freq_cum_par5,  #Graphique préliminaire de la fréquence de vocalises par intervalle de 5 minutes précédant et suivant le lever de soleil
     type = "b",
     main = "Fréquence cumulée de vocalises de GRBI par rapport au lever de soleil", 
     xlab = "Temps par rapport au lever de soleil (par 5 minutes)", 
     ylab = "Fréquence cumulée",
     par(cex.axis = 0.8, cex.lab = 1)) 


# SOIR
#Voir commentaires de la section «MATIN»

hms_debut_PM <- lubridate::hms(act_PM$Heure_debut)
s_debut_PM <- period_to_seconds(hms_debut_PM)

hms_sun_PM <- lubridate::hm(sun_PM$Coucher_soleil)
s_sun_PM <- period_to_seconds(hms_sun_PM)

sun_PM <- sun_PM %>% 
  cbind(s_sun_PM)

act_PM <- act_PM %>% 
  cbind(s_debut_PM) %>% 
  mutate(Heure_voc = s_debut_PM + voc_start) %>% #"Many-to-many relationship èa prendre en compte
  left_join(sun_PM, by = "Date")
  
voc_sun_PM <- round((act_PM$Heure_voc - act_PM$s_sun_PM)/60, digits = 0)
voc_sun_PM

act_PM <- act_PM %>% 
  cbind(voc_sun_PM)

tab_voc_PM <- table(act_PM$voc_sun_PM)
print(tab_voc_PM)

n_voc_PM <- as.data.frame(tab_voc_PM) %>% 
  rename(sun_relatif = "Var1")

plot(Freq ~ sun_relatif, data = n_voc_PM)

n_voc_PM$relatif_par5 <- as.numeric(levels(n_voc_PM$sun_relatif))[n_voc_PM$sun_relatif]

min(n_voc_PM$relatif_par5) #Intervalle de [-31; 44] -> [-35; 45]
max(n_voc_PM$relatif_par5)

breaks_PM <- seq(from = -35, to = 45, by = 5)

n_voc_PM_par5 <- cut(n_voc_PM$relatif_par5, breaks = breaks_PM, right = FALSE)

n_voc_PM <- n_voc_PM %>% 
  cbind(n_voc_PM_par5)

n_voc_PM$freq_cum_par5 <- ave(n_voc_PM$Freq, n_voc_PM$n_voc_PM_par5, FUN = sum)

freq_par5_PM <- as.data.frame(distinct(n_voc_PM, n_voc_PM_par5, freq_cum_par5))

plot(freq_cum_par5 ~ n_voc_PM_par5, data = freq_par5_PM, 
     main = "Fréquence cumulée de vocalises de GRBI par rapport au coucher de soleil", 
     xlab = "Temps par rapport au coucher de soleil (par 5 minutes)", 
     ylab = "Fréquence cumulée",
     par(cex.axis = 0.8, cex.lab = 1))



# PROCHAINE SÉANCE:

#1 Mise en page des graphiques (pour le 1er octobre)
#2 Calcul SNR
#3 Fenêtre d'activité vocale






