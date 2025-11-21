setwd("c:\\Users\\alexe\\OneDrive\\PROG\\GRBI\\GRBI\\Metadonnee") #Set working directory

# Chargement des librairies####

library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(bioacoustics)
library(tuneR)
library(seewave)
library(DescTools)

# Chargement du jeu de données####

#Fichier Excel (.xlsx) Activite_depressage a été transformé en fichier texte (.txt)
#Bien s'assurer de séparer le fichier «Occurence» et «Activité»

act <- read.table(file = "DATA_depressage_ACT.txt", header = TRUE, fill =TRUE) %>% #Jeu de données mère
  select(-Latitude, -Longitude, -Visite,-start_time, -end_time, -type, -filtre_20dB, -end_time_cor2018) %>% #Drop les colonnes non-poertinentes
  rename(voc_start = "start_time_cor2018")  #Renommé voc_start

#Jeu de données météo

meteo <- read.table(file = "DATA_depressage_meteo.txt", header = TRUE, fill = TRUE) #Heures de lever et coucher de soleil

#TRI ET SÉLECTION####

#Exclusion des parcelles traitées
act <- distinct(act) %>% 
  filter(Annee == "2018"|Annee == "2019"|Traitement != "EPC_SYS")

act_PM <- distinct(act) %>% 
  filter(Periode == "PM")

sun_PM <- meteo %>% #Date + Coucher de soleil
  select(-Lever_soleil) %>% 
  distinct(Date, Coucher_soleil)


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
  mutate(Heure_voc = s_debut_PM + voc_start) %>% #"Many-to-many relationship à prendre en compte
  left_join(sun_PM, by = "Date", relationship = "many-to-many")

voc_sun_PM <- round((act_PM$Heure_voc - act_PM$s_sun_PM)/60, digits = 0) 

act_PM <- act_PM %>% 
  cbind(voc_sun_PM)

tab_voc_PM <- table(act_PM$voc_sun_PM)
print(tab_voc_PM)

n_voc_PM <- as.data.frame(tab_voc_PM) %>% 
  rename(sun_relatif = "Var1")

plot(Freq ~ sun_relatif, data = n_voc_PM)

n_voc_PM$relatif_par5 <- as.numeric(levels(n_voc_PM$sun_relatif))[n_voc_PM$sun_relatif]

range(n_voc_PM$relatif_par5) #Intervalle de [-31; 44] -> [-35; 45]

breaks_PM <- seq(from = -30, to = 55, by = 5)

n_voc_PM_par5 <- cut(n_voc_PM$relatif_par5, breaks = breaks_PM, right = FALSE)

n_voc_PM <- n_voc_PM %>% 
  cbind(n_voc_PM_par5)

n_voc_PM$freq_cum_par5 <- ave(n_voc_PM$Freq, n_voc_PM$n_voc_PM_par5, FUN = sum)

par5_corr_PM <- seq(from = -30, to = 50 , by = 5)

freq_par5_PM <- as.data.frame(distinct(n_voc_PM, n_voc_PM_par5, freq_cum_par5)) %>% 
  cbind(par5_corr_PM)


#Graphique vocalisations PM

graph_freqPM <- ggplot(data = freq_par5_PM, aes(x = par5_corr_PM, y = freq_cum_par5))+
  geom_point(size = 2)+
  geom_line(linetype = 1)+
  scale_x_continuous(breaks = par5_corr_PM)+
  scale_y_continuous(breaks = seq(from = 0, to = 900, by = 100))+
  geom_vline(xintercept = 0, colour = "orange", linewidth = 1.3 )+
  xlab("Temps relatif au coucher de soleil (5 min.)")+
  ylab("Fréquence de vocalisations")+
  ggtitle("Fréquence de vocalisations de la Grive de Bicknell en fonction du coucher de soleil")+
  theme_light()+
  theme(panel.grid.major.x  = element_blank(),
        panel.grid.minor.x = element_blank())
graph_freqPM

# FENÊTRE D'ACTIVITÉ VOCALE ####

seg <- read.table(file = "DATA_depressage_occu.txt", header = TRUE, fill = TRUE) %>% 
  select(-start_time, -end_time) %>%  # Drop des colonnes impertinentes
  filter(Analyse_CATBIC_CALL != "0") %>% # Sélection des données traitées ("1" indique que chaque call/song ont été vérifiés)
  filter(Analyse_CATBIC_SONG != "0") %>% 
  filter(Annee == "2018" | Annee == "2019"|Traitement != "EPC_SYS")
# C'est normal qu'on ait pas le même nb. d'enregistrement car ACT. dépend de l'occurrence (0 ou 1 selon la présence/absence de la grive)

seg_PM <- seg %>% 
  filter(Periode == "PM")

#  ------------------------ # On doit adapter le code après ça pour fit le soir












