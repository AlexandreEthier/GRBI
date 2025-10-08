setwd("c:\\Users\\alexe\\OneDrive\\PROG\\GRBI\\GRBI") #Set working directory

# Installation et chargement des bibliothèques

install.packages("openxlsx")

library(seewave)
library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)

# Importation de la base de données (untouched)

bd_occu <- read_excel("c:/Users/alexe/OneDrive/PROG/GRBI/GRBI/Data_GRBI_depressage_FM_CallSeekerV2.0-FINAL.xlsx", sheet = 1, col_names = TRUE)

bd_occu <- bd_occu %>% # Clean-up de la base de données
           select(-start_time, -end_time, -Obs, -Note) %>%  # Drop des colonnes impertinentes
           filter(Analyse_CATBIC_CALL != "0") %>% # Sélection des données traitées ("1" indique que chaque call/song ont été vérifiés)
           filter(Analyse_CATBIC_SONG != "0") %>% 
           rename(duree = "duree (sec)") # Renommé duree car R n'aime pas les espaces

bd_act <- read_excel("c:/Users/alexe/OneDrive/PROG/GRBI/GRBI/Data_GRBI_depressage_FM_CallSeekerV2.0-FINAL.xlsx", sheet = 2, col_names = TRUE)

bd_act <- bd_act %>% 
          select(-Occurrence_GRBI, -start_time, -end_time, -filtre_20dB) %>% 
          rename(duree = "duree (sec)", start_time = "start_time_cor2018", end_time = "end_time_cor2018")


# SÉLECTION####

# Durée (ACT a généralement été fait pour les 3600 sec.)

bd_occu <- bd_occu %>% 
           filter(duree == 3600) # Sélection des enregistrements qui ont une durée de 3600 secondes (1h)

bd_act <- bd_act %>% 
          filter(duree == 3600)

# Année

bd_occu <- bd_occu %>% 
           filter(Annee != "2023") # Suppression de l'année 2023 car données non traitées
                                   # À voir pour les autres années
bd_act <- bd_act %>% 
          filter(Annee != "2023")

# Sites

bd_occu <- bd_occu %>% 
           filter(Traitement != "EPC_SYS") # Sélection des traitements qui ne sont pas de l'EPC
                                           # À voir pour les bandes
bd_act <- bd_act %>% 
          filter(Traitement != "EPC_SYS")

# Période

bd_AM <- bd_occu %>% 
         filter(Periode == "AM") # Période du matin

bd_PM <- bd_occu %>% 
         filter(Periode == "PM") # Période du soir

# Occurence

occu <- bd_occu %>% 
        group_by(Periode, Occurrence_GRBI) %>%
        summarise(n()) # Donne l'occurence de GRBI dans le dataset en fonction de la période.

occu_mat <- matrix(occu$`n()`, nrow = 2, ncol = 2, dimnames = list(c("0", "1"),
                                                                  c("AM", "PM")))
occu_mat # Visualisation matricielle

# Regroupement des 22 enregistrements (AM et PM) pour voir si ils sont aux mêmes stations/dates

grbi_occu <- bd_occu %>% 
             filter(Occurrence_GRBI == 1)# NON... Stations 2-7-9 revoient souvent de la GRBI année aprèes année

grbi_occu_AM <- grbi_occu %>% 
                filter(Periode == "AM") # Enregistrement AM

grbi_occu_PM <- grbi_occu %>% 
                filter(Periode == "PM") # Enregistrement PM
  
# On veut aussi associer le nombre d'étiquettes validées avec chaque enregistrement (ACT)

bd_occu_act <- merge(grbi_occu, bd_act, by = c("Annee", "Station", "Latitude", "Longitude", "Traitement", "Date", "Heure_debut", "Periode", "Visite", "duree"))


# Extraction en fichier Excel 

write.xlsx(bd_occu_act, "bd_occu_act.xlsx")

#-------------------------------------------------------------------------------------------

# TRAITEMENT####
# Section qui porte sur les modifications à apporter sur les enregistrements dans le cadre du projet

# Fenêtre d'activité vocale
# On veut raccourcir les enregistrements bruts à la fenêtre d'activité vocale (Voir analyse 1)


# Segmentation des enregistrements
# En groupes de 5 min. pour faciliter l'analyse. Stocker dans un dossier (1,2,3...x)


# Attribution des positifs
# Extraction des étiquettes et des positifs des enregistrements pour les subdiviser dans les segments créés



























