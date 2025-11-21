setwd("c:\\Users\\alexe\\OneDrive\\PROG\\GRBI\\GRBI\\Metadonnee") #Set working directory

# Chargement des librairies####

install.packages("lubridate")
install.packages("car")
install.packages("hms")
install.packages("data.table")
install.packages("bioacoustics")
install.packages("tuneR")
install.packages("seewave")
install.packages("DescTools")

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

act_AM <- distinct(act) %>% 
  filter(Periode == "AM")

sun_AM <- meteo %>% #Date + Lever de soleil
  select(-Coucher_soleil) %>% 
  distinct(Date, Lever_soleil)

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

# DÉBUGGÉ - Il faut absolument faire attention aux doublons - Ne double pas le nb. de lignes lorsque left_join()
# DÉBUGGÉ - Insertion de 260 NA dans la case (coucher de soleil (à vérifier pour les levers de soleil))

act_AM <- act_AM %>% 
  cbind(s_debut_AM) %>% #Ajout du début de l'enregistrement, en secondes
  mutate(Heure_voc = s_debut_AM + voc_start) %>%  #Heure_voc correspond à l'heure d'une vocalise, en secondes
  left_join(sun_AM, by = "Date", relationship = "many-to-many") #Jointure attributaire de l'heure du lever de soleil en fonction de la date

voc_sun_AM <- round((act_AM$Heure_voc - act_AM$s_sun_AM)/60, digits = 0) # Heure de la vocalise (en secondes) - Lever de soleil (en secondes), ramené en minutes
  
act_AM <- act_AM %>% 
  cbind(voc_sun_AM)

tab_voc_AM <- table(act_AM$voc_sun_AM)

n_voc_AM <- as.data.frame(tab_voc_AM) %>%  #Me permet de trouver la fréquence de vocalisations par minutes p/r au lever/coucher de soleil
  rename(sun_relatif = "Var1")

#Il faut maintenant regrouper les observations par 5 min.

str(n_voc_AM) #On doit transformer sun_relatif en numeric
n_voc_AM$sun_relatif <- as.numeric(levels(n_voc_AM$sun_relatif))[n_voc_AM$sun_relatif]

range(n_voc_AM$sun_relatif) #Intervalle de n_voc: [-73; 45] -> Nous allons déterminer l'intervalle comme suit: [-75; +45]

breaks_AM <- seq(from = -75, to = 50, by = 5)

n_voc_AM_par5 <- cut(n_voc_AM$sun_relatif, breaks = breaks_AM, right = FALSE) #Création des intervalles

n_voc_AM <- n_voc_AM %>% #Ajout des intervalles sur le dataframe n_voc
  cbind(n_voc_AM_par5)

n_voc_AM$freq_cum_par5 <- ave(n_voc_AM$Freq, n_voc_AM$n_voc_AM_par5, FUN = sum) # Calcul de la somme des vocalises dans mon intervalle de 5 minutes
                                                                                #La fonction "ave" me permet de réaliser une fonction sur des valeurs en fonction d'une catégorie d'un même dataframe

par5_corr_AM <- seq(from = -75, to = 45, by = 5) #Correspond à la limite inférieure des intervalles de 5 minutes

freq_par5_AM <- as.data.frame(distinct(n_voc_AM, n_voc_AM_par5, freq_cum_par5)) %>% #Sélection des valeurs uniques. La somme de la fréquence des intervalles a été généré à plusieurs reprises; il s'agit de choisir les valeurs uniques
                cbind(par5_corr_AM)

#Graphique fréquence vocalisations AM

graph_freqAM <- ggplot(data = freq_par5_AM, aes(x = par5_corr_AM, y = freq_cum_par5))+
  geom_point(size = 2)+
  geom_line(linetype = 1)+
  scale_x_continuous(breaks = par5_corr_AM)+
  scale_y_continuous(breaks = seq(from = 0, to = 900, by = 100))+
  geom_vline(xintercept = 0, colour = "orange", linewidth = 1.3 )+
  xlab("Temps relatif au lever de soleil (5 min.)")+
  ylab("Fréquence de vocalisations")+
  ggtitle("Fréquence de vocalisations de la Grive de Bicknell en fonction du lever de soleil")+
  theme_light()+
  theme(panel.grid.major.x  = element_blank(),
        panel.grid.minor.x = element_blank())
graph_freqAM


# FENÊTRE D'ACTIVITÉ VOCALE ####

seg <- read.table(file = "DATA_depressage_occu.txt", header = TRUE, fill = TRUE) %>% 
  select(-start_time, -end_time) %>%  # Drop des colonnes impertinentes
  filter(Analyse_CATBIC_CALL != "0") %>% # Sélection des données traitées ("1" indique que chaque call/song ont été vérifiés)
  filter(Analyse_CATBIC_SONG != "0") %>% 
  filter(Annee == "2018" | Annee == "2019"|Traitement != "EPC_SYS")
# C'est normal qu'on ait pas le même nb. d'enregistrement car ACT. dépend de l'occurrence (0 ou 1 selon la présence/absence de la grive)

seg_AM <- seg %>% 
  filter(Periode == "AM")

hms_debseg_AM <- lubridate::hms(seg_AM$Heure_debut) #Transformation de l'heure du début en période HH: mm: ss
segdeb_AM <- period_to_seconds(hms_debseg_AM)

hms_debsun_AM <- lubridate::hm(sun_AM$Lever_soleil) #Transformation similaire pour l'heure du lever de soleil
s_debsun_AM <- period_to_seconds(hms_debsun_AM)

debsun_AM <- meteo %>% #Date + Lever de soleil
  select(-Coucher_soleil) %>% 
  distinct(Date, Lever_soleil) %>% 
  cbind(s_debsun_AM)

seg_AM <- seg_AM %>% 
  cbind(segdeb_AM) %>% #Ajout du début de l'enregistrement, en secondes
  mutate(segfin_AM = segdeb_AM + duree) %>% 
  left_join(debsun_AM, by = "Date", relationship = "many-to-many") #Jointure attributaire de l'heure du lever de soleil en fonction de la date

tab_AM <- as.data.frame(table(list(seg_AM$duree, seg_AM$segdeb_AM, seg_AM$segfin_AM, seg_AM$s_debsun_AM)))

debut_H_AM<- round((seg_AM$segdeb_AM - seg_AM$s_debsun_AM)/60, digits = 0) 

tab_AM <- tab_AM %>% 
  filter(Freq != 0) %>% 
  rename(duree = "X.1", segdeb = "X.2", segfin = "X.3", sun = "X.4")
  
str(tab_AM) # On doit transformer les variables d'intérêt de facteur à numérique

tab_AM$segdeb <- as.numeric(levels(tab_AM$segdeb))[tab_AM$segdeb] # segdeb en num.
tab_AM$segfin <- as.numeric(levels(tab_AM$segfin))[tab_AM$segfin]
tab_AM$sun <- as.numeric(levels(tab_AM$sun))[tab_AM$sun] # segdeb en num.

# On compte le nb. de 5 min. normalisé par rapport au lever de soleil

# Avant le lever de soleil

tab_AM$debut_H_AM <- round((tab_AM$segdeb - tab_AM$sun)/60, digits = 0) 

# Après le lever de soleil

tab_AM$fin_H_AM <- round((tab_AM$segfin - tab_AM$sun)/60, digits = 0)

range(tab_AM$debut_H_AM) # [-77; 13]
range(tab_AM$fin_H_AM) # [-49; 46]

# Turn debut_H_AM et fin_H_AM par 5

tab_AM$debut_par5 <- floor(tab_AM$debut_H_AM/5) * 5 # Floor() arrondit à la plus petite valeur spécifiée
tab_AM$fin_par5 <- floor(tab_AM$fin_H_AM/5) * 5     # qui ici correspond à ma limite inférieure

range(tab_AM$debut_par5) # Bel et bien par 5. [-80; -10]
range(tab_AM$fin_par5) # [-50; 45]

breaks_tot_AM <- seq(from = -80, to = 50, by = 5) # Intervalle total du matin

debut_par5 <- tab_AM$debut_par5 # Transformation des colonnes du dataframe en vecteur
fin_par5 <- tab_AM$fin_par5

# For loop qui permet, pour chaque segment de 5 min. avant le lever du soleil, faire suivre l'intervalle et la fréquence jusqu'à la fin du segment, avant de passer au prochain
# ET CE, minute par minute...
vec_AM <- c()
for(i in seq_along(along.with = debut_par5))
{
vec_AM <- c(vec_AM, debut_par5[i]: fin_par5[i])
}
lengths <- (fin_par5 - debut_par5 + 1)
tab2_AM <- cbind(debut_par5 = rep.int(x = debut_par5, times = lengths),
              fin_par5 = rep.int(x = fin_par5, times = lengths),
              vec_AM, Freq = rep.int(x = tab_AM$Freq, times = lengths))

tab2_AM <- as.data.frame(tab2_AM)

# Transformation de vec_AM en sélectionnant les limites des segments de 5 min. (multiples de 5)

tab2_AM$vec_par5 <- cut((tab2_AM$vec_AM), breaks = breaks_tot_AM, right = FALSE) # En fonction de l'intervalle totale

tab2_par5_AM <- tab2_AM[tab2_AM$vec_AM %% 5 == 0,] #Sélectionne les lignes du dataframe qui sont des multiples de 5 (aka limites inférieures)

tab3_AM <- as.data.frame(tab2_par5_AM) %>% 
  group_by(vec_par5) %>% 
  summarise(Freq_par5 = sum(Freq)) %>% 
  rename(voc_cut = "vec_par5")

# Maintenant qu'on obtient le nombre de 5 min. enregistrées par segment, il faut le nb. de segment de 5 min. qui conntiennent de la GRBI

seg_occu_AM <- act_AM %>%    # Ce bon code permet de cut les lignes qui sont groupées (par date et par heure de début) pour pas avoir d'overlap avec plusieurs vocalises dans le même segment de 5 minutes pour les mêmes enregistrements
  group_by(Date, Heure_debut) %>% 
  mutate(voc_cut = cut(voc_sun_AM, breaks = breaks_tot_AM, right = FALSE))

# On a besoin de calculer la fréquence de chaque segment de 5 min. au sein du dataframe
# On a plusieurs vocalises du même enregistrement pour les différents segments de 5 minutes

split_occu_AM <- seg_occu_AM %>% 
  distinct(Annee, Station, Traitement, Date, Heure_debut, Periode, duree, voc_cut) %>% # Choisi les lignes qui ont du metadata unique -> Évite les vocalises répétées dans un même segment
  group_by(voc_cut) %>% 
  summarise(Freq_occu = n())

split_occu_AM #OK. Il s'agit du nombre de segments de 5 min. avec une vocalise de grive validée

tab3_AM$voc_cut <- as.character(tab3_AM$voc_cut)
split_occu_AM$voc_cut <- as.character(split_occu_AM$voc_cut)

proba_par5_AM <- merge(tab3_AM, split_occu_AM, by = "voc_cut", all.x = TRUE) 
  
proba_par5_AM <- proba_par5_AM %>% 
  replace_na(list(Freq_occu = 0)) # Remplace la seule valeur de NA par 0

proba_par5_AM$prob <- as.numeric(proba_par5_AM$Freq_occu/proba_par5_AM$Freq_par5) # Probabilité de détection de la GRBI par segment de 5 min.
                                                                                  # Correspond au nb. de segment de 5 min./ 
# Class imbalance vu qu'il n'y a pas le même nb. de segment enregistré... À voir

# RÉGRESSION POLYNOMIALE









