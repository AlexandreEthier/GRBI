setwd("c:\\Users\\alexe\\OneDrive\\PROG\\GRBI\\GRBI\\Metadonnee") #Set working directory


# Chargement des librairies####

install.packages("lubridate")
install.packages("car")
install.packages("hms")
install.packages("data.table")
install.packages("bioacoustics")
install.packages("tuneR")
install.packages("seewave")

library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(bioacoustics)
library(tuneR)
library(seewave)


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

# Séparer les deux classes en AM et PM

act_AM <- distinct(act) %>% 
  filter(Periode == "AM")

sun_AM <- meteo %>% #Date + Lever de soleil
  select(-Coucher_soleil) %>% 
  distinct(Date, Lever_soleil)

act_PM <- distinct(act) %>% 
  filter(Periode == "PM")

sun_PM <- meteo %>% #Date + Coucher de soleil
  select(-Lever_soleil) %>% 
  distinct(Date, Coucher_soleil)

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
n_voc_AM$sun_relatif <- as.numeric(levels(n_voc_AM$sun_relatif))[n_voc_AM$sun_relatif]

n_voc_AM #Intervalle de n_voc: [-72; 44] -> Nous allons déterminer l'intervalle comme suit: [-75; +45]

breaks_AM <- seq(from = -75, to = 50, by = 5)

n_voc_AM_par5 <- cut(n_voc_AM$sun_relatif, breaks = breaks_AM, right = FALSE) #Création des intervalles
n_voc_AM_par5

n_voc_AM <- n_voc_AM %>% #Ajout des intervalles sur le dataframe n_voc
  cbind(n_voc_AM_par5)

n_voc_AM$freq_cum_par5 <- ave(n_voc_AM$Freq, n_voc_AM$n_voc_AM_par5, FUN = sum) # Calcul de la somme des vocalises dans mon intervalle de 5 minutes
                                                                                #La fonction "ave" me permet de réaliser une fonction sur des valeurs en fonction d'une catégorie d'un même dataframe

par5_corr_AM <- seq(from = -75, to = 45, by = 5) #Correspond à la limite inférieure des intervalles de 5 minutes

freq_par5_AM <- as.data.frame(distinct(n_voc_AM, n_voc_AM_par5, freq_cum_par5)) %>% #Sélection des valeurs uniques. La somme de la fréquence des intervalles a été généré à plusieurs reprises; il s'agit de choisir les valeurs uniques
                cbind(par5_corr_AM)

sum(freq_par5_AM$freq_cum_par5)
sum(n_voc_AM$Freq)
count(act_AM$Heure_voc)
print(tab_voc_AM)
sum(tab_voc_AM)

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
  left_join(sun_PM, by = "Date", relationship = "many-to-many")
  
voc_sun_PM <- round((act_PM$Heure_voc - act_PM$s_sun_PM)/60, digits = 0) 

sum(voc_sun_PM)
sum(is.na(act_PM$Coucher_soleil))


act_PM <- act_PM %>% 
  cbind(voc_sun_PM)

tab_voc_PM <- table(act_PM$voc_sun_PM)
print(tab_voc_PM)

sum(tab_voc_PM)
sum(n_voc_PM$Freq)

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

sum(freq_par5_PM$freq_cum_par5)

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


mod_poly_4 <- lm(freq_cum_par5 ~ par5_corr_AM + I(par5_corr_AM^2) + I(par5_corr_AM^3) + I(par5_corr_AM^4), data = freq_par5_AM)

mod_poly_4

ggplot(freq_par5_AM, aes(par5_corr_AM, freq_cum_par5))+
  geom_point()+
  stat_smooth(method = lm, formula = y ~ poly(x, 4, raw = TRUE))

# Axe des Y doit être la probabilité de détection

# Probabilité de détection = N. obs./ N. total de 5 min.

# On calcule le N total de 5 min. via bd occu

seg <- read.table(file = "DATA_depressage_occu.txt", header = TRUE, fill = TRUE) %>% 
  select(-start_time, -end_time) %>%  # Drop des colonnes impertinentes
  filter(Analyse_CATBIC_CALL != "0") %>% # Sélection des données traitées ("1" indique que chaque call/song ont été vérifiés)
  filter(Analyse_CATBIC_SONG != "0") %>% 
  filter(Annee == "2018" | Annee == "2019"|Traitement != "EPC_SYS")
# C'est normal qu'on ait pas le même nb. d'enregistrement car ACT. dépend de l'occurrence (0 ou 1 selon la présence/absence de la grive)

seg_AM <- seg %>% 
  filter(Periode == "AM")

seg_PM <- seg %>% 
  filter(Periode == "PM")

# On doit faire une jointure attributaire de la métadonnée «occu» avec la donnée «act»
# Sélection unique des durée de chaque enregistrement pour avoir le nb. total de segments de 5 min. (duree totale (en sec.)/300 sec. (5 min.))

n_seg_AM <- (sum(seg_AM$duree)/300)
n_seg_AM #5126 segments de 5 min.

n_seg_PM <- (sum(seg_PM$duree)/300)
n_seg_PM #3367 segments de 5 min.

# On a le total du nombre de segments de 5 min. On doit ensuite trouver le nb. de vocalises au sein de chaque 5 min.

# Tableau qui représente: ID 5 min. / nb. de positifs au sein du 5 min. / nb. de segments de ce 5 min. récolté
# Probabilité qui sera utilisée dans lm correspondra aux col2/col3


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

tab_seg_AM <- as.data.frame(table(duree, segdeb_AM, segfin_AM), data = seg_AM)
tab_seg_AM

tab1 <- as.data.frame(table(list(seg_AM$duree, seg_AM$segdeb_AM, seg_AM$segfin_AM)))
  
tab1 <- tab1 %>% 
  filter(Freq != 0)



?table
?cut

range(seg_AM$segdeb_AM)
range(seg_AM$segfin_AM)

breaks_5min <- seq(from = 12660, to =  77340, by = 300) #Min. du début d'enregistrement et Max durée enregistrement
breaks_5min

nb_5min <- cut(seg_AM$segfin_AM, breaks = breaks_5min, right = FALSE)
nb_5min

segduree_AM <- seg_AM$segfin_AM - seg_AM$segdeb_AM
segduree_AM

# On compte le nb. de 5 min.




# Reprendre «freq_cum_par5» et diviser par seg




