# ouverture de la base de données
data_base <- read.csv("../Data/NBA_Season_Data.csv",sep=",",header =TRUE)
View(data_base)

colnames(data_base) <- c("Année", "Équipe", "Joueur", "Age", "NbMatchs", "MinutesJouées", "NbPaniers", "PerfParMin", "EfficacitéTir", "Tentative3pts", "TentativesLancersFrancs", "PrctRebondOffensif", "PrctRebondDefensif", "NbTotalRebonds", "ContrôleBallon", "BallonsVolés", "BlocksParJeu", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "NbTirs", "JoueurID", "Supp", "Supp", "Supp", "Supp", "EfficacitéTirÉquipe", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp")
data_base <- data_base[,c(1:17, 31:32, 37)]
#moyenne d'âge pour chaque équipe
tapply(data_base$Age, data_base$Équipe, mean)

data_players <-read.csv("../Data/players.csv",sep=",",header =TRUE)
View(data_players)

colnames(data_players) = c("X_i","DateNaiss","LieuNaiss","Supp","Supp","supp","Supp","Supp","Supp","Supp","Supp","Supp","Supp","Universite","Supp","Supp","Supp","Supp","Taille","Lycee","Nom","position","MainTire","Poids")
data_players = data_players[,c(1:3,14,19:24)]
View(data_players)



