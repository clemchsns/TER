# ouverture de la base de données
data_base <- read.csv("../Data/NBA_Season_Data.csv",sep=",",header =TRUE)
View(data_base)

colnames(data_base) <- c("Année", "Équipe", "Joueur", "Age", "NbMatchs", "MinutesJouées", "NbPaniers", "PerfParMin", "EfficacitéTir", "Tentative3pts", "TentativesLancersFrancs", "PrctRebondOffensif", "PrctRebondDefensif", "NbTotalRebonds", "ContrôleBallon", "BallonsVolés", "BlocksParJeu", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "NbTirs", "JoueurID", "Supp", "Supp", "Supp", "EfficacitéTirÉquipe", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp")

data_players <-read.csv("../Data/players.csv",sep=",",header =TRUE)
View(data_players)


