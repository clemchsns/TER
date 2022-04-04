library(shiny)
library(DT)
library(shinydashboard)
library(dplyr)

# ouverture de la base de donnees
data_equipe <- read.csv("../Data/NBA_Season_Data.csv", header=TRUE, stringsAsFactors=TRUE)

colnames(data_equipe) <- c("Annee", "Equipe", "Nom", "Age", "NbMatchs", "MinutesJouees", "NbPaniers", "PerfParMin", "EfficaciteTir", "Tentative3pts", "TentativesLancersFrancs", "PrctRebondOffensif", "PrctRebondDefensif", "NbTotalRebonds", "ControleBallon", "BallonsVoles", "BlocksParJeu", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "NbTirs", "JoueurID", "Supp", "Supp", "Supp", "Supp", "EfficaciteTirEquipe", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp")
data_equipe <- data_equipe[,c(1:17, 31:32, 37)]
data_equipe$Equipe = factor(data_equipe$Equipe)
levels(data_equipe$Equipe)=c("Atlanta Hawks", "Boston Celtics","Brooklyn Nets","Buffalo Braves", "Charlotte Hornets", "Chicago Hustle", "Chicago Bulls","Chicago Bruins", "Cleveland Cavaliers","Dallas Mavericks", "Denver Nuggets", "Detroit Pistons", "Golden State Warriors","Houston Rockets","Indiana Pacers","Kings of Sacramento","Los Angeles Clippers","Los Angeles Lakers","Memphis Grizzlies","Miami Heat", "Milwaukee Bucks","Minnesota Timberwolves","Brooklyn Nets","New Orleans Hurricanes","New Orleans Jazz Roster ans Stats","New Orleans/Oklahoma City","New Orleans Pelicans","New York Knicks","Oklahoma City Thunder","Orlando Magic","Philadelphia 76ers", "Phoenix Suns","Portland Trail Blazers","Sacramento Kings","San Antonio Spurs", "San Diego Clippers","Seattle SuperSonics", "Toronto Raptors","Utah Jazz","Vancouver Grizzlies", "Washington Wizards","Washington Bullets") 
View(data_equipe)
nrow(data_equipe)

data_players <- read.csv("../Data/players.csv", header=TRUE, stringsAsFactors=TRUE)
summary(data_players)
colnames(data_players) = c("supp","DateNaiss","LieuNaiss","Supp","Supp","supp","Supp","Supp","Supp","Supp","Supp","Supp","Supp","Universite","Supp","Supp","Equipe","Ann?ePro","Taille","Lycee","Nom","position","MainTire","Poids")
data_players = data_players[,c(2:3,14,17:24)]
View(data_players)
nrow(data_players)

# Trouver les correspondances entre data et data_players 
table(data_equipe$Nom %in% data_players$Nom)
data_base <- semi_join(data_equipe,data_players,by="Nom")

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Menu"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Accueil", tabName = "Accueil", icon = icon("basketball-ball")),
            menuItem("Clubs", tabName = "Clubs", icon = icon("running")),
            menuItem("Joueurs", tabName = "Joueurs", icon = icon("arrow-right")),
            menuItem("Base de données des clubs", tabName = "DonClubs", icon = icon("arrow-right")),
            menuItem("Base de données des joueurs", tabName = "DonJoueurs", icon = icon("arrow-right"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem("Accueil",
                    fluidPage(
                        h1("Accueil")
                    )
            ),
            tabItem("Clubs",
                    tabsetPanel( #diviser le tableau principal en onglets
                        tabPanel('Clubs', h1("Les différents clubs de la NBA"),HTML("<p style=\"font-size:x-large\"> Voici la liste des clubs de Basketball de la NBA depuis 1978 jusqu'à 2015:"),textOutput('noms_clubs',inline = FALSE)),
                        tabPanel('Clubs-Joueurs',selectInput('varcj','Choisissez un club :',choices=list("Atlanta Hawks", "Boston Celtics","Brooklyn Nets","Buffalo Braves", "Charlotte Hornets", "Chicago Hustle", "Chicago Bulls","Chicago Bruins", "Cleveland Cavaliers","Dallas Mavericks", "Denver Nuggets", "Detroit Pistons", "Golden State Warriors","Houston Rockets","Indiana Pacers","Kings of Sacramento","Los Angeles Clippers","Los Angeles Lakers","Memphis Grizzlies","Miami Heat", "Milwaukee Bucks","Minnesota Timberwolves","Brooklyn Nets","New Orleans Hurricanes","New Orleans Jazz Roster ans Stats","New Orleans/Oklahoma City","New Orleans Pelicans","New York Knicks","Oklahoma City Thunder","Orlando Magic","Philadelphia 76ers", "Phoenix Suns","Portland Trail Blazers","Sacramento Kings","San Antonio Spurs", "San Diego Clippers","Seattle SuperSonics", "Toronto Raptors","Utah Jazz","Vancouver Grizzlies", "Washington Wizards","Washington Bullets")),verbatimTextOutput("textcj"),verbatimTextOutput("varcj")
                        ),
                        tabPanel('Statistiques',HTML("<p style=\"font-size:x-large\">Ci-dessous le résumé statistique de la base de données 'data'. <br />On y retrouve des données sur des clubs que l'on calcul grâce aux joueurs. Par exemple, on peut évaluer l'efficacité des tirs des joueurs d'un club ou encore l'âge moyen des joueurs d'un club entre 1978 et 2015."),verbatimTextOutput('summary')),#verbatimTextOutput : permet d'afficher le résumé stat
                        tabPanel('Histogramme', plotOutput('hist')),
                        tabPanel('Carte des clubs')
                    )
            ),
            tabItem("Joueurs",
                    fluidPage(h1("Caratéristiques des joueurs"),
                              tabsetPanel(
                                  tabPanel('Carte des lieux de naissance'),
                                  tabPanel('Caractéristiques générales'),
                                  tabPanel('Caractéristiques d\'un joueur', 
                                           selectizeInput('joueurs_id', 'Joueurs', choices = joueurs_names,
                                                          options = list(
                                                              placeholder = 'Ecrivez pour chercher un joueur',
                                                              onInitialize = I('function() { this.setValue(""); }')
                                                          )), verbatimTextOutput("j"), verbatimTextOutput("E"),verbatimTextOutput("Tirs"),verbatimTextOutput("NbTirs")),
                                  tabPanel('Position du joueur')
                              ))),
            tabItem("DonClubs",
                    fluidPage(h1("Base de données des Clubs"),verbatimTextOutput('view1'))),
            tabItem("DonJoueurs",
                    fluidPage(h1("Base de données des joueurs"),verbatimTextOutput('view2')))
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$noms_clubs <- renderText({ #a chaque type de sortie, il faut un render qui correspond dans le server
        levels(data_base$Equipe)
    }) 
    
    #Résumé statistique
    output$summary <- renderPrint({
        summary(data_base)
    })
    #Histogramme (output$id_sortie de plotOutput)
    output$hist <- renderPlot({
        hist(data_base[,input$var1],main="Histogramme",xlab=input$var1)
    })
    
    #Afficher les caractéristiques des joueurs
    output$j <- renderText({
        paste("Joueur :", input$joueurs_id)
    })
    
    output$E <- renderText({
        paste("Equipe dans lesquelles le joueur a été")
        paste(unique(data_base$Equipe[data_base$Nom==input$joueurs_id]),sep="-")
    })
    
    output$Tirs <- renderPrint({
        data_base$EfficaciteTir[data_base$Nom==input$joueurs_id & data_base$Annee==2015]
    })
    output$NbTirs <- renderPrint({
        data_base$NbTirs[data_base$Nom==input$joueurs_id & data_base$Annee==2015]
    })
    
    #
    #data_base$NbMatchs[data_base$Joueur==input$varcj & data_base$Annee==2015,]
    #data_base$MinutesJouees[data_base$Joueur==input$varcj & data_base$Annee==2015,]
    
    #Afficher la base de données des clubs
    #output$view1 <- renderPrint({
    #    data_base
    #})
    #Afficher la base de données des joueurs
    #output$view2 <- renderPrint({
    #data_players
    #})
    
    output$textcj <- renderText({
        paste("Voici les joueurs ayant été dans le club", input$varcj,"entre 1978 et 2015")
    })
    
    output$varcj <- renderPrint({
        data_base$Nom[which(data_base$Equipe==input$varcj)]
    })
}
# Run the application 
shinyApp(ui = ui, server = server)