library(shiny)
library(DT)
library(shinydashboard)

# ouverture de la base de donnees
data_base <- read.csv("../Data/NBA_Season_Data.csv", header=TRUE, stringsAsFactors=TRUE)

colnames(data_base) <- c("Annee", "Equipe", "Joueur", "Age", "NbMatchs", "MinutesJouees", "NbPaniers", "PerfParMin", "EfficaciteTir", "Tentative3pts", "TentativesLancersFrancs", "PrctRebondOffensif", "PrctRebondDefensif", "NbTotalRebonds", "ControleBallon", "BallonsVoles", "BlocksParJeu", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "NbTirs", "JoueurID", "Supp", "Supp", "Supp", "Supp", "EfficaciteTirEquipe", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp")
data_base <- data_base[,c(1:17, 31:32, 37)]
data_base$Equipe = factor(data_base$Equipe)
levels(data_base$Equipe)=c("Atlanta Hawks", "Boston Celtics","Brooklyn Nets","Buffalo Braves", "Charlotte Hornets", "Chicago Hustle", "Chicago Bulls","Chicago Bruins", "Cleveland Cavaliers","Dallas Mavericks", "Denver Nuggets", "Detroit Pistons", "Golden State Warriors","Houston Rockets","Indiana Pacers","Kings of Sacramento","Los Angeles Clippers","Los Angeles Lakers","Memphis Grizzlies","Miami Heat", "Milwaukee Bucks","Minnesota Timberwolves","Brooklyn Nets","New Orleans Hurricanes","New Orleans Jazz Roster ans Stats","New Orleans/Oklahoma City","New Orleans Pelicans","New York Knicks","Oklahoma City Thunder","Orlando Magic","Philadelphia 76ers", "Phoenix Suns","Portland Trail Blazers","Sacramento Kings","San Antonio Spurs", "San Diego Clippers","Seattle SuperSonics", "Toronto Raptors","Utah Jazz","Vancouver Grizzlies", "Washington Wizards","Washington Bullets") 


data_players <- read.csv("../Data/players.csv", header=TRUE, stringsAsFactors=TRUE)
colnames(data_players) = c("X_i","DateNaiss","LieuNaiss","Supp","Supp","supp","Supp","Supp","Supp","Supp","Supp","Supp","Supp","Universite","Supp","Supp","Supp","Supp","Taille","Lycee","Nom","position","MainTire","Poids")
data_players = data_players[,c(1:3,14,19:24)]
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
                    fluidPage(  #faire la division en 2 parties
                        sidebarLayout(
                            sidebarPanel( #afficher la barre laterale ou se trouve les inputs
                                selectInput('var1','Choisissez une première variable :',choices=names(data_base)),
                            ),
                            mainPanel( #afficher le panneau principal ou on affiche les sorties
                                tabsetPanel( #diviser le tableau principal en onglets
                                    tabPanel('Clubs',tableOutput('data_base')),#tableOutput("data_base")  : table de donnees
                                    tabPanel('Clubs-Joueurs',tableOutput('clubjoueur_sortie')),
                                    tabPanel('Statistiques',verbatimTextOutput('summary')),#verbatimTextOutput : permet d'afficher le résumé stat
                                    tabPanel('Histogramme', plotOutput('hist')),
                                )
                            )
                        )
                    )
            ),
            tabItem("Joueurs",
                    fluidPage(
                        h1("Caratéristiques des joueurs")
                    )),
            tabItem("DonClubs",
                    fluidPage(
                        h1("Base de données des Clubs"),
                        verbatimTextOutput('view1')
                    )),
            tabItem("DonJoueurs",
                    fluidPage(
                        h1("Base de données des joueurs"),
                        verbatimTextOutput('view2')
                    ))
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$data_base <- renderTable({ #a chaque type de sortie, il faut un render qui correspond dans le server
        data_base 
    }) 
    
    #Résumé statistique
    output$summary <- renderPrint({
        summary(data_base)
    })
    #Histogramme (output$id_sortie de plotOutput)
    output$hist <- renderPlot({
        hist(data_base[,input$var1],main="Histogramme",xlab=input$var1)
    })
    #Afficher la base de données des clubs
    output$view1 <- renderPrint({
        data_base
    })
    #Afficher la base de données des joueurs
    output$view2 <- renderPrint({
        data_players
    })
}
# Run the application 
shinyApp(ui = ui, server = server)