library(shiny)
library(DT)
library(shinydashboard)

# ouverture de la base de donnees
data_base <- read.csv("../Data/NBA_Season_Data.csv", header=TRUE, stringsAsFactors=TRUE)

colnames(data_base) <- c("Annee", "Equipe", "Joueur", "Age", "NbMatchs", "MinutesJouees", "NbPaniers", "PerfParMin", "EfficaciteTir", "Tentative3pts", "TentativesLancersFrancs", "PrctRebondOffensif", "PrctRebondDefensif", "NbTotalRebonds", "ControleBallon", "BallonsVoles", "BlocksParJeu", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "NbTirs", "JoueurID", "Supp", "Supp", "Supp", "Supp", "EfficaciteTirEquipe", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp")
data_base <- data_base[,c(1:17, 31:32, 37)]
tapply(data_base$Age, data_base$Equipe, mean)

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
            )
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
}
# Run the application 
shinyApp(ui = ui, server = server)