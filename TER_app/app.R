library(shiny)
library(DT)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(leaflet)

# ouverture de la base de donnees
data_equipe <- read.csv("../Data/NBA_Season_Data.csv", header=TRUE, stringsAsFactors=TRUE)

colnames(data_equipe) <- c("Annee", "Equipe", "Nom", "Age", "NbMatchs", "MinutesJouees", "NbPaniers", "PerfParMin", "EfficaciteTir", "Tentative3pts", "TentativesLancersFrancs", "PrctRebondOffensif", "PrctRebondDefensif", "NbTotalRebonds", "ControleBallon", "BallonsVoles", "BlocksParJeu", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "NbTirs", "JoueurID", "Supp", "Supp", "Supp", "Supp", "EfficaciteTirEquipe", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp")
data_equipe <- data_equipe[,c(1:17, 31:32, 37)]
data_equipe$Equipe = factor(data_equipe$Equipe)
levels(data_equipe$Equipe)=c("Atlanta Hawks", "Boston Celtics","Brooklyn Nets","Buffalo Braves", "Charlotte Hornets", "Chicago Hustle", "Chicago Bulls","Chicago Bruins", "Cleveland Cavaliers","Dallas Mavericks", "Denver Nuggets", "Detroit Pistons", "Golden State Warriors","Houston Rockets","Indiana Pacers","Kings of Sacramento","Los Angeles Clippers","Los Angeles Lakers","Memphis Grizzlies","Miami Heat", "Milwaukee Bucks","Minnesota Timberwolves","Brooklyn Nets","New Orleans Hurricanes","New Orleans Jazz Roster ans Stats","New Orleans/Oklahoma City","New Orleans Pelicans","New York Knicks","Oklahoma City Thunder","Orlando Magic","Philadelphia 76ers", "Phoenix Suns","Portland Trail Blazers","Sacramento Kings","San Antonio Spurs", "San Diego Clippers","Seattle SuperSonics", "Toronto Raptors","Utah Jazz","Vancouver Grizzlies", "Washington Wizards","Washington Bullets") 

data_players <- read.csv("../Data/players.csv", header=TRUE, stringsAsFactors=TRUE)
summary(data_players)
colnames(data_players) = c("supp","DateNaiss","LieuNaiss","Supp","Supp","supp","Supp","Supp","Supp","Supp","Supp","Supp","Supp","Universite","Supp","Supp","Equipe","Ann?ePro","Taille","Lycee","Nom","position","MainTire","Poids")
data_players = data_players[,c(2:3,14,17:24)]

#création dataframe pour pouvoir sélectionner les joueurs
joueurs_names = data_base$Nom

# Trouver les correspondances entre data et data_players 
table(data_equipe$Nom %in% data_players$Nom)
data_base <- semi_join(data_equipe,data_players,by='Nom')

#Création data frame pour les cartes


nom_equipe <- c("Atlanta Hawks", "Boston Celtics","Brooklyn Nets","Buffalo Braves", "Charlotte Hornets", "Chicago Hustle", "Chicago Bulls","Chicago Bruins", "Cleveland Cavaliers","Dallas Mavericks", "Denver Nuggets", "Detroit Pistons", "Golden State Warriors","Houston Rockets","Indiana Pacers","Kings of Sacramento","Los Angeles Clippers","Los Angeles Lakers","Memphis Grizzlies","Miami Heat", "Milwaukee Bucks","Minnesota Timberwolves","Brooklyn Nets","New Orleans Hurricanes","New Orleans Jazz Roster ans Stats","New Orleans/Oklahoma City","New Orleans Pelicans","New York Knicks","Oklahoma City Thunder","Orlando Magic","Philadelphia 76ers", "Phoenix Suns","Portland Trail Blazers","Sacramento Kings","San Antonio Spurs", "San Diego Clippers","Seattle SuperSonics", "Toronto Raptors","Utah Jazz","Vancouver Grizzlies", "Washington Wizards","Washington Bullets") 


data_map <- data.frame(Nom_equipe = nom_equipe) 
data_map

data_map$Lat=c(33.75693,42.37128,40.68631,42.88760,35.22480,41.90392,41.88134,41.93095,41.49649,43.64387,39.74935,42.34124,37.76817,29.75085,39.76417,38.58029,34.04314,34.04314,35.13821,25.78156,43.04518,44.98418,40.688310,25.715889,40.769163,29.949602,29.949602,40.751133,35.464039,28.539707,39.901832,33.446499,45.532140,38.580932,29.427794,34.044002 ,47.622624, 43.64360,40.76858,49.28841, 38.89831,38.89831 ) 
data_map$Lat = as.numeric(as.character(data_map$Lat))

data_map$Long=c(-84.39215,-71.05998,-73.94432,-78.87063,-80.83986,-87.62480,-87.67376,-87.99805,-81.68848,-79.37841,-105.00773,-83.05530,-122.38767,-95.36206,-86.15543,-121.49972,-118.26726,-118.26726,-90.05059,-80.18697,-87.91738,-93.27690,-73.97541,-80.279204,-111.900518,-90.082026,-90.081925,-73.993272,-97.514831,-81.383502,-75.171531,-112.070782,-122.666472,-121.499249,-98.437227,-118.266694,-122.353855,-79.37909,-111.90107,-123.10644, -77.02083, -77.02083)
data_map$Long = as.numeric(as.character(data_map$Long))


# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Menu"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Accueil", tabName = "Accueil", icon = icon("basketball-ball")),
            menuItem("Clubs", tabName = "Clubs", icon = icon("hands-helping")),
            menuItem("Joueurs", tabName = "Joueurs", icon = icon("running")),
            menuItem("Restrospective du projet", tabName = "Retro", icon = icon("tasks"))
    )),
    dashboardBody(
        tabItems(
            tabItem("Accueil",
                    fluidPage(
                        h1("Accueil")
                    )
            ),
            tabItem("Clubs",
                    tabsetPanel( #diviser le tableau principal en onglets
                        tabPanel('Clubs', h1("Les différents clubs de la NBA"),
                                 HTML("<p style=\"font-size:x-large\"> Voici la liste des clubs de Basketball de la NBA depuis 1978 jusqu'à 2015:"),dataTableOutput('noms_clubs')),
                        tabPanel('Clubs-Joueurs',selectInput('varcj','Choisissez un club :',choices=list("Atlanta Hawks", "Boston Celtics","Brooklyn Nets","Buffalo Braves", "Charlotte Hornets", "Chicago Hustle", "Chicago Bulls","Chicago Bruins", "Cleveland Cavaliers","Dallas Mavericks", "Denver Nuggets", "Detroit Pistons", "Golden State Warriors","Houston Rockets","Indiana Pacers","Kings of Sacramento","Los Angeles Clippers","Los Angeles Lakers","Memphis Grizzlies","Miami Heat", "Milwaukee Bucks","Minnesota Timberwolves","Brooklyn Nets","New Orleans Hurricanes","New Orleans Jazz Roster ans Stats","New Orleans/Oklahoma City","New Orleans Pelicans","New York Knicks","Oklahoma City Thunder","Orlando Magic","Philadelphia 76ers", "Phoenix Suns","Portland Trail Blazers","Sacramento Kings","San Antonio Spurs", "San Diego Clippers","Seattle SuperSonics", "Toronto Raptors","Utah Jazz","Vancouver Grizzlies", "Washington Wizards","Washington Bullets")),verbatimTextOutput("textcj"),dataTableOutput("varcj")
                        ),
                        tabPanel('Statistiques',HTML("Ci-dessous le résumé statistique de la base de données 'data'. <br />On y retrouve des données sur des clubs que l'on calcul grâce aux joueurs. Par exemple, on peut évaluer l'efficacité des tirs des joueurs d'un club ou encore l'âge moyen des joueurs d'un club entre 1978 et 2015."),verbatimTextOutput('summary')),#verbatimTextOutput : permet d'afficher le résumé stat
                        tabPanel('Graphiques',selectInput('varclub','Choisissez un club :',choices=list("Atlanta Hawks", "Boston Celtics","Brooklyn Nets","Buffalo Braves", "Charlotte Hornets", "Chicago Hustle", "Chicago Bulls","Chicago Bruins", "Cleveland Cavaliers","Dallas Mavericks", "Denver Nuggets", "Detroit Pistons", "Golden State Warriors","Houston Rockets","Indiana Pacers","Kings of Sacramento","Los Angeles Clippers","Los Angeles Lakers","Memphis Grizzlies","Miami Heat", "Milwaukee Bucks","Minnesota Timberwolves","Brooklyn Nets","New Orleans Hurricanes","New Orleans Jazz Roster ans Stats","New Orleans/Oklahoma City","New Orleans Pelicans","New York Knicks","Oklahoma City Thunder","Orlando Magic","Philadelphia 76ers", "Phoenix Suns","Portland Trail Blazers","Sacramento Kings","San Antonio Spurs", "San Diego Clippers","Seattle SuperSonics", "Toronto Raptors","Utah Jazz","Vancouver Grizzlies", "Washington Wizards","Washington Bullets")), plotOutput('varclub')),
                        tabPanel('Carte des clubs', h3("Points sur  carte"),
                                 leafletOutput("map_points"))
                    )
            ),
            tabItem("Joueurs",
                    fluidPage(h1("Caratéristiques des joueurs"),
                              tabsetPanel(
                                  tabPanel('Carte des lieux de naissance'),
                                  tabPanel('Caractéristiques générales', plotOutput('varpie')),
                                  tabPanel('Caractéristiques d\'un joueur', 
                                           selectizeInput('joueurs_id', 'Joueurs', choices = joueurs_names,
                                                          options = list(
                                                              placeholder = 'Ecrivez pour chercher un joueur',
                                                              onInitialize = I('function() { this.setValue(""); }')
                                                          )), verbatimTextOutput("j"), dataTableOutput("cara"))
                              ))),
            tabItem("Retro",
                    fluidPage(h1("Rétrospective"),
                        HTML('Conclusion de notre TER')))
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #    output$noms_clubs <- renderText({ #a chaque type de sortie, il faut un render qui correspond dans le server
    #        levels(data_base$Equipe)
    #    }) 
    output$noms_clubs <- renderDataTable(unique(data_base[,1:2]))   
    
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
    
    output$cara <- renderDataTable(data_base[which(data_base$Nom==input$joueurs_id),][,c(1:2,4:18,20)])
    
    output$textcj <- renderText({
        paste("Voici les joueurs ayant été dans le club", input$varcj,"entre 1978 et 2015")
    })
    
    output$varcj <- renderDataTable(data_base[which(data_base$Equipe==input$varcj),][,c(1,3)])
    
    #Nuage de points
    output$varclub <- renderPlot({
        data_base1 <- data_base %>% dplyr::filter(Equipe==input$varclub)
        data_base$EfficaciteTirEquipe[which(data_base$Equipe==input$varclub)]
        #        plot(EfficaciteTirEquipe ~ Annee, data = data_base1)
        ggplot(data_base1)+aes(x=Annee,y=EfficaciteTirEquipe)+
            geom_point()+geom_smooth()+theme_bw()
        
    })
    
    #Carte 
    output$map_points <- renderLeaflet({
        
        leaflet(data=data_map) %>%
            addTiles() %>%
            addMarkers(lng = ~Long, lat = ~Lat, popup = as.character(~Nom_equipe))
        
    })
    
    #Camembert
    output$varpie <- renderPlot({
        df <- data.frame(
            group <- c("Left", "Left/Right", "Right"), 
            value <- c(283, 1, 4400))
        pie <- ggplot(df, aes(x="", y=value, fill=group)) +
            geom_bar(width = 1, stat = "identity") + 
            coord_polar(theta = "y") + 
            scale_fill_brewer(type = "seq", direction = -1, palette="Blues") +
            geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
                          label=paste(group,"\n",round((value/sum(value))*100), "%")))+
            theme_minimal()
        pie
    })
}
# Run the application 
shinyApp(ui = ui, server = server)

