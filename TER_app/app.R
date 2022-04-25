library(shiny)
library(DT)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(leaflet)
library(DT)
library(shinyWidgets)

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
levels(data_players$Taille) <- c(1.78, 1.80, 1.60, 1.65, 1.68, 1.70, 1.73, 1.75, 1.83, 1.85, 2.08, 2.11, 1.88, 1.91, 1.93, 1.96, 1.98, 2.01, 2.03, 2.06, 2.13, 2.16, 2.18, 2.21, 2.24, 2.26, 2.29, 2.31)

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

#pour sélectionner les années
annees = data_base$Annee

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "NBA Menu"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Accueil", tabName = "Accueil", icon = icon("basketball-ball"),
                     menuSubItem("Présentation de notre TER",tabName="TER"),
                     menuSubItem("Meilleurs Dunks de 2016",tabName="dunks")),
            menuItem("Clubs", tabName = "Clubs", icon = icon("hands-helping")),
            menuItem("Joueurs", tabName = "Joueurs", icon = icon("running")),
            menuItem("Restrospective du projet", tabName = "Retro", icon = icon("tasks"))
    )),
    dashboardBody(
        headerPanel('image de fond'),
        tabItems(
            tabItem(tabName="TER",
                    fluidPage(h1("Présentation de notre TER"),
                              tabsetPanel(
                                  tabPanel("Présentation de notre TER",HTML('<p>Lors du second semestre de notre licence 3 en Mathématiques et Informatique Appliquées Aux Sciences Humaines et Sociales, nous avons pu choisir un sujet de travail encadré par un de nos enseignants. Pour notre part, Clémence, Margaux, Marie et Oriane, nous avons choisi de réaliser une application WEB à l’aide du logiciel R Shiny. Effectivement, notre attrait pour l’informatique nous a poussé à choisir ce sujet.  Aussi, nous sommes toutes passionnées de sport, il nous a donc paru important d’introduire cette thématique dans notre projet. Nous avons choisi d’analyser une compétition connue dans le monde entier : la NBA. Grâce aux nombreuses bases de données trouvées au sujet de la National Basketball Association, nous avons pu nous intéresser aux statistiques des clubs et des joueurs présents au sein de cette compétition entre 1978 et 2015.</p>

                                    	<p>Tout d’abord, afin de pouvoir réaliser notre application, nous avons recherché une base de données intéressante à étudier. Il semblait évident que le thème qui nous allions aborder serait le sport ou la santé puisque nous sommes passionnées de ces sujets. Au départ, nous avons voulu analyser l’impact des drogues sur la santé mais ne disposant pas de base de données adéquates, nous nous sommes focalisées définitivement sur la thématique du sport. Nous avons d’abord voulu étudier un sport au niveau local, et plus particulièrement le Stade Rennais, mais le travail était trop compliqué pour pouvoir récupérer sous un format correct notre base de données. Ainsi, nous avons décidé d’opter pour une compétition internationale de basketball : la NBA. La recherche des données à analyser s’est donc avéré plus longue et compliquée que ce que nous avions prévu. Au final, nous avons sélectionné deux bases de données : la première au sujet des clubs et la seconde à propos des différents joueurs de basketball présents lors d’une ou plusieurs saisons de NBA.
                                    Ensuite, notre travail de compréhension du logiciel R Shiny a pu réellement débuter. Notre objectif initial était de créer une application avec des extraits de vidéos sur le basket et des pages interactives. Nous souhaitions reprendre un modèle d’application déjà existant et l’adapter à nos propres données. Cependant, cette tâche s’est révélée très complexe puisque le code R comportait de l’HTML, du CSS mais aussi des liens pour accéder à des vidéos provenant d’internet. N’ayant pas encore intégré toutes les particularités du fonctionnement du logiciel, à ce moment donné, notre niveau de compréhension de R Shiny était insuffisant pour mettre ce code en place. Nous avons donc décidé de reprendre les bases, en réalisant de petites applications simples, en regardant des vidéos et tutoriels afin de comprendre le fonctionnement de ce logiciel, pour ensuite complexifier le code. C’est ainsi que nous avons commencer à réaliser notre application. Nous avions pour but de réaliser des statistiques sur les clubs de basketball mais aussi sur les joueurs eux-mêmes. 
                                    <p>Premièrement, nous avons créé la structure de l’application avec deux onglets : le premier concernant les clubs et le deuxième, les joueurs.</p>
                                    Dans l’onglet des clubs, il nous paraissait primordial d’afficher les clubs présents lors de cette compétition. Nous avons donc réalisé un datatable pour présenter ces variables. Puis, dans le sous-onglet suivant, nous voulions que l’utilisateur puisse sélectionner un club et une année pour pouvoir accès aux différents joueurs jouant dans le club choisi. La présentation sous forme de datatable avec des barres de recherches nous a semblé la plus esthétique et adaptée. Ensuite, le résumé statistique nous permet de comprendre les données et les graphiques de mieux les visualiser. La prise en main du package ggplot a aussi été un challenge puisque nous étions en autonomie. Enfin, la création d’une carte localisant les différents clubs nous a permis d’avoir une application un peu plus ludique pour l’utilisateur. 
                                    Par ailleurs, l’onglet joueurs est un peu moins exhaustif puisque nous avons effectué des statistiques sur les caractéristiques générales. Par exemple, nous avons réalisé des graphiques au sujet de la main de tir mais aussi de la taille des joueurs. Dans un autre sous-onglet, nous avons affiché les informations personnelles de chaque joueur grâce à une barre de recherche.
                                    Deuxièmement, après avoir réalisé l’ensemble de notre application, nous avons voulu faciliter son utilisation et rendre la rendre ergonomique et esthétique. Nous nous sommes focalisées sur les détails tels que la mise en place d’icônes pour accéder aux différents onglets mais aussi le thème choisi. De plus, la recherche de la mise en place d’un fond d’écran dynamique nous a permis de mettre l’application à notre goût et à notre image.</p> 
                                     
                                    <p>En conclusion, la réalisation de notre application WEB à l’aide de R Shiny nous a permis de comprendre le fonctionnement d’un nouveau logiciel en totale autonomie. La pertinence des données présentées a été primordiale pour nous quatre. Notre application témoigne donc de notre passion partagée pour le sport mais aussi pour l’informatique.</p>')),
                                  tabPanel("Présentation de la NBA",imageOutput("logo")),
                                  ))),
            tabItem(tabName="dunks", 
                    fluidPage(h1("Meilleurs Dunks de 2016"),tags$iframe(width="560", height="315", src="https://www.youtube.com/embed/wpizP7Vehnw", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA))
            ),
            tabItem("Clubs",
                    tabsetPanel( #diviser le tableau principal en onglets
                        tabPanel('Clubs', h1("Les différents clubs de la NBA"),
                                 HTML("<p style=\"font-size:x-large\"> Voici la liste des clubs de Basketball de la NBA depuis 1978 jusqu'à 2015:"),dataTableOutput('noms_clubs')),
                        tabPanel('Clubs-Joueurs',selectInput('varcj','Choisissez un club :',choices=list("Atlanta Hawks", "Boston Celtics","Brooklyn Nets","Buffalo Braves", "Charlotte Hornets", "Chicago Hustle", "Chicago Bulls","Chicago Bruins", "Cleveland Cavaliers","Dallas Mavericks", "Denver Nuggets", "Detroit Pistons", "Golden State Warriors","Houston Rockets","Indiana Pacers","Kings of Sacramento","Los Angeles Clippers","Los Angeles Lakers","Memphis Grizzlies","Miami Heat", "Milwaukee Bucks","Minnesota Timberwolves","Brooklyn Nets","New Orleans Hurricanes","New Orleans Jazz Roster ans Stats","New Orleans/Oklahoma City","New Orleans Pelicans","New York Knicks","Oklahoma City Thunder","Orlando Magic","Philadelphia 76ers", "Phoenix Suns","Portland Trail Blazers","Sacramento Kings","San Antonio Spurs", "San Diego Clippers","Seattle SuperSonics", "Toronto Raptors","Utah Jazz","Vancouver Grizzlies", "Washington Wizards","Washington Bullets")),selectizeInput('annee_club', 'Années', choices = annees,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           options = list(
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               placeholder = 'Ecrivez pour chercher une année',
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               onInitialize = I('function() { this.setValue(""); }')
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           )),verbatimTextOutput("textcj"),dataTableOutput("varcj")
                        ),
                        tabPanel('Statistiques',HTML("Ci-dessous le résumé statistique de la base de données 'data'. <br />On y retrouve des données sur des clubs que l'on calcul grâce aux joueurs. Par exemple, on peut évaluer l'efficacité des tirs des joueurs d'un club ou encore l'âge moyen des joueurs d'un club entre 1978 et 2015."),dataTableOutput('summary')),
                        tabPanel('Graphiques',selectInput('varclub','Choisissez un club :',choices=list("Atlanta Hawks", "Boston Celtics","Brooklyn Nets","Buffalo Braves", "Charlotte Hornets", "Chicago Hustle", "Chicago Bulls","Chicago Bruins", "Cleveland Cavaliers","Dallas Mavericks", "Denver Nuggets", "Detroit Pistons", "Golden State Warriors","Houston Rockets","Indiana Pacers","Kings of Sacramento","Los Angeles Clippers","Los Angeles Lakers","Memphis Grizzlies","Miami Heat", "Milwaukee Bucks","Minnesota Timberwolves","Brooklyn Nets","New Orleans Hurricanes","New Orleans Jazz Roster ans Stats","New Orleans/Oklahoma City","New Orleans Pelicans","New York Knicks","Oklahoma City Thunder","Orlando Magic","Philadelphia 76ers", "Phoenix Suns","Portland Trail Blazers","Sacramento Kings","San Antonio Spurs", "San Diego Clippers","Seattle SuperSonics", "Toronto Raptors","Utah Jazz","Vancouver Grizzlies", "Washington Wizards","Washington Bullets")), plotOutput('varclub')),
                        tabPanel('Carte des clubs', h3("Points sur  carte"),
                                 leafletOutput("map_points"))
                    )
            ),
            tabItem("Joueurs",
                    fluidPage(h1("Caratéristiques des joueurs"),
                              tabsetPanel(
                                  tabPanel('Carte des lieux de naissance'),
                                  tabPanel('Caractéristiques générales', plotOutput('varpie'), plotOutput('varhisto')),
                                  tabPanel('Caractéristiques d\'un joueur', 
                                           selectizeInput('joueurs_id', 'Joueurs', choices = joueurs_names,
                                                          options = list(
                                                              placeholder = 'Ecrivez pour chercher un joueur',
                                                              onInitialize = I('function() { this.setValue(""); }')
                                                          )),selectizeInput('annee', 'Années', choices = annees,
                                                                            options = list(
                                                                                placeholder = 'Ecrivez pour chercher une année',
                                                                                onInitialize = I('function() { this.setValue(""); }'), selected = "1978"
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
    #affichage du logo 
    output$logo <- renderImage({
        list(src="../Data/logo_NBA.png",alt="logo NBA",width=150,height=250,vspace=25,style='position : relative')
        },deleteFile=FALSE)
    
    #Sorti de l'onglet club
    output$noms_clubs <- renderDataTable(unique(data_base[,1:2]),rownames=FALSE)   
    output$textcj <- renderText({
        paste("Voici les joueurs ayant été dans le club", input$varcj,"pour l'année",input$annee_club)
    })
    output$varcj <- renderDataTable(data_base[which(data_base$Equipe==input$varcj & data_base$Annee==input$annee_club),][,c(1,3)],rownames=FALSE)
    
    #Résumé statistique
    output$summary <- renderDataTable(summary(data_base),rownames=FALSE)
    
    #Histogramme (output$id_sortie de plotOutput)
    output$hist <- renderPlot({
        hist(data_base[,input$var1],main="Histogramme",xlab=input$var1)
    })
    
    #Afficher les caractéristiques des joueurs
    output$j <- renderText({
        paste("Vous avez séléctionné le joueur", input$joueurs_id, "pour l'année",input$annee)
    })
    
    output$cara <- renderDataTable({DT::datatable(data=data_base[which(data_base$Nom==input$joueurs_id & data_base$Annee==input$annee),][,c(1:2,4:18,20)],option= list(scrollX=TRUE),rownames=FALSE)})
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
    
    #Histogramme
    output$varhisto <- renderPlot({
        histo <- ggplot(data_players, aes(x = as.numeric(as.character(data_players$Taille)))) +
            geom_histogram(color="black", fill="white") +
            xlab ("Taille des joueurs") +
            ylab("Nombre de joueurs") +
            geom_vline(aes(xintercept=mean(as.numeric(as.character(data_players$Taille)))),
                       color="blue", linetype="dashed", size=1) +
            geom_vline(aes(xintercept=1.754),
                       color="red", linetype="dashed", size=1)
        histo
    })
}
# Run the application 
shinyApp(ui = ui, server = server)

