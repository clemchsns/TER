library(shiny)
library(DT)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(leaflet)
library(shinyWidgets)
library(dashboardthemes)

# ouverture de la base de donnees
data_base <- read.csv("../Data/NBA_Season_Data.csv", header=TRUE, stringsAsFactors=TRUE)

colnames(data_base) <- c("Annee", "Equipe", "Nom", "Age", "NbMatchs", "MinutesJouees", "NbPaniers", "PerfParMin", "EfficaciteTir", "Tentative3pts", "TentativesLancersFrancs", "PrctRebondOffensif", "PrctRebondDefensif", "NbTotalRebonds", "ControleBallon", "BallonsVoles", "BlocksParJeu", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "NbTirs", "JoueurID", "Supp", "Supp", "Supp", "Supp", "EfficaciteTirEquipe", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp", "Supp")
data_base <- data_base[,c(1:17, 31:32, 37)]
data_base$Equipe = factor(data_base$Equipe)
levels(data_base$Equipe)=c("Atlanta Hawks", "Boston Celtics","Brooklyn Nets","Buffalo Braves", "Charlotte Hornets", "Chicago Hustle", "Chicago Bulls","Chicago Bruins", "Cleveland Cavaliers","Dallas Mavericks", "Denver Nuggets", "Detroit Pistons", "Golden State Warriors","Houston Rockets","Indiana Pacers","Kings of Sacramento","Los Angeles Clippers","Los Angeles Lakers","Memphis Grizzlies","Miami Heat", "Milwaukee Bucks","Minnesota Timberwolves","Brooklyn Nets","New Orleans Hurricanes","New Orleans Jazz Roster ans Stats","New Orleans/Oklahoma City","New Orleans Pelicans","New York Knicks","Oklahoma City Thunder","Orlando Magic","Philadelphia 76ers", "Phoenix Suns","Portland Trail Blazers","Sacramento Kings","San Antonio Spurs", "San Diego Clippers","Seattle SuperSonics", "Toronto Raptors","Utah Jazz","Vancouver Grizzlies", "Washington Wizards","Washington Bullets") 

data_players <- read.csv("../Data/players.csv", header=TRUE, stringsAsFactors=TRUE)
summary(data_players)
colnames(data_players) = c("supp","DateNaiss","LieuNaiss","Supp","Supp","supp","Supp","Supp","Supp","Supp","Supp","Supp","Supp","Universite","Supp","Supp","Equipe","Ann?ePro","Taille","Lycee","Nom","position","MainTire","Poids")
data_players = data_players[,c(2:3,14,17:24)]
levels(data_players$Taille) <- c(1.78, 1.80, 1.60, 1.65, 1.68, 1.70, 1.73, 1.75, 1.83, 1.85, 2.08, 2.11, 1.88, 1.91, 1.93, 1.96, 1.98, 2.01, 2.03, 2.06, 2.13, 2.16, 2.18, 2.21, 2.24, 2.26, 2.29, 2.31)

#création dataframe pour pouvoir sélectionner les joueurs
joueurs_names = data_base$Nom

#Création data frame pour les cartes
nom_equipe <- c("Atlanta Hawks", "Boston Celtics","Brooklyn Nets","Buffalo Braves", "Charlotte Hornets", "Chicago Hustle", "Chicago Bulls","Chicago Bruins", "Cleveland Cavaliers","Dallas Mavericks", "Denver Nuggets", "Detroit Pistons", "Golden State Warriors","Houston Rockets","Indiana Pacers","Kings of Sacramento","Los Angeles Clippers","Los Angeles Lakers","Memphis Grizzlies","Miami Heat", "Milwaukee Bucks","Minnesota Timberwolves","Brooklyn Nets","New Orleans Hurricanes","New Orleans Jazz Roster ans Stats","New Orleans/Oklahoma City","New Orleans Pelicans","New York Knicks","Oklahoma City Thunder","Orlando Magic","Philadelphia 76ers", "Phoenix Suns","Portland Trail Blazers","Sacramento Kings","San Antonio Spurs", "San Diego Clippers","Seattle SuperSonics", "Toronto Raptors","Utah Jazz","Vancouver Grizzlies", "Washington Wizards","Washington Bullets") 

data_map <- data.frame(Nom_equipe = nom_equipe) 

data_map$Lat=c(33.75693,42.37128,40.68631,42.88760,35.22480,41.90392,41.88134,41.93095,41.49649,43.64387,39.74935,42.34124,37.76817,29.75085,39.76417,38.58029,34.04314,34.04314,35.13821,25.78156,43.04518,44.98418,40.688310,25.715889,40.769163,29.949602,29.949602,40.751133,35.464039,28.539707,39.901832,33.446499,45.532140,38.580932,29.427794,34.044002 ,47.622624, 43.64360,40.76858,49.28841, 38.89831,38.89831 ) 
data_map$Lat = as.numeric(as.character(data_map$Lat))

data_map$Long=c(-84.39215,-71.05998,-73.94432,-78.87063,-80.83986,-87.62480,-87.67376,-87.99805,-81.68848,-79.37841,-105.00773,-83.05530,-122.38767,-95.36206,-86.15543,-121.49972,-118.26726,-118.26726,-90.05059,-80.18697,-87.91738,-93.27690,-73.97541,-80.279204,-111.900518,-90.082026,-90.081925,-73.993272,-97.514831,-81.383502,-75.171531,-112.070782,-122.666472,-121.499249,-98.437227,-118.266694,-122.353855,-79.37909,-111.90107,-123.10644, -77.02083, -77.02083)
data_map$Long = as.numeric(as.character(data_map$Long))

#pour sélectionner les années
annees = data_base$Annee

#Definir le contenu pour siege sociale 
content <- paste(sep = "<br/>",
                 "<b><a href='https://www.beinsports.com/france/nba/'>Site officiel NBA</a></b>",
                 "l'Olympic Tower au 645, de la 5 Avenue à New York" 
)


# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "NBA Menu",
                    tags$li(class="dropdown",tags$a(href="https://twitter.com/NBA?ref_src=twsrc%5Egoogle%7Ctwcamp%5Eserp%7Ctwgr%5Eauthor", icon("twitter"), "Twitter", target = "_blank")),
                    tags$li(class="dropdown",tags$a(href="https://www.beinsports.com/france/nba/?gr=www",  "Site Officiel", target = "_blank")),
                    tags$li(class="dropdown",tags$a(href="https://www.instagram.com/nba/?hl=fr",icon("instagram") , "Instagram", target = "_blank")),
                    dropdownMenu(type="message", messageItem(from="Notification",message="Bienvenue sur notre application WEB!",icon=icon("envelope-open")))),
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
        shinyDashboardThemes(
            theme = "grey_dark"
        ),
        tabItems(
            tabItem(tabName="TER",
                    fluidPage(HTML("<h1 style=\"color : white ; background-color : lightgrey ; text-align : center ; border-radius : 10px \"> Présentation de notre TER</h1>"),setBackgroundImage(src = "https://images5.alphacoders.com/662/662367.jpg", shinydashboard = TRUE),
                              tags$style(HTML("
    .tabbable > .nav > li > a {background-color: #32444F ; color:white}")),
                              tabsetPanel(
                                  tabPanel("Présentation de notre TER",
                                           fluidRow(style='margin:3px;'),
                                           box(title='Choix du sujet',status = "danger",solidHeader = TRUE, p('Lors du second semestre de notre licence 3 en Mathématiques et Informatique Appliquées Aux Sciences Humaines et Sociales, nous avons pu choisir un sujet de travail encadré par un de nos enseignants. Pour notre part, Clémence, Margaux, Marie et Oriane, nous avons choisi de réaliser une application WEB à l’aide du logiciel R Shiny. Effectivement, notre attrait pour l’informatique nous a poussé à choisir ce sujet.  Aussi, nous sommes toutes passionnées de sport, il nous a donc paru important d’introduire cette thématique dans notre projet. Nous avons choisi d’analyser une compétition connue dans le monde entier : la NBA. Grâce aux nombreuses bases de données trouvées au sujet de la National Basketball Association, nous avons pu nous intéresser aux statistiques des clubs et des joueurs présents au sein de cette compétition entre 1978 et 2015.')),
                                           box(title='Choix de la base de données',status = "primary",solidHeader = TRUE,p('Afin de pouvoir réaliser notre application, nous avons recherché une base de données intéressante à étudier. Il semblait évident que le thème qui nous allions aborder serait le sport ou la santé puisque nous sommes passionnées de ces sujets. Au départ, nous avons voulu analyser l’impact des drogues sur la santé mais ne disposant pas de base de données adéquates, nous nous sommes focalisées définitivement sur la thématique du sport. Nous avons d’abord voulu étudier un sport au niveau local, et plus particulièrement le Stade Rennais, mais le travail était trop compliqué pour pouvoir récupérer sous un format correct notre base de données. Ainsi, nous avons décidé d’opter pour une compétition internationale de basketball : la NBA. La recherche des données à analyser s’est donc avéré plus longue et compliquée que ce que nous avions prévu. Au final, nous avons sélectionné deux bases de données : la première au sujet des clubs et la seconde à propos des différents joueurs de basketball présents lors d’une ou plusieurs saisons de NBA.')), 
                                           box(title='Travail de recherche',status = "primary",solidHeader = TRUE, p('Ensuite, notre travail de compréhension du logiciel R Shiny a pu réellement débuter. Notre objectif initial était de créer une application avec des extraits de vidéos sur le basket et des pages interactives. Nous souhaitions reprendre un modèle d’application déjà existant et l’adapter à nos propres données. Cependant, cette tâche s’est révélée très complexe puisque le code R comportait de l’HTML, du CSS mais aussi des liens pour accéder à des vidéos provenant d’internet. N’ayant pas encore intégré toutes les particularités du fonctionnement du logiciel, à ce moment donné, notre niveau de compréhension de R Shiny était insuffisant pour mettre ce code en place. Nous avons donc décidé de reprendre les bases, en réalisant de petites applications simples, en regardant des vidéos et tutoriels afin de comprendre le fonctionnement de ce logiciel, pour ensuite complexifier le code. C’est ainsi que nous avons commencer à réaliser notre application. Nous avions pour but de réaliser des statistiques sur les clubs de basketball mais aussi sur les joueurs eux-mêmes.')), 
                                           box(title='Structure de la page',status = "primary",solidHeader = TRUE,p('Nous avons créé la structure de l’application avec quatres pages : la première concernant pour l\'accueil, la seconde concernant les clubs, la toisième pour les informations au sujet des joueurs et la dernière afin de conclure notre TER. Dans la deuxième page, l\'onglet des équipes affiche les clubs présents lors de cette compétition. Nous avons donc réalisé un datatable pour présenter ces variables. Puis, dans l\'onglet suivant, nous voulions que l’utilisateur puisse sélectionner un club et une année pour pouvoir accès aux différents joueurs jouant dans le club choisi. La présentation sous forme de datatable avec des barres de recherches nous a semblé la plus esthétique et adaptée. Ensuite, le résumé statistique nous permet de comprendre les données et les graphiques de mieux les visualiser. La prise en main du package ggplot a aussi été un challenge puisque nous étions en autonomie. Enfin, la création d’une carte localisant les différents clubs nous a permis d’avoir une application un peu plus ludique pour l’utilisateur.Par ailleurs, la page sur les joueurs est un peu moins exhaustive puisque nous avons effectué des statistiques sur les caractéristiques générales. Par exemple, nous avons réalisé des graphiques au sujet de la main de tir mais aussi de la taille des joueurs.Dans un autre onglet, nous avons affiché les informations personnelles de chaque joueur grâce à une barre de recherche.')),
                                           box(title='Visualisation de l\'application',status = "primary",solidHeader = TRUE,p('Après avoir réalisé l’ensemble de notre application, nous avons voulu faciliter son utilisation et rendre la rendre ergonomique et esthétique. Nous nous sommes focalisées sur les détails tels que la mise en place d’icônes pour accéder aux différents onglets mais aussi le thème choisi. De plus, la recherche de la mise en place d’un fond d’écran dynamique nous a permis de mettre l’application à notre goût et à notre image.')),
                                           box(title = 'Conclusion',status = "danger",solidHeader = TRUE,('En conclusion, la réalisation de notre application WEB à l’aide de R Shiny nous a permis de comprendre le fonctionnement d’un nouveau logiciel en totale autonomie. La pertinence des données présentées a été primordiale pour nous quatre. Notre application témoigne donc de notre passion partagée pour le sport mais aussi pour l’informatique.'))),
                                  tabPanel("Présentation de la NBA", fluidRow(style='margin:3px;'),box(title="Cliquez ici pour avoir des informations sur la NBA",solidHeader=TRUE,status="primary",collapsible=TRUE,collapsed=TRUE,imageOutput("logo_info", height = "225px"), HTML("Création : 6 juin 1946 à New York</br > Caractéristique : Principale ligue de Basketball au monde</br >Siège social : New York </br > Nombre d'équipes lors de la saison actuelle : 30</br >Statut des participants : professionnel </br > Champion acutel : Milwaukee Bucks")), leafletOutput("map_nba")),
                                  tabPanel("Présentation des créateurs de l'application",fluidRow(style='margin:3px;'),box(imageOutput("margaux")),box(imageOutput("oriane")),box(imageOutput("marie")),box(imageOutput("clemence"))),
                              ))),
            tabItem(tabName="dunks", 
                    fluidPage(HTML("<h1 style=\"color : white ; background-color : lightgrey ; text-align : center ; border-radius : 10px\">Dix dunks impressionnants !</h1>"), HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/uVuYt7mJIfc" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'))
            ),
            tabItem("Clubs",
                    tabsetPanel( #diviser le tableau principal en onglets
                        tabPanel('Les clubs de la NBA', HTML("<h1 style=\"color : white ; background-color : lightgrey ; text-align : center ; border-radius : 10px\">Les différents clubs de la NBA</h1>"),
                                 HTML("<p style=\" font-size:large ; color : white ; background-color : lightgrey ; text-align : center ; border-radius : 10px\"> Voici la liste des clubs de Basketball de la NBA depuis 1978 jusqu'à 2015:"),dataTableOutput('noms_clubs')),
                        tabPanel('Les joueurs des différents clubs',fluidRow(style='margin:3px;'),box(selectInput('varcj','Choisissez un club :',choices=list("Atlanta Hawks", "Boston Celtics","Brooklyn Nets","Buffalo Braves", "Charlotte Hornets", "Chicago Hustle", "Chicago Bulls","Chicago Bruins", "Cleveland Cavaliers","Dallas Mavericks", "Denver Nuggets", "Detroit Pistons", "Golden State Warriors","Houston Rockets","Indiana Pacers","Kings of Sacramento","Los Angeles Clippers","Los Angeles Lakers","Memphis Grizzlies","Miami Heat", "Milwaukee Bucks","Minnesota Timberwolves","Brooklyn Nets","New Orleans Hurricanes","New Orleans Jazz Roster ans Stats","New Orleans/Oklahoma City","New Orleans Pelicans","New York Knicks","Oklahoma City Thunder","Orlando Magic","Philadelphia 76ers", "Phoenix Suns","Portland Trail Blazers","Sacramento Kings","San Antonio Spurs", "San Diego Clippers","Seattle SuperSonics", "Toronto Raptors","Utah Jazz","Vancouver Grizzlies", "Washington Wizards","Washington Bullets"))),box(sliderInput("annee_club", label = h3("Années"), min = 1978,max = 2015, value = 1978)),verbatimTextOutput("textcj"),dataTableOutput("varcj")),
                        tabPanel('Statistiques',fluidRow(style='margin:3px;'),verbatimTextOutput("summary_text"),dataTableOutput('summary')),
                        tabPanel('Efficacité des clubs',fluidRow(style='margin:3px;'), selectInput('varclub','Choisissez un club :',choices=list("Atlanta Hawks", "Boston Celtics","Brooklyn Nets","Buffalo Braves", "Charlotte Hornets", "Chicago Hustle", "Chicago Bulls","Chicago Bruins", "Cleveland Cavaliers","Dallas Mavericks", "Denver Nuggets", "Detroit Pistons", "Golden State Warriors","Houston Rockets","Indiana Pacers","Kings of Sacramento","Los Angeles Clippers","Los Angeles Lakers","Memphis Grizzlies","Miami Heat", "Milwaukee Bucks","Minnesota Timberwolves","Brooklyn Nets","New Orleans Hurricanes","New Orleans Jazz Roster ans Stats","New Orleans/Oklahoma City","New Orleans Pelicans","New York Knicks","Oklahoma City Thunder","Orlando Magic","Philadelphia 76ers", "Phoenix Suns","Portland Trail Blazers","Sacramento Kings","San Antonio Spurs", "San Diego Clippers","Seattle SuperSonics", "Toronto Raptors","Utah Jazz","Vancouver Grizzlies", "Washington Wizards","Washington Bullets")), 
                                 fluidRow(box(plotOutput('varclub')),
                                          box(title = "Interprétation", HTML("La courbe représente l'évolution de l'efficacité de l'équipe en fonction de l'annéé. À CONTINUER"))),
                                 fluidRow(box(plotOutput('nbpaniers')),
                                          box(title = "Interprétation", HTML("Pour une année il y a plusieurs points puisqu'un point correspond à chaque panier marqué par un joueur.
                                                                            La courbe bleue est la tendance du nombre de paniers pour chaque année. Nous remarquons que certaines valeurs sont négatives : ce sont des valeurs absurdes. La courbe bleue représente donc la moyenne du nombre de paniers par équipe en fonction des annnées.")))),
                        tabPanel('Régression',selectizeInput('var_reg','Choisissez une variable à expliquer',choices=c("NbMatchs", "MinutesJouees", "NbPaniers", "PerfParMin", "EfficaciteTir", "Tentative3pts", "TentativesLancersFrancs", "PrctRebondOffensif", "PrctRebondDefensif", "NbTotalRebonds", "ControleBallon", "BallonsVoles", "BlocksParJeu", "EfficaciteTirEquipe"),multiple=FALSE),selectizeInput('var_explicative','Choisissez une variable explicative',choices=c("NbMatchs", "MinutesJouees", "NbPaniers", "PerfParMin", "EfficaciteTir", "Tentative3pts", "TentativesLancersFrancs", "PrctRebondOffensif", "PrctRebondDefensif", "NbTotalRebonds", "ControleBallon", "BallonsVoles", "BlocksParJeu", "NbTirs", "JoueurID", "EfficaciteTirEquipe"),multiple=FALSE),verbatimTextOutput(outputId = "RegSum"),verbatimTextOutput(outputId = "IndPrint"),verbatimTextOutput(outputId = "DepPrint")),
                        tabPanel('Carte des clubs', HTML("<h3 style=\"color : white ; background-color : lightgrey ; text-align : center ; border-radius : 10px\">Carte des clubs</h3>"),
                                 leafletOutput("map_points"))
                    )
            ),
            tabItem("Joueurs",
                    fluidPage(HTML("<h1 style=\"color : white ; background-color : lightgrey ; text-align : center ; border-radius : 10px\">Caratéristiques des joueurs</h1>"),
                              tabsetPanel(
                                  tabPanel('Caractéristiques générales', 
                                           fluidRow(style='margin:3px;'),
                                           fluidRow(
                                               box(title = "Diagramme concernant la manualité des joueurs", status = "primary", solidHeader = TRUE, plotOutput('varpie')), 
                                               box(title = "Interprétation", status = "primary", solidHeader = TRUE, HTML('Nous remarquons sur ce diagramme que 94% des joueurs de la NBA sont droitiers. D\'après internet, 90% des humains seraient droitiers, 10% seraient gauchers et 1% seraient ambidextres. Les joueurs de la NBA sont donc plutôt représentatifs de la population mondiale concernant leur préférence manuelle.'))),
                                           fluidRow( 
                                               box(title = "Histogramme sur la répartition de la taille des joueurs", status = "danger", solidHeader = TRUE, plotOutput('varhisto')),
                                               box(title = "Interprétation", status = "danger", solidHeader = TRUE, HTML('L\'histogramme représente donc la répartition des joueurs en fonction de leur taille. La courbe en pointillés bleue représente la taille moyenne des joueurs de basket dans notre base de données. La courbe en pointillés rouge représente quant à elle la taille moyenne des hommes aux États-Unis. Cet histogramme met donc en avant le fait que les hommes jouant au baksets aux États-unis sont beaucoup plus grand que le reste de la population.')))),
                                  tabPanel('Caractéristiques d\'un joueur', 
                                           fluidRow(style='margin:3px;'),
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
                    fluidPage(HTML("<h1 style=\"color : white ; background-color : lightgrey ; text-align : center ; border-radius : 10px\">Rétrospective de notre TER</h1>"),
                              uiOutput("liste_acquis"),uiOutput("liste_difficultes")))
        )
    )
    
)



# Define server logic required to draw a histogram
server <- function(input, output) {
    #affichage du logo pour la présentation de la NBA
    output$logo_info <- renderImage({
        list(src="../Data/nba_carte.png",alt="logo NBA",width=350,height=200,style='position : relative')
    },deleteFile=FALSE)
    
    #Sorti de l'onglet club
    output$noms_clubs <- renderDataTable(unique(data_base[,1:2]),rownames=FALSE)   
    output$textcj <- renderText({
        paste("Voici les joueurs ayant été dans le club", input$varcj,"pour l'année",input$annee_club)
    })
    output$varcj <- renderDataTable(data_base[which(data_base$Equipe==input$varcj & data_base$Annee==input$annee_club),][,c(1,3)],rownames=FALSE)
    
    #carte présentation
    output$marie <- renderImage({
        list(src="../Data/Pokémon Marie .png",alt="marie",width=300,height=400,style='position : relative')
    },deleteFile=FALSE)
    output$clemence <- renderImage({
        list(src="../Data/Pokemon Clemence.jpg",alt="clemence",width=300,height=400,style='position : relative')
    },deleteFile=FALSE)
    output$oriane <- renderImage({
        list(src="../Data/Pokemon Oriane.png",alt="oriane",width=300,height=400,style='position : relative')
    },deleteFile=FALSE)
    output$margaux <- renderImage({
        list(src="../Data/Pokémon Margaux.png",alt="margaux",width=300,height=400,style='position : relative')
    },deleteFile=FALSE)
    
    #Résumé statistique
    output$summary <- renderDataTable(summary(data_base),rownames=FALSE)
    output$summary_text <- renderText({"Ci-dessous le résumé statistique de la base de données 'data'.
On y retrouve des données sur des clubs que l'on calcul grâce aux joueurs. 
Par exemple, on peut évaluer l'efficacité des tirs des joueurs d'un club ou encore l'âge moyen des joueurs d'un club entre 1978 
et 2015."})
    
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
    output$nbpaniers <- renderPlot({
        data_base1 <- data_base %>% dplyr::filter(Equipe==input$varclub)
        data_base$NbPaniers[which(data_base$Equipe==input$varclub)]
        #        plot(EfficaciteTirEquipe ~ Annee, data = data_base1)
        ggplot(data_base1)+aes(x=Annee,y=NbPaniers)+
            geom_point()+geom_smooth()+theme_bw()
        
    })
    
    #Carte des clubs 
    output$map_points <- renderLeaflet({
        
        leaflet(data=data_map) %>%
            addTiles() %>%
            addCircleMarkers(lng = ~Long, lat = ~Lat,  popup = ~ paste("Nom equipe:",'<b>',nom_equipe, '</b>'))
        
    })
    #Carte du siege sociale
    output$map_nba <- renderLeaflet({
        leaflet() %>% addTiles() %>%
            addPopups(-73.97369, 40.75961, content,
                      options = popupOptions(closeButton = FALSE)
            )
    })
    
    #Camembert
    output$varpie <- renderPlot({ 
        df <- data.frame(
            group <- c("Left", "Left/Right", "Right"), 
            value <- c(283, 1, 4400))
        pie <- ggplot(df, aes(x="", y=value, fill=group)) + # Fonction aes qui nous permet d'indiquer quelle donnée nous voulons représenter
            xlab ("") +
            ylab("") +
            geom_bar(width = 1, stat = "identity") + # Geom sont des outils de représentation graphique, ici va remplir l'intérieur du diagramme, si vous souhaitez que les hauteurs des barres représentent des valeurs dans les données, utilisez stat="identity" 
            coord_polar(theta = "y") + # Variable pour l'angle, les camemberts sont des diagrammes à barres empilées en coordonnées polaires.
            scale_fill_brewer(type = "seq", direction = 1, palette="Blues") + # Seq = de type sequence, direction va donner le sens dans lequel les couleurs vont être affichées, palette va donner les couleurs
            geom_text(aes(y = value/4 + c(0, cumsum(value)[-length(value)]), # y va diviser la part en parties, va permettre de placer le texte (pas très bien compris)
                          label=paste(group,round((value/sum(value))*100), "%")))+ # Affichage des pourcentages à l'intérieur du diagramme
            theme_minimal() # Pour mettre le thème du pie
        pie
    })
    
    #Histogramme
    output$varhisto <- renderPlot({
        histo <- ggplot(data_players, aes(x = as.numeric(as.character(data_players$Taille)))) + # Fonction aes qui nous permet d'indiquer quelle donnée nous voulons représenter
            geom_histogram(color="black", fill="white") + # On choisit la couleur de l'histogramme
            xlab ("Taille des joueurs") + # Nom des x 
            ylab("Nombre de joueurs") + # Nom des y 
            geom_vline(aes(xintercept=mean(as.numeric(as.character(data_players$Taille)))),
                       color="blue", linetype="dashed", size=1) + # Ajout de la ligne pour la moyenne de nos données 
            geom_vline(aes(xintercept=1.754),
                       color="red", linetype="dashed", size=1) # Ajout de la ligne pour la moyenne des hommes
        histo
    })
    
    #régression linéaire
    lm1 <- reactive({lm(paste0(input$var_reg) ~ paste0(input$var_explicative), data = data_base)})
    
    output$DepPrint <- renderPrint({input$DepVar})
    output$IndPrint <- renderPrint({input$IndVar})
    output$RegSum <- renderPrint({summary(lm1())})
    
    #liste rétrospective
    output$liste_acquis <- renderUI(
        fluidRow(
            box(title = "Apprentissage de nouvelles compétences", status = "success", solidHeader = TRUE, HTML("&#x1F5F9; Réalisation d'une application WEB à l'aide du logiciel R Shiny<br>&#x1F5F9 Apprentissage de R Shiny <br><p>&#x1F5F9; 
            Packages : <ul><li>shinydashboard</li><li>dplyr</li><li>ggplot</li><li>leaflet</li><li>DT</li><li>shinyWidgets</li><li>dashboardthemes</li></ul>&#x1F5F9; Autonomie et responsabilité<br>&#x1F5F9; Efficacité<br>&#x1F5F9; Créativité<br>&#x1F5F9; Travail de groupe<br>&#x1F5F9; Recherche"))))
    output$liste_difficultes <- renderUI(
        fluidRow(box(title = "Difficultés rencontrées lors de notre TER", status = "danger", solidHeader = TRUE, HTML("&#x1F5F9; Compréhension du fonctionnement du logiciel R Shiny<br>&#x1F5F9; Recherche d'une base de données adaptée à nos envies<br>&#x1F5F9; Trouver une image de fond d'écran publique et de bonne qualité<br>&#x1F5F9; Prise en main de ggplot<br>&#x1F5F9; Adaptation de l'application suite à la modification du thème<br>&#x1F5F9; Problème de PUSH et PULL avec GIT<br>&#x1F5F9; Difficulté à se projeter et donc à établir le planning")),
                 box(title = "Limites de notre application", status = "danger", solidHeader = TRUE,HTML("&#x2610; Mise en place d'un fond d'écran animé<br>&#x2610; Affichage des derniers tweets concernant la NBA (en temps réel)<br>&#x2610; Mise en place d'un dossier CSS<br>&#x2610; Mise en place d'une régression dynamique<br>"))))
}
# Run the application 
shinyApp(ui = ui, server = server)