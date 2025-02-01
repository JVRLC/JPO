#### Installer les packages nécessaires####
if (!require("shiny")) install.packages("shiny")
if (!require("shinythemes")) install.packages("shinythemes")
if (!require("DT")) install.packages("DT")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("plotly")) install.packages("plotly")
if (!require("shinydashboard")) install.packages("shinydashboard")
if (!require("leaflet")) install.packages("leaflet")
if(!require("wordcloud2"))install.packages("wordcloud2")
if(!require("tm"))install.packages("tm")
#### Charger les librairies####
library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(plotly)
library(shinydashboard)
library(leaflet)
library(tidyr)
library(dplyr)
library(wordcloud)
library(tm)
#### Données###
#### module####
#### Liste des modules####
modules <- c(
  "Utilisation avancée d’outils de reporting",
  "Systèmes d’information décisionnels",
  "Technologies web",
  "Programmation statistique automatisée",
  "Algèbre linéaire",
  "Tests d’hypothèses pour l’analyse bi-variée",
  "Anglais professionnel",
  "Communication organisationnelle et professionnelle",
  "Les données de l’environnement entrepreneurial  économique pour l’aide à la décision",
  "Programmation",
  "Programmation VBA",
  "Techniques de sondage  méthodologie d’enquête",
  "Projet Personnel Professionnel",
  "Recueil  analyse de données par échantillonnage ou plan d’expérience",
  "Intégration de données dans un datawarehouse",
  "Description et prévision de données temporelles",
  "Conformité réglementaire pour analyser des données",
  "Démarche portfolio",
  "Intelligence Artificielle  science des données",
  "Automatisation  test en programmation",
  "Méthodes factorielles",
  "Classification automatique",
  "Anglais scientifique et argumentation",
  "Communication scientifique et argumentation",
  "Exploration  valorisation de la donnée dans un cadre juridique et économique",
  "Projet Personnel  Professionnel",
  "Modèle linéaire",
  "Expliquer ou prédire une variable quantitative à partir de plusieurs facteurs",
  "Reporting d’une analyse multivariée",
  "Démarche portfolio",
  "Stage",
  "Big Data : enjeux, stockage et extraction",
  "Méthodes statistiques pour le Big Data",
  "Anglais pour la communication d’entreprise",
  "Communication pour le management",
  "Apprentissage statistique pour l’IA",
  "Modélisation statistique pour les données complexes  le Big Data",
  "Portfolio",
  "Stage"
)
# Création du corpus et nettoyage du texte
corpus <- Corpus(VectorSource(modules))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("fr"))

# Convertir le corpus en un format que wordcloud2 peut comprendre
word_freq <- table(unlist(strsplit(tolower(paste(modules, collapse = " ")), "\\s+")))
word_freq <- sort(word_freq, decreasing = TRUE)
word_freq_df <- data.frame(word = names(word_freq), freq = as.numeric(word_freq))


# Convertir le corpus en un format que wordcloud2 peut comprendre
word_freq <- table(unlist(strsplit(tolower(paste(modules, collapse = " ")), "\\s+")))
word_freq <- sort(word_freq, decreasing = TRUE)
word_freq_df <- data.frame(word = names(word_freq), freq = as.numeric(word_freq))
### Création dataFrame Activés d'enseignement ####
semestres <- paste("semestre", 1:6, sep = " ")
nombre_dheures_Enseignement <- c(400, 380, 400, 200, 270, 150)
nombre_dheures_SAE <- c(100, 114, 148, 70, 108, 60)
nombre_dheures_tp <- c(10, 35, 30, 20, 30, 25)
nombre_dheures_Projet_Tutore <- c(10, 35, 30, 20, 30, 25)

#  transformation en format long
data <- data.frame(
  activité = c("Enseignement", "SAE", "TP", "Projet_Tutore"),
  rbind(
    nombre_dheures_Enseignement,
    nombre_dheures_SAE,
    nombre_dheures_tp,
    nombre_dheures_Projet_Tutore
  )
)
colnames(data) <- c("activité", semestres)

# Transformation en format long pour ggplot
data_long <- data %>%
  pivot_longer(
    cols = -activité,
    names_to = "semestre",
    values_to = "heures"
  ) %>%
  mutate(semestre = factor(semestre, levels = semestres))

#### dataframe UE ####
UE1 <- c("Traiter des données à des fins décisionnelles", "Analyser statistiquement les données", "Valoriser une production dans un contexte professionnel")
UE2 <- c(UE1, "Modéliser les données dans un cadre statistique")
UE3 <- UE2
df <- data.frame(
  UE = c(rep("UE1", length(UE1)), rep("UE2", length(UE2)), rep("UE3", length(UE3))),
  Objectifs = c(UE1, UE2, UE3),
  StringsAsFactors = FALSE
)

cours <- data.frame(
  Cours = c("Data Science", "Statistiques", "Visualisation", "Machine Learning", "BDD"),
  Heures = c(60, 80, 45, 70, 50),
  Type = c("Théorie", "Théorie", "Pratique", "Pratique", "Théorie"),
  Description = c("Fondamentaux des données", "Analyses statistiques", "Création de dashboards", 
                  "Modèles prédictifs", "Gestion de bases de données")
)



#### dataframe gris d'évaluation ####
# Charger la bibliothèque dplyr si nécessaire
library(dplyr)

# Création du dataframe
evaluation_niveau<- data.frame(
  Critère_dévaluation = c(
    "Niveau général",
    "Résultats académiques de la dernière année d’enseignement suivie",
    "Acquisition de la démarche scientifique",
    "Méthode de travail",
    "Qualité de l’expression écrite",
    "Concentration en classe",
    "Implication",
    "Autonomie",
    "Capacité à réussir dans la formation",
    "Motivation",
    "Adéquation de la formation avec le projet professionnel",
    "Intérêt pour le domaine de l’informatique"
  ),
  Importance = c(
    "Essentiel",
    "Important",
    "Très important",
    "Important",
    "Important",
    "Essentiel",
    "Très important",
    "Très important",
    "Essentiel",
    "Très important",
    "Très important",
    "Important"
  ),
  Eléments_évalués = c(
    "Notes et appréciations en mathématiques",
    "Notes du candidat",
    "Notes et appréciations du candidat",
    "Appréciations du candidat",
    "Appréciations du candidat, lettre de motivation",
    "Appréciations du candidat",
    "Appréciations du candidat",
    "Appréciations du candidat",
    "Notes et appréciations du candidat, entretien éventuel",
    "Appréciations du candidats, lettre de motivation, participation aux Journées Portes Ouvertes du BUT SD ou prise de contact à distance",
    "Lettre de motivation",
    "Lettre de motivation, entretien, notes et appréciations éventuelles en spécialité NSI"
  )
)



#### Interface utilisateur ####
ui <- fluidPage(

  theme = shinytheme("flatly"),
  
  headerPanel(
    tags$div(
      tags$img(src = "Logo-SD-Nice-768x768.png", height = 100, style = "margin-right:20px;"),
      "BUT Science des Données - IUT Nice Côte d'Azur",
      style = "display: flex; align-items: center; padding: 20px;"
    )
  ),
  
  navlistPanel(
    widths = c(2, 10),
    "Menu",
    tabPanel("Présentation",
             fluidRow(
               column(8,
                      h3("Bienvenue dans le BUT Science des Données Nice Côte d'Azur"),
                      tags$div(
                        style = "text-align: justify;",
                        p(HTML("Notre formation <b>pluridisciplinaire</b> combine des heures d'enseignement <b>théorique</b>, <b>des travaux dirigés (TD) </b>, des <b>projets tutorés (TP) </b> et des <b>activités de Situation d'Apprentissage et d'Évaluation (SAE)</b>.")),
                        p(HTML("Elle est structurée sur <b>6 semestres</b>, et permet à nos étudiants de développer des compétences pratiques et théoriques dans des domaines variés.")),
                        p(HTML("Grâce à un programme bien équilibré, chaque semestre est dédié à une progression continue dans les domaines de l'enseignement, des activités pratiques, et des projets en groupe, tout en encourageant l'autonomie des étudiants dans leur parcours académique.")),
                        p("Quelques cours enseignés en BUT SD :"),
                        tags$ul(
                          tags$li("Analyse statistique"),
                          tags$li("Programmation (R, Python)"),
                          tags$li("Visualisation de données (Power Bi)"),
                          tags$li("Big Data "),
                          tags$li("Économie "),
                          tags$li("Communication ...")
                        )
                      ),
                      wordcloud2Output("wordcloud")               ),
               column(4,
                      wellPanel(
                        h4("Points clés"),
                        tags$div(
                          style = "padding:10px;",
                          tags$span(icon("graduation-cap"), " Diplôme : BAC+3", br()),
                          tags$span(icon("briefcase"), " Insertion professionnelle : 95%", br()),
                          tags$span(icon("calendar-alt"), " Possibilité de faire un stage dès la 2e année")
                        )
                      )
               )
             )
    ),
    
    tabPanel("Programme",
             h3("Répartition des cours"),
             fluidRow(
               column(12, plotOutput("repartitionCours")),
               column(10,DTOutput("rep_tab"))
             )
    ),
    
    tabPanel("Statistiques",
             h2("Les chiffres globaux d'accès à cette formation en 2024"),
            
             fluidRow(
               valueBoxOutput("total_candidats", width = 4),
               valueBoxOutput("prop", width = 4),
               valueBoxOutput("intégrer", width = 4),
               
               column(12,DTOutput("tab_ev"))
               
             ),
             br(),
             
             
    ),
    
    tabPanel("Contacts",
             h3("Nous trouver"),
             tags$div(
               style = "padding:20px;",
               tags$p(icon("map-marker-alt"), " Adresse : 650, Route des Colles 06560 Valbonne FRANCE"),
               tags$p(icon("envelope"), " Email : Michel.SOLIVERES@univ-cotedazur.fr"),
               tags$p(icon("phone"), " Tél : +33 4 89 15 32 61"),
               tags$br(),
               leafletOutput("carte")
             )
    )
  )
)

#### Serveur ####

server <- function(input, output) {
  observe({
    showModal(
      modalDialog(
        title = "Bienvenue dans notre BUT Science des Données Nice  Sophia Antipolis",
        "Nous sommes très ravis de vous accueillir aujourd'hui dans notre établissement",
        easyClose = TRUE,
        footer = NULL
      )
    )
  })
  
  output$wordcloud <- renderWordcloud2({
    wordcloud2(word_freq_df, size = 0.5)
  })
  
  output$repartitionCours <- renderPlot({
    ggplot(data_long, aes(x = semestre, y = heures, fill = activité)) +
      geom_col(position = "stack", color = "white") + 
      scale_fill_manual(values = c("#0073e6", "#33cc33", "#ffcc00", "#ff6666")) + 
      labs(title = "Répartition des Heures par Activité", 
           x = "Semestre", 
           y = "Nombre d'heures") +
      theme_minimal(base_size = 15) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
            axis.title = element_text(size = 14),
            plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            legend.position = "bottom",
            legend.title = element_blank())
  })
  output$rep_tab <- renderDT({
    data
  })
  
  output$total_candidats <- renderValueBox({
    valueBox(490, "candidats ont postulé à la formation", icon = icon("users"), color = "blue")
  })
  
  output$prop <- renderValueBox({
    valueBox(246, "candidats ont pu recevoir une proposition d'admission", icon = icon("smile"), color = "green")
  })
  
  output$intégrer <- renderValueBox({
    valueBox(25, "candidats ont pu intégrer à la formation", icon = icon("check-circle"), color = "orange")
  })
  
  output$tab_ev <- renderDT({
    evaluation_niveau
  })
  
  output$carte <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(lng = 7.051524, lat = 43.61769, popup = "650, Route des Colles 06560 Valbonne, France")
  })
}

# Lancer l'application
shinyApp(ui, server)