## Partie 3 : Application RShiny

## Contenus

- [x] Script de l'application
- [x] Liens de l'application


##  Script de l'application
```r
# installation des packages nécessaires
install.packages("shiny")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("leaflet")
install.packages("DT")
install.packages("shinydashboard")
install.packages("shinymanager")
library(rsconnect)

# Charger les bibliothèques nécessaires
library(shiny)
library(ggplot2)
library(leaflet)
library(dplyr)
library(DT)
library(shinydashboard)
library(shinymanager)

# Charger les données
data <- read.csv("C:/Users/33622/OneDrive/Documents/études_sup/Projet_R/Shiny/DF_Finale.csv",sep=",",dec=".",header=T)



# Définir l'ordre des étiquettes DPE en tant que facteur
data$Etiquette_DPE <- factor(data$Etiquette_DPE, 
                             levels = c("A", "B", "C", "D", "E", "F", "G"))

credentials <- data.frame(
  user = c("Anthony"),     # Nom d'utilisateur
  password = c("Sardellitti"),  # Mot de passe
  stringsAsFactors = FALSE
)

# Interface utilisateur
ui <- secure_app(
  dashboardPage(
  dashboardHeader(title = "Analyse DPE"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Contexte", tabName = "context", icon = icon("info-circle")),
      menuItem("Visualisations", tabName = "visualizations", icon = icon("chart-bar")),
      menuItem("Carte interactive", tabName = "map", icon = icon("map"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # Onglet 1: Contexte
      tabItem(tabName = "context",
              fluidPage(
                titlePanel("Contexte et données"),
                fluidRow(
                  column(4,
                         img(src = "https://sondages.aviso.fr/Media/1/LOGO_house1_ProjetR.jpg", width = "20%"),
                         
                         h3("Diagnostic de Performance Energétique (DPE)"),
                         p("Le DPE permet d'estimer la consommation énergétique d'un logement"),  
                         dataTableOutput("table")
                  ),
                  column(8,
                         h4("Indicateurs clés"),
                         infoBoxOutput("kpi1"),
                         infoBoxOutput("kpi2"),
                         infoBoxOutput("kpi3"),
                  )
                )
              )
      ),
      
      # Onglet 2: Visualisations
      tabItem(tabName = "visualizations",
              fluidPage(
                titlePanel("Visualisations"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("type_bat", "Type de bâtiment :", 
                                choices = unique(data$Type_bâtiment)),
                    checkboxGroupInput("dpe_filter", "Filtrer par étiquette DPE :", 
                                       choices = levels(data$Etiquette_DPE), 
                                       selected = levels(data$Etiquette_DPE)),
                    sliderInput("conso_filter", "Filtrer par consommation d'énergie (kWh)", 
                                min = min(data$Conso_5_usages_é_finale, na.rm = TRUE),
                                max = max(data$Conso_5_usages_é_finale, na.rm = TRUE),
                                value = c(min(data$Conso_5_usages_é_finale, na.rm = TRUE),
                                          max(data$Conso_5_usages_é_finale, na.rm = TRUE))),
                    selectInput("logement_filter", "Filtrer par type de logement :", 
                                choices = unique(data$Logement),
                                selected = unique(data$Logement)[1])
                  ),
                  mainPanel(
                    plotOutput("barDPE"),
                    hr(),
                    plotOutput("boxplotDPE"),
                    hr(),
                    plotOutput("scatterConsoSurface"),
                    hr(),
                    plotOutput("pieChart")
                  )
                )
              )
      ),
      
      # Onglet 3: Carte interactive
      tabItem(tabName = "map",
              fluidPage(
                titlePanel("Carte des des logements en fonction de leur étiquettes DPE"),
                leafletOutput("cartePassoires", height = 600),
                checkboxGroupInput("filter_dpe", "Choisir étiquette DPE :", 
                                   choices = levels(data$Etiquette_DPE), selected = "F")
              )
      )
    )
  )
)
)

# Serveur pour générer les graphiques et la carte
server <- function(input, output) {
  # Authentification
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  
  # KPI calculs
  output$kpi1 <- renderInfoBox({
    infoBox(HTML("Total <br> Logements"), nrow(data), icon = icon("home"))
  })
  output$kpi2 <- renderInfoBox({
    passoires <- nrow(data %>% filter(Etiquette_DPE %in% c("F", "G")))
    infoBox(HTML("Passoires <br> thermiques"), passoires, icon = icon("exclamation-triangle"), color = "red")
  })
  output$kpi3 <- renderInfoBox({
    conso_moy <- round(mean(data$Conso_5_usages_é_finale, na.rm = TRUE), 2)
    infoBox(HTML("Conso. <br> énergétique <br> moyenne"), paste0(conso_moy, " kWh"), icon = icon("bolt"), color = "blue")
  })
  
  output$table <- renderDataTable({
    datatable(data, options = list(pageLength = 5, autoWidth = TRUE))
  })

 
  # Histogramme des étiquettes DPE
  output$barDPE <- renderPlot({
    filtered_data <- data %>%
      filter(Type_bâtiment == input$type_bat, Etiquette_DPE %in% input$dpe_filter, 
             Conso_5_usages_é_finale >= input$conso_filter[1] & Conso_5_usages_é_finale <= input$conso_filter[2])
    
    # Calcul des effectifs et pourcentages
    bar_data <- filtered_data %>%
      group_by(Etiquette_DPE) %>%
      summarise(count = n()) %>%
      mutate(perc = count / sum(count) * 100)
    
    ggplot(bar_data, aes(x = Etiquette_DPE, y = count)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(title = "Répartition des étiquettes DPE", x = "Étiquette DPE", y = "Nombre de logements") +
      geom_text(aes(label = paste0(count, " (", round(perc, 1), "%)")), 
                vjust = -0.5, size = 5) +  # Ajoutez des étiquettes au-dessus des barres
      theme_minimal()  + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold")) # Ajoutez un thème minimal pour un meilleur rendu visuel
  })
  
  
  # Boxplot des coûts énergétiques par étiquette DPE
  output$boxplotDPE <- renderPlot({
    filtered_data <- data %>%
      filter(Type_bâtiment == input$type_bat, Etiquette_DPE %in% input$dpe_filter)
    ggplot(filtered_data, aes(x = Etiquette_DPE, y = Coût_total_5_usages)) +
      geom_boxplot(fill = "lightgreen") +
      labs(title = "Coûts énergétiques par étiquette DPE", x = "Étiquette DPE", y = "Coût total (en euros)")+
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  # Scatter plot Consommation vs Surface
  output$scatterConsoSurface <- renderPlot({
    filtered_data <- data %>%
      filter(Type_bâtiment == input$type_bat, Etiquette_DPE %in% input$dpe_filter)
    ggplot(filtered_data, aes(x = Surface_habitable_logement, y = Conso_5_usages_é_finale)) +
      geom_point(alpha = 0.5, color = "blue") +
      geom_smooth(method = "lm", col = "red") +
      labs(title = "Consommation énergétique vs Surface habitable", 
           x = "Surface habitable (m²)", y = "Consommation énergétique (kWh)") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  output$pieChart <- renderPlot({
    # Calcul des données pour le diagramme
    pie_data <- data %>%
      group_by(Etiquette_DPE) %>%
      summarise(count = n()) %>%
      mutate(perc = count / sum(count) * 100,
             label = paste0(Etiquette_DPE, ": ", count, " (", round(perc, 1), "%)"))  # Combinez étiquette, count et perc
    
    # Palette de couleurs personnalisée
    dpe_colors <- c("A" = "#319a31", "B" = "#33cc33", "C" = "#ccff33", 
                    "D" = "#ffff00", "E" = "#ffcc00", "F" = "#ff9a33", "G" = "#ff0000") 
    
    # Création du diagramme circulaire
    ggplot(pie_data, aes(x = "", y = perc, fill = Etiquette_DPE)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      scale_fill_manual(values = dpe_colors) +  
      labs(title = "Répartition par Étiquette DPE", x = NULL, y = NULL) +
      theme_void() +
      theme(legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5, face = "bold")) +
      geom_text(aes(label = label), position = position_stack(vjust = 0.5))  # Ajoutez des étiquettes au centre de chaque part
  })
  
  
  
  # Carte interactive
  output$cartePassoires <- renderLeaflet({
    filtered_data <- data %>% filter(Etiquette_DPE == input$filter_dpe)
    leaflet(filtered_data) %>%
      addTiles() %>%
      addCircleMarkers(
        ~lon, ~lat, 
        color = ~ifelse(Etiquette_DPE == "A", "#319a31",
                        ifelse(Etiquette_DPE == "B", "#33cc33",
                               ifelse(Etiquette_DPE == "C","#ccff33",
                                      ifelse(Etiquette_DPE == "D", "#ffff00",
                                             ifelse(Etiquette_DPE == "E", "#ffcc00",
                                                    ifelse(Etiquette_DPE == "F", "#ff9a33", "#ff0000")))))),
        radius = 3, opacity = 0.7, popup = ~paste("DPE:", Etiquette_DPE)
      ) %>%
      addLegend(
        "bottomright", 
        colors = c("#319a31", "#33cc33", "#ccff33", "#ffff00", "#ffcc00", "#ff9a33", "#ff0000"), 
        labels = c("A", "B", "C", "D", "E", "F", "G"),
        title = "Etiquettes DPE"
      )
  })
  
  # Filtre pour l'étiquette DPE dans l'onglet carte
  radioButtons("filter_dpe", "Choisir étiquette DPE :", 
               choices = levels(data$Etiquette_DPE), 
               selected = "F")
  
}

# Exécuter l'application
shinyApp(ui = ui, server = server)


```
## Lien de l'application
