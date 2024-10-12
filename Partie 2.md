## Partie 2 : Rapport d'étude RMarkdown

## Contenus

- [x] Script du rapport statistique
- [x] Liens du document HTML : https://candicesd.github.io/iut_sd2_rshiny_enedis/


##  Script du rapport statistique

Il est possible de choisir sur quel paramètre la rapport statistique portera, en modifiant les paramètres (params) d'entrée : 
- code_postal : "tous" ou un des codes postaux du 69 (ex: 69100)
- Logement : "neuf" ou "ancien" ou "les deux groupés" (rapport sur toutes les données) ou "les deux comparés" (rapport sur toutes les données en dissociant les logements neufs et anciens) 
```
---
title: "Rapport Statistique"
output:
  html_document: default
  pdf_document: default
date: "`r Sys.Date()`"
params:
  code_postal: "tous"
  Logement: "les deux comparés"
---

```{r setup, include=FALSE, echo = FALSE, warning=FALSE,message=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
## Introduction : 
Ce rapport présente une étude statistique approfondie portant sur les logements du département du Rhône (69). En s’appuyant sur un ensemble de données assez variées, comprenant des informations telles que le type de logement (neuf ou ancien), la consommation énergétique totale, la consommation par mètre carré ou encore la surface habitable du logement, l’objectif de cette analyse est de répondre à plusieurs problématiques liées à la performance énergétique des logements.  
L’étiquette DPE (Diagnostic de Performance Énergétique) est souvent considérée comme un indicateur principal de la consommation d’énergie d’un bien immobilier. Dans cette étude, nous allons donc montrer que l’étiquette DPE est un réel indicateur pour la consommation énergétique.  Cependant, nous cherchons aussi à aller au-delà de cette simple évaluation en explorant d’autres facteurs qui pourraient potentiellement influencer la consommation énergétique des logements. À l’aide de diverses visualisations et analyses comparatives, nous examinerons si des paramètres tels que l’ancienneté du bâtiment, la surface, ou encore le type d’habitat (appartement ou maison) ont un impact significatif sur la consommation énergétique, et comment ces variables interagissent entre elles.

```{r paramètre , include=FALSE, echo = FALSE, warning=FALSE,message=FALSE}
library(dplyr)
#Encoadage des paramètres
DF_Finale =read.csv("DF_Finale.csv",header=TRUE, dec=".",sep=",")

param_choisi <- DF_Finale %>% 
  filter(params$code_postal == "tous" | Code_postal_.BAN. == params$code_postal)

if (params$Logement == "ancien") {
  param_choisi <- param_choisi %>% filter(Logement == "ancien")
  
} else if (params$Logement == "neuf") {
  param_choisi <- param_choisi %>% filter(Logement == "neuf")
  
} else if (params$Logement == "les deux comparés") {
  # Séparer les données pour les logements anciens et neufs
  param_choisi_ancien <- param_choisi %>% filter(Logement == "ancien")
  param_choisi_neuf <- param_choisi %>% filter(Logement == "neuf")
  
} else if (params$Logement == "les deux groupés") {
  # Regrouper les données pour les logements anciens et neufs
  param_choisi <- param_choisi %>% filter(Logement %in% c("ancien", "neuf"))
}
```
## Paramètres choisis pour ce rapport :

Code postal : `r params$code_postal`     
Type de logement : `r params$Logement`  

 

```{r nombre, include=FALSE, echo = FALSE, warning=FALSE,message=FALSE}
# nombre de logements 
nb_log = if (params$Logement == "les deux comparés") {
  paste("Dans ce jeu de donnée, il y à ", nrow(param_choisi_ancien) , "logements ancien et " ,nrow(param_choisi_neuf), "logements neufs." )
} else {
paste("Dans ce jeu de donnée, il y a ",nrow(param_choisi), "logements.")
}
```
## Présentation des données :
`r nb_log`

```{r moyenne, include = FALSE, echo = FALSE, warning=FALSE,message=FALSE}
# Calcul de la moyenne de consommation énergétique par m²

# Vérifier si l'utilisateur a sélectionné "les deux"
moy_log = if (params$Logement == "les deux comparés") {
  
  # Calculer la moyenne pour les logements neufs
  moyenne_neuf <- mean(param_choisi_neuf$Conso_5_usages_par_m._é_primaire, 
                       na.rm = TRUE)
  
  # Calculer la moyenne pour les logements anciens
  moyenne_ancien <- mean(param_choisi_ancien$Conso_5_usages_par_m._é_primaire, 
                         na.rm = TRUE)
  
  # Afficher les moyennes
  paste("La moyenne de consommation énergétique par m² des logements neufs est de  : ", round(moyenne_neuf, 2), " kWh/m²\n et celle des logements anciens est de : ", round(moyenne_ancien, 2), " kWh/m²\n.")
  
} else {
  # Calculer la moyenne pour le type de logement sélectionné (neuf ou ancien)
  moyenne <- mean(param_choisi$Conso_5_usages_par_m._é_primaire, na.rm = TRUE)
  
  # Afficher la moyenne
  paste("La moyenne de consommation énergétique par m² des logements est de : ", round(moyenne, 2), " kWh/m².\n", sep= "")
}

```
`r moy_log`



```{r répartition_DPE, echo = FALSE, warning=FALSE,message=FALSE}
library(dplyr)
library(ggplot2)
# Calculer la répartition des logements par étiquette DPE pour chaque type de logement
# Créer le graphique comparatif si "les deux comparés" est sélectionné
if (params$Logement == "les deux comparés") {
 repartition_DPE_neuf <- param_choisi_neuf %>%
  group_by(Etiquette_DPE) %>%
  summarise(Nombre_logements = n(), .groups = 'drop')
  repartition_DPE_ancien <- param_choisi_ancien %>%
  group_by(Etiquette_DPE) %>%
  summarise(Nombre_logements = n(), .groups = 'drop')
  # Créer le graphique pour les logements neufs
  plot_neuf <- ggplot(repartition_DPE_neuf, aes(x = Etiquette_DPE, y = Nombre_logements)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(title = "Répartition des logements neufs par étiquette DPE",
         x = "Étiquette DPE",
         y = "Nombre de logements") +
    theme_minimal() +
    theme(legend.position = "none")  # Supprimer la légende car il n'y a qu'un seul type de logement

  # Créer le graphique pour les logements anciens
  plot_ancien <- ggplot(repartition_DPE_ancien, aes(x = Etiquette_DPE, y = Nombre_logements)) +
    geom_bar(stat = "identity", fill = "orange") +
    labs(title = "Répartition des logements anciens par étiquette DPE",
         x = "Étiquette DPE",
         y = "Nombre de logements") +
    theme_minimal() +
    theme(legend.position = "none")  # Supprimer la légende car il n'y a qu'un seul type de logement

print(plot_neuf)
print(plot_ancien)

  
} else {
  repartition_DPE <- param_choisi %>%
    group_by(Etiquette_DPE) %>%
    summarise(Nombre_logements = n(), .groups = 'drop')
  
  ggplot(repartition_DPE, aes(x = Etiquette_DPE, y = Nombre_logements)) +
    geom_bar(stat = "identity", fill = "lightblue") +
    labs(title = paste("Répartition du nombre de logements par étiquette DPE (", params$Logement, ")", sep = ""),
         x = "Étiquette DPE",
         y = "Nombre de logements") +
    theme_minimal()
}
```
```{r analyse_répartition, include = FALSE, echo = FALSE, warning=FALSE,message=FALSE}
# Analyse de la répartition
texte_analyse4 <- if (params$Logement == "les deux comparés") {
  
  # Répartition DPE pour les logements neufs
  repartition_DPE_neuf <- param_choisi_neuf %>%
    group_by(Etiquette_DPE) %>%
    summarise(Nombre_logements = n(), .groups = 'drop')
  
  # Répartition DPE pour les logements anciens
  repartition_DPE_ancien <- param_choisi_ancien %>%
    group_by(Etiquette_DPE) %>%
    summarise(Nombre_logements = n(), .groups = 'drop')
  
  # Étiquettes les plus fréquentes
  DPE_ancien_max <- repartition_DPE_ancien$Etiquette_DPE[which.max(repartition_DPE_ancien$Nombre_logements)]
  DPE_neuf_max <- repartition_DPE_neuf$Etiquette_DPE[which.max(repartition_DPE_neuf$Nombre_logements)]
  
  # Texte d'analyse
  texte <- paste(
  "L'étiquette DPE la plus représentée pour les **logements anciens** est la ", DPE_ancien_max, ".",
    "<br>Pour les **logements neufs**, l'étiquette la plus représentée est la ", DPE_neuf_max, ".",
    "<br>Cela montre que les logements anciens et neufs ne présentent pas la même distribution, en globalité, les logements neufs présentent des étiquettes DPE plus faible que les logements anciens."
  )
  
} else {
  repartition_DPE <- param_choisi %>%
    group_by(Etiquette_DPE) %>%
    summarise(Nombre_logements = n(), .groups = 'drop')
  
  # Étiquette la plus fréquente
  DPE_max <- repartition_DPE$Etiquette_DPE[which.max(repartition_DPE$Nombre_logements)]
  
  # Texte d'analyse
  texte <- paste(
    "L'étiquette DPE la plus représentée est la ", DPE_max, "."
  )
}
```
`r texte`
```{r conso_DPE, echo = FALSE, warning=FALSE,message=FALSE}

# Charger les bibliothèques nécessaires
library(ggplot2)
library(dplyr)
if (params$Logement == "les deux comparés") {
# Calculer la consommation moyenne par m² pour chaque étiquette DPE
conso_moyenne_DPE_ancien <- param_choisi_ancien %>%
  group_by(Etiquette_DPE) %>%
  summarise(Conso_moyenne = mean(Conso_5_usages_par_m._é_primaire, na.rm = TRUE)) %>%
  filter(!is.na(Conso_moyenne))  # Filtrer les NA
conso_moyenne_DPE_neuf <- param_choisi_neuf %>%
  group_by(Etiquette_DPE) %>%
  summarise(Conso_moyenne = mean(Conso_5_usages_par_m._é_primaire, na.rm = TRUE)) %>%
  filter(!is.na(Conso_moyenne))  # Filtrer les NA

# Créer le graphique en barres
ancien = ggplot(conso_moyenne_DPE_ancien, aes(x = Etiquette_DPE, y = Conso_moyenne, fill = Etiquette_DPE)) +
  geom_bar(stat = "identity") +
  labs(title = "Consommation Énergétique Moyenne des logements anciens par m² \nselon l'Étiquette DPE",
       x = "Étiquette DPE",
       y = "Consommation Moyenne (kWh/m²)") +
  theme_minimal() +
  theme(legend.position = "none") +  # Supprimer la légende
  geom_text(aes(label = round(Conso_moyenne, 2)), vjust = -0.5, size = 4)  # Ajouter les étiquettes de valeur sur chaque barre
neuf = ggplot(conso_moyenne_DPE_neuf, aes(x = Etiquette_DPE, y = Conso_moyenne, fill = Etiquette_DPE)) +
  geom_bar(stat = "identity") +
  labs(title = "Consommation Énergétique Moyenne des logements neufs par m² \nselon l'Étiquette DPE",
       x = "Étiquette DPE",
       y = "Consommation Moyenne (kWh/m²)") +
  theme_minimal() +
  theme(legend.position = "none") +  # Supprimer la légende
  geom_text(aes(label = round(Conso_moyenne, 2)), vjust = -0.5, size = 4)  # Ajouter les étiquettes de valeur sur chaque barre
print(ancien)
print(neuf)

 DPE_ancien_max <- conso_moyenne_DPE_ancien$Etiquette_DPE[which.max(conso_moyenne_DPE_ancien$Conso_moyenne)]
  DPE_ancien_min <- conso_moyenne_DPE_ancien$Etiquette_DPE[which.min(conso_moyenne_DPE_ancien$Conso_moyenne)]
  DPE_neuf_max <- conso_moyenne_DPE_neuf$Etiquette_DPE[which.max(conso_moyenne_DPE_neuf$Conso_moyenne)]
  DPE_neuf_min <- conso_moyenne_DPE_neuf$Etiquette_DPE[which.min(conso_moyenne_DPE_neuf$Conso_moyenne)]
  
  conso_ancien_max <- max(conso_moyenne_DPE_ancien$Conso_moyenne)
  conso_ancien_min <- min(conso_moyenne_DPE_ancien$Conso_moyenne)
  conso_neuf_max <- max(conso_moyenne_DPE_neuf$Conso_moyenne)
  conso_neuf_min <- min(conso_moyenne_DPE_neuf$Conso_moyenne)
 # Texte d'analyse
  DPE_analyse <- paste(
    "<div style='clear: both;'></div>","Pour les **logements anciens**, l'étiquette DPE la plus représentée est ", 
    DPE_ancien_max, " avec une consommation moyenne de ", round(conso_ancien_max, 2), " kWh/m², tandis que l'étiquette avec la consommation la plus faible est ",
    DPE_ancien_min, " avec ", round(conso_ancien_min, 2), " kWh/m². \n",
    "<br>Pour les **logements neufs**, l'étiquette DPE la plus représentée est ", 
    DPE_neuf_max, " avec une consommation moyenne de ", round(conso_neuf_max, 2), " kWh/m², et l'étiquette avec la consommation la plus faible est ",
    DPE_neuf_min, " avec ", round(conso_neuf_min, 2), " kWh/m².\n",
    "<br>En comparant les deux types de logements, on observe que les logements anciens ont généralement une consommation énergétique moyenne ", 
    ifelse(conso_ancien_max > conso_neuf_max, "plus élevée", "plus faible"),
    " que les logements neufs pour l'étiquette DPE la plus représentée (", DPE_ancien_max, " vs ", DPE_neuf_max, ")."
  )
} else {
# Calculer la consommation moyenne par m² pour chaque étiquette DPE
conso_moyenne_DPE <- param_choisi %>%
  group_by(Etiquette_DPE) %>%
  summarise(Conso_moyenne = mean(Conso_5_usages_par_m._é_primaire, na.rm = TRUE)) %>%
  filter(!is.na(Conso_moyenne))  # Filtrer les NA

# Créer le graphique en barres
DPE = ggplot(conso_moyenne_DPE, aes(x = Etiquette_DPE, y = Conso_moyenne, fill = Etiquette_DPE)) +
  geom_bar(stat = "identity") +
  labs(title = "Consommation Énergétique Moyenne par m² selon l'Étiquette DPE",
       x = "Étiquette DPE",
       y = "Consommation Moyenne (kWh/m²)") +
  theme_minimal() +
  theme(legend.position = "none") +  # Supprimer la légende
  geom_text(aes(label = round(Conso_moyenne, 2)), vjust = -0.5, size = 4)  # Ajouter les étiquettes de valeur sur chaque barre
print(DPE)
 DPE_max <- conso_moyenne_DPE$Etiquette_DPE[which.max(conso_moyenne_DPE$Conso_moyenne)]
  DPE_min <- conso_moyenne_DPE$Etiquette_DPE[which.min(conso_moyenne_DPE$Conso_moyenne)]
  conso_max <- max(conso_moyenne_DPE$Conso_moyenne)
  conso_min <- min(conso_moyenne_DPE$Conso_moyenne)

  # Texte d'analyse
  DPE_analyse <- paste(
    "<div style='clear: both;'></div>","L'étiquette DPE la plus représentée est ", 
    DPE_max, " avec une consommation moyenne de ", round(conso_max, 2), " kWh/m², tandis que l'étiquette avec la consommation la plus faible est ",
    DPE_min, " avec ", round(conso_min, 2), " kWh/m²."
  )
}

```
`r DPE_analyse`

```{r écart-type, echo = FALSE, warning=FALSE,message=FALSE}
library(dplyr)
library(knitr)

if (params$Logement == "les deux comparés") {
  # Calculer l'écart-type pour les logements neufs et anciens 
  ecart_type_dpe_neuf <- param_choisi_neuf %>%
    group_by(Etiquette_DPE) %>%
    summarise(
      ecart_type_neuf = round(sd(Conso_5_usages_par_m._é_primaire, na.rm = TRUE), 2)
    )

  ecart_type_dpe_ancien <- param_choisi_ancien %>%
    group_by(Etiquette_DPE) %>%
    summarise(
      ecart_type_ancien = round(sd(Conso_5_usages_par_m._é_primaire, na.rm = TRUE), 2)
    )

  # Fusionner les deux jeux de données sur l'étiquette DPE pour avoir les deux écart-types
  ecart_type_dpe <- full_join(ecart_type_dpe_neuf, ecart_type_dpe_ancien, by = "Etiquette_DPE")

  # Afficher le tableau avec les colonnes Étiquette DPE, neuf et ancien
kable(ecart_type_dpe,
        col.names = c("Étiquette DPE", "Écart Type (neuf)", "Écart Type (ancien)"),
        caption = "Écart Type de la Consommation par Étiquette DPE (logements neufs et anciens)")

} else {
  # Calculer l'écart-type pour le type de logement sélectionné
  ecart_type_dpe <- param_choisi %>%
    group_by(Etiquette_DPE) %>%
    summarise(
      ecart_type_conso = round(sd(Conso_5_usages_par_m._é_primaire, na.rm = TRUE), 2)
    )
  
  # Afficher le tableau pour le type de logement sélectionné
kable(ecart_type_dpe, 
        col.names = c("Étiquette DPE", "Écart Type de Consommation"), 
        caption = paste("Écart Type de la Consommation par Étiquette DPE (logements", params$Logement, ")"))
 
}
```

```{r analyse_écart-type, include = FALSE, echo = FALSE, warning=FALSE,message=FALSE}
if (params$Logement == "les deux comparés") {
   DPE_neuf_max <- ecart_type_dpe$Etiquette_DPE[which.max(ecart_type_dpe$ecart_type_neuf)]
  DPE_neuf_min <- ecart_type_dpe$Etiquette_DPE[which.min(ecart_type_dpe$ecart_type_neuf)]
  ecart_type_neuf_max <- max(ecart_type_dpe$ecart_type_neuf, na.rm = TRUE)
  ecart_type_neuf_min <- min(ecart_type_dpe$ecart_type_neuf, na.rm = TRUE)

  DPE_ancien_max <- ecart_type_dpe$Etiquette_DPE[which.max(ecart_type_dpe$ecart_type_ancien)]
  DPE_ancien_min <- ecart_type_dpe$Etiquette_DPE[which.min(ecart_type_dpe$ecart_type_ancien)]
  ecart_type_ancien_max <- max(ecart_type_dpe$ecart_type_ancien, na.rm = TRUE)
  ecart_type_ancien_min <- min(ecart_type_dpe$ecart_type_ancien, na.rm = TRUE)

  # Analyse pour les deux types de logements
  texte_analyse_ecart_type <- paste(
    "Pour les **logements neufs**, l'étiquette DPE avec l'écart-type le plus élevé est la ", DPE_neuf_max, 
    " avec un écart-type de ", ecart_type_neuf_max, 
    ", tandis que l'étiquette avec l'écart-type le plus faible est la ", DPE_neuf_min, " avec ", ecart_type_neuf_min, ".<br>",
    "Pour les **logements anciens**, l'étiquette DPE avec l'écart-type le plus élevé est la ", DPE_ancien_max, 
    " avec un écart-type de ", ecart_type_ancien_max, 
    ", tandis que l'étiquette avec l'écart-type le plus faible est la ", DPE_ancien_min, " avec ", ecart_type_ancien_min, ".<br>",
    "En comparant les deux types de logements, on constate que l'écart-type de la consommation pour les logements neufs et anciens est plutôt similaire, hormis pour l'étiquette DPE G
. En effet, l’écart-type des logements anciens pour l’étiquette DPE G est largement supérieurs à celui des logements neufs ce qui amène à se questionner sur la variabilité des performances énergétiques des bâtiments anciens et la potentielle influence de facteurs tels que l'année de construction, la qualité des matériaux utilisés ou encore l'absence de rénovations énergétiques."
)
} else {
   DPE_max <- ecart_type_dpe$Etiquette_DPE[which.max(ecart_type_dpe$ecart_type_conso)]
  DPE_min <- ecart_type_dpe$Etiquette_DPE[which.min(ecart_type_dpe$ecart_type_conso)]
  ecart_type_max <- max(ecart_type_dpe$ecart_type_conso)
  ecart_type_min <- min(ecart_type_dpe$ecart_type_conso)

  # Analyse 
  texte_analyse_ecart_type <- paste(
    "Dans ce jeu de données pour les **logements", params$Logement, 
    "**, l'étiquette DPE avec l'écart-type le plus élevé est la ", DPE_max, 
    " avec un écart-type de ", ecart_type_max, 
    ", tandis que l'étiquette avec l'écart-type le plus faible est la ", DPE_min, 
    " avec un écart-type de ", ecart_type_min, ". Un écart-type plus élevé indique une plus grande variabilité dans la consommation énergétique pour cette étiquette DPE."
  )
}
```
`r texte_analyse_ecart_type`

```{r période_construction, echo = FALSE, warning=FALSE,message=FALSE}
# Créer une colonne pour les périodes de construction
if (params$Logement == "neuf" || params$Logement == "les deux comparés") {texte_analyse = paste("Pour ces paramètres, le graphique sur la **Répartition de la Consommation Moyenne par Période de Construction** n'est pas pertinent.")
  } else{
param_choisi$Periode_construction <- cut(param_choisi$Année_construction,
                               breaks = c(-Inf, 1960, 1970, 1980, 1990, 2000, 2010, Inf),
                               labels = c("Avant 1960", "1961 - 1970", "1971 - 1980", 
                                          "1981 - 1990", "1991 - 2000", "2001 - 2010", "Après 2010"),
                               right = TRUE)

library(dplyr)
library(ggplot2)
# Calculer la consommation totale par période de construction 
conso_par_periode <- param_choisi %>%
  filter(!is.na(Periode_construction), !is.na(Conso_5_usages_par_m._é_primaire)) %>%  # Exclure les NA dans les deux colonnes
  group_by(Periode_construction) %>%
  summarise(consommation_moyenne = mean(Conso_5_usages_par_m._é_primaire, na.rm = TRUE)) %>%
  filter(consommation_moyenne > 0)

# Créer le diagramme circulaire
diagramme = (ggplot(conso_par_periode, aes(x = "", y = consommation_moyenne, fill = Periode_construction)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Répartition de la Consommation Moyenne par Période de Construction",
       x = NULL, y = NULL) +
  theme_void() +
  theme(legend.title = element_blank()) +
  geom_text(aes(label = round(consommation_moyenne, 2)), 
            position = position_stack(vjust = 0.5), 
            color = "white"))
 print(diagramme) 
 conso_par_periode <- param_choisi %>%
    filter(!is.na(Periode_construction), !is.na(Conso_5_usages_par_m._é_primaire)) %>%
    group_by(Periode_construction) %>%
    summarise(consommation_moyenne = mean(Conso_5_usages_par_m._é_primaire, na.rm = TRUE)) %>%
    filter(consommation_moyenne > 0) # Exclure les périodes avec des consommations nulles
  
  # Récupération des consommations extrêmes et moyenne générale
  max_periode <- conso_par_periode[which.max(conso_par_periode$consommation_moyenne), ]$Periode_construction
  max_conso <- round(max(conso_par_periode$consommation_moyenne), 2)
  
  min_periode <- conso_par_periode[which.min(conso_par_periode$consommation_moyenne), ]$Periode_construction
  min_conso <- round(min(conso_par_periode$consommation_moyenne), 2)
  
  moyenne_globale <- round(mean(conso_par_periode$consommation_moyenne), 2)
  
  # Analyse 
  texte_analyse <- paste("<div style='clear: both;'></div>","Les logements construits", min_periode, 
                         "ont la consommation énergétique moyenne la plus faible avec", 
                         min_conso, "kWh/m². À l'inverse, les logements construits durant la période", 
                         max_periode, "présentent la consommation la plus élevée avec", 
                         max_conso, "kWh/m².",
                         "La consommation énergétique moyenne globale est de", moyenne_globale, "kWh/m².")
  # Comparaison avec moyenne globale
  if (min_conso < moyenne_globale && max_conso > moyenne_globale) {
    texte_analyse = paste(texte_analyse, "Cela montre une variation significative entre les différentes périodes de construction. Les logements plus récents tendent à consommer moins d'énergie probablement en raison des nouvelles normes énergétiques.")
  } else {
    texte_analyse = paste(texte_analyse, "Les variations sont moins marquées entre les périodes, ce qui pourrait suggérer une uniformité dans la performance énergétique des bâtiments.")
  }
}

```
`r texte_analyse`


```{r type_batiment, echo = FALSE, warning=FALSE,message=FALSE}


  # Créer le graphique à barres horizontales
if (params$Logement == "les deux comparés") {
  data1 <- param_choisi_neuf %>%
  mutate(Type_bâtiment_groupé = ifelse(Type_bâtiment %in% c("immeuble", "appartement"), 
                                       "Immeuble/Appartement", 
                                       Type_bâtiment))
data2 <- param_choisi_ancien %>%
  mutate(Type_bâtiment_groupé = ifelse(Type_bâtiment %in% c("immeuble", "appartement"), 
                                       "Immeuble/Appartement", 
                                       Type_bâtiment))
moyennes_conso1 <- data1 %>%
  group_by(Type_bâtiment_groupé) %>%
  summarise(moyenne_conso1 = mean(Conso_5_usages_par_m._é_primaire, na.rm = TRUE))
moyennes_conso2 <- data2 %>%
  group_by(Type_bâtiment_groupé) %>%
  summarise(moyenne_conso2 = mean(Conso_5_usages_par_m._é_primaire, na.rm = TRUE))

A = ggplot(moyennes_conso1, aes(x = moyenne_conso1, y = reorder(Type_bâtiment_groupé, moyenne_conso1))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Moyenne de consommation énergétique par m²  \npar type de bâtiment neuf",
       x = "Consommation moyenne (kWh/m²)",
       y = "Type de bâtiment") +
  theme_minimal()
B = ggplot(moyennes_conso2, aes(x = moyenne_conso2, y = reorder(Type_bâtiment_groupé, moyenne_conso2))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Moyenne de consommation énergétique par m²  \npar type de bâtiment ancien",
       x = "Consommation moyenne (kWh/m²)",
       y = "Type de bâtiment") +
  theme_minimal()
print(A)
print(B)
resultats_comparaison <- merge(moyennes_conso1, moyennes_conso2, by = "Type_bâtiment_groupé", all = TRUE)
  
 analyse_neuf <- if (all(abs(diff(moyennes_conso1$moyenne_conso1)) < 10)) {
    "<div style='clear: both;'></div> Les consommations énergétiques moyennes des bâtiments **neufs** sont relativement similaires entre les différents types de bâtiments."
  } else {
    "<div style='clear: both;'></div> Les consommations énergétiques moyennes des bâtiments **neufs** varient de manière notable entre les différents types de bâtiments. Par exemple, les immeubles ont tendance à consommer plus que les maisons."
  }

  analyse_ancien <- if (all(abs(diff(moyennes_conso2$moyenne_conso2)) < 10)) {
    "Les consommations énergétiques moyennes des bâtiments **anciens** sont relativement similaires entre les différents types de bâtiments."
  } else {
    "<div style='clear: both;'></div> Les consommations énergétiques moyennes des bâtiments **anciens** varient de manière notable entre les différents types de bâtiments. Par exemple, les immeubles ont tendance à consommer plus que les maisons."
  }
  analyse_conso = paste(analyse_neuf, "<br>", analyse_ancien, "<br>")

  # Analyse des différences
  analyse_comparaison = if (all(abs(resultats_comparaison$moyenne_conso1 - resultats_comparaison$moyenne_conso2)) < 10) {
    paste("<br>Les consommations énergétiques entre les types de batiments sont relativement similaires pour les logements neufs et anciens<br>")
  } else {
    paste("<br>Les consommations énergétiques entre les types de batiments sont relativement différentes pour les logements neufs et anciens<br>")
  }
  analyse_conso <- paste(analyse_conso, analyse_comparaison)

 
     } else {
  data <- param_choisi %>%
  mutate(Type_bâtiment_groupé = ifelse(Type_bâtiment %in% c("immeuble", "appartement"), 
                                       "Immeuble/Appartement", 
                                       Type_bâtiment))

moyennes_conso <- data %>%
  group_by(Type_bâtiment_groupé) %>%
  summarise(moyenne_conso = mean(Conso_5_usages_par_m._é_primaire, na.rm = TRUE))

I = ggplot(moyennes_conso, aes(x = moyenne_conso, y = reorder(Type_bâtiment_groupé, moyenne_conso))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = paste("Moyenne de consommation énergétique par m²  \npar type de bâtiment (logements", params$Logement,")"),
       x = "Consommation moyenne (kWh/m²)",
       y = "Type de bâtiment") +
  theme_minimal()
print(I)
analyse_conso =  if (nrow(moyennes_conso) > 1) {
    if (all(abs(diff(moyennes_conso$moyenne_conso)) < 10)) {
      paste("<div style='clear: both;'></div> Les consommations énergétiques sont presque similaires entre les types de bâtiments.<br>")
    } else {
      paste("<div style='clear: both;'></div> Les consommations énergétiques varient entre les types de bâtiments.<br>")
    }
  } else {
    paste("<div style='clear: both;'></div> Pas assez de types de bâtiments pour effectuer l'analyse.<br>")
  }
}
```
`r analyse_conso`
```{r nuage_point, echo = FALSE, warning=FALSE,message=FALSE}
library(ggplot2)
library(dplyr)
if (params$Logement == "les deux comparés") {
# Scatter plot Consommation vs Surface
param_choisi_ancien <- param_choisi_ancien %>%
  filter(Surface_habitable_logement > 10 & Surface_habitable_logement < quantile(Surface_habitable_logement, 0.95, na.rm = TRUE))  %>%  
    filter(Conso_5_usages_é_finale > 0 & Conso_5_usages_é_finale < quantile(Conso_5_usages_é_finale, 0.95, na.rm = TRUE)) 
  
# Créer le nuage de points avec la ligne de régression linéaire
nuage_ancien = (ggplot(param_choisi_ancien, aes(x = Surface_habitable_logement, y = Conso_5_usages_é_finale)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Surface Habitable des logements anciens vs Consommation Totale",
    x = "Surface Habitable (m²)",
    y = "Consommation Totale (kWh)"
  ) +
  theme_minimal())
# Scatter plot Consommation vs Surface
param_choisi_neuf <- param_choisi_neuf %>%
   filter(Surface_habitable_logement > 10 & Surface_habitable_logement < quantile(Surface_habitable_logement, 0.95, na.rm = TRUE))  %>%  # Supprimer les 5% supérieurs
    filter(Conso_5_usages_é_finale > 0 & Conso_5_usages_é_finale < quantile(Conso_5_usages_é_finale, 0.95, na.rm = TRUE))

# Créer le nuage de points avec la ligne de régression linéaire
nuage_neuf = (ggplot(param_choisi_neuf, aes(x = Surface_habitable_logement, y = Conso_5_usages_é_finale)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Surface Habitable des logements neufs vs Consommation Totale",
    x = "Surface Habitable (m²)",
    y = "Consommation Totale (kWh)"
  ) +
  theme_minimal())
print(nuage_ancien)
print(nuage_neuf)
} else {
# Scatter plot Consommation vs Surface
param_choisi <- param_choisi %>%
   filter(Surface_habitable_logement > 10 & Surface_habitable_logement < quantile(Surface_habitable_logement, 0.95, na.rm = TRUE))  %>%  # Supprimer les 5% supérieurs
    filter(Conso_5_usages_é_finale > 0 & Conso_5_usages_é_finale < quantile(Conso_5_usages_é_finale, 0.95, na.rm = TRUE))
# Créer le nuage de points avec la ligne de régression linéaire
ggplot(param_choisi, aes(x = Surface_habitable_logement, y = Conso_5_usages_é_finale)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Surface Habitable vs Consommation Totale",
    x = "Surface Habitable (m²)",
    y = "Consommation Totale (kWh)"
  ) +
  theme_minimal()
}
```

```{r nuage_point_analyse, echo = FALSE, warning=FALSE, message=FALSE, include = FALSE}

# Fonction pour générer l'analyse du nuage de points
analyser_nuage_points <- function(data, titre) {
  # Calculer la corrélation entre la surface habitable et la consommation totale
  correlation <- cor(data$Surface_habitable_logement, data$Conso_5_usages_é_finale, use = "complete.obs")
  
  # Modèle de régression linéaire
  model <- lm(Conso_5_usages_é_finale ~ Surface_habitable_logement, data = data)
  summary_model <- summary(model)
  
  # Extraire les paramètres du modèle
  slope <- summary_model$coefficients[2, 1]  # Coefficient de la pente
  intercept <- summary_model$coefficients[1, 1]  # Interception
  p_value <- summary_model$coefficients[2, 4]  # p-value de la pente

  # Évaluation de la relation
  relation <- if (p_value < 0.05) {
    if (slope > 0) {
      paste("Il existe une relation positive significative entre la surface habitable et la consommation énergétique. Cela signifie que lorsque la surface augmente, la consommation tend également à augmenter.")
    } else {
      paste("Il existe une relation négative significative entre la surface habitable et la consommation énergétique. Cela signifie que lorsque la surface augmente, la consommation tend à diminuer.")
    }
  } else {
    paste("Il n'existe pas de relation significative entre la surface habitable et la consommation énergétique.")
  }
  
  # Texte d'analyse
  nuage_analyse1 <- paste0(
    "Pour le graphique : **", titre, "**<br>",
    "- **Corrélation** entre la surface habitable et la consommation totale : ", round(correlation, 2), "<br>",
    "- **Pente de la régression** : ", round(slope, 2),"<br>",
    "- **Interception** : ", round(intercept, 2),"<br>",
    "- **p-value de la pente** : ", format.pval(p_value, digits = 4),"<br>",
    "- ", relation, "<br>"
  )
  
  
}

if (params$Logement == "les deux comparés") {
  # Analyser le nuage de points pour les logements anciens
 
  analyse_ancien = analyser_nuage_points(param_choisi_ancien, "Surface Habitable des logements anciens vs Consommation Totale")
  
  analyse_neuf = analyser_nuage_points(param_choisi_neuf, "Surface Habitable des logements neufs vs Consommation Totale")
  nuage_analyse1 = paste(analyse_ancien, analyse_neuf, sep = "\n\n")
} else {
  
  nuage_analyse1 = analyser_nuage_points(param_choisi, paste("Surface Habitable vs Consommation Totale (logements", params$Logement, ")"))
}
```
`r nuage_analyse1`

## Conclusion : 
À la suite de l’analyse de tous les graphiques, nous pouvons conclure que l’étiquette DPE est un réel indicateur de la consommation énergétique. De plus, nous avons pu observer que les logements neufs ont tendance à avoir des étiquettes DPE plus faible que les logements anciens. Nous pourrions penser que les techniques de constructions en terme d'isolation énergétique ce sont améliorés. 
Cependant, l’analyse a montré que certains facteurs qui pourraient influencer la consommation énergétique ne l’influence finalement pas ou peu comme les différent type de logement (maison/immeuble) par exemple dont la consommation énergétique ne varie pas explicitement d’un type à l’autre.


