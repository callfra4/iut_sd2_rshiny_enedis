## Partie 1 : Boucle de récuparation des données de l'API

```r
# Import des bibliothèques 
library(httr)
library(jsonlite)
library(dplyr)

# Chargement des codes postaux depuis le fichier CSV
adresses <- read.csv("adresses-69.csv", sep = ';', dec = ",")

# Extraction des codes postaux uniques
codes_postaux <- unique(adresses$code_postal)

# URL de base pour les logements existants et neufs
base_url_existants <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe-v2-logements-existants/lines"
base_url_neufs <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe-v2-logements-neufs/lines"

# Initialisation des deux dataframes vides pour stocker les résultats
df_existants <- data.frame()
df_neufs <- data.frame()

# Récupération des logements existants

for (code_postal in codes_postaux) {
  # Paramètres pour logements existants
  params_existants <- list(
    page = 1,
    size = 10000,
    select = "Identifiant__BAN,N°DPE,Code_postal_(BAN),Etiquette_DPE,Date_réception_DPE,Coût_total_5_usages,Type_bâtiment,Surface_habitable_logement,Surface_habitable_immeuble,Coût_chauffage,Coût_éclairage,Coût_ECS,Coût_refroidissement,Coût_auxiliaires,Conso_5_usages_par_m²_é_primaire,Conso_5_usages_é_finale,Année_construction",
    q = as.character(code_postal),
    q_fields = "Code_postal_(BAN)",
    qs = "Date_réception_DPE:[2021-01-01 TO 2024-12-31]"
  )
  
  # Encodage de l'URL pour logements existants
  url_encoded_existants <- modify_url(base_url_existants, query = params_existants)
  
  # Requête GET pour les logements existants
  response_existants <- GET(url_encoded_existants)
  
  if (status_code(response_existants) == 200) {
    content_existants <- fromJSON(rawToChar(response_existants$content), flatten = TRUE)
    
    if (!is.null(content_existants$result)) {
      df_temp <- as.data.frame(content_existants$result)
      df_existants <- bind_rows(df_existants, df_temp)
    }
  }
}


# Récupération des logements neufs

for (code_postal in codes_postaux) {
  # Paramètres pour logements neufs
  params_neufs <- list(
    page = 1,
    size = 10000,
    select = "Identifiant__BAN,N°DPE,Code_postal_(BAN),Etiquette_DPE,Date_réception_DPE,Coût_total_5_usages,Type_bâtiment,Surface_habitable_logement,Coût_chauffage,Coût_éclairage,Coût_ECS,Coût_refroidissement,Coût_auxiliaires,Conso_5_usages_par_m²_é_primaire,Conso_5_usages_é_finale",
    q = as.character(code_postal),
    q_fields = "Code_postal_(BAN)",
    qs = "Date_réception_DPE:[2021-01-01 TO 2024-12-31]"
  )
  
  # Encodage de l'URL pour logements neufs
  url_encoded_neufs <- modify_url(base_url_neufs, query = params_neufs)
  
  Requête GET pour les logements neufs
  response_neufs <- GET(url_encoded_neufs)
  
  if (status_code(response_neufs) == 200) {
    content_neufs <- fromJSON(rawToChar(response_neufs$content), flatten = TRUE)
    
    if (!is.null(content_neufs$result)) {
      df_temp <- as.data.frame(content_neufs$result)
      df_neufs <- bind_rows(df_neufs, df_temp)
    }
  }
}


#Rajouter les colonnes nécessaires 
df_neufs$Année_construction <- 2024
df_neufs$Logement <- "neuf"
df_existants$Logement <- "ancien"

#On rassemble neuf et existant : 
finale = rbind(df_existants,df_neufs)


# Sélectionner les colonnes id, longitude et latitude 
adresses <- adresses %>%
  select(id, lon, lat)

#Jointure entre le fichier adresses et finale
DF_Finale <- finale %>%
  left_join(adresses, by = c("Identifiant__BAN" = "id"))

# Exporter les fichiers vers des fichiers CSV
write.csv(df_existants, file = "df_existant1.csv", row.names = FALSE)
write.csv(df_neufs, file = "df_neuf1.csv", row.names = FALSE)
write.csv(DF_Finale, file = "DF_Finale.csv", row.names = FALSE)

``` 
