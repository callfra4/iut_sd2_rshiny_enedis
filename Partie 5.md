## Partie 5 : Documentation fonctionnelle

Page "Contexte" :
Intérêt : Cette page introduit l’utilisateur au projet en fournissant un contexte sur les données utilisées et en présentant des indicateurs clés. Elle permet de mieux comprendre les données avant d'aller plus loin dans l'analyse.
Fonctionnalités :
Tableau de données interactif : Il présente les données de manière claire et permet à l’utilisateur de les explorer via une interface interactive.
KPI (Indicateurs clés de performance) : Trois informations essentielles sont affichées en haut de la page :
Total des logements : Nombre total de logements dans le jeu de données.
Passoires thermiques : Nombre de logements classés F ou G sur l’étiquette DPE, considérés comme des passoires thermiques.
Consommation énergétique moyenne : La consommation énergétique moyenne des logements, affichée en kWh.
2. Page "Visualisations" :
Intérêt : Cette page permet une exploration visuelle des données. L'utilisateur peut analyser la répartition des étiquettes DPE et leur relation avec divers paramètres, comme la consommation énergétique et la surface habitable.
Fonctionnalités :
Sélection du type de bâtiment : L'utilisateur peut choisir de filtrer les données par type de bâtiment.
Filtrage par étiquette DPE et consommation énergétique : Des options sont disponibles pour affiner l'analyse en fonction de l'étiquette DPE ou des niveaux de consommation.
Histogramme de répartition des étiquettes DPE : Ce graphique montre la répartition des logements selon leur étiquette DPE. Il inclut également les pourcentages pour une compréhension plus précise.
Boxplot des coûts énergétiques : Ce diagramme permet de visualiser la distribution des coûts énergétiques par étiquette DPE, révélant des différences entre les catégories.
Scatter plot Consommation vs Surface habitable : Il permet de voir la relation entre la consommation énergétique et la surface habitable, avec une régression linéaire ajoutée pour mieux comprendre les tendances.
Diagramme circulaire (pie chart) : Un graphique circulaire présente la proportion des logements en fonction de leur étiquette DPE, accompagné des pourcentages pour chaque catégorie.
3. Page "Carte interactive" :
Intérêt : Cette page fournit une vue géographique des logements en fonction de leur étiquette DPE. Elle permet d’analyser la répartition spatiale des bâtiments, notamment ceux considérés comme des passoires thermiques.
Fonctionnalités :
Carte interactive avec marqueurs : Les logements sont représentés sur une carte avec des marqueurs colorés en fonction de leur étiquette DPE. L'utilisateur peut interagir avec la carte pour zoomer et explorer les logements individuellement.
Filtrage par étiquette DPE : L’utilisateur peut choisir d'afficher uniquement certains logements selon leur étiquette DPE, par exemple, pour se concentrer sur les passoires thermiques.
Fonctionnalités majeures :
Authentification utilisateur : Grâce à shinymanager, l’application est protégée par un système de connexion avec un nom d’utilisateur et un mot de passe. Cela permet de restreindre l’accès à certaines informations sensibles.
Exploration interactive des données : Chaque page propose des filtres dynamiques pour affiner les analyses selon différents critères (type de bâtiment, étiquette DPE, consommation énergétique). Cela permet à l’utilisateur d’explorer les données de manière personnalisée et en temps réel.
Visualisations avancées : L'application propose plusieurs types de graphiques (histogrammes, boxplots, diagrammes circulaires, cartes interactives) qui facilitent la compréhension des données de manière visuelle et intuitive.
Analyse géospatiale avec Leaflet : La carte interactive permet de visualiser la localisation des logements et d'obtenir des informations précises sur chaque point.
