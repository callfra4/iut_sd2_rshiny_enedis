## Partie 5 : Documentation fonctionnelle

### Intérêt et fonctionnalités majeures de chaque page de l'application

Page **Contexte** :

**Intérêt :** Cette page introduit l’utilisateur au projet en fournissant un contexte sur les données utilisées et en présentant des indicateurs clés. Elle permet de mieux comprendre les données et de les visualiser avant d'aller plus loin dans l'analyse.
**Fonctionnalités :** 
Tableau de données interactif : Il présente les données de manière claire et permet à l’utilisateur de les explorer via une interface interactive.
KPI (Indicateurs clés de performance) : Trois informations essentielles sont affichées en haut de la page :
Total des logements (Nombre total de logements dans le jeu de données), passoires thermiques (Nombre de logements classés F ou G sur l’étiquette DPE, considérés comme des passoires thermiques) et consommation énergétique moyenne (La consommation énergétique moyenne des logements, affichée en kWh).

Page **Visualisations** :

**Intérêt :** Cette page permet une analyse plus approfondie des données. L'utilisateur peut analyser la répartition des étiquettes DPE et leur relation avec divers paramètres, comme la consommation énergétique et la surface habitable.
**Fonctionnalités :**
L'utilisateur peut choisir de filtrer les données par type de bâtiment par étiquette DPE et par consommation énergétique.
La page contient plusieurs graphiques tel qu'un histogramme de répartition des étiquettes DPE, ce graphique montre la répartition des logements selon leur étiquette DPE. Il inclut également les pourcentages pour une compréhension plus précise. Un boxplot des coûts énergétiques, ce diagramme permet de visualiser la distribution des coûts énergétiques par étiquette DPE, révélant des différences entre les catégories. Un scatter plot : Consommation vs Surface habitable, il permet de voir la relation entre la consommation énergétique et la surface habitable, avec une régression linéaire ajoutée pour mieux comprendre les tendances. Et enfin, un diagramme circulaire (pie chart) ce graphique présente la proportion des logements en fonction de leur étiquette DPE, accompagné des pourcentages pour chaque catégorie.

Page **Carte interactive** :

**Intérêt :** Cette page fournit une vue géographique des logements en fonction de leur étiquette DPE. Elle permet d’analyser la répartition spatiale des bâtiments, en fonction de leur étiquette DPE.
**Fonctionnalités :**
Les logements sont représentés sur une carte avec des marqueurs colorés en fonction de leur étiquette DPE. L'utilisateur peut interagir avec la carte pour zoomer et explorer les logements individuellement. L’utilisateur peut également choisir d'afficher uniquement certains logements selon leur étiquette DPE, par exemple, pour se concentrer sur les passoires thermiques.

**Fonctionnalités majeures de l'application :**
L’application est protégée par un système de connexion avec un nom d’utilisateur et un mot de passe. De plus, chaque page propose des filtres dynamiques pour affiner les analyses selon différents critères (type de bâtiment, étiquette DPE, consommation énergétique). Cela permet à l’utilisateur d’explorer les données de manière personnalisée et en temps réel.

