summary(actphys_sedent)
head(actphys_sedent)
colnames(actphys_sedent)

#Pour récupérer les pondérations
data_actphys <- actphys_sedent %>% 
  left_join(description_indiv %>% select(NOIND, pond_indiv_adu_pop2, pond_indiv_enf_pop1), by = "NOIND")

data_actphys$pond <- ifelse(data_actphys$POPULATION == "Pop1 Individu", data_actphys$pond_indiv_enf_pop1, data_actphys$pond_indiv_adu_pop2)


actphys_sedent %>% 
  count(POPULATION) %>% 
  mutate(total = n/nrow(actphys_sedent))
#51.6% d'enfants et 48.4% d'adultes

actphys_sedent$POPULATION2 <- case_when(
  actphys_sedent$POPULATION == "Pop1 Individu" ~ "Enfant",
  actphys_sedent$POPULATION == "Pop2 Individu" ~ "Adulte"
)


boxplot <- function(variable){
  ggplot(actphys_sedent, aes(POPULATION2, eval(parse(text = variable)))) +
    geom_boxplot() +
    ylab(variable) +
    theme_classic()
}

boxplot("tv_duree")
boxplot("jvideo_duree")
boxplot("ordi_duree")


barplot <- function(variable){
  ggplot(actphys_sedent) +
    aes(x = factor(eval(parse(text = variable))), fill = factor(POPULATION2)) +
    geom_bar(position = "fill") +
    xlab(variable) +
    ylab("Proportion") +
    labs(fill = "Population") +
    scale_y_continuous(labels = scales::percent) +
    theme_classic() +
    scale_fill_brewer()
} 

barplot("transport_personnel")
barplot("transport_ecole")
barplot("eps_ouinon")

actphys_sedent$sedentarite_qual <- case_when(
  actphys_sedent$sedentarite == "0" ~ "Faible",
  actphys_sedent$sedentarite == "1" ~ "Modéré",
  actphys_sedent$sedentarite == "2" ~ "Elevé",
  is.na(actphys_sedent$sedentarite) ~ "Inconnu"
) 
barplot("sedentarite_qual")

actphys_sedent$profil_activite_qual <- case_when(
  actphys_sedent$profil_activite == "1" ~ "Comportement inactif et sédentaire",
  actphys_sedent$profil_activite == "2" ~ "Comportement inactif et non sédentaire",
  actphys_sedent$profil_activite == "3" ~ "Comportement actif et sédentaire",
  actphys_sedent$profil_activite == "4" ~ "Comportement actif et non sédentaire",
  is.na(actphys_sedent$profil_activite) ~ "Inconnu"
) 

barplot("profil_activite_qual")




barplot_categ <- function(variable){
  ggplot(actphys_sedent) +
    aes(x = factor(eval(parse(text = variable)))) +
    geom_bar(stat = "count") +
    xlab(variable) +
    ylab("Effectif") +
    theme_classic()
}

barplot_categ("club_nb")

summary(actphys_sedent[,25:83])

var_score <- names(actphys_sedent)[grep("_score", names(actphys_sedent))][-c(43,57,63)]
var_duree <- names(actphys_sedent)[grep("_duree", names(actphys_sedent))][-4]

library(FactoMineR)
install.packages("GGally")
library(GGally)

ggcorr(actphys_sedent[actphys_sedent$POPULATION2 == "Adulte", var_duree], size = 3, hjust = 0.75)
ggcorr(actphys_sedent[actphys_sedent$POPULATION2 == "Adulte", var_score], size = 3, hjust = 0.75)

ggpairs(actphys_sedent[, c("POPULATION2", var_duree)], aes(colour = POPULATION2))


summary(actphys_sedent[actphys_sedent$POPULATION2 == "Adulte", var_score])

#score = duree*MET

res.pca <- PCA(actphys_sedent[actphys_sedent$POPULATION2 == "Adulte", c("transport_personnel", "sedentarite_qual", "profil_activite_qual", var_duree, var_score)],
               quali.sup = c(1:3),
               scale.unit = TRUE,
               graph = FALSE)

install.packages("explor")
library(explor)
explor(res.pca)

library(ggpubr)
library(factoextra)

coord <- res.pca$ind$coord
res.kmeans <- kmeans(coord, centers = 3)

fviz_cluster(res.kmeans, data = coord, 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw())

#nombre maximal de classes
n_clusters <- 10

#within sum of squares error: wss
wss <- numeric(n_clusters)

for (i in 1:n_clusters) {
  #fit le modele : km.out
  km.out <- kmeans(coord, centers = i, nstart = 20)
  #sauvegarde la wss
  wss[i] <- km.out$tot.withinss
}

#on trace un scree plot
wss_df <- tibble(clusters = 1:n_clusters, wss = wss)

ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4) +
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  xlab("Nombre de clusters")


#on choisit le nombre de classes
k <- 3

set.seed(1234)

#on construit un modele avec k classes: km.out
km.out <- kmeans(coord, centers = k, nstart = 20)

data <- actphys_sedent %>% filter(POPULATION2 == "Adulte")
data$cluster_id <- factor(km.out$cluster)

ggplot(data, aes(profil_activite_qual, fill = cluster_id)) +
  geom_bar(stat = "count", width = 0.6, position=position_dodge())


colnames(description_indiv)

summary(description_indiv$imc)
summary(description_indiv$poids)
summary(description_indiv$taille)

description_indiv$POPULATION2 <- case_when(
  description_indiv$ech == "1" ~ "Adulte",
  description_indiv$ech == "2" ~ "Enfant"
)

ggplot(description_indiv, aes(x = poids, y = taille, colour = POPULATION2)) +
  geom_point(alpha = 0.5)
  
url <- "https://minio.lab.sspcloud.fr/projet-cartiflette/diffusion/shapefiles-test1/year%3D2022/administrative_level%3DREGION/crs%3D4326/FRANCE_ENTIERE%3Dmetropole/vectorfile_format%3D%27geojson%27/provider%3D%27IGN%27/source%3D%27EXPRESS-COG-CARTO-TERRITOIRE%27/raw.geojson"

region <- sf::st_read(url)

# Passons le fonds de carte dans le système de coordonnées de référence utilisé pour la FRance, Lambert 93 (code : 2154) au lieu de WGS 84
region <- region %>% sf::st_transform(2154)

# Représentons les contours de notre fond de carte
plot(sf::st_geometry(region))

region$NOM_M
region <- region %>% mutate(NOM_M=ifelse(NOM_M=="CORSE", "PROVENCE-ALPES-COTE D'AZUR", NOM_M))

description_x_fpq <- left_join(description_indiv, fpq, by="NOIND")



# Recodage de la variable région pour avoir les mêmes noms que dans notre fond de carte
description_x_fpq <- description_x_fpq %>% mutate(region_recode=case_when(region_adm_12cl==1 ~ "ILE-DE-FRANCE",
                                                                          region_adm_12cl==2 ~ "NORMANDIE",
                                                                          region_adm_12cl==3 ~ "CENTRE-VAL DE LOIRE",
                                                                          region_adm_12cl==4 ~ "PAYS DE LA LOIRE",
                                                                          region_adm_12cl==5 ~ "BRETAGNE",
                                                                          region_adm_12cl==6 ~ "HAUTS-DE-FRANCE",
                                                                          region_adm_12cl==7 ~ "GRAND EST",
                                                                          region_adm_12cl==8 ~ "BOURGOGNE-FRANCHE-COMTE",
                                                                          region_adm_12cl==9 ~ "AUVERGNE-RHONE-ALPES",
                                                                          region_adm_12cl==10 ~ "PROVENCE-ALPES-COTE D'AZUR",
                                                                          region_adm_12cl==11 ~ "OCCITANIE",
                                                                          region_adm_12cl==12 ~ "NOUVELLE-AQUITAINE",))




# Variable à représenter géographiquement : nombre de bière consommées par mois. 
biere_par_region <- description_x_fpq %>% group_by(region_recode) %>% summarise(freq_conso_biere_moyenne=round(mean(BA_biere_freq_M,na.rm=TRUE), 0))



# On crée un petit tableau avec nos régions et leurs attributs géographiques, 
# et surtout la variable qu'on vient de calculer (c'est-à-dire le nombre de bières consommées par mois par région en moyenne)
region_inca <- left_join(region,biere_par_region,by=c("NOM_M"="region_recode"))


ggplot(data=region_inca) +
  geom_sf(aes(fill=freq_conso_biere_moyenne)) +
  scale_fill_continuous(low="yellow", high="Red", name="Nombre de bières consommées par mois en moyenne") +
  labs(title="Nombre de bières consommées par mois en moyenne par région")

region_inca %>% 
  #mutate(part_cereales = round(100 * cereales/total_des_terres_labourables, -1)) %>% 
  tm_shape(projection = sf::st_crs(2154)) +
  tm_polygons("freq_conso_biere_moyenne",
              textNA="Valeur manquante",
              style = "jenks",
              palette = get_brewer_pal("Oranges", n = 5, contrast = c(0.2, 1)),
              title = "Consommation de bières") +
  tm_layout(main.title = "Nombre moyen de bières consommées \n par jour et par région",
            main.title.size = 1.2,
            bg.color = "skyblue",
            legend.position = c("left","bottom"),
            legend.bg.color = "white",
            legend.bg.alpha = .4,
            legend.outside = T,
            main.title.position = "left",
            frame = FALSE)
