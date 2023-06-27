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
res.kmeans <- kmeans(coord, centers = 4)

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
  

