library(vegan)
library(LearnCommAnalysis)

## Load data
data(dune)

## conduct pca
pca <- rda(dune)
#PCA axis 3 should be under half of axis 2, otherwise your pca is not representing the data well in 2d.


## see output variables of PCA
summary(pca)
biplot(pca)

## 
## Load data, with a hellinger transformation for normality
dune.hell <- decostand(dune, method="hellinger")

## conduct pca
pca <- rda(dune.hell)

## see output variables of PCA
summary(pca)
biplot(pca)

## conduct DCA
dca <- decorana(dune.hell)
dca
## PCA & RDA <3 and 4< CA & CCA


# 4.2 Testing the gradient

data(varechem)
data(varespec)
str(varechem)
## transform data
species <- decostand(varespec, method="hellinger")

## create ordination
pca <- rda(species)
pca
## extract the scores from the ordination
score.vals <- scores(pca)$sites

## compare vis pearson correlation  
cor(score.vals, varechem)



# 4.3 Redundancy analysis
## load soil chem variables
data(varechem)

## conduct RDA
rda <- rda(species, varechem)

##Output
summary(rda)

anova(rda)#Marginal significance

plot(rda)#Called triplot, notice the colinearity

## 4.4 Cluster Analysis revisited

## calculate distances
dis <- vegdist(species, method="bray")

## cluster analysis
clus <- hclust(dis, "ward.D2")#Higharchial clustering method ward 2

## plot cluster
plot(clus)

## cut the tree based on a specific height
grp <- cutree(clus, h=0.7)

## cut the tree into four clusters
grp <- cutree(clus, 4)

library(cluster)
library(factoextra)

fviz_dend(clus, k = 4, # Cut in four groups
          cex = 0.7, cex.axis=2,  # label size
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
) 

## Compare environmental variables to cluster groups
boxplot(varechem$N ~ grp)

## Statistically compare groups
m1 <- aov(varechem$N ~ grp)
summary(m1)

## conduct ordination
ord <- rda(species)

## calculate priority
spp.priority <- colSums(species)

## plot ordination with clusters, 
plot(ord, type="n")
ordiellipse(ord, grp, lty = 2, col = "grey80", draw="polygon", alpha=150)
orditorp(ord, display = "species", cex = 0.7, col = "darkorange3", priority=spp.priority, air=0.8)
orditorp(ord, display = "sites", cex = 0.7, col = "darkslateblue", air=0.1)

## 4.5 Indiciator species analysis

library(indicspecies)

data(dune.env)

## Compare species assemblages among manure levels
indval <- multipatt(dune, dune.env$Manure, control=how(nperm=999))
summary(indval)

## Compare species assemblages among management strategies
indval2 <- multipatt(dune, dune.env$Management, control=how(nperm=999))
summary(indval2)







