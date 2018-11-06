#Reducing dimensionality

ibrary(vegan)

multivar <- multivar
## check to see where the species list are
head(multivar)
str(multivar)

## separate species and predictor columns
spp.data <- multivar[,7:46] ## select only species
pred.data <- multivar[,1:6] ## select only predictors and identifiers (predators)

## calculate shannon index
div.data <- diversity(spp.data, index = "shannon")
head(div.data)

## combine with predictors
div.data <- cbind(pred.data, div.data)

## see what it looks like
head(div.data)

## example of plot
mean.div <- div.data %>% group_by(Microsite, Elevation) %>% summarize(Average.Diversity=mean(div.data))

ggplot(mean.div) + geom_bar(aes(x=Elevation, 
                                y=Average.Diversity, 
                                fill=Microsite), 
                            color="black", 
                            stat="identity", 
                            position = "dodge") +
  scale_fill_brewer()
#3.2 Diversity indices

data(dune)

beta.n <- betadiver(dune)
plot(beta.n)

beta.whit <- betadiver(dune, "w")#Whittaker’s index
beta.whit

data(dune.env)

## join column to summarize by
dune2 <- cbind(dune.env["Manure"],dune)
dune2
dune.man <- dune2 %>% group_by(Manure) %>% summarise_all(sum) %>% data.frame(.) #spp presence per manure level
dune.man
BC.div <- vegdist(dune.man[,-1], method="bray") ## drop first column with treatment Manure, Bray-Curtis distance because abundance data

clus <- hclust(BC.div, "ward.D2") ## run cluster analysis
plot(clus, label=dune.man$Manure) ## add labels for manure levels

#3.3 Species accumulation curves
data(dune)

##more the species accumulation curve deviates from the rarefaction curve, 
#the less often individuals of the same species are found clustered together.
  

accum1 <- specaccum(dune, method="rarefaction")
accum2 <-specaccum(dune, method="random")

plot(accum1, col="blue", ylim=c(0,30), ci.type="polygon", ci.col="#00FFFF40")
plot(accum2, add=T, col="orange", ci.type="polygon", ci.col="#FFA50040")

#Rarifaction curve is blue
#Spp accumulation curve is orange. 
#We can estimate approximately the expected number of species that were missed

specpool(dune)
#see that We are missing btween 1-5 species based on the different approaches and standard error

data(dune.env)

specpool(dune, dune.env$Management)

#3.4 Effect-size estimates
# One of the more commonly refered effect-size estimates are Hedge’s g or Cohen’s d
#  relative interaction index (RII) was created by Cristina Armas
#Where this shines for community ecology, 
#is that treatment can represent the presence of competing of facilitating species, 
#and the control is the absence of that species.

?se
?rii.fun
head(div.data)
## convert identifier values as factors
div.data[,1:5] <- apply(div.data[,1:5], 2, as.factor)
str(div.data)
## Use RII function to calculate RII
rii.data <- rii.fun(div.data, "Microsite", Shrub, Open)

## see what it looks like
head(rii.data)#notice no microsite column

## example of plot showing effect at diff elevations
mean.rii <- rii.data %>% group_by(Elevation) %>% summarize(RII.diversity=mean(div.data),error=se(div.data))

## plot RII with confidence band
ggplot(mean.rii, aes(x=Elevation, y=RII.diversity)) + geom_point() +  geom_errorbar(aes(ymin=RII.diversity-error, ymax=RII.diversity+error), width=.2)
## Add line of significance
ggplot(mean.rii, aes(x=Elevation, y=RII.diversity)) + geom_point() +  geom_errorbar(aes(ymin=RII.diversity-error, ymax=RII.diversity+error), width=.2)+ geom_hline(yintercept=0, linetype="dashed", color = "red", lwd=1)




