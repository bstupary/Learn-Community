library(devtools)
install_github("afilazzola/LearnCommAnalysis", dependencies=TRUE, force=TRUE)
library(LearnCommAnalysis)

library(tidyverse)
library(knitr)

data(samplelong)

kable(samplelong[1:20,1:8])## select only the first 20 rows and 8 columns

str(samplelong[,1:8])
practice
practice <- practice
head(practice)
data.wide <- practice %>% spread(Species, Abundance) #to make wide
data.wide
data.long <- data.wide %>% gather(Species, Abundance, 6:13) #make long
data.long

data.summary <- data.long %>% group_by(Exclosure) %>%  summarise(avg=mean(Abundance), n=length(Abundance))
data.summary

data.summary <- data.long %>% group_by(Exclosure, Species) %>%  summarise(avg=mean(Abundance), n=length(Abundance))
data.summary

