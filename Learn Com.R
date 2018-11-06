library(devtools)
install_github("afilazzola/LearnCommAnalysis", dependencies=TRUE, force=TRUE)


library(tidyverse)
library(knitr)

data(samplelong)

kable(samplelong[1:20,1:8])## select only the first 20 rows and 8 columns