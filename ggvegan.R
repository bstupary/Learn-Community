devtools::install_github("gavinsimpson/ggvegan")
library(ggvegan)
# NOT RUN {
data(dune)
data(dune.env)

sol <- cca(dune ~ A1 + Management, data = dune.env)
sol.plot <- autoplot(sol)
sol.plot + theme_gray()

ord <- metaMDS(dune)
head(fortify(ord))

sol.NMDS <- metaMDS(dune)
autoplot(sol.NMDS)
sol.NMDS + geom_contour()
