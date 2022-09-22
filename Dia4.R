############################
## Analisis Multivariante ##
#############################

# this script will introduce multivariante analysis

# load libraries----
library(vegan)
library(mvabund)
library(here)
library(tidyverse)
library(reshape2)
library(ggplot2)

# load data----
data(iris)
head(iris)

#plot
pairs(iris[,1:4])
ir=iris[,1:4] #solo columnas numericas
ir_species <- iris[, 5]
cor(ir)


## PCA ##----

#basado en matriz de correlacion
#la funcion prcomp por defecto scale=False, pero es recomendable escalar porque muchas veces
# tenemos variables medidas en distintas unidades 
iris.pca <- prcomp(ir, center = T, scale. = T)
iris.pca
summary(iris.pca)
biplot(iris.pca)
predict(iris.pca, newdata = (ir[1:2,]))

# PCA plot bonito:
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
g <- ggbiplot(iris.pca, obs.scale = 1, var.scale = 1,
              groups = ir_species, ellipse = TRUE,
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal',
               legend.position = 'top')
print(g)
#more tricks
predict(pca, newdata=(tail(ir, 2)))



## PERMANOVA  and NMDS##----

# load data
Herb <- read.csv(here("Data/Herbivore_specialisation.csv"))
head(Herb)

# let's get a matrix (with reshape2)
Herbivores<- dcast(Herb, 
                   formula= Habitat + DayNight + Replicate + Mass ~ species, 
                   value.var = "abundance", fun.aggregate = sum)

head(Herbivores)

# simplify objets
Habitat <- Herbivores$Habitat
DayNight <- Herbivores$DayNight
Herb_community <- Herbivores[, 5:11]

#The basic is the distance measure you use:
#distance selection!
?dist
?vegdist
?betadiver


# NMDS
Herb_comunity.mds <- metaMDS(Herb_community,distance = "bray", k=2)
Herb_comunity.mds

plot(Herb_comunity.mds$points, col = as.factor(Habitat),pch = 16)

plot(Herb_comunity.mds$points, col = as.factor(DayNight),pch = 16)

# stress menor de 0.2 


# PERMANOVA

# hay diferencias entre habitat??
a <- adonis2(Herb_community ~ Habitat, method="bray")
a

b <- adonis2(Herb_community ~ Habitat * DayNight, method="bray")
b

# hay diferencias en la varianza dentro del habitat??
c <- betadisper(vegdist(Herb_community), group = Habitat )
c
anova(c)
boxplot(c)


# The mvabund way----

# volvemos a los datos originales
head(Herbivores)
Herb_spp <- mvabund(Herbivores[,5:11]) 

# explorar datos
boxplot (Herb_spp)
meanvar.plot(Herb_spp, xlab= "mean", ylab="var")

# fit manyglm
mod1 <- manyglm(Herb_spp ~ Habitat, family = "poisson")
plot(mod1)

mod2 <- manyglm(Herb_spp ~ Habitat, family = "negative_binomial")
plot(mod2)
anova(mod2)


######################################################3

### Mixed model ###

library(lme4) # este paquete no presenta p-value

trees= read.csv(here("Data/trees.csv"))
head(trees)

mm1_trees<- lmer(height ~ dbh + (1 | site), data=trees)
summary(mm1_trees)
coef(mm1_trees)
plot(allEffects(mm1_trees))

sjPlot::plot_model(mm1_trees, type = "re")

plot(mm1_trees)
library(performance)
check_model(mm1_trees)

DHARMa::simulateResiduals(mm1_trees, plot = TRUE, re.form = NULL)


# Varying intercepts and slopes

mixed.slopes <- lmer(height ~ dbh + (1 + dbh | site), data=trees)
summary(mixed.slopes)
