library(tidyverse)
library(here) 
library(tidylog) 
library(summarytools) 
library(knitr)
library(performance)



##############################
### Functional programming ###
##############################


# script to play with loops and functional using null models as an excuse

# a simple null model to test correlations----

#data
abundance<- c(1,3,4,7,8,13)
body_size<- c(9,6,3,3,1,1)

#correlation
plot(abundance, body_size)
cor(abundance,body_size)
cor.test(abundance, body_size)

# null model: crear un data set muy parecido al tuyo que rompa el patron de correlación
# let's breake the pattern
cor(abundance, sample(body_size, size = length(body_size), replace = FALSE))

# crear distribucion de valores rompiendo la regla y ver si parace nuestro valor esperado

# learn loops
out<- c()  #para guardar generar primero un objeto vacio

for(i in 1:10){
  out[i]<-paste("numero", i)
}
out

# now, we use loops to calculte a distribution of correlations between abundances
# and randomized body size
out<-rep(NA,99)
for(i in 1:99){
  out[i]<-cor(abundance, sample(body_size, size = length(body_size), replace = FALSE))
}
out
hist(out)
corr=cor(abundance,body_size)
lines(c(corr, corr), c(0,20), col="red")

# p-values
length(which(out < corr))/ length(out) # la correlacion es diferente a lo esperado por azar


# is our community uneven?
abundance
# let's calculate Pielou's index -> J= shannon/ log (S)
p<- abundance/sum(abundance)
shannon<- - sum(p * log2(p))
J<- shannon/log2(length(abundance))

# funcionalizamos
J <- function(x){
  p<- x/sum(x)
  shannon<- - sum(p * log2(p))
  shannon/log2(length(x))
}

J(x= abundance)


# is this evenness higher than expected?
out_ev<-sample(c(1:6), sum(abundance), replace = T)
abund<-table(out_ev)
eve<-J(abund)

# let's create the distribution
out<-c()
for(k in 1:99){
  out_ev<-sample(c(1:6), sum(abundance), replace = T)
  abund<-table(out_ev)
  out[k] <- J(abund)
}
hist(out)
lines (c(eve,eve),c(0,60), col="red")

#p-value
length(which(out < eve))/length(out)


##################################################################

### Generalised Linear Models ###

#Logistic regression


library(effects)

titanic <- read_csv(here("Data/titanic_long.csv"))
factor(levels(titanic$class))
str(titanic)
glimpse(titanic)

m<-lm(survived~class, data=titanic )
summary(m)
plot(allEffects(m))
plot(m)


table(titanic$class,titanic$survived)
plot(factor(survived)~factor(class), data=titanic)


m.glm<-glm(survived~class, data = titanic, family = binomial)
summary(m.glm)

allEffects(m.glm)   # obtener la probabilidad
plot(allEffects(m.glm))

# con otro paquete
library(modelbased)
estimate_means(m.glm)
estimate_contrasts(m.glm) #comparacion por pares

#obtener r cuadrado
library(performance)
r2(m.glm) 

#checking
binned_residuals(m.glm)

library("DHARMa")
simulateResiduals(m.glm, plot = TRUE)

check_predictions(m.glm)

# plot de calibracion: valores observados frente a predichos
#install.packages("predtools")
library(predtools)
titanic$surv.pred <- predict(m.glm, type = "response")
calibration_plot(data = titanic, obs = "survived", pred = "surv.pred",
                 x_lim = c(0,1), y_lim = c(0,1))



# supervivencia en funcion del sexo
table(titanic$sex,titanic$survived)
plot(factor(survived)~factor(sex), data=titanic)
m2.glm<-glm(survived~sex, data = titanic, family = binomial)
summary(m2.glm)

allEffects(m2.glm)   # obtener la probabilidad
plot(allEffects(m2.glm))

simulateResiduals(m2.glm, plot = TRUE)

# sexo y clase
table(titanic$class, titanic$survived, titanic$sex)
m3.glm<-glm(survived~class *sex, data = titanic, family = binomial)
summary(m3.glm)
allEffects(m3.glm)
plot(allEffects(m3.glm))

library(sjPlot)
plot_model(m3.glm, type = "int")

r2(m3.glm)

######################################################

#  Reproducible Workflows  #

