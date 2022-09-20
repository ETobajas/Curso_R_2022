
#####################################
          ###  GLM  ###


# Always plot your data first!!

# la variable respuesta y la variable predictora no tienen porque ser normal
# lo que tiene que ser normal son los residuos 

# package equatiomatic nos devuelve la estructura del modelo
# equatiomatic::extract_eq(modelo, use_coefs=TRUE)



# summary of lm model 
# Residual std error:desviacion tipica de los residuos = desviacion media de los observados frente a los predichos
# degrees of freedom = tamaño de la muestra (N) - numero de parametros estimados

# funcion coef(modelo)-> parametros del modelo
# funcion confint (modelo)-> intervalos de confianza al 95%

#library(”broom”)-> tidy(m1)
#library(”parameters”)-> parameters(m1)
#library(”effects”)-> summary(allEffects(m1))
#library(”report”)-> report(m1) = nos da un parrafo con la descripcion del modelo

#++++++++++++++++++++++++++++++++++++++
# visualising fitted model ##

#library(”effects”)-> plot(allEffects(m1))
#library(”visreg”)-> visreg(m1)
#library(”sjPlot”)-> plot_model(m1, type = ”eff”)

#+++++++++++++++++++++++++++++++++++++++
# model checking #

# hist(residuals(m1)
# plot (modelo)
# library(”performance”)->check_model(m1)
# library(”easystats”)-> model_dashboard(m1)


#++++++++++++++++++++++++++++++++++++++++
library(tidyverse)
library(here) #refiere la ruta a la carpeta del proyecto
library(tidylog) #informa sobre operaciones dplyr y tidyr
#install.packages("summarytools")
library(summarytools) #resume de forma clara y rápida datos numéricos y categóricos
library(knitr) #reportar datos en varios formatos



## Ejercicio ###
trees= read.csv("Data/trees.csv")

head(trees)
str(trees)
plot(height ~ as.factor(sex), data = trees)

# altura ~ sexo
m1 = lm(height ~ sex, data = trees)
summary(m1)

report::report(m1)
modelbased::estimate_contrasts(m1)

plot(allEffects(m1))

performance::check_model(m1)
plot(m1)

# altura ~ site
plot(height~site, data=trees)
m2=lm (height~site, data=trees)
summary(m2)

trees$site=as.factor(trees$site)
m2.1=lm (height~site, data=trees)
summary(m2.1)


modelbased::estimate_contrasts(m2.1)
plot(allEffects(m2.1))

performance::check_model(m2.1)

plot(simulate_parameter(m2.1))


#############################################

##  ggplot- visualizacion de datos ##
##

library(here) #refiere la ruta a la carpeta del proyecto
library(tidyverse) #incluye la librería ggplot2
library(ggplot2) #VISUALIZACIÓN DE DATOS
library(RColorBrewer) #paletas de color
library(plotly) #hacer los gráficos interactivos
library(patchwork) #combinar gráficos de ggplot


but <- read_csv(here("Data/butterfly_dataset.csv"))
glimpse(but)


but <- but %>%
  mutate(date = str_c(year,"-", month,"-", day)) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

glimpse(but)

but_sum <- but %>%
  group_by(plot, habitat, hab_type, date) %>%
  summarise(n_species = n(),
            abundance = sum(abundance))


ggplot(data = but_sum)

ggplot(but_sum, aes(x = date,y = abundance))+
geom_point(color = "purple",
           shape = "triangle",
           size = 3.8,
           alpha = 0.5)

ggplot(but_sum, aes(x = date,y = abundance, color = n_species)) +
geom_point()


ggplot(but_sum, aes(x = date, y = abundance,color = n_species)) +
  geom_point() +  scale_color_gradient(low = "gold", high = "red")

# diferentes paletas de color 
#library(RColorBrewer)
#library(rcartocolor)
#library(MetBrewer)


ggplot(but_sum, aes(x = date,
                    y = abundance,
                    color = habitat)) +
  geom_point()


ggplot(but_sum, aes(x = n_species,y = abundance,color = hab_type)) +
  geom_point(alpha = 0.5) + scale_color_manual(values = c("green","red"))



ggplot(but_sum, aes(x = n_species,y = abundance)) +
  geom_point() +
  geom_smooth()+
  geom_rug()    # te pinta las observaciones de tus datos en los ejes


ggplot(but_sum, aes(x = n_species,y = abundance,color = hab_type)) +
  geom_point() +
  geom_smooth()


# funcion coord_cartesian(ylim = c(0, 300)) -> Los puntos que quedan
# fuera del límite siguen siendo considerandos y se
# mantienen para el calculo de la línea de tendencia.


ggplot(but_sum, aes(x = date, y = n_species,  color = abundance)) +
  geom_point() +
  scale_color_distiller(palette = "Spectral") +
  labs(x = "Año",
       y = "Número de especies",
       title = "Muestreos de mariposas en Vietnan",
       subtitle = "Datos de Bonebrake et al. 2016",
       color = "Abundancia")


#library(plotly)-> funcion ggplotly() grafico interactivo

# Pie-chart
ggplot(but_sum,aes(x = habitat,fill = habitat)) +
  geom_bar() +
  coord_polar()


# Gráfico de barras apilados
but_sum %>%
  mutate(year = lubridate::year(date)) %>%
  ggplot(aes(x = as.factor(year),
             fill = habitat)) +
  geom_bar() +
  MetBrewer::scale_fill_met_d("Archambault")


# library (patchwork) para juntar plots

p1 <- ggplot(but_sum,aes(x = n_species, y = abundance)) +
  geom_point(color = "#FFB90F",alpha = 0.7) +
  geom_smooth(color = "grey70") +
  theme_light() +
  labs(x = "No. of species",
       y = "Abundance")
p1 + labs(title = "Butterfly census in tropical areas")


# tag_levels = 'A' # pone letras a los diferentes plots

# para guardar el grafico-> ggsave(plot_to_save, width = 20, units = "cm", filename = here("img/figure_1.pdf"))


# library ggdist  graficar datos con distribuciones amplias
# library ggrepel permite que las etiquetas no se sobrepongan 
# library GGally::ggpairs(dataset) exploracion de datos y relacion entre variables 