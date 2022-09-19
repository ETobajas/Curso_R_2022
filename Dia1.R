
library(tidyverse)
library(here) #refiere la ruta a la carpeta del proyecto
library(tidylog) #informa sobre operaciones dplyr y tidyr
#install.packages("summarytools")
library(summarytools) #resume de forma clara y rápida datos numéricos y categóricos
library(knitr) #reportar datos en varios formatos


#Datos
waste <- read_csv(here("Data/country_level_data.csv"))
glimpse(waste)
head(waste)
tail(waste)


# Ordenar variables
waste%>%arrange(population_population_number_of_people)
waste%>%arrange(desc(population_population_number_of_people)) # ordena de forma descendiente


# Cambiar nombre columnas:
waste%>% rename(population=population_population_number_of_people)

# Organizar columnas
waste %>% relocate(country_name, .before = iso3c)

# (Des)seleccionar variables:
waste %>% select(-region_id) # con menos no seleccionamos la variable


# Seleccionar sólo variables de interés:
waste_select <- waste %>%
  select(iso3c,
         region_id,
         country = country_name,   # cambiar el nombre directamente a la variable
         income_id,
         gdp,
         population = population_population_number_of_people,
         total_waste = total_msw_total_msw_generated_tons_year,
         starts_with("composition")) %>%   # selecciona todas las variables que empiezan por composition
  arrange(desc(total_waste))


glimpse(waste_select)

# tabla de resumen de datos (libreria summarytools)
dfSummary(waste_select$region_id)

waste_select%>%
  select(total_waste)%>%
  dfSummary()

dfSummary(waste_select) # para todo el dataset


# Extraer valores únicos (niveles) de una(s) variable(s):
waste_select %>%  distinct(income_id)   # para variables categoricas
waste_select %>%  distinct(region_id)

# Recodificar niveles de una variable:
waste_regions <- waste_select %>%
  mutate(region_id = recode(region_id,
                            "EAS" = "East_Asia_Pacific",
                            "NAC" = "North_America",
                            "SAS" = "South_Asia",
                            "LCN" = "Latin_America",
                            "ECS" = "Europe_Central_Asia",
                            "SSF" = "Sub-Saharan_Africa",
                            "MEA" = "Middle_East_North_Africa"))

# Agrupar datos y resumir:
waste_regions %>%
  group_by(region_id) %>%
  summarise(total_waste = sum(total_waste, na.rm = TRUE))


# Crear nueva variable - Ej: transformar basura a millones de toneladas
waste_regions %>%
  group_by(region_id) %>%
  summarise(total_waste = sum(total_waste, na.rm = TRUE)) %>%
  mutate(waste_mtons = total_waste/1000000)


# Filtrar datos:
waste_regions %>%
  filter(region_id == "Latin_America")


waste_regions %>%
  filter(region_id == "Europe_Central_Asia" & population <= 1000000)

# & = y, filtrar region id y population 
# | = o, filtra regrion id o population
# ! = filtrame por todo lo que no sea 
# para hacer cadena de datos %in% C()


#Crear nuevo factor:
# case_when crea una variable nueva creada en condicionales
waste_regions %>%
  mutate(pop_size = case_when(
    population >= 1000000 ~ "big",
    population < 1000000 & population > 500000 ~ "medium",
    population <= 500000 ~ "small")) %>%
  relocate(pop_size, .before = population)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++
### combinar base de datos con join  ###

# datos
world_data <- read_csv2(here("data/world_data.csv"))

glimpse(world_data)

continent <- world_data %>%
  select(iso_a3,
         country_name = name_long,
         continent)

glimpse(continent)


waste_world <- waste_regions %>%
  rename(iso_a3 = iso3c) %>%           # renombrar variable
  full_join(continent, by = "iso_a3")

waste_world <- waste_regions %>%
  rename(iso_a3 = iso3c) %>%
  left_join(continent, by = "iso_a3")


# ¿Qué paises se han quedado sin identificar?
waste_world %>%
  filter(is.na(continent)) %>%
  pull(country, iso_a3)    # pull funcion equivalente a $

#Buscar los paises que faltan en el dataset de continente:
continent %>%
  filter(country_name %in%
           c("Channel Islands",
             "Gibraltar",
             "Tuvalu",
             "Kosovo",
             "Taiwan"))

continent_corrected=continent %>%
  mutate(iso_a3= ifelse(country_name== "Kosovo", "XKX", iso_a3),
         iso_a3= ifelse(country_name== "Taiwan", "TWN ", iso_a3))


#Alternativa para buscar los paises que faltan en el dataset de continente:
#package stringr
continent %>%
  filter(str_detect(country_name,"Kosovo|Gibraltar|Tuvalu|Channel Islands|Taiwan"))


#Corregir un dato:
continent_corrected <- continent %>%
  mutate(iso_a3 = ifelse(country_name == "Kosovo","XKX", iso_a3)) %>%
  mutate(iso_a3 = ifelse(country_name == "Taiwan","TWN", iso_a3))


#Volver a combinar dataset:
waste_world <- waste_regions %>%
  rename(iso_a3 = iso3c) %>%
  left_join(continent_corrected, by = "iso_a3")

#Guardar dataset para el próximo día:
write_csv(waste_world, here("Data/waste_world.csv"))


#++++++++++++++++++++++++++++++++++++++++++++++

### library tidyr  ###

# Reestructurar el data set

# funcion pivot_wider() o spread()
# funcion pivot_longer() o gather()

composition <- waste_regions %>%
  pivot_longer(cols = starts_with("composition"), names_to = "composition", 
               values_to = "percent")
glimpse(composition)


distinct(composition, composition)

# limpia los nombre dentro de una variable
composition <- composition %>%
  mutate(composition=str_remove(composition,"composition_")) %>%
  mutate(composition=str_remove(composition,"_percent"))



composition %>%
  group_by(country) %>%
  mutate(per_sum = sum(percent, na.rm = TRUE)) %>%
  filter(per_sum >= 99.9) %>%
  filter(per_sum <= 100.1)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
#   Git / GitHub   #

library(usethis)
git_sitrep()

datos<- read_csv(here("Data/github_data.csv"))

plot(happiness~work.hours, data=datos)


ggplot(datos)+
  geom_point(aes(work.hours, happiness))

