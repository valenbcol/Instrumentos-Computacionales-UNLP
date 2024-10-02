############################################################
#       Seminario de Instrumentos Computacionales
#               Trabajo Práctico – R

rm(list=ls())

# Clase 1 -----

## Punto A ----

#Creación de valores 
valor_1<-5
valor_2<-"Economía"
valor_3<-FALSE
valor_4<-NA

#Imprimo para verificar sus valores
print(valor_1)
print(valor_2)
print(valor_3)
print(valor_4)

#Averiguar tipo de objeto
class(valor_1)
class(valor_2)
is.numeric(valor_3)
is.character(valor_4)

## Punto B ----
información <- c('Dpto', 'Edad', 'Estado civil', 'Mujer', 'Altura')
sebastian <-c(7, 22, 'Casado/a', FALSE, 1.75)
isolda <- c(2, 23, 'Viudo/a', TRUE, 1.60)
amelia <- c(5, 24, 'Soltero/a', TRUE, 1.64)

#Eliminamos el tercer elemento de un vector y lo guardamos en un nuevo vector
vector_sub <- información[-3]

#Imprimimos el segundo elemento de un vector
print(amelia[2])

#Del mismo vector, imprimimos el último valor 
print(amelia[length(amelia)]) 

#Imprimimos del primer al tercer valor del vector
print(amelia[1:3]) 

## Punto C ----

edificio <- data.frame(información, sebastian, isolda, amelia)

edificio[3,4]

objeto_1<- edificio[3, 4]
#objeto_1<-edificio$amelia[3] forma alternativa
objeto_2<- edificio[4,]
objeto_3<-edificio$información

# Clase 2 -----

## Importar base -----

#Defino la ruta
setwd("C:/Documents/ME-UNLP/Curso Inicial/Instrumentos Computacionales/TP R")

#Importo la base 
eph_ind <- read.csv("usu_individual_T322.txt", sep=";", dec=",")


## Punto 1 —-

library(tidyverse)

eph_sub <- eph_ind %>%
        filter(AGLOMERADO==02 & ESTADO==1)

eph_sub <- eph_sub %>%
        rename_with(tolower)

eph_first15 <- eph_sub %>% 
        select(1:15)

eph_last20 <- eph_sub %>% 
        select(1:(ncol(eph_sub)-20))

eph_2c <- eph_sub %>%
        select(codusu, nro_hogar, componente, starts_with("ch"))

sum(is.na(eph_sub$ch04))

eph_3 <- eph_sub %>% 
        mutate(MUJER = ifelse(ch04=="2", 1, 0))

chequeo <- eph_3 %>%
        filter(MUJER == 1 & ch04 == 1)

eph_4 <- eph_ind %>%
        rename_with(tolower)%>%
        filter(aglomerado==02 & estado==1) %>%
        select(codusu, nro_hogar, componente, starts_with("ch")) %>%
        mutate(MUJER = ifelse(ch04=="2", 1, 0))


## Punto 5 ----

sum(is.na(eph_ind$CH12))

eph_5 <- eph_ind %>%
        rename_with(tolower) %>%
        mutate(educ = case_when(
                ch12 %in% (1:3) ~ 'Baja',
                ch12 %in% (4:5) ~ 'Media',
                ch12 %in% (6:8) ~ 'Alta',
                TRUE ~ "NA" 
        ))

chequeo1 <- eph_5 %>%
        filter(ch12==9)

chequeo2 <- eph_5 %>%
        filter(ch12==99)

chequeo3 <- eph_5 %>%
        filter(ch12==0)

chequeo4 <- eph_5 %>%
        filter((ch12==9 | ch12==99 | ch12==0) & educ=="NA")

chequeo5 <- eph_5 %>%
        filter(educ=="NA")

tableeduc <- table(eph_5$educ)

print(tableeduc)

## Punto 7 ----

eph_7 <- eph_5 %>%
        filter(ch06 %in% (25:70))

tableeduc2 <- table(eph_7$educ)

print(tableeduc2)

## Punto 8----

media_salarial <- eph_7 %>% 
        mutate(mujer = ifelse(ch04=="2", "Mujer" ,"Hombre")) %>%
        group_by(mujer, educ) %>%
        summarise(mean_p21 = mean(p21, na.rm=TRUE))

print(media_salarial)


# Clase 3 -----

## Punto 7 ----

media_salarial_wide <- media_salarial %>% 
        pivot_wider(names_from = mujer,
        values_from = mean_p21 )

print(media_salarial_wide)

## Punto 8 ----

media_salarial_wide <- media_salarial_wide %>% 
        mutate(Diferencia = Hombre - Mujer)

print(media_salarial_wide)

## Punto 9 ----

media_salarial_long <- media_salarial_wide %>% 
        select(-Diferencia)%>% 
        pivot_longer(cols = c(Mujer, Hombre),
                     names_to = "sexo",
                     values_to = "mean_p21")

print(media_salarial_long)

# Clase 4 -----

## Punto 10 ----

library(ggplot2)

grafico_1 <- ggplot(
        data = media_salarial_long %>% filter(educ != 'NA'),
        mapping = aes(x = factor(educ, levels = c("Alta", "Media", "Baja")), y = mean_p21, fill = sexo)
) + 
        geom_col(position = "dodge")

print(grafico_1)

## Punto 10a ----

grafico_2 <- grafico_1 +
        labs(title= "Media salarial por educación según género", x="Educación", y ="Ingreso laboral", fill="Sexo")

print(grafico_2)

## Punto 10b ----
install.packages("RColorBrewer")                   
library("RColorBrewer")  

grafico_3 <- grafico_2 + 
        scale_fill_brewer(palette="Set1")

print(grafico_3)


##########################################################

# Extra: testeo para el examen —----


rm(list=ls())
library(ggplot2)
library(tidyverse)

setwd("C:/Documents/ME-UNLP/Curso Inicial/Instrumentos Computacionales/TP R")

#Importo la base 
eph_ind <- read.csv("usu_individual_T322.txt", sep=";", dec=",")



# Se crean cuatro variables para el análisis:
# acc_ter_uni = personas con acceso a educación superior (terciario y universitario)
# acc_ter_uni_sec = personas con acceso a educación superior (terciario y universitario), y que cumplen con la condición de secundario completo
# acc_uni = personas con acceso a educación superior universitaria (se excluye terciario)
# acc_ter_uni_sec = personas con acceso a educación superior universitaria (se excluye terciario), y que cumplen con la condición de secundario completo



eph_test<- eph_ind%>%
  rename_with(tolower) %>%
  filter(ch06 %in% (19:24) ) %>%
  select(codusu, nro_hogar, componente, nivel_ed, adecifr, starts_with("ch"), ) %>%
  mutate(Sexo = ifelse(ch04=="2", 'Mujer', 'Hombre')) %>%
  mutate(acc_ter_uni = ifelse( ch10==1 & ch12 %in% (6:7), 1,  0)) %>%
  mutate(acc_ter_uni_sec = ifelse( ch10==1 & ch12 %in% (6:7) & nivel_ed %in% (4:6), 1, 0)) %>%
  mutate(acc_uni = ifelse( ch10==1 & ch12 ==7, 1,  0)) %>%
  mutate(acc_uni_sec = ifelse( ch10==1 & ch12==7 & nivel_ed %in% (4:6), 1, 0)) %>%
  filter(adecifr %in% (1:10))%>%
  mutate(quintil = case_when(
    adecifr %in% (1:2) ~ 'Q1',
    adecifr %in% (3:4) ~ 'Q2',
    adecifr %in% (5:6) ~ 'Q3',
    adecifr %in% (7:8) ~ 'Q4', 
    adecifr %in% (9:10) ~ 'Q5'
  ))

accesoeduc <- eph_test %>% 
  group_by(mujer, quintil) %>%
  summarise(acc_ter_uni = mean(acc_ter_uni, na.rm = TRUE),
            acc_ter_uni_sec = mean(acc_ter_uni_sec, na.rm = TRUE),
            acc_uni = mean(acc_uni, na.rm = TRUE),
            acc_uni_sec = mean(acc_uni_sec, na.rm = TRUE))

view(accesoeduc)




grafico_acc_ter_uni <- ggplot(
  data = accesoeduc,
  mapping = aes(x = quintil , y = acc_ter_uni, fill = mujer)
) + 
  geom_col(position = "dodge") +
  labs(title= "Tasa de acceso a educación superior por quintil", x="", y ="", fill="Sexo") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.6), breaks = seq(0, 0.6, by = 0.1))+
  scale_fill_manual(values=c("#4C88C8", "#C773B6"))



print(grafico_acc_ter_uni)

grafico_acc_ter_uni_sec <- ggplot(
  data = accesoeduc,
  mapping = aes(x=quintil , y = acc_ter_uni_sec, fill = mujer)
) + 
  geom_col(position = "dodge") +
  labs(title= "Tasa de acceso a educación superior por quintil", subtitle= "(condicionado a secundario terminado)", x="", y ="", fill="Sexo") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.6), breaks = seq(0, 0.6, by = 0.1)) +
  scale_fill_manual(values=c("#4C88C8", "#C773B6"))


print(grafico_acc_ter_uni_sec)



#Al condicionar por haber terminado el secundario obtenemos el mismo resultado y 
#esto es lógico porque el nivel educativo de todos aquellos que acceden al nivel
#universitario tienen, por definición de nivel_ed, al menos secundario terminado

table(eph_test$nivel_ed[eph_test$accesoeduc==1])

###ELIMINANDO TERCIARIO: 

grafico_acc_uni <- ggplot(
  data = accesoeduc,
  mapping = aes(x = quintil , y = acc_uni, fill = mujer)
) + 
  geom_col(position = "dodge") +
  labs(title= "Tasa de acceso a educación superior por quintil", subtitle = "(excluyendo terciario)", x="", y ="", fill="Sexo") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.6), breaks = seq(0, 0.6, by = 0.1)) +
  scale_fill_manual(values=c("#4C88C8", "#C773B6"))

print(grafico_acc_uni)

grafico_acc_uni_sec <- ggplot(
  data = accesoeduc,
  mapping = aes(x=quintil , y = acc_uni_sec, fill = mujer)
) + 
  geom_col(position = "dodge") +
  labs(title= "Tasa de acceso a educación superior por quintil", subtitle= "(condicionado a secundario terminado y excluyendo terciario)", x="", y ="", fill="Sexo") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.6), breaks = seq(0, 0.6, by = 0.1)) +
  scale_fill_manual(values=c("#4C88C8", "#C773B6"))


print(grafico_acc_uni_sec)



