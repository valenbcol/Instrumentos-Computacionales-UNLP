############################################################

rm(list=ls())
library(tidyverse)
library(ggplot2)
# install.packages("RColorBrewer")                   
library("RColorBrewer")  

#Defino la ruta
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
#Esto es lógico porque el nivel educativo de todos aquellos que acceden al nivel
#universitario tienen, por definición de nivel_ed, al menos secundario terminado

table(eph_test$nivel_ed[eph_test$accesoeduc==1])

### Si eliminamos terciario: 

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



