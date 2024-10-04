Repositorio con algunos trabajos realizados para el seminario de instrumentos computacionales de la Maestría en Economía de la Universidad Nacional de La Plata, usando principalmente R.

## Mapas

En la carpeta "Mapas" se encuentra el archivo CrimenesLondres.r utilizado para crear los siguientes mapas:
- Mapa de robos en Londres con ggplot 
- Mapa de robos en Londres con tmap 
En la carpeta "data" se encuentran los archivos "mps-recordedcrime-borough.csv", que contiene los datos de crímenes por municipio en Londres para 2011, y "london_sport.zip", con los límites de los municipios, su población en 2001 y la proporción de personas que practicaban deportes en ese año.

## Gráficos
El archivo AccesoEduc.R contiene la resolución de un problema particular: revisar este tweet (https://twitter.com/ltornarolli/status/1783220425511534930) y generar con datos de la encuesta de hogares de Argentina el mismo análisis (gráfico), pero comparando tasas de accesos entre hombres y mujeres, en vez de países. El acceso se define como las personas que: (1) declaran asistir actualmente a un establecimiento educativo (variable CH10); (2) cuyo nivel cursado es terciario o más (variable CH12), sobre el total de personas en esa franja de edad.
- Se provee el código en R para replicar el grafico del tweet pero con el análisis por género solo para Argentina.
- Además, es importante condicionar esta medida en haber terminado la secundaria. Por lo tanto, se replica el grafico anterior pero solo para personas que cumplan esa condición.
