# Definir librerías 
x <- c("ggmap", "rgdal", "rgeos", "dplyr", "tmap", "broom")

# Instalar las librerías 
#install.packages(x) 

# Cargar las librerías requeridas
lapply(x, library, character.only = TRUE) 

# Definir el directorio de trabajo
setwd("C:/Clases Herramientas Computacionales/5. Data Visualization/videos_2_3") 

# Cargar el shapefile con los municipios de Londres y datos de población
lnd <- readOGR(dsn = "data/london_sport.shp") 

crime_data <- read.csv("data/mps-recordedcrime-borough.csv",
                       stringsAsFactors = FALSE)

# Extraer los datos de tipo Theft & Handling
crime_theft <- crime_data[crime_data$CrimeType == "Theft & Handling", ]

# Calcular el total de robos por municipio, guardar el resultado
crime_ag <- aggregate(CrimeCount ~ Borough, FUN = sum, data = crime_theft)

# Cambiar el nombre del dato NULL en el DF crime_ag (se corresponde con City of London)
crime_ag <- mutate(crime_ag, Borough = recode(Borough, 'NULL' = 'City of London'))

# Agregar una variable con la abreviatura de cada municipio
crime_ag$abbr <- toupper(substr(crime_ag$Borough, 0, 3))


# left join en base a las variables que contienen el nombre del municipio en cada base
lnd@data <- left_join(lnd@data, crime_ag, by = c('name' = 'Borough'))


# Mapa
tmap_map <- qtm(shp = lnd, fill = "CrimeCount", fill.palette = "Blues", fill.title = "Crímenes", main.title = "Total de robos por municipio en Londres (2011)", main.title.position = "center", text = 'abbr', text.size = 0.5, style="gray", fill.labels=c("1 a 20000", "20001 a 40000", "40001 a 60000", "60001 a 80000")) + tm_scale_bar(position = c("left", "bottom"))

tmap_save(tmap_map, "tmap_map.png")


# Para poder trabajar con la función ggmap es necesario transformar los objetos espaciales y formar un df 

lnd_f <- broom::tidy(lnd)

lnd$id <- row.names(lnd)

# Unimos los datos de la base original con la generada anteriormente
lnd_f <- left_join(lnd_f, lnd@data)

# Mapa usando ggplot 
ggplot_map <- ggplot(lnd_f, aes(long, lat, group = group, fill = CrimeCount)) +
  geom_polygon() + coord_equal() +
  labs(x = "Easting (m)", y = "Northing (m)",
       fill = "Cantidad de\ncrímenes") +
  ggtitle("Total de robos por municipio en Londres (2011)")
ggplot_map <- ggplot_map + geom_path(data = lnd_f, aes(x = long, y = lat, group = group), 
                                      color = "black", size = 1)
ggplot_map <- ggplot_map + scale_fill_distiller(palette = "YlOrBr", direction=1) #Agregamos la escala de colores
ggplot_map <- ggplot_map + theme(axis.ticks.x = element_blank(),axis.text.x = element_blank(),axis.ticks.y = element_blank(),axis.text.y = element_blank()) #Removemos los nombres y marcas de los ejes.
ggplot_map <- ggplot_map + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), legend.background = element_blank(), legend.box.background = element_rect(colour = "black")) #Eliminamos la grilla y el color de fondo, agregando también bordes a la leyenda.

 
ggsave("ggplot_map.png", plot = ggplot_map) 
