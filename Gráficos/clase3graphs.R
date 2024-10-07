# Cargamos las librerias necesarias
library(c("ggplot2","tibble","gridExtra","dplyr","Lock5Data","ggthemes","fun","zoo","corrplot","maps","mapproj","ggthemes","purrr"))

# Seteamos directorio de trabajo
setwd("C:/Documents/ME-UNLP/Curso Inicial/Instrumentos Computacionales/TP R")

getwd()


# Cargamos las bases de datos 
df <- read.csv("data/gapminder-data.csv")
df2 <- read.csv("data/xAPI-Edu-Data.csv")


str(df)
str(df2)


# Grafico consumo de electricidad

dfs <- subset(df,Country %in% c("Germany","India","China","United States"))
var1<-"Electricity_consumption_per_capita"
var2<-"gdp_per_capita"
name1<- "Electricity/capita"
name2<- "GDP/capita"
# Change color and shape of points
p1<- ggplot(dfs,aes_string(x=var1,y=var2))+
  geom_point(color=2,shape=2)+xlim(0,10000)+xlab(name1)+ylab(name2)
p1
#Grouping points by a variable mapped to colour and shape
p2 <- ggplot(dfs,aes_string(x=var1,y=var2))+
  geom_point(aes(color=Country,shape=Country))+xlim(0,10000)+xlab(name1)+ylab(name2)
grid.arrange(p1, p2, nrow = 2)
p2


#Grafico corregido


fig1 <- ggplot(dfs,aes_string(x=var1,y=var2))+
  geom_point(aes(color=Country))+xlim(0,10000)+xlab("")+ylab("") +
  labs(title="Consumo de electricidad (eje X) vs PBI (eje Y)", 
       subtitle= "En términos per cápita")

fig1




fig2 = fig1 +  
  geom_text(x=7500, y=45000, label="Alemania", colour="olivedrab", size = 3.5) + 
  geom_text(x=9500, y=33000, label="Estados Unidos", colour="mediumorchid2", size = 3.5) + 
  geom_text(x=3800, y=11000, label="China", colour="indianred2", size = 3.5) + 
  geom_text(x=1000, y=8000, label="India", colour="deepskyblue2", size = 3.5) + 
 scale_color_manual(values=c("indianred2", "olivedrab", "deepskyblue2", "mediumorchid2")) +
  theme_minimal() + theme(legend.position = "none") +
  theme(
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "grey")
  ) +
  theme(panel.border = element_rect(colour = "grey", fill = NA),
            panel.grid.major = element_line(colour = "#E5E7E9", size=0.0001, linetype = "dashed"),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill="white")
   )


fig2



# Cantidad de peliculas categorizadas según genero realizadas durante 2013 por diferentes estudios cinematograficos

# Grafico 
dfn <- subset(HollywoodMovies2013, Genre %in% c("Action","Adventure","Comedy","Drama","Romance")
              & LeadStudio %in% c("Fox","Sony","Columbia","Paramount","Disney"))
p1 <- ggplot(dfn,aes(Genre,WorldGross)) 
p1
p2 <- p1+geom_bar(stat="Identity",aes(fill=LeadStudio),position="dodge")
p2
p3 <- p2+theme(axis.title.x=element_text(size=15),
               axis.title.y=element_text(size=15),
               plot.background=element_rect(fill="gray87"),
               panel.background = element_rect(fill="beige"),
               panel.grid.major = element_line(color="Gray",linetype=1)
)
p3


dfn <- subset(HollywoodMovies2013, Genre %in% c("Action","Adventure","Comedy","Drama","Romance")
              & LeadStudio %in% c("Fox","Sony","Columbia","Paramount","Disney"))
p1 <- ggplot(dfn,aes(Genre,WorldGross)) 
p1
p2 <- p1+geom_bar(stat="Identity",aes(fill=LeadStudio),position="dodge")
p2
p7b <- p2+theme_economist()+ggtitle("theme_economist()")+scale_colour_economist()
p7b




# Corrección

positions <- c("Action", "Comedy", "Drama", "Adventure", "Romance")

p1 <- ggplot(dfn,aes(x = Genre, y=WorldGross)) + geom_bar(stat="identity", fill = "lightseagreen") + scale_x_discrete(limits = positions)
p2 <- p1 +ggtitle("theme_minimal()") +  theme_minimal() +
  xlab("") + 
  ylab("") +
  labs(title = "Ganancia bruta mundial (USD) - peliculas 2013", subtitle="Por género") +
  theme(
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "grey")
  )

positions2 <- c("Fox", "Paramount", "Disney", "Sony", "Columbia")
p3 <- ggplot(dfn,aes(LeadStudio,WorldGross)) + geom_bar(stat="identity", fill = "mediumorchid") + scale_x_discrete(limits = positions2)
p4 <- p3 +ggtitle("theme_minimal()") +  theme_minimal() +
  xlab("") + 
  ylab("") +
  labs(subtitle="Por estudio cinematográfico", title = "") + theme(
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "grey")
  )
  
peliculascorr1 <- grid.arrange(p2,p4,ncol=2) 
