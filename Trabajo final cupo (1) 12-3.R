## Ejercicio N° 1 

# Cargar la biblioteca necesaria

library(tidyverse)

# Se carga la base
Base <- txhousing

# Se analiza los datos del dataframe

summary(Base$median)
Base$city <- as.factor(Base$city)

summary(Base$city)

# se le asignan los nombres a los números de meses

Base$month <- factor(Base$month, levels = 1:12, labels = month.name)

# Se crea nueva variable para mes y año
Base$year_month <- paste(Base$month, Base$year, sep = " - ")

#Se crea nueva columna de fecha usando el primer día del mes, para que tenga el formato DATE

Base$dia <- as.Date(paste(Base$year, Base$month, "01", sep = "-"), format = "%Y-%B-%d")

# Crear el plot

ggplot(data = Base, aes(x = dia, y = median, group = city)) +
  geom_line(color = "gray") +                                       
  geom_line(data = subset(Base, city == "Collin County"), color = "red") +
  theme(legend.position = "none")+
  scale_y_continuous(breaks = seq(0, 300000, by = 100000),
                     labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  labs(x = NULL, title = "Mercado inmobiliario en Texas", color = "Variable",
       subtitle = "Precio mediano de venta en Collin County (rojo) y ciudades de Texas (gris)",
       y = "Dólares",
       caption = "Fuente: TAMU real state center")+ theme_minimal()

# Guardar el gráfico
ggsave("mercado_inmobiliario_texas.png", width = 10, height = 6)


##########################################################################################################


# Ejercicio N°2

library(tidyverse)         
library(sf)


install.packages("rnaturalearthdata") 

library(rnaturalearth)


mundo <- ne_countries(scale = "medium", returnclass = "sf")


# creamos un objeto con los datos correspondientes a Argentina 

argentina <- mundo %>% filter(sovereignt == "Argentina")


# se plotea el mapa del mundo con la proyección Robinson, haciendo la selección de Argentina. 

MAPA <- ggplot()+
  geom_sf(data = mundo, fill = 'grey', color = 'white') + 
  geom_sf(data = argentina, fill = 'purple',color= 'white')+
  coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")+
  labs(title = "Mapa mundial", color = "Variable",
       subtitle = "Argentina (púrpura) y resto del mundo (gris)",
       caption = "Fuente: Mapa de Natural Earth Data, proyección Robinson")+
  theme(plot.title = element_text(face = "bold"), 
        panel.background = element_rect(fill = "white"))

MAPA

# Guardar el mapa
ggsave("mapa_mundial_argentina.png", width = 12, height = 8, dpi = 300)

