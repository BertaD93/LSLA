library(foreign)
library(car)
library(MASS)
library(ggplot2)
library(rgeos)
library(rgdal)
library(scales)
library(ggmap)
library(dplyr)
library(Cairo)
library(raster)
library(maptools)
library(readr)
library(ggrepel)
library(tidyr)

setwd("C:/Users/B Diaz/Google Drive/A. Investigacion/Politicas de Cuidado Infantil")
national_estatal <- readOGR(dsn = path.expand("Capitulo 5/Data_Dropbox/Mapas Desigualdades/MAPAS R/national"), "national_estatal")
national_estatal <- fortify(national_estatal, region="CVEGEO")
head(national_estatal)

inp <- ("Capítulo 4/Mapas")

carencia <- read_csv("Capítulo 4/Datos/carencia_ss.csv")
carencia <- carencia %>% gather(`2008`:`2018`, key = "year", value = "rate")


plotData <- left_join(carencia, national_estatal, by = "id")

greydiver<-c("grey92","grey72", "grey52","grey32", "grey2")


gr <- ggplot() + geom_polygon(data = plotData, aes(x = long, y = lat, group = group,                
                 fill = cut_number(rate, n = 5)),size=0.3, linetype=1, colour="grey85")+
                labs(title = "")+
                scale_fill_manual("", values=greydiver, na.value = "white", labels=c("30-46.6","47.7-54.5","54.6-62.5","62.6-71.6", "71.7-86"))+ 
                theme(line = element_blank(),                           
                axis.text=element_blank(),                       
                axis.title=element_blank(),                      
                panel.background = element_blank(), 
                strip.background = element_blank(),
                legend.title = element_text(size = 8), legend.text = element_text(size = 8)) +
                facet_wrap(~ year, ncol = 3)+
                coord_map()                                   

gr




ggsave(paste(inp, "facet_carenciass.png", sep="/"), plot=gr, width = 6, height = 4.5, dpi=600)

carencia_18 <- filter(carencia, year %in% c("2018"))

plotData <- left_join(carencia_18, national_estatal, by = "id")

greydiver<-c("grey92","grey72", "grey52","grey32", "grey2")


gr <- ggplot() + geom_polygon(data = plotData, aes(x = long, y = lat, group = group,                
                 fill = cut_number(rate, n = 5)),size=0.3, linetype=1, colour="grey85")+
                labs(title = "")+
                scale_fill_manual("", values=greydiver, na.value = "white", labels=c("30-44","45-49","50-59","60-69", "70-84"))+
                theme(line = element_blank(),                           
                axis.text=element_blank(),                       
                axis.title=element_blank(),                      
                panel.background = element_blank(), 
                strip.background = element_blank(),
                legend.title = element_text(size = 8), legend.text = element_text(size = 8)) +
                facet_wrap(~ year, ncol = 3)+
                coord_map()                                   

gr




ggsave(paste(inp, "carencia_18.png", sep="/"), plot=gr, width = 6, height = 4.5, dpi=600)


