library("readr")
library("dplyr")
library("stringr")
library("downloader")
library("rgeos")
library("rgdal") 
library("ggplot2")
library("viridis")
library("raster")
library("leaflet")+
library("htmltools")
library("htmlTable")
library("scales")
library("broom")
library("tidyr")
library("maptools")

URL <- "http://www3.inegi.org.mx/sistemas/microdatos/Descargas.aspx?sr=microdatos_archivos/encuesta_intercensal/2015/eic2015_"
#Download the encuesta intercensal microdata
for(i in 1:32) {
            download(str_c(URL, str_pad(i, 2, "left", "0"), "_csv.zip&ht=02"),
                     str_c("data/", i), mode = "wb")
            unzip(str_c("data/", i), exdir = "data")
            file.remove(str_c("data/", i))
}


getwd()
setwd("e:/Proyectos R/VIVIENDA")

VIVIENDA <- data.frame()
for (i in 1:32) {
            print(i)
            df <- read.csv(str_c("data/TR_VIVIENDA", str_pad(i,2, "left", "0"), ".CSV"), fileEncoding = "latin1", colClasses = c("ENT"="factor", "MUN"="factor"))
            
            VIVIENDA_STATE <- df %>%
                        group_by(ENT,NOM_ENT, MUN, NOM_MUN) %>%
                        summarise(total = sum(FACTOR)) %>%
                        left_join(
                                    df %>%
                                    group_by(ENT,NOM_ENT, MUN,NOM_MUN) %>%
                                    filter(INGR_PEROTROPAIS %in% c(1)) %>% #Si
                                    summarise ( INGR_PEROTROPAIS= sum (FACTOR))
                        )%>%
                        left_join(
                                          
                                    df %>%
                                    group_by(ENT,NOM_ENT, MUN,NOM_MUN) %>%
                                    filter(INGR_AYUGOB %in% c(5)) %>%  #Si
                                    summarise (INGR_AYUGOB = sum (FACTOR))
                        )%>%
                        left_join(                  
                                    df %>%
                                    group_by(ENT,NOM_ENT, MUN,NOM_MUN) %>%
                                    filter(INGR_JUBPEN %in% c(7)) %>%  #Si
                                    summarise (INGR_JUBPEN = sum (FACTOR))
                        )%>%
                        left_join(                  
                                          
                                    df %>%
                                    group_by(ENT,NOM_ENT, MUN,NOM_MUN) %>%
                                    filter(JEFE_SEXO %in% c(3)) %>%  #Mujer
                                    summarise (JEFE_SEXO = sum (FACTOR))
                        )%>%
                        left_join(                  
                        
                              df %>%
                              group_by(ENT,NOM_ENT, MUN,NOM_MUN) %>%
                              filter(JEFE_SEXO %in% c(3)) %>%  #Mujer
                              summarise (JEFE_SEXO = sum (FACTOR))
                        )%>%
                        mutate(PER_INGR_PEROTROPAIS = INGR_PEROTROPAIS/total) %>%
                        mutate(PER_INGR_AYUGOB = INGR_AYUGOB/total) %>%
                        mutate(PER_INGR_JUBPEN = INGR_JUBPEN/total) %>%
                        mutate(PER_JEFE_SEXO = JEFE_SEXO/total) %>%
                        arrange(desc(INGR_PEROTROPAIS))
            VIVIENDA <- rbind(VIVIENDA, VIVIENDA_STATE)
            rm(df)
            gc()
}

VIVIENDA$id<- str_c(VIVIENDA$ENT, VIVIENDA$MUN)
#Remesas % INGR_PEROTROPAIS
VIVIENDA$INGR_PEROTROPAIS[is.na(INGR_PEROTROPAIS$VAR)]<-0 # 22 Na
VIVIENDA$PER_INGR_PEROTROPAIS[is.na(VIVIENDA$PER_INGR_PEROTROPAIS)] <- 0
#Ingresos por transferencias gubernamentales INGR_AYUGOB
VIVIENDA$INGR_AYUGOB[is.na(VIVIENDA$INGR_AYUGOB)]<-0
VIVIENDA$PER_INGR_AYUGOB[is.na(VIVIENDA$PER_INGR_AYUGOB)] <- 0
#Ingresos por Jubilaciones y pensiones INGR_JUBPEN
VIVIENDA$INGR_JUBPEN[is.na(VIVIENDA$INGR_JUBPEN)]<-0
VIVIENDA$PER_INGR_JUBPEN[is.na(VIVIENDA$PER_INGR_JUBPEN)] <- 0
#Sexo del jefe del hogar JEFE_SEXO --- MUJER
VIVIENDA$JEFE_SEXO[is.na(VIVIENDA$JEFE_SEXO)]<-0
VIVIENDA$PER_JEFE_SEXO[is.na(VIVIENDA$PER_JEFE_SEXO)] <- 0
VIVIENDA$concat<-VIVIENDA$id

class(VIVIENDA$concat)

write_csv(VIVIENDA, "data/VIVIENDA2.csv") 

#si ya se hizo lo anterior antes se puede empezar desde aquí
#setwd("e:/Intercensal/Intercensal")
#VIVIENDA <- read_csv("./data/VIVIENDA1.csv")

muns = readOGR("map/mgm2013v6_2.shp", "mgm2013v6_2") 
states <- readOGR("map/mge2013v6_2.shp", "mge2013v6_2")

states_df <- fortify(states)  #id da el estado
bb <- bbox(as(extent(muns) , "SpatialPolygons" ) )
muns@data$id = as.numeric(muns@data$concat)
muns@data <- plyr::join(muns@data, VIVIENDA, by = "id")
muns_df <- fortify(muns,region = "concat")
muns_df <- plyr::join(muns_df, VIVIENDA, by="id")

theme_bare <-theme(axis.line=element_blank(),
                   axis.text.x=element_blank(),
                   axis.text.y=element_blank(),
                   axis.ticks=element_blank(),
                   axis.title.x=element_blank(),
                   axis.title.y=element_blank(),
                   panel.background=element_blank(),
                   panel.border=element_blank(),
                   panel.grid.major=element_blank(),
                   panel.grid.minor=element_blank(),
                   plot.background=element_blank())

####Mapa nacional 
MAPA <- ggplot()+
            geom_map(data = muns_df, map = muns_df, 
                     aes(map_id = id, x = long, y = lat, group = group, fill= PER_JEFE_SEXO),
                     color = "white", size = 0.04) +
            geom_polygon(data = states_df,
                         aes(long, lat, group = group),
                         color = "#aaaaaa", fill = NA, size = 0.3) +
            scale_fill_viridis("Porcentaje", trans = "sqrt", labels = percent) +
            coord_map() +
            labs(x = "", y = "", title = "Mujeres jefas del hogar>>")
coord_map("albers", lat0 = bb[2,1], lat1 = bb[2,2]) +
            theme_bw() +
            theme(legend.key = element_rect(fill = NA)) +
            theme_bare
ggsave("graphs/Jefas del hogar.png", plot = MAPA, dpi=500, width =14, height = 11)
MAPA

#Municipios m[as altos]
top <- head(VIVIENDA[order(-VIVIENDA$PER_VAR),],100)[,c(2,4,5,7)]
head(VIVIENDA)

#Mapa Estatal
MAPA_EST <- ggplot()+
      #geom_map(data = muns_df, map = muns_df, 
       #        aes(map_id = id, x = long, y = lat, group = group, fill= PER_VAR),
       #        color = "white", size = 0.04) +
      geom_polygon(data = states_df,
                   aes(long, lat, group = group),
                   color = "#aaaaaa", fill = NA, size = 0.3) +
      scale_fill_viridis("Porcentaje", trans = "sqrt", labels = percent) +
      coord_map() +
      labs(x = "", y = "", title = "Porcentaje de hogares en los que respondió <<Sí>> a la pregunta <<Álguna persona que vive en esta vivienda recibe dinero de alguien que vive en otro país>>")
coord_map("albers", lat0 = bb[2,1], lat1 = bb[2,2]) +
      theme_bw() +
      theme(legend.key = element_rect(fill = NA)) +
      theme_bare
MAPA_EST

ggsave("graphs/DINERO_EXTERIOR.png", plot = MAPA, dpi=500, width =14, height = 11)
MAPA
#mapa por estados
#edad del jefe del hogar por municipio
