# Librerias
library(raster)
library(sf)
library(ggspatial)
library(sp)
library(ggplot2)
library(sf)
library(spData)
library(ggplot2)
library(cowplot)
library(rcartocolor)
library(raster)
library(RStoolbox)
library(landsat8)
library(ggspatial)
library(grid)
library(png)
library(ggrepel)
# Cargamos data
Bra         <- getData('GADM', country='Brazil', level=0) %>%st_as_sf()
Brazil      <- getData('GADM', country='Brazil', level=1) %>%st_as_sf() 
Brazi      <- getData('GADM', country='Brazil', level=3) %>%st_as_sf() 
SurAmerica     <- st_read ("SHP/SurAmerica.shp")  
SurAmeric      <- st_transform(SurAmerica,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Sergipe       =  subset(Brazi, NAME_1 == "Sergipe")
Area1 <- st_read ("Raster/Brazil/Area.shp") 
Area  <- shapefile("Raster/Brazil/Area.shp")
Area1_box = st_as_sfc(st_bbox(Area1))
band4 <- raster("Raster/Brazil/2021/GRANULE/L1C_T24LXP_A022643_20191023T125311/IMG_DATA/T24LXP_20191023T125311_B04.jp2")
band3 <- raster("Raster/Brazil/2021/GRANULE/L1C_T24LXP_A022643_20191023T125311/IMG_DATA/T24LXP_20191023T125311_B03.jp2")
band2 <- raster("Raster/Brazil/2021/GRANULE/L1C_T24LXP_A022643_20191023T125311/IMG_DATA/T24LXP_20191023T125311_B02.jp2")

# Combinancion de bandas agricultura
Sentinel_Natu = stack(band4, band3, band2)
##### A nivel de estudio 
#ambito <- mapedit::drawFeatures()       # Creamos el objeto
#ambito <- ambito %>% st_as_sf()         # Convertimos el objeto sf_ee

Poligonox  <-spTransform(Area, CRS=crs(band4))
PoligonoxDataFrame <- Poligonox %>% fortify
#cortar con la zona de estudio
paute17n  <- spTransform(Area , CRS=crs(band4))
bandas1   <- crop(Sentinel_Natu , extent(paute17n))
bandas    <- mask(bandas1,paute17n)
ventana= extent( 677270, 689830, 8801540, 8815920)

g2=ggRGB(Sentinel_Natu, r=1,g=2,b=3, stretch="lin", ext = ventana)+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  geom_polygon(col = 'gold3',
               fill = NA,
               data = PoligonoxDataFrame,
               aes(x = long, y = lat, group = group))+
  theme_bw()+
  theme(panel.grid.major = element_line(color = gray(.8),linetype = "dashed", size = 0.5),
        axis.text = element_text(colour = "black"),
        # plot.background = element_rect(colour = "gray",size = 2),
        axis.text.x  = element_text(face="bold", color="black", size=7),
        axis.text.y  = element_text(angle = 90,face="bold", color="black", size=7))+
  coord_equal()+
  labs(x = NULL, y = NULL)
g2.grob  <- ggplotGrob(g2)

g1=ggplot()+
  geom_sf(data = Sergipe, fill="darkgoldenrod2", color="black", size=0.5)+
  geom_sf(data = Area1,size=0.5)+
  geom_sf(data = Area1_box, fill=NA, color="red",size=0.5)+
  theme_void()
g1.grob  <- ggplotGrob(g1)

im=ggplot()+
  geom_sf(data =SurAmeric, color="black", fill="gray",size=0.5)+
  geom_sf(data = Brazil, color="black", fill="white",size=0.5)+
  geom_sf(data = Sergipe, fill="black",size=0.5)+
  coord_sf(xlim = c(-89.99999, 45), ylim = c( -55.9795 ,15))+
  annotation_custom(g1.grob, xmin = -35, xmax = -10, ymin =-30, ymax=-5)+
  annotation_custom(g2.grob, xmin = -10, xmax =  45, ymin =-50, ymax= 15)+
  geom_segment(aes(x=-22, xend=-5, y=-19, yend=10),  linetype = "solid", color = "black", size = 0.6) +
  geom_segment(aes(x=-22, xend=-5, y=-20, yend=-40), linetype = "solid", color = "black", size = 0.6) +
  geom_segment(aes(x=-38, xend=-30, y=-10, yend=-6),  linetype = "solid", color = "gold3", size = 0.6) +
  geom_segment(aes(x=-38, xend=-30, y=-10, yend=-20), linetype = "solid", color = "gold3", size = 0.6)+
  annotate(geom = "text", x = 20, y = 12, label = "Parque Nacional Serra de Itabaiana", 
           family="serif", color = "black", size = 3,fontface = "bold")+
  annotate(geom = "text", x = 20, y = -50, label = "Esta imagen, capturada por la \nmisión Copernicus Sentinel-2", 
           family="serif", color = "black", size = 2,fontface = "bold")+
  theme_void()+
  theme(panel.background = element_rect(fill = "white"))

# Exportacion
ggsave(plot = im ,"MAPAS/Brazil_area.png",
       units = "cm", width = 21,height = 10, dpi = 900)# guardar grafico  








