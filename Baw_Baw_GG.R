#load leaflet and other packages
#devtools::install_github('rstudio/leaflet')
library(leaflet)
library(dplyr)
library(knitr)
library(ggplot2)

#Load htmltools
library(htmltools)
library(htmlwidgets)

#Load spatial packages
library(rgdal)
library(rmapshaper)
library(geojsonio)
library(ggmap)

register_google("AIzaSyAXMVFK9WUOJv4EoTiTcP2mxcwv-nPp76w", account_type = "standard")

# library(devtools)
# install_git('https://github.com/yohanboniface/Leaflet.TileLegend')
# install_git('https://github.com/consbio/Leaflet.HtmlLegend')

GG_detections <- readOGR("data/Southern_Baw_Baw_GG_sightings.gpx")
GG.df <- GG_detections@coords %>% as.data.frame() %>% `colnames<-`(c("long", "lat")) %>% mutate(col = "Greater Glider")

#2017 TRP
#TRP
TRP.dissolve <- st_read("data/TRP_Dissolve/out.shp", stringsAsFactors = F) %>% st_cast(to = "POLYGON")
TRP.dissolve <- readOGR("data/TRP_Dissolve/out.shp", stringsAsFactors = F)

#TRP 2019
Approved_TRP_2019 <-  readOGR("data/Approved_TRP_2019/Approved_TRP_Boundaries__April_2019.shp", stringsAsFactors = F )
#Reformat to TRP.dissolve@proj4string
Approved_TRP_2019 <- spTransform(x = Approved_TRP_2019, CRSobj = TRP.dissolve@proj4string)

TRP_df <- fortify(Approved_TRP_2019) %>% mutate(col = "Logging Coupe \n(2019 TRP)")

map.sat <- get_map(location = c(lon = mean(GG_detections@bbox[1,]), lat = mean(GG_detections@bbox[2,])), zoom = 14,
               maptype = "satellite", source = "google")
baw_baw_map <- ggmap(map.sat, extent = "panel")



baw_baw_map + 
  theme_void()+
  geom_polygon(aes(x = long, 
                   y = lat, 
                   group = group, 
                   fill = col),
               colour = 'white', 
               alpha = .25, 
               data = TRP_df) +
  
  geom_point(aes(x = long, y = lat, colour = col), 
             data = GG.df, 
             inherit.aes = FALSE,
             shape = 21, 
             fill = "#6a51a3",
             size = 2.5,
             alpha = 0.75) +
  scale_fill_manual(name = NULL,
                    values = c("Logging Coupe \n(2019 TRP)" = "Grey", 
                               "Greater Glider" = "#6a51a3"))+
  scale_colour_manual(name = NULL, 
                      values = c("Greater Glider" = "Black")) +
  #set order of legend items (fill first)
  guides(fill = guide_legend(order = 2),  color = guide_legend(order = 1)) +
  #set legend position and vertical arrangement
  theme(legend.text = element_text(size = 9), legend.position = "right", legend.box = "vertical")

