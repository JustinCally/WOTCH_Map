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

register_google("APIKEY", account_type = "standard")

# library(devtools)
# install_git('https://github.com/yohanboniface/Leaflet.TileLegend')
# install_git('https://github.com/consbio/Leaflet.HtmlLegend')

GG_detections <- readOGR("data/Southern_Baw_Baw_GG_sightings.gpx")
GG.df <- GG_detections@coords %>% as.data.frame() %>% `colnames<-`(c("long", "lat"))
#TRP 2019
Approved_TRP_2019 <-  readOGR("data/Approved_TRP_2019/Approved_TRP_Boundaries__April_2019.shp", stringsAsFactors = F )
#Reformat to TRP.dissolve@proj4string
Approved_TRP_2019 <- spTransform(x = Approved_TRP_2019, CRSobj = TRP.dissolve@proj4string)

baw_baw <- get_googlemap()

map.sat <- get_map(location = c(lon = mean(GG_detections@bbox[1,]), lat = mean(GG_detections@bbox[2,])), zoom = 14,
               maptype = "satellite", source = "google")
baw_baw_map <- ggmap(map)

baw_baw_map + geom_point(aes(x = long, y = lat), data = GG.df)
