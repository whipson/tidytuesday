library(tidyverse)
library(lubridate)
library(leaflet)
library(htmlwidgets)
library(htmltools)
library(raster)
library(KernSmooth)
library(mapview)

sqrls <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv") %>%
  mutate(date = mdy(date))

sqrl_kde <- bkde2D(as.matrix(sqrls[, c("long", "lat")]), bandwidth = c(.0045, .0068), gridsize = c(1000, 1000))

kde_raster <- raster(list(x = sqrl_kde$x1, y = sqrl_kde$x2, z = sqrl_kde$fhat))

raster_data <- kde_raster@data@values

raster_data[which(raster_data < 250)] <- NA

raster_palette <- colorNumeric("YlOrRd", domain = raster_data, na.color = "transparent")

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    padding-left: 10px; 
    padding-right: 10px; 
    font-weight: bold;
    font-size: 30px;
  }
"))

title <- tags$div(tag.map.title, HTML('Squirrel Sightings in Central Park, NYC'))  

leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addRasterImage(kde_raster,
                 colors = raster_palette,
                 opacity = .8) %>%
  addLegend(pal = raster_palette,
            values = raster_data,
            title = "Squirrel Density") %>%
  setView(lng = mean(sqrls$long), lat = mean(sqrls$lat), zoom = 13.5) %>%
  addControl(title, className = "map-title") %>%
  mapshot(file = "~/squirrels.png", remove_controls = c("homeButton", "layersControl"))


