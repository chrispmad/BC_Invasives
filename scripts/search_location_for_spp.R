library(tidyverse)
library(sf)
library(dplyr)
library(leaflet)
library(leaflet.extras)


spp = "Western mosquitofish"

occs = sf::read_sf('app/www/occ_dat.gpkg')

location = data.frame(lon = 49.05338918634006, lat = -122.35285337725774)

loc_sf = st_as_sf(location, coords = c("lat", "lon"), crs = 4326, remove = F)

loc_buff = loc_sf |>
  st_transform(3005) |> 
  st_buffer(5000) |> 
  st_transform(4326)

occs_in_area = occs |> 
  filter(Species == spp) |> 
  st_filter(loc_buff)

leaflet() |>
  addProviderTiles(providers$Esri.WorldTopoMap) |>
  
  # Buffer polygon
  addPolygons(
    data = loc_buff,
    fillColor = "blue",
    fillOpacity = 0.2,
    color = "blue",
    weight = 2,
    group = "Buffer"
  ) |>
  
  # Original point
  addCircleMarkers(
    data = loc_sf,
    radius = 6,
    color = "red",
    fillColor = "red",
    fillOpacity = 1,
    popup = "Input location",
    group = "Location"
  ) |>
  
  # Occurrence points
  addCircleMarkers(
    data = occs_in_area,
    radius = 4,
    color = "black",
    fillColor = "orange",
    fillOpacity = 0.8,
    popup = ~Species,
    group = "Occurrences"
  ) |>
  
  addLayersControl(
    overlayGroups = c("Buffer", "Location", "Occurrences"),
    options = layersControlOptions(collapsed = FALSE)
  )

write.csv(occs_in_area, "./output/Mosquitofish_Fishtrap.csv")
