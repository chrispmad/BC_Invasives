library(bcdata)
library(stringr)
library(bcinvadeR)
library(leaflet)

grab_aq_occ_data()
bcl = bcdata::bcdc_list()

bcl[str_detect(bcl, "permit") & str_detect(bcl, "fish")]

# [19] "fish-collection-permit-data-other-fish-species-1998-sir"                                             
# [20] "fish-collection-permit-data-salmon-species-1998-sir"                                                 
# [21] "fish-collection-permit-data-sport-fish-species-1998-sir"  

# Testing candidate fish permit layers
bcdc_query_geodata("fish-collection-permit-data-sport-fish-species-1998-sir")

pk = bcdc_query_geodata("aca81811-4b08-4382-9af7-204e0b9d2448") |> 
  filter(SPECIES_NAME %in% c("Pumpkinseed","Pumpkinseed sunfish","Pumpkinseed Sunfish")) |> 
  collect()

pk = pk |> sf::st_transform(4326)

bg = bcdc_query_geodata("aca81811-4b08-4382-9af7-204e0b9d2448") |> 
  filter(SPECIES_NAME %in% c("bluegill","Bluegill","Bluegill sunfish","Bluegill Sunfish")) |> 
  collect()

bg = bg |> sf::st_transform(4326)

# Spot Filters to check for Amalis' record.
delta_point = data.frame(spot = "delta", lat = 49.036487064881925, lng = -123.07011073757323) |> 
  sf::st_as_sf(coords = c("lng","lat"), crs = 4326) |> 
  sf::st_buffer(dist = 5000) # 5 km buffer

dewdney_point = data.frame(spot = "dewdney", lat = 49.163375812039206, lng = -122.19664164306508) |> 
  sf::st_as_sf(coords = c("lng","lat"), crs = 4326) |> 
  sf::st_buffer(dist = 5000) # 5 km buffer

pk_f = pk |> 
  sf::st_filter(delta_point)

bg_f = bg |> 
  sf::st_filter(dewdney_point)

leaflet() |> 
  addTiles() |> 
  addCircleMarkers(
    data = pk_f,
    label = ~paste0(SPECIES_NAME,"; ",OBSERVATION_DATE)
  )

# Mystery locations

# 1. Non-native fish species report (SU24-845896)

locs = data.frame(
  zone = rep(10,5),
  easting = c(504121,504132,504140,504138,504133),
  northing = c(5443807,5443796,5443776,5443752,5443734)
) |> 
  sf::st_as_sf(coords = c("easting","northing"), crs = 32610) |> 
  sf::st_transform(4326) |> 
  sf::st_buffer(dist = 1000) |> 
  sf::st_make_valid()

locs_d = sf::st_union(locs) |> 
  st_make_valid()

bcdc_query_geodata("aca81811-4b08-4382-9af7-204e0b9d2448") |> 
  filter(SPECIES_NAME %in% c("Pumpkinseed","Pumpkinseed sunfish","Pumpkinseed Sunfish")) |> 
  filter(INTERSECTS(locs_d)) |> 
  collect()

# No results!
