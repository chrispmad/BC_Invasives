library(tidyverse)
library(dplyr)
library(leaflet)
library(sf)

lan_folder = "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/"

kmz_path <- paste0(
  lan_folder,
  "2 SCIENCE - Invasives/SPECIES/Yellow_Perch/Yellow Perch Eradication locations/Treated Rotonone Lakes R3.kmz"
)
# Get the folder containing the KMZ
kmz_dir <- dirname(kmz_path)
# Unzip directly into the source folder
# unzip(kmz_path, exdir = kmz_dir)
kml_path <- file.path(kmz_dir, "doc.kml")
yellow_treatment <- sf::st_read(kml_path)

named_wbs = read_rds(paste0(lan_folder,"2 SCIENCE - Invasives/AIS_R_Projects/LargeDataFiles/named_lakes_and_rivers.rds"))

named_wbs_treated <- named_wbs |>
  sf::st_join(
    yellow_treatment |> dplyr::select(Name),
    join = sf::st_intersects,
    left = FALSE
  )


#create a bounding box around the yellow treatments
the_bounds = st_bbox(yellow_treatment)

## bounds the named_wbs - we dont' want what is outside the treatment locations
named_wbs <- named_wbs |>
  sf::st_crop(the_bounds)

#leaflet map of the yellow_treatment and the named_wbs
# leaflet() |> 
#   addProviderTiles(providers$Esri.WorldTopoMap) |>
#   addPolygons(
#     data = named_wbs,
#     fillColor = "blue",
#     fillOpacity = 0.4,
#     color = "blue",
#     weight = 1
#   )  |> 
#   addPolygons(
#     data = named_wbs_treated,
#     fillColor = "red",
#     fillOpacity = 0.9,
#     color = "red",
#     weight = 1,
#     popup = ~(
#       waterbody
#     ),
#   )  
  


named_wbs_treated <- named_wbs_treated |>
  mutate(
    year = str_extract(Name, "\\b(19|20)\\d{2}\\b"),
    date = as.Date(paste0("01-01-", year), format = "%d-%m-%Y")
  )

saveRDS(named_wbs_treated,"app/www/treatment_events.rds")              

