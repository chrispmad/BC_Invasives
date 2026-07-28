library(tidyverse)
library(sf)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(rinat)

spp_name <- "Catostomus"

location = data.frame(lat = 49.05338918634006, lon = -122.35285337725774)

loc_sf = st_as_sf(location, coords = c("lon", "lat"), crs = 4326, remove = F)

loc_buff = loc_sf |>
  st_transform(3005) |> 
  st_buffer(5000) |> 
  st_transform(4326)

bbox <- st_bbox(loc_buff)

inat_salish <- get_inat_obs(
  taxon_name = spp_name,
  bounds = c(
    bbox["ymin"],
    bbox["xmin"],
    bbox["ymax"],
    bbox["xmax"]
  ),
  maxresults = 1000,
  quality = 'research'
)

inat_sf <- inat_salish |>
  filter(!is.na(longitude), !is.na(latitude)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)


leaflet() |>
  addProviderTiles(providers$Esri.WorldTopoMap) |>
  
  addPolygons(
    data = loc_buff,
    fillColor = "blue",
    fillOpacity = 0.2,
    color = "blue",
    weight = 2
  ) |>
  
  addCircleMarkers(data = loc_sf, color = "red", radius = 6) |>
  
  # addCircleMarkers(
  #   data = occs_in_area_multi,
  #   radius = 4,
  #   color = "black",
  #   fillColor = "orange",
  #   group = "Local data",
  #   popup = ~Species
  # ) |>
  
  addCircleMarkers(
    data = inat_sf,
    radius = 4,
    color = "darkgreen",
    fillColor = "lightgreen",
    fillOpacity = 0.8,
    group = "iNaturalist",
    popup = ~paste("User:", user_login)
  ) |>
  
  addLayersControl(
    overlayGroups = c("Local data", "iNaturalist"),
    options = layersControlOptions(collapsed = FALSE)
  )

write.csv(st_drop_geometry(inat_sf),
          "./output/Sucker_spp_iNat_fishtrap.csv",
          row.names = FALSE)





## Check Chrissy's files
base_dir <- "C:/Users/JPHELAN/Documents/R_projects/long_term_projects/AEB_CCzembor_SAR_visualizer/app/www/dfo"

# find the Nooksack Dace folder
nooksack_dir <- list.dirs(base_dir, recursive = FALSE, full.names = TRUE) |>
  keep(~ basename(.x) == "Salish Sucker")

# population subfolders
pop_dirs <- list.dirs(nooksack_dir, recursive = FALSE, full.names = TRUE)

# all rds files inside population folders
rds_files <- pop_dirs |>
  map(~ list.files(.x,
                   pattern = "\\.rds$",
                   full.names = TRUE,
                   recursive = TRUE)) |>
  unlist()



# read all sf objects
sf_list <- map(rds_files, readRDS)

# merge into one sf object
merged_sf <- bind_rows(sf_list) |>
  st_as_sf()


merged_sf_clean <- merged_sf |>
  st_make_valid() |>
  st_cast("MULTIPOLYGON", warn = FALSE)

# leaflet map
leaflet() |>
  addProviderTiles(providers$Esri.WorldTopoMap) |>
  
  addPolygons(
    data = merged_sf_clean,
    fillColor = "red",
    fillOpacity = 0.4,
    color = "red",
    weight = 1,
    popup = ~paste0(
      "<b>Species:</b> ", Common_Name_EN,
      "<br><b>Population:</b> ", Population_EN,
      "<br><b>Waterbody:</b> ", Waterbody,
      "<br><b>Status:</b> ", SARA_Status
    )
  ) 
