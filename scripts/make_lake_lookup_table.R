library(bcdata)
library(tidyverse)

all_names_lakes = bcdc_query_geodata('freshwater-atlas-lakes') |>
  filter(!is.na(GNIS_NAME_1)) |>
  collect()

all_names_lakes = sf::st_transform(all_names_lakes, 4326)

lake_centroids = 1:nrow(all_names_lakes) |>
  purrr::map( ~ {
    initial_cent = sf::st_centroid(all_names_lakes[.x,])
    # Assuming it worked with st_centroid!
    output_point = initial_cent |>
      dplyr::select(GNIS_NAME_1, BLUE_LINE_KEY, WATERSHED_GROUP_ID)
    if(nrow(sf::st_filter(initial_cent,all_names_lakes[.x,])) == 0){
      # It didn't work with st_centroid! Time to get creative.
      # print("Initial st_centroid didn't work - doing it as mean of bounding box.")
      lk_bbox = sf::st_bbox(all_names_lakes[.x,]) |>
        as.matrix() |>
        as_tibble()
      lk_bbox$coord = c("xmin","ymin","xmax","ymax")
      new_coord = data.frame(x = mean(lk_bbox[c(1,3),]$V1),
                             y = mean(lk_bbox[c(2,4),]$V1)) |>
        sf::st_as_sf(coords = c("x","y"), crs = 4326)
      new_overlap_test = nrow(sf::st_filter(new_coord, all_names_lakes[.x,]))

      coord_bump = 0
      start_from_left = T

      if(new_overlap_test == 0) {
        while(new_overlap_test == 0 & coord_bump <= 19){
          coord_bump = coord_bump + 1
          # print(paste0("Attempting coord bump of ",coord_bump,"/20"))
          if(start_from_left){
            new_coord = data.frame(
              # Try making a coord that's 10% east from the bounding box's western limit.
              # If that fails, move over to 20%, then 30%, etc.
              x = lk_bbox[1,]$V1 + (coord_bump/20) * (lk_bbox[3,]$V1 - lk_bbox[1,]$V1),
              y = lk_bbox[2,]$V1 + (coord_bump/20) * (lk_bbox[4,]$V1 - lk_bbox[2,]$V1)
            ) |>
              sf::st_as_sf(coords = c("x","y"), crs = 4326)
          } else {
            new_coord = data.frame(
              # Try making a coord that's 10% east from the bounding box's western limit.
              # If that fails, move over to 20%, then 30%, etc.
              x = lk_bbox[1,]$V1 + (coord_bump/20) * (lk_bbox[3,]$V1 - lk_bbox[1,]$V1),
              y = lk_bbox[4,]$V1 - (coord_bump/20) * (lk_bbox[4,]$V1 - lk_bbox[2,]$V1)
            ) |>
              sf::st_as_sf(coords = c("x","y"), crs = 4326)
          }
          if(interactive()){
            print(ggplot() + geom_sf(data = all_names_lakes[.x,]) + geom_sf(data = new_coord))
          }
          new_overlap_test = nrow(sf::st_filter(new_coord, all_names_lakes[.x,]))
          if(coord_bump == 20 & start_from_left){
            start_from_left = F
            coord_bump = 0
          }
        }
      }
      output_point = new_coord |>
        dplyr::mutate(GNIS_NAME_1 = all_names_lakes[.x,]$GNIS_NAME_1,
                      BLUE_LINE_KEY = all_names_lakes[.x,]$BLUE_LINE_KEY,
                      WATERSHED_GROUP_ID = all_names_lakes[.x,]$WATERSHED_GROUP_ID)
    }
    output_point
  }, .progress = TRUE)

lake_centroids_bound = lake_centroids |>
  dplyr::bind_rows()

# Double check that all points lie within the original lake polygons.

intersect_test = 1:nrow(lake_centroids_bound) |>
  purrr::map( ~ {
    overlap_result = sf::st_intersects(lake_centroids_bound[.x,], all_names_lakes[.x,]) |>
      as.matrix() |>
      as_tibble()
    overlap_result |>
      dplyr::rename(overlaps = V1) |>
      dplyr::mutate(row_index = .x)
  }, .progress = T) |>
  dplyr::bind_rows()

intersect_test |>
  dplyr::filter(!overlaps)
# We are all good!

# Do a little overlap to get the NR Region of overlap.

the_regs = bcmaps::nr_regions() |>
  sf::st_transform(4326) |>
  dplyr::select(region_name = REGION_NAME) |>
  dplyr::mutate(region_name = stringr::str_remove(region_name, " Natural.*"))

# Write out table of wb names, coordinates, etc.
lake_centroids_bound = lake_centroids_bound |>
  sf::st_join(the_regs)

lake_centroids_output_tbl = lake_centroids_bound |>
  dplyr::mutate(latitude = sf::st_coordinates(geometry)[,2],
                longitude = sf::st_coordinates(geometry)[,1]) |>
  sf::st_drop_geometry()

lake_centroids_output_tbl = lake_centroids_output_tbl |>
  dplyr::mutate(wb_name = paste0(GNIS_NAME_1, " (", region_name,")")) |>
  dplyr::select(wb_name, BLUE_LINE_KEY, latitude, longitude)

saveRDS(lake_centroids_output_tbl, "app/www/lake_name_coordinate_lookup_tbl.rds")
